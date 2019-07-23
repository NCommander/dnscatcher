.PHONY: coverage doc build_release

BASE_DIR=$(shell pwd)

all: build_release doc

doc:
	gnatdoc -w -P dnscatcher_project.gpr

build_release:
	gprbuild -XBUILD=RELEASE -Pdnscatcher_project.gpr -j0

pretty_print:
	find . -name *.adb | xargs \
		gnat pretty -rnb -Pdnscatcher_project.gpr
	find . -name *.ads | xargs \
		gnat pretty -rnb -Pdnscatcher_project.gpr

coverage:
	gprbuild -XBUILD=RELEASE -XCOVERAGE_ENABLED=TRUE -Pgnat/test_harness
	-rm -rf gcov

	# Build gcov files
	mkdir gcov
	export BASE_DIR=`pwd` && \
	cd gcov && \
	find $(BASE_DIR)/src -name *.adb | xargs gcov -abcfu -o $(BASE_DIR)/obj -s $(BASE_DIR)/src && \
	find $(BASE_DIR)/tests -name *.adb | xargs gcov -abcfu -o $(BASE_DIR)/obj -s $(BASE_DIR)/src

	# Run coverage test
	cd gcov && \
	ln -s ../tests && \
	GCOV_PREFIX=`pwd`  GCOV_PREFIX_STRIP=99 ../bin/test_runner

	# Process output
	find build -name *.gcno | xargs -I{} cp -u {} gcov

	# Needed cause genhtml is braindead
	cp build/test-obj/b__test_runner.adb $(BASE_DIR)
	cd gcov && lcov \
		--base-directory $(BASE_DIR) \
		-d . \
		--no-external \
		--capture \
		--output-file app.info && \
	genhtml app.info
	rm -f $(BASE_DIR)/b__test_runner.adb

distclean:
	-rm -rf bin lib gcov build
