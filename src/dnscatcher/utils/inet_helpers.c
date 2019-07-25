/**
 * Copyright 2019 Michael Casadevall <michael@casadevall.pro>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/**
 * Helper functions for DNS Catcher written in C
 *
 * These primarily are wrappers around various functionality that
 * GNAT.Sockets simply doesn't provide good alternatives to. It's
 * compiled in C to deal with platform differences due to preprocessor
 * directives being needed for most of them
 */

#include <stdio.h>
#include <assert.h>
#include <arpa/inet.h>

#define ADA_INET4 1
#define ADA_INET6 2

void ada_inet_ntop(int ada_addr_family,
		   const void *src,
		   char * dst,
		   socklen_t size) {
  int family = 0;
  switch(ada_addr_family) {
    case ADA_INET4:
      family = AF_INET;
      break;
    case ADA_INET6:
      family = AF_INET6;
      break;
    default:
      assert("Unknown address family!");
      dst[0] = '\0';
  }

  inet_ntop(family, src, dst, size);
}
