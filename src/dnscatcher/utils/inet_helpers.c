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
