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
 * This module handles setting up, loading, and managing the main libuv event
 * loop and dispatching processing calls into the Ada handlers for said loop.
 *
 * This is considerably more straightforward to do in C then trying to wrap
 * libuv in Ada code
 **/

#include <uv.h>
#include <stdlib.h>
#include <stdio.h>

/* Global variables */
uv_loop_t *loop;
uv_udp_t udp_send_socket;
uv_idle_t idle_handle;
uv_udp_t udp_recv_socket;
uv_async_t pending_data_handle;
struct sockaddr_in recv_addr;

int shutting_down = 0;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  buf->base = malloc(suggested_size);
  buf->len = suggested_size;
}

/* Ada functions */
struct raw_packet_record {
  char from_address[40];
  int from_port;
  char to_address[40];
  int to_port;
  void * raw_data;
  size_t raw_data_len;
};

extern void dc_internal_handle_inbound_packet(void * packet,
					      size_t length,
					      char * ip_addr,
					      int port);

extern struct raw_packet_record * dc_internal_spool_packet_to_uv();

/**
 * UDP received handler; handles loading UDP requests in from the network and loading them
 * into the DNSCatcher transaction system.
 */

void on_udp_read(uv_udp_t *req, ssize_t nread, const uv_buf_t *buf, const struct sockaddr *addr, unsigned flags) {
  struct sockaddr_storage incoming_addr;
  if (nread < 0) {
    fprintf(stderr, "Read error %s\n", uv_err_name(nread));
    uv_close((uv_handle_t*) req, NULL);
    free(buf->base);
    return;
  }

  if (nread > 0) {

    //printf("on_udp_read buf=%d addr=%d\n", buf, addr);
    char sender[255] = { 0 };
    if (addr != NULL) {
      uv_ip4_name((const struct sockaddr_in*) addr, sender, 16);
      //fprintf(stderr, "Recv from %s\n", sender);
    }

      dc_internal_handle_inbound_packet(buf->base, nread, sender, ntohs(((struct sockaddr_in*)addr)->sin_port));
  }
  free(buf->base);
}

/**
 * UDP send handler
 */

void on_send(uv_udp_send_t *req, int status) {
    if (status) {
        fprintf(stderr, "Send error %s\n", uv_strerror(status));
        return;
    }
}

/**
 * Async event handler denoting that data is ready to be send
 */

void raise_pending_data() {
  printf("raised pending data\n");
  uv_async_send(&pending_data_handle);
}

/**
 * Pull the data from the Ada side
 */

void pending_data_present(uv_async_t *req) {
  struct raw_packet_record * rpp;

  printf("pending_data_present()\n");
  /* Process packets until we get a NULL indicated we've pumped the queue clean */
  for ( ; ; ) {
    rpp = dc_internal_spool_packet_to_uv();
    if (rpp == NULL) {
      break;
    }

    printf("outbound packet record:\n");
    printf("\tremote ip: %s\n", rpp->from_address);
  }
}

/**
 * Clean up on controlled shutdown
 */

void dc_uv_sigint_handler(void) {
  uv_stop(uv_default_loop());
  shutting_down = 1;
};


/**
 * Idle loop handler
 */

void idle_handler(uv_idle_t * req) {

}

int dnscatcher_async_event_loop(void) {
  /* Load default loop to handle events */
  loop = uv_default_loop();

  // Create the idle loop notifier
  // This is required because it causes the loop to not block and allow for clean
  // exit

  uv_idle_init(loop, &idle_handle);
  uv_idle_start(&idle_handle, idle_handler);

  // Create pending data notifier
  uv_async_init(loop, &pending_data_handle, pending_data_present);

  // Setup UDP sockets
  // FIXME: Load this in from config properly

  uv_udp_init(loop, &udp_recv_socket);
  uv_ip4_addr("0.0.0.0", 1053, &recv_addr);
  uv_udp_bind(&udp_recv_socket, (const struct sockaddr *)&recv_addr, UV_UDP_REUSEADDR);
  uv_udp_recv_start(&udp_recv_socket, alloc_buffer, on_udp_read);

  uv_run(loop, UV_RUN_DEFAULT);
  if (shutting_down == 0) {
    printf("libuv unexpected returned!");
  }
  return 0;
}
