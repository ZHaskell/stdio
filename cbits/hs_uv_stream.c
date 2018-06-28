/*
 * Copyright (c) 2017-2018 Dong Han
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors or the names of any contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <hs_uv.h>

////////////////////////////////////////////////////////////////////////////////
//
// stream
//
// We reuse buffer_size_table as the result table, i.e. after haskell threads
// are unblocked, they should peek result(length, errcode..) from buffer_size_table

// This callback simply copy buffer from buffer table and buffer size table
void hs_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    HsInt slot = (HsInt)handle->data;
    hs_loop_data* loop_data = handle->loop->data;
    assert(slot < loop_data->size);
    buf->base = loop_data->buffer_table[slot];      // fetch buffer_table from buffer_table table
    buf->len = loop_data->buffer_size_table[slot];  // we ignore suggested_size completely
}

// We only do single read per uv_run with uv_read_stop
void hs_read_cb (uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf){
    HsInt slot = (HsInt)stream->data;
    hs_loop_data* loop_data = stream->loop->data;
    assert(slot < loop_data->size);

    if (nread != 0) {
        loop_data->buffer_size_table[slot] = nread;
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
        uv_read_stop(stream);
    }
}

int hs_uv_read_start(uv_stream_t* stream){
    return uv_read_start(stream, hs_alloc_cb, hs_read_cb);
}

void hs_write_cb(uv_write_t* req, int status){
    HsInt slot = (HsInt)req->data;
    uv_loop_t* loop = req->handle->loop;
    hs_loop_data* loop_data = loop->data;
    assert(slot < loop_data->size);

    loop_data->buffer_size_table[slot] = (HsInt)status;      // 0 in case of success, < 0 otherwise.

    loop_data->event_queue[loop_data->event_counter] = slot;   // push the slot to event queue
    loop_data->event_counter += 1;

    free_slot(loop, slot);  // free the uv_req_t

}

HsInt hs_uv_write(uv_stream_t* handle, char* buf, HsInt buf_siz){
    uv_loop_t* loop = handle->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_write_t* req = 
        (uv_write_t*)loop_data->uv_struct_table + (slot*sizeof(hs_uv_struct));
    req->data = (void*)slot;

    // on windows this struct is captured by WSASend
    // on unix this struct is copied by libuv's uv_write
    // so it's safe to allocate it on stack
    uv_buf_t buf_t = { .base = buf, .len = buf_siz };
    
    int r = uv_write(req, handle, &buf_t, 1, hs_write_cb);   // we never use writev: we do our own
                                                           // user-space buffering in haskell.
    if (r < 0) {
        free_slot(loop, slot);  // free the uv_req_t, the callback won't fired
        return (HsInt)r;
    } else return slot;
}

////////////////////////////////////////////////////////////////////////////////
//
// tcp

/* on windows uv_tcp_open doesn't work propery for sockets that are not
 * connected or accepted by libuv because the lack of some state initialization,
 * so we do it by manually set those flags
 *
 * referenes:   https://github.com/libuv/libuv/issues/397
 *              https://github.com/libuv/libuv/pull/1150
 */
#if defined(_WIN32)
void hs_uv_connection_init(uv_stream_t* handle){
  handle->flags |= UV_HANDLE_CONNECTION;
  handle->stream.conn.write_reqs_pending = 0;
  (&handle->read_req)->type = UV_READ;                                                        \
  (&handle->read_req)->u.io.overlapped.Internal = 0;  /* SET_REQ_SUCCESS() */ 
  handle->read_req.event_handle = NULL;
  handle->read_req.wait_handle = INVALID_HANDLE_VALUE;
  handle->read_req.data = handle;
  handle->stream.conn.shutdown_req = NULL;
}

int hs_uv_tcp_open(uv_tcp_t* handle, int32_t sock) {
  int r = uv_tcp_open(handle, (uv_os_sock_t)sock);
  if (r == 0) {
    hs_uv_connection_init((uv_stream_t*)handle);
    handle->flags |= UV_HANDLE_BOUND | UV_HANDLE_READABLE | UV_HANDLE_WRITABLE;
  }
  return r;
}
#else
int hs_uv_tcp_open(uv_tcp_t* handle, int32_t sock) {
  return uv_tcp_open(handle, (uv_os_sock_t)sock);
}
#endif

void hs_connect_cb(uv_connect_t* req, int status){
    HsInt slot = (HsInt)req->data;
    uv_loop_t* loop = req->handle->loop;
    hs_loop_data* loop_data = loop->data;  // uv_connect_t has handle field
    assert(slot < loop_data->size);

    loop_data->buffer_size_table[slot] = status;             // 0 in case of success, < 0 otherwise.
    loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
    loop_data->event_counter += 1;

    free_slot(loop, slot);  // free the uv_req_t
    
}

HsInt hs_uv_tcp_connect(uv_tcp_t* handle, const struct sockaddr* addr){
    uv_loop_t* loop = handle->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_connect_t* req = 
        (uv_connect_t*)loop_data->uv_struct_table + (slot*sizeof(hs_uv_struct));
    req->data = (void*)slot;
    int r = uv_tcp_connect(req, handle, addr, hs_connect_cb);
    if (r < 0) {
        free_slot(loop, slot);  // free the uv_req_t, the callback won't fired
        return r;
    } else return slot;
}

// When libuv listen's callback is called, client is actually already accepted, 
// so our customized accept function just return the fd directly, Following code
// doesn't support IPC for now.
//
// TODO research on accepting fds sent by IPC pipes.
//
#if defined(_WIN32)
int32_t hs_uv_accept(uv_tcp_t* server) {
    int fd;
    switch (server->type) {
        case UV_TCP:
            fd = hs_uv_tcp_accept((uv_tcp_t*)server);
            break;
        case UV_NAMED_PIPE:
            fd = hs_uv_pipe_accept((uv_pipe_t*)server);
            break;
        default:
            assert(0);
    }
    return fd;
}
int32_t hs_uv_tcp_accept(uv_tcp_t* server) {
  int32_t fd = 0;

  uv_tcp_accept_t* req = server->tcp.serv.pending_accepts;

  if (!req) {
    /* No valid connections found, so we error out. */
    return WSAEWOULDBLOCK;
  }

  if (req->accept_socket == INVALID_SOCKET) {
    return WSAENOTCONN;
  }

  fd = (int32_t)req->accept_socket;

  /* Prepare the req to pick up a new connection */

  server->tcp.serv.pending_accepts = req->next_pending;
  req->next_pending = NULL;
  req->accept_socket = INVALID_SOCKET;

  if (!(server->flags & UV__HANDLE_CLOSING)) {
    /* Check if we're in a middle of changing the number of pending accepts. */
    if (!(server->flags & UV_HANDLE_TCP_ACCEPT_STATE_CHANGING)) {
      uv_tcp_queue_accept(server, req);
    } else {
      /* We better be switching to a single pending accept. */
      assert(server->flags & UV_HANDLE_TCP_SINGLE_ACCEPT);
      server->tcp.serv.processed_accepts++;
      if (server->tcp.serv.processed_accepts >= uv_simultaneous_server_accepts) {
        server->tcp.serv.processed_accepts = 0;
        /*
         * All previously queued accept requests are now processed.
         * We now switch to queueing just a single accept.
         */
        uv_tcp_queue_accept(server, &server->tcp.serv.accept_reqs[0]);
        server->flags &= ~UV_HANDLE_TCP_ACCEPT_STATE_CHANGING;
        server->flags |= UV_HANDLE_TCP_SINGLE_ACCEPT;
      }
    }
  }
  return fd;
}
int hs_uv_pipe_accept(uv_pipe_t* server) {
    uv_loop_t* loop = server->loop;
    uv_pipe_accept_t* req;

    int fd;
    req = server->pipe.serv.pending_accepts;
    if (!req) {
      /* No valid connections found, so we error out. */
      return WSAEWOULDBLOCK;
    }

    fd = req->pipeHandle;

    /* Prepare the req to pick up a new connection */
    server->pipe.serv.pending_accepts = req->next_pending;
    req->next_pending = NULL;
    req->pipeHandle = INVALID_HANDLE_VALUE;
    if (!(server->flags & UV__HANDLE_CLOSING)) {
        uv_pipe_queue_accept(loop, server, req, FALSE);
    }
    return fd;
}
#else
int32_t hs_uv_accept(uv_stream_t* server) {
    int32_t fd = (int32_t)server->accepted_fd;
    server->accepted_fd = -1;
    return fd;
}
#endif

void hs_listen_cb(uv_stream_t* server, int status){
    HsInt slot = (HsInt)server->data;
    hs_loop_data* loop_data = server->loop->data;
    assert(slot < loop_data->size);

    int32_t* accept_buf = (int32_t*)loop_data->buffer_table[slot];      // fetch accept buffer from buffer_table table
    HsInt accepted_number = loop_data->buffer_size_table[slot];

    if (status == 0) {
        if (accepted_number < ACCEPT_BUFFER_SIZE - 1) {
            accept_buf[accepted_number] = (int32_t)hs_uv_accept(server);       
            loop_data->buffer_size_table[slot] = accepted_number + 1;
        } else {
#if defined(_WIN32)
            // we have no way to deal with this situation on windows, since 
            // we can't stop accepting after request has been inserted
            // but this should not happen on windows anyway,
            // since on windows simultaneous_accepts is small, e.g. pending accept
            // requests' number is small.
            // It must takes many uv_run without copying accept buffer on haskell side
            // which is very unlikely to happen.
            closesocket(hs_uv_accept(server)); 
#else
            // on unix, we can stop accepting using uv__io_stop, this is
            // important because libuv will loop accepting until EAGAIN/EWOULDBLOCK,
            // If we return to accept thread too slow in haskell side, the 
            // accept buffer may not be able to hold all the clients queued in backlog.
            // And this is very likely to happen under high load. Thus we
            // must stop accepting when the buffer is full.
            //
            // Limit this number may also be good for stop a non-block uv_run from
            // running too long, which will affect haskell's GC.
            //
            // do last accept without clearing server->accepted_fd
            // libuv will take this as a no accepting, thus call uv__io_stop for us.
            accept_buf[accepted_number] = (int32_t)hs_uv_accept(server);       
            // set back accepted_fd so that libuv break from accept loop
            // upon next resuming, we clear this accepted_fd with -1 and call uv__io_start
            server->accepted_fd = accept_buf[accepted_number];
            loop_data->buffer_size_table[slot] = accepted_number + 1;
#endif
        }
    } else {
        accept_buf[accepted_number] = (int32_t)status;
        loop_data->buffer_size_table[slot] = accepted_number + 1;
    }

}

int hs_uv_listen(uv_stream_t* stream, int backlog){
    return uv_listen(stream, backlog, hs_listen_cb);
}

// on windows we don't need to do anything, since we didn't and can't stopped. 
void hs_uv_listen_resume(uv_stream_t* server){
#if !defined(_WIN32)
    server->accepted_fd = -1;
    uv__io_start(server->loop, &server->io_watcher, POLLIN);
#endif
}

// Check if the socket's accept buffer is still filled, if so, unlock the accept thread
//
void hs_accept_check_cb(uv_check_t* check){
    uv_stream_t* server=(uv_stream_t*)check->data;
    HsInt slot = (HsInt)server->data;
    hs_loop_data* loop_data = server->loop->data;
    assert(slot < loop_data->size);

    if (loop_data->buffer_size_table[slot] > 0){
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

// It's hard to arrange accepting notification without check handler, we can't
// do it in listen's callback, since it'll be called multiple times during uv_run.
uv_check_t* hs_uv_accept_check_init(uv_stream_t* server){
    uv_check_t* check = malloc(sizeof(uv_check_t));
    if (check == NULL) return NULL;
    check->data = (void*)server;    // we link server to the buffer field

    if (uv_check_init(server->loop, check) < 0) {
        free(check);
        return NULL;
    }
    if (uv_check_start(check, hs_accept_check_cb) < 0){
        free(check);
        return NULL;
    }
    return check;
}

void hs_uv_accept_check_close(uv_check_t* check){
    uv_close((uv_handle_t*)check, (uv_close_cb)free);
}

int hs_set_socket_reuse(uv_stream_t* server) {
#if (SO_REUSEPORT_LOAD_BALANCE == 1)
    int yes = 1;
    if (setsockopt(server->io_watcher.fd, SOL_SOCKET, SO_REUSEPORT, &yes, sizeof(yes)))
        return uv_translate_sys_error(errno);
    return 0;
#else
    return 0;
#endif
}
