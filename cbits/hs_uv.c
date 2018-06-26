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

#include <uv.h>
#include <hs_uv.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// loop
//
// initialize a loop with its data to give slot size. return NULL on fail.
uv_loop_t* hs_uv_loop_init(size_t siz){
    int r; 
    size_t i;

    uv_loop_t* loop = malloc(sizeof(uv_loop_t));
    r = uv_loop_init(loop);

    if (r < 0) { 
        free(loop);
        return NULL;
    }

    hs_loop_data* loop_data = malloc(sizeof(hs_loop_data));
        
    size_t* event_queue = malloc(siz*sizeof(size_t));
    char** buffer_table = malloc(siz*sizeof(char*));
    ssize_t* buffer_size_table = malloc(siz*sizeof(ssize_t));
    size_t* slot_table = malloc(siz*sizeof(size_t));
    hs_uv_struct* uv_struct_table = malloc(siz*sizeof(hs_uv_struct));

    uv_async_t* async = malloc(sizeof(uv_async_t));
    uv_timer_t* timer = malloc(sizeof(uv_timer_t));

    if (loop_data == NULL || event_queue == NULL || buffer_table == NULL ||
            buffer_size_table == NULL || slot_table == NULL ||
                async == NULL || timer == NULL ||
                    uv_timer_init(loop, timer) < 0 ||
                        uv_async_init(loop, async, NULL) < 0){
        free(event_queue);
        free(loop_data);
        free(buffer_table);
        free(buffer_size_table);
        free(slot_table);
        free(uv_struct_table);

        free(async);
        free(timer);

        uv_loop_close(loop);
        free(loop);
        free(loop_data);
        return NULL;    // before return NULL, free all structs
    } else {
        // initialize slot table
        for (i = 0; i < siz; i++) {
            slot_table[i] = i+1;
        }
        loop_data->event_queue          = event_queue;
        loop_data->buffer_table         = buffer_table;
        loop_data->buffer_size_table    = buffer_size_table;
        loop_data->slot_table           = slot_table;
        loop_data->free_slot            = 0;
        loop_data->uv_struct_table      = uv_struct_table;
        loop_data->size                 = siz;
        loop_data->async                = async;
        loop_data->timer                = timer;
        loop->data = loop_data;
        return loop;
    }
}

// allocate free slot, resize loop data if neccessary
size_t alloc_slot(uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    size_t r = loop_data->free_slot;
    loop_data->free_slot = loop_data->slot_table[r];
    // the slot exceed range, we should resize
    if (r == loop_data->size - 1) {
         hs_uv_loop_resize(loop, (loop_data->size) * 2);
    }
    return r;
}

void free_slot(uv_loop_t* loop, size_t slot){
    hs_loop_data* loop_data = loop->data;
    assert(slot < loop_data->size);
    loop_data->slot_table[slot] = loop_data->free_slot;
    loop_data->free_slot = slot;
}

// resize a loop's data to given slot size, return NULL on fail.
uv_loop_t* hs_uv_loop_resize(uv_loop_t* loop, size_t siz){
    size_t i;
    hs_loop_data* loop_data = loop->data;
    size_t* event_queue_new       = realloc(loop_data->event_queue, (siz*sizeof(size_t)));
    char** buffer_table_new       = realloc(loop_data->buffer_table, (siz*sizeof(char*)));
    ssize_t* buffer_size_table_new = realloc(loop_data->buffer_size_table, (siz*sizeof(ssize_t)));
    size_t* slot_table_new = realloc(loop_data->slot_table, (siz*sizeof(ssize_t)));
    hs_uv_struct* uv_struct_table_new = realloc(loop_data->uv_struct_table, (siz*sizeof(hs_uv_struct)));

    if (event_queue_new == NULL || buffer_table_new == NULL ||
            buffer_size_table_new == NULL || slot_table_new == NULL ||
                uv_struct_table_new == NULL){
        // release new memory
        if (event_queue_new != loop_data->event_queue) free(event_queue_new);
        if (buffer_table_new != loop_data->buffer_table) free(buffer_table_new);
        if (buffer_size_table_new != loop_data->buffer_size_table) free(buffer_size_table_new);
        if (slot_table_new != loop_data->slot_table) free(slot_table_new);
        if (uv_struct_table_new != loop_data->uv_struct_table) free(uv_struct_table_new);
        return NULL;
    } else {
        for (i = loop_data->size; i < siz; i++) {
            slot_table_new[i] = i+1;
        }
        loop_data->event_queue        = event_queue_new;
        loop_data->buffer_table       = buffer_table_new;
        loop_data->buffer_size_table  = buffer_size_table_new;
        loop_data->slot_table         = slot_table_new;
        loop_data->free_slot          = loop_data->size;
        loop_data->uv_struct_table    = uv_struct_table_new;
        loop_data->size               = siz;
        return loop;
    }
}

void hs_uv_walk_close_cb(uv_handle_t* handle, void* arg){
    if (uv_is_closing(handle) == 0) uv_close(handle, hs_uv_handle_free);
}

// This function close all the handles live on that loop and the loop itself,
// then release all the memory.
//
// https://stackoverflow.com/questions/25615340/closing-libuv-handles-correctly
//
void hs_uv_loop_close(uv_loop_t* loop){
    uv_stop(loop);
    uv_walk(loop, hs_uv_walk_close_cb, NULL);
    uv_run(loop, UV_RUN_NOWAIT);
    while(uv_loop_close(loop) == UV_EBUSY);

    hs_loop_data* loop_data = loop->data;
    free(loop);
    free(loop_data->event_queue);
    free(loop_data->buffer_table);
    free(loop_data->buffer_size_table);
    free(loop_data->slot_table);
    free(loop_data->uv_struct_table);

    free(loop_data->async);
    free(loop_data->timer);

    free(loop_data);
    free(loop);
}

////////////////////////////////////////////////////////////////////////////////
// thread-safe wake up

// A empty callback for wake up uv_run, since timer is not allowed to have NULL callback.
void uv_timer_wake_cb(uv_timer_t* handle){}

int hs_uv_wake_up(uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    return uv_timer_start(loop_data->timer, uv_timer_wake_cb, 1, 1);
}
int hs_uv_wake_up_threaded(uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    return uv_async_send(loop_data->async);
}

////////////////////////////////////////////////////////////////////////////////
// handle
//
// Get handle's OS file
int32_t hs_uv_fileno(uv_handle_t* handle){
    uv_os_fd_t fd;
    int r;
    r = uv_fileno(handle, &fd);
    if (r < 0) { return (int32_t)r; } else { return (int32_t)fd; }
}

// Initialize a uv_handle_t with give type, return NULL on fail.
// the returned handle's data field is a new allocated slot
uv_handle_t* hs_uv_handle_alloc(uv_handle_type typ, uv_loop_t* loop){
    uv_handle_t* handle = malloc(uv_handle_size(typ));
    if (handle == NULL) { return NULL; }
    else {
        handle->loop = loop;
        handle->data = (void*)alloc_slot(loop);
        return handle;
    }
}

// Initialize a uv_handle_t with give type, return NULL on fail.
// No new slot will be allocated, thus the data field is uninitialized.
uv_handle_t* hs_uv_handle_alloc_no_slot(uv_handle_type typ){
    return malloc(uv_handle_size(typ));
}

// Free slot
void hs_uv_handle_free(uv_handle_t* handle){
    free_slot(handle->loop, (size_t)handle->data);
}

// Free uv_handle_t 's memory only
void hs_uv_handle_free_no_slot(uv_handle_t* handle){
    free(handle);
}

// Close a uv_handle_t, free its memory & slot
void hs_uv_handle_close(uv_handle_t* handle){
    uv_close(handle, hs_uv_handle_free);
}

void hs_uv_handle_close_no_slot(uv_handle_t* handle){
    uv_close(handle, hs_uv_handle_free_no_slot);
}

////////////////////////////////////////////////////////////////////////////////
// request
//
// Initialize a uv_req_t with give type, return NULL on fail.
// the returned request's data field is a new allocated slot
uv_req_t* hs_uv_req_alloc(uv_req_type typ, uv_loop_t* loop){
    uv_req_t* req = malloc(uv_req_size(typ));
    if (req == NULL) { return NULL; }
    else {
        req->data = (void*)alloc_slot(loop);
        return req;
    }
}

// free uv_req_t's memory & slot
void hs_uv_req_free(uv_req_t* req, uv_loop_t* loop){
    free_slot(loop, (size_t)req->data);
    free(req);
}

// Initialize a uv_fs_t with give type, return NULL on fail.
// no new slot will be allocated
uv_req_t* hs_uv_req_alloc_no_slot(uv_req_type typ){
    return malloc(uv_req_size(typ));
}

// free uv_req_t's memory without its slot
void hs_uv_req_free_no_slot(uv_req_t* req){
    free(req);
}

////////////////////////////////////////////////////////////////////////////////
//
// stream
//
// We reuse buffer_size_table as the result table, i.e. after haskell threads
// are unblocked, they should peek result(length, errcode..) from buffer_size_table

// This callback simply copy buffer from buffer table and buffer size table
void hs_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    size_t slot = (size_t)handle->data;
    hs_loop_data* loop_data = handle->loop->data;
    buf->base = loop_data->buffer_table[slot];      // fetch buffer_table from buffer_table table
    buf->len = loop_data->buffer_size_table[slot];  // we ignore suggested_size completely
}

// We only do single read per uv_run with uv_read_stop
void hs_read_cb (uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf){
    size_t slot = (size_t)stream->data;
    hs_loop_data* loop_data = stream->loop->data;

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
    size_t slot = (size_t)req->data;
    hs_loop_data* loop_data = req->handle->loop->data;

    loop_data->buffer_size_table[slot] = (ssize_t)status;                   // 0 in case of success, < 0 otherwise.

    loop_data->event_queue[loop_data->event_counter] = slot;   // push the slot to event queue
    loop_data->event_counter += 1;
}

int hs_uv_write(uv_write_t* req, uv_stream_t* handle){

    hs_loop_data* loop_data = handle->loop->data;
    size_t slot = (size_t)req->data;            // fetch the request slot

    // on windows this struct is captured by WSASend
    // on unix this struct is copied by libuv's uv_write
    // so it's safe to allocate it on stack
    uv_buf_t buf = { 
        .base = loop_data->buffer_table[slot],
        .len = loop_data->buffer_size_table[slot]
    };
    
    return uv_write(req, handle, &buf, 1, hs_write_cb);    // we never use writev: we do our own
                                                           // user-space buffering in haskell.
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
    size_t slot = (size_t)req->data;
    hs_loop_data* loop_data = req->handle->loop->data;       // uv_connect_t has handle field

    loop_data->buffer_size_table[slot] = status;                  // 0 in case of success, < 0 otherwise.
    loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
    loop_data->event_counter += 1;
}

int hs_uv_tcp_connect(uv_connect_t* req, uv_tcp_t* handle, const struct sockaddr* addr){
    return uv_tcp_connect(req, handle, addr, hs_connect_cb);
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
    size_t slot = (size_t)server->data;
    hs_loop_data* loop_data = server->loop->data;

    int32_t* accept_buf = (int32_t*)loop_data->buffer_table[slot];      // fetch accept buffer from buffer_table table
    size_t accepted_number = loop_data->buffer_size_table[slot];

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
    size_t slot = (size_t)server->data;
    hs_loop_data* loop_data = server->loop->data;

    if (loop_data->buffer_size_table[slot] > 0){
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

// It's hard to arrange accepting notification without check handler, we can't
// do it in listen's callback, since it'll be called multiple times during uv_run.
int hs_uv_accept_check_init(uv_loop_t* loop, uv_check_t* check, uv_stream_t* server){
    int r = uv_check_init(loop, check);
    check->data = (void*)server;    // we link server to the buffer field
    if (r < 0) return r;
    return uv_check_start(check, hs_accept_check_cb);
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


////////////////////////////////////////////////////////////////////////////////
// fs, none thread pool version
// we wrappered non-threaded pool version functions, so that we can move the allocation
// of uv_fs_t to stack, most of the functions can be optimized in this way.
// in none thread pool version, req->result is directly returned.
int32_t hs_uv_fs_open(const char* path, int flags, int mode){
    uv_fs_t req;
    int r = uv_fs_open(NULL, &req, path, flags, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (int32_t)r;
}

int hs_uv_fs_close(int32_t file){
    uv_fs_t req;
    int r = uv_fs_close(NULL, &req, file, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

ssize_t hs_uv_fs_read(int32_t file, char* buffer, size_t buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { 
        .base = buffer,
        .len = buffer_size
    };
    uv_fs_read(NULL, &req, file, &buf, 1, offset, NULL);
    return req.result;
}

ssize_t hs_uv_fs_write(int32_t file, char* buffer, size_t buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { 
        .base = buffer,
        .len = buffer_size
    };
    uv_fs_write(NULL, &req, file, &buf, 1, offset, NULL);
    return req.result;
}

int hs_uv_fs_unlink(char* path){
    uv_fs_t req;
    return uv_fs_unlink(NULL, &req, path, NULL);
}

int hs_uv_fs_mkdir(char* path, int mode){
    uv_fs_t req;
    return uv_fs_mkdir(NULL, &req, path, mode, NULL);
}

// fs, thread pool version
int hs_uv_fs_close_threaded(uv_loop_t* loop, uv_fs_t* req, int32_t file){
    // do the last clean up, since uv_fs_req_cleanup is idempotent
    uv_fs_req_cleanup(req);
    return uv_fs_close(loop, req, (uv_file)file, (uv_fs_cb)hs_uv_req_free);
}

int hs_uv_fs_read_threaded(uv_loop_t* loop, uv_fs_t* req, int32_t file,
        char* buf, size_t buf_siz, int64_t offset){
    uv_buf_t buf_t = { 
        .base = buf,
        .len = buf_siz
    };
    uv_fs_read(loop, req, (uv_file)file, &buf_t, 1, offset, hs_uv_fs_callback);
}

uv_dirent_t* hs_uv_dirent_alloc(){
    return malloc(sizeof(uv_dirent_t));
}

void hs_uv_dirent_free(uv_dirent_t* ent){
    free(ent);
}

void hs_uv_fs_callback(uv_fs_t* req){
    hs_loop_data* loop_data = req->loop->data;
    assert(loop_data->event_counter < loop_data->size);
    loop_data->event_queue[loop_data->event_counter] = (size_t)req->data; // push the slot to event queue
    loop_data->event_counter += 1;
}
