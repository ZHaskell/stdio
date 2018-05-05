#include <uv.h>
#include "hs_uv.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

/********************************************************************************/

// initialize a loop with its data to give slot size. return NULL on fail.
uv_loop_t* hs_uv_loop_init(size_t siz){
    int r;

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

    if (loop_data == NULL || event_queue == NULL || buffer_table == NULL || buffer_size_table == NULL){
        free(event_queue);
        free(loop_data);
        free(buffer_table);
        free(buffer_size_table);

        uv_loop_close(loop);
        free(loop);
        free(loop_data);
        return NULL;    // before return NULL, free all structs
    } else {

        loop_data->event_queue             = event_queue;
        loop_data->buffer_table            = buffer_table;
        loop_data->buffer_size_table       = buffer_size_table;
        loop->data = loop_data;
        return loop;
    }
}

// resize a loop's data to given slot size, return NULL on fail.
uv_loop_t* hs_uv_loop_resize(uv_loop_t* loop, size_t siz){

    hs_loop_data* loop_data = loop->data;
    size_t* event_queue_new       = realloc(loop_data->event_queue, (siz*sizeof(size_t)));
    char** buffer_table_new       = realloc(loop_data->buffer_table, (siz*sizeof(char*)));
    ssize_t* buffer_size_table_new = realloc(loop_data->buffer_size_table, (siz*sizeof(ssize_t)));

    if (event_queue_new == NULL || buffer_table_new == NULL || buffer_size_table_new == NULL){
        // release new memory
        if (event_queue_new != loop_data->event_queue) free(event_queue_new);
        if (buffer_table_new != loop_data->buffer_table) free(buffer_table_new);
        if (buffer_size_table_new != loop_data->buffer_size_table) free(buffer_size_table_new);
        return NULL;
    } else {
        loop_data->event_queue             = event_queue_new;
        loop_data->buffer_table            = buffer_table_new;
        loop_data->buffer_size_table       = buffer_size_table_new;
        return loop;
    }
}

void hs_uv_walk_close_cb(uv_handle_t* handle, void* arg){
    if (uv_is_closing(handle) == 0) uv_close(handle, hs_uv_handle_free);
}

// This function close all the handles live on that loop and the loop itself,
// then release all the memory.
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
}

/********************************************************************************/

// Initialize a uv_handle_t with give type, return NULL on fail.
uv_handle_t* hs_uv_handle_alloc(uv_handle_type typ){
    return malloc(uv_handle_size(typ));
}

// Free uv_handle_t 's memory
void hs_uv_handle_free(uv_handle_t* handle){
    free(handle);
}

// Close a uv_handle_t, free its memory.
void hs_uv_handle_close(uv_handle_t* handle){
    uv_close(handle, hs_uv_handle_free);
}

// Get handle's OS file
int32_t hs_uv_fileno(uv_handle_t* handle){
    uv_os_fd_t fd;
    int r;
    r = uv_fileno(handle, &fd);
    if (r < 0) { return (int32_t)r; } else { return (int32_t)fd; }
}

// Initialize a uv_req_t with give type, return NULL on fail.
uv_handle_t* hs_uv_req_alloc(uv_req_type typ){
    return malloc(uv_req_size(typ));
}

// free uv_req_t's memory.
void hs_uv_req_free(uv_req_t* req){
    free(req);
}


/********************************************************************************/

void hs_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    size_t slot = (size_t)handle->data;
    hs_loop_data* loop_data = handle->loop->data;
    buf->base = loop_data->buffer_table[slot];      // fetch buffer_table from buffer_table table
    buf->len = loop_data->buffer_size_table[slot];  // we ignore suggested_size completely
}

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

/********************************************************************************/

/* on windows uv_tcp_open doesn't work propery for sockets that are not
 * connected or accepted by libuv because the lack of some state initialization,
 * so we do it by manually set those flags
 *
 * referenes:   https://github.com/libuv/libuv/issues/397
 *              https://github.com/libuv/libuv/pull/1150
 */
#if defined(_WIN32)
#define UV_HANDLE_READING                       0x00000100
#define UV_HANDLE_BOUND                         0x00000200
#define UV_HANDLE_LISTENING                     0x00000800
#define UV_HANDLE_CONNECTION                    0x00001000
#define UV_HANDLE_READABLE                      0x00008000
#define UV_HANDLE_WRITABLE                      0x00010000

void uv_connection_init(uv_stream_t* handle){
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
    uv_connection_init((uv_stream_t*)handle);
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
// we don't consider ipc case in stdio
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
            closesocket(hs_uv_accept(server));  // this should not happen since simultaneous_accepts is small
#else
            // do last accept
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
#if !defined(_WIN32)
    return uv_listen(stream, backlog, hs_listen_cb);
#endif
}

void hs_uv_listen_resume(uv_stream_t* server){
    server->accepted_fd = -1;
    uv__io_start(server->loop, &server->io_watcher, POLLIN);
}

// check if the socket's accept buffer is still filled, if so, unlock the accept thread
void hs_accept_check_cb(uv_check_t* check){
    uv_stream_t* server=(uv_stream_t*)check->data;
    size_t slot = (size_t)server->data;
    hs_loop_data* loop_data = server->loop->data;

    if (loop_data->buffer_size_table[slot] > 0){
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

int hs_uv_accept_check_init(uv_loop_t* loop, uv_check_t* check, uv_stream_t* server){
    int r = uv_check_init(loop, check);
    check->data = (void*)server;    // we link server to the buffer field
    if (r < 0) return r;
    return uv_check_start(check, hs_accept_check_cb);
}

/********************************************************************************/

// a empty callback for wake up uv_run
void uv_timer_wake_cb(uv_timer_t* handle){}

// a timer handler whose sole purpose is to break current uv_run from other thread
// this is used with none-threaded GHC rts.
int hs_uv_timer_wake_start(uv_timer_t* handle, uint64_t timeout){
    return uv_timer_start(handle, uv_timer_wake_cb, timeout, timeout);
}

// a async handler whose sole purpose is to break current uv_run from other thread
// this is used with multi-threaded GHC rts.
int hs_uv_async_wake_init(uv_loop_t* loop, uv_async_t* async){
    return uv_async_init(loop, async, NULL);
}

/********************************************************************************/

uv_dirent_t* hs_uv_dirent_alloc(){
    return malloc(sizeof(uv_dirent_t));
}

void hs_uv_dirent_free(uv_dirent_t* ent){
    free(ent);
}

void hs_uv_fs_callback(uv_fs_t* req){
    hs_loop_data* loop_data = req->loop->data;
    loop_data->event_queue[loop_data->event_counter] = (size_t)req->data; // push the slot to event queue
    loop_data->event_counter += 1;
}
