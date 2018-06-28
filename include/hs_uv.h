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
#include <assert.h>
#include <HsFFI.h>  // for HsInt
#include <stdlib.h> // for malloc, free, etc.

#if defined(__sun)
# include <sys/port.h>
# include <port.h>
#endif /* __sun */

#if defined(_AIX)
# define reqevents events
# define rtnevents revents
# include <sys/poll.h>
#else
# include <poll.h>
#endif /* _AIX */

////////////////////////////////////////////////////////////////////////////////
// CONSTANT
#define ACCEPT_BUFFER_SIZE 1024

#if defined(__linux__) && defined(SO_REUSEPORT)
#define SO_REUSEPORT_LOAD_BALANCE 1
#else
#define SO_REUSEPORT_LOAD_BALANCE 0
#endif

// this function will be a noop when SO_REUSEPORT_LOAD_BALANCE == 0
int hs_set_socket_reuse(uv_stream_t* server);

////////////////////////////////////////////////////////////////////////////////
// error handling
int uv_translate_sys_error(int sys_errno);

////////////////////////////////////////////////////////////////////////////////
// loop

typedef union {
    uv_tcp_t tcp;
    uv_pipe_t pipe;
    uv_tty_t tty;
    uv_udp_t udp;
    uv_write_t write;
    uv_connect_t connect;
    uv_udp_send_t udp_send;
    uv_fs_t fs;
} hs_uv_struct;

typedef struct {
    // following two fields record events during uv_run, inside callback which
    // wants to record a event, push the handler's slot into the queue 
    HsInt    event_counter;
    HsInt*   event_queue;
    // following two fields provide buffers allocated in haskell to uv_alloc_cb,
    // the buffer_size_table are also used to record operation's result
    char**    buffer_table;
    HsInt*  buffer_size_table;
    // following fields are used to implemented a stable slot allocator, we used
    // to do slot allocation in haskell, but doing it in C allow us to free slot
    // in the right place, e.g. uv_close_cb.
    HsInt*   slot_table;
    HsInt    free_slot;
    // following field points a memory pools for uv_handle_t / uv_req_t struct,
    // it's inefficient to manange malloced memory in haskell side, so we use
    // a memory pool to simplify memory management.
    hs_uv_struct*  uv_struct_table;
    HsInt    size;  
    // following fields are handlers used to wake up event loop under threaded and
    // non-threaded RTS respectively.
    uv_async_t* async;
    uv_timer_t* timer;
} hs_loop_data;

uv_loop_t* hs_uv_loop_init(HsInt siz);
uv_loop_t* hs_uv_loop_resize(uv_loop_t* loop, HsInt siz);
void hs_uv_loop_close(uv_loop_t* loop);

HsInt alloc_slot(uv_loop_t* loop);
void free_slot(uv_loop_t* loop, HsInt slot);

////////////////////////////////////////////////////////////////////////////////
// wake up
int hs_uv_wake_up_timer(hs_loop_data* loop_data);
int hs_uv_wake_up_async(hs_loop_data* loop_data);

////////////////////////////////////////////////////////////////////////////////
// handle
uv_handle_t* hs_uv_handle_alloc(uv_loop_t* loop);
void hs_uv_handle_free(uv_handle_t* handle);
void hs_uv_handle_close(uv_handle_t* handle);

////////////////////////////////////////////////////////////////////////////////
// request
void hs_uv_cancel(uv_loop_t* loop, HsInt slot);

////////////////////////////////////////////////////////////////////////////////
// stream
int hs_uv_read_start(uv_stream_t* handle);
HsInt hs_uv_write(uv_stream_t* handle, char* buf, HsInt buf_size);

////////////////////////////////////////////////////////////////////////////////
// tcp 
int hs_uv_tcp_open(uv_tcp_t* handle, int sock);
HsInt hs_uv_tcp_connect(uv_tcp_t* handle, const struct sockaddr* addr);

#if defined(_WIN32)

#define UV_HANDLE_READING                       0x00000100
#define UV_HANDLE_BOUND                         0x00000200
#define UV_HANDLE_LISTENING                     0x00000800
#define UV_HANDLE_CONNECTION                    0x00001000
#define UV_HANDLE_READABLE                      0x00008000
#define UV_HANDLE_WRITABLE                      0x00010000
enum {
  UV__SIGNAL_ONE_SHOT = 0x80000,  /* On signal reception remove sighandler */
  UV__HANDLE_INTERNAL = 0x8000,
  UV__HANDLE_ACTIVE   = 0x4000,
  UV__HANDLE_REF      = 0x2000,
  UV__HANDLE_CLOSING  = 0 /* no-op on unix */
};
#define UV_HANDLE_TCP_SINGLE_ACCEPT             0x08000000
#define UV_HANDLE_TCP_ACCEPT_STATE_CHANGING     0x10000000
extern unsigned int uv_simultaneous_server_accepts;
extern void uv_tcp_queue_accept(uv_tcp_t* handle, uv_tcp_accept_t* req);
#else
void uv__io_start(uv_loop_t* loop, uv__io_t* w, unsigned int events);
#endif

////////////////////////////////////////////////////////////////////////////////
// fs

// we define file open flag here for compatibility on libuv < v1.16
// see https://github.com/libuv/libuv/commit/4b666bd2d82a51f1c809b2703a91679789c1ec01#diff-a5e63f9b16ca783355e2d83941c3eafb

#if defined(_WIN32)
/* fs open() flags supported on this platform: */
#ifndef UV_FS_O_APPEND
#define UV_FS_O_APPEND       _O_APPEND
#endif
#ifndef UV_FS_O_CREAT
#define UV_FS_O_CREAT        _O_CREAT
#endif
#ifndef UV_FS_O_EXCL
#define UV_FS_O_EXCL         _O_EXCL
#endif
#ifndef UV_FS_O_RANDOM
#define UV_FS_O_RANDOM       _O_RANDOM
#endif
#ifndef UV_FS_O_RDONLY
#define UV_FS_O_RDONLY       _O_RDONLY
#endif
#ifndef UV_FS_O_RDWR
#define UV_FS_O_RDWR         _O_RDWR
#endif
#ifndef UV_FS_O_SEQUENTIAL
#define UV_FS_O_SEQUENTIAL   _O_SEQUENTIAL
#endif
#ifndef UV_FS_O_SHORT_LIVED
#define UV_FS_O_SHORT_LIVED  _O_SHORT_LIVED
#endif
#ifndef UV_FS_O_TEMPORARY
#define UV_FS_O_TEMPORARY    _O_TEMPORARY
#endif
#ifndef UV_FS_O_TRUNC
#define UV_FS_O_TRUNC        _O_TRUNC
#endif
#ifndef UV_FS_O_WRONLY
#define UV_FS_O_WRONLY       _O_WRONLY
#endif

/* fs open() flags supported on other platforms (or mapped on this platform): */
#ifndef UV_FS_O_DIRECT
#define UV_FS_O_DIRECT       0x2000000 /* FILE_FLAG_NO_BUFFERING */
#endif
#ifndef UV_FS_O_DIRECTORY
#define UV_FS_O_DIRECTORY    0
#endif
#ifndef UV_FS_O_DSYNC
#define UV_FS_O_DSYNC        0x4000000 /* FILE_FLAG_WRITE_THROUGH */
#endif
#ifndef UV_FS_O_EXLOCK
#define UV_FS_O_EXLOCK       0
#endif
#ifndef UV_FS_O_NOATIME
#define UV_FS_O_NOATIME      0
#endif
#ifndef UV_FS_O_NOCTTY
#define UV_FS_O_NOCTTY       0
#endif
#ifndef UV_FS_O_NOFOLLOW
#define UV_FS_O_NOFOLLOW     0
#endif
#ifndef UV_FS_O_NONBLOCK
#define UV_FS_O_NONBLOCK     0
#endif
#ifndef UV_FS_O_SYMLINK
#define UV_FS_O_SYMLINK      0
#endif
#ifndef UV_FS_O_SYNC
#define UV_FS_O_SYNC         0x8000000 /* FILE_FLAG_WRITE_THROUGH */
#endif

#else

#ifndef UV_FS_O_APPEND
#if defined(O_APPEND)
# define UV_FS_O_APPEND       O_APPEND
#else
# define UV_FS_O_APPEND       0
#endif
#endif
#ifndef UV_FS_O_CREAT
#if defined(O_CREAT)
# define UV_FS_O_CREAT        O_CREAT
#else
# define UV_FS_O_CREAT        0
#endif
#endif
#ifndef UV_FS_O_DIRECT
#if defined(O_DIRECT)
# define UV_FS_O_DIRECT       O_DIRECT
#else
# define UV_FS_O_DIRECT       0
#endif
#endif
#ifndef UV_FS_O_DIRECTORY
#if defined(O_DIRECTORY)
# define UV_FS_O_DIRECTORY    O_DIRECTORY
#else
# define UV_FS_O_DIRECTORY    0
#endif
#endif
#ifndef UV_FS_O_DSYNC
#if defined(O_DSYNC)
# define UV_FS_O_DSYNC        O_DSYNC
#else
# define UV_FS_O_DSYNC        0
#endif
#endif
#ifndef UV_FS_O_EXCL
#if defined(O_EXCL)
# define UV_FS_O_EXCL         O_EXCL
#else
# define UV_FS_O_EXCL         0
#endif
#endif
#ifndef UV_FS_O_EXLOCK
#if defined(O_EXLOCK)
# define UV_FS_O_EXLOCK       O_EXLOCK
#else
# define UV_FS_O_EXLOCK       0
#endif
#endif
#ifndef UV_FS_O_NOATIME
#if defined(O_NOATIME)
# define UV_FS_O_NOATIME      O_NOATIME
#else
# define UV_FS_O_NOATIME      0
#endif
#endif
#ifndef UV_FS_O_NOCTTY
#if defined(O_NOCTTY)
# define UV_FS_O_NOCTTY       O_NOCTTY
#else
# define UV_FS_O_NOCTTY       0
#endif
#endif
#ifndef UV_FS_O_NOFOLLOW
#if defined(O_NOFOLLOW)
# define UV_FS_O_NOFOLLOW     O_NOFOLLOW
#else
# define UV_FS_O_NOFOLLOW     0
#endif
#endif
#ifndef UV_FS_O_NONBLOCK
#if defined(O_NONBLOCK)
# define UV_FS_O_NONBLOCK     O_NONBLOCK
#else
# define UV_FS_O_NONBLOCK     0
#endif
#endif
#ifndef UV_FS_O_RDONLY
#if defined(O_RDONLY)
# define UV_FS_O_RDONLY       O_RDONLY
#else
# define UV_FS_O_RDONLY       0
#endif
#endif
#ifndef UV_FS_O_RDWR
#if defined(O_RDWR)
# define UV_FS_O_RDWR         O_RDWR
#else
# define UV_FS_O_RDWR         0
#endif
#endif
#ifndef UV_FS_O_SYMLINK
#if defined(O_SYMLINK)
# define UV_FS_O_SYMLINK      O_SYMLINK
#else
# define UV_FS_O_SYMLINK      0
#endif
#endif
#ifndef UV_FS_O_SYNC
#if defined(O_SYNC)
# define UV_FS_O_SYNC         O_SYNC
#else
# define UV_FS_O_SYNC         0
#endif
#endif
#ifndef UV_FS_O_TRUNC
#if defined(O_TRUNC)
# define UV_FS_O_TRUNC        O_TRUNC
#else
# define UV_FS_O_TRUNC        0
#endif
#endif
#ifndef UV_FS_O_WRONLY
#if defined(O_WRONLY)
# define UV_FS_O_WRONLY       O_WRONLY
#else
# define UV_FS_O_WRONLY       0
#endif
#endif

/* fs open() flags supported on other platforms: */
#ifndef UV_FS_O_RANDOM
#define UV_FS_O_RANDOM        0
#endif
#ifndef UV_FS_O_SHORT_LIVED
#define UV_FS_O_SHORT_LIVED   0
#endif
#ifndef UV_FS_O_SEQUENTIAL
#define UV_FS_O_SEQUENTIAL    0
#endif
#ifndef UV_FS_O_TEMPORARY
#define UV_FS_O_TEMPORARY     0
#endif

#endif

void hs_uv_fs_callback(uv_fs_t* req);
