#include <uv.h>

#ifndef ACCEPT_BUFFER_SIZE
#define ACCEPT_BUFFER_SIZE 1024
#endif

int uv_translate_sys_error(int sys_errno);

typedef struct {
   size_t    event_counter;
   size_t*   event_queue;
   char**    buffer_table;
   ssize_t*   buffer_size_table;
} hs_loop_data;

uv_loop_t* hs_uv_loop_init(size_t siz);
uv_loop_t* hs_uv_loop_resize(uv_loop_t* loop, size_t siz);
void hs_uv_loop_close(uv_loop_t* loop);

uv_handle_t* hs_uv_handle_alloc(uv_handle_type typ);
void hs_uv_handle_free(uv_handle_t* handle);
void hs_uv_handle_close(uv_handle_t* handle);

uv_handle_t* hs_uv_req_alloc(uv_req_type typ);
void hs_uv_req_free(uv_req_t* req);

int hs_uv_read_start(uv_stream_t* stream);
int hs_uv_write(uv_write_t* req, uv_stream_t* handle);


int hs_uv_tcp_open(uv_tcp_t* handle, int sock);
int hs_uv_tcp_connect(uv_connect_t* req, uv_tcp_t* handle, const struct sockaddr* addr);


int hs_uv_timer_wake_start(uv_timer_t* handle, uint64_t timeout);
int hs_uv_async_wake_init(uv_loop_t* loop, uv_async_t* async);



#if defined(_WIN32)
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
int32_t hs_uv_tcp_accept(uv_tcp_t* server);
int32_t hs_uv_pipe_accept(uv_pipe_t* server);
#else
ssize_t read(int fd, void *buf, size_t count); 
int uv__close(int fd); /* preserves errno */
int uv__stream_open(uv_stream_t* stream, int fd, int flags);
typedef struct uv__stream_queued_fds_s uv__stream_queued_fds_t;
void uv__io_start(uv_loop_t* loop, uv__io_t* w, unsigned int events);
void uv__io_stop(uv_loop_t* loop, uv__io_t* w, unsigned int events);
struct uv__stream_queued_fds_s {
  unsigned int size;
  unsigned int offset;
  int fds[1];
};
void uv__free(void* ptr);

/* handle flags */
enum {
  UV_CLOSING              = 0x01,   /* uv_close() called but not finished. */
  UV_CLOSED               = 0x02,   /* close(2) finished. */
  UV_STREAM_READING       = 0x04,   /* uv_read_start() called. */
  UV_STREAM_SHUTTING      = 0x08,   /* uv_shutdown() called but not complete. */
  UV_STREAM_SHUT          = 0x10,   /* Write side closed. */
  UV_STREAM_READABLE      = 0x20,   /* The stream is readable */
  UV_STREAM_WRITABLE      = 0x40,   /* The stream is writable */
  UV_STREAM_BLOCKING      = 0x80,   /* Synchronous writes. */
  UV_STREAM_READ_PARTIAL  = 0x100,  /* read(2) read less than requested. */
  UV_STREAM_READ_EOF      = 0x200,  /* read(2) read EOF. */
  UV_TCP_NODELAY          = 0x400,  /* Disable Nagle. */
  UV_TCP_KEEPALIVE        = 0x800,  /* Turn on keep-alive. */
  UV_TCP_SINGLE_ACCEPT    = 0x1000, /* Only accept() when idle. */
  UV_HANDLE_IPV6          = 0x10000, /* Handle is bound to a IPv6 socket. */
  UV_UDP_PROCESSING       = 0x20000, /* Handle is running the send callback queue. */
  UV_HANDLE_BOUND         = 0x40000  /* Handle is bound to an address and port */
};

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

#endif

////////////////////////////////////////////////////////////////////////////////

void hs_uv_fs_callback(uv_fs_t* req);
