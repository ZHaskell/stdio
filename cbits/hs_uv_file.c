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
// fs, none thread pool version
//
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
    int r = uv_fs_close(NULL, &req, (uv_file)file, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

HsInt hs_uv_fs_read(int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    uv_fs_read(NULL, &req, (uv_file)file, &buf, 1, offset, NULL);
    return (HsInt)req.result;
}

HsInt hs_uv_fs_write(int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    uv_fs_write(NULL, &req, (uv_file)file, &buf, 1, offset, NULL);
    return (HsInt)req.result;
}

int hs_uv_fs_unlink(char* path){
    uv_fs_t req;
    return uv_fs_unlink(NULL, &req, path, NULL);
}

int hs_uv_fs_mkdir(char* path, int mode){
    uv_fs_t req;
    return uv_fs_mkdir(NULL, &req, path, mode, NULL);
}

////////////////////////////////////////////////////////////////////////////////
// fs, thread pool version
//
void hs_uv_fs_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    assert(slot < loop_data->size);
    // push the slot to event queue
    loop_data->buffer_size_table[slot] = (HsInt)req->result;
    loop_data->event_queue[loop_data->event_counter] = slot;
    loop_data->event_counter += 1;
    uv_fs_req_cleanup(req);
    free_slot(loop, slot);  // free the uv_req_t
}

HsInt hs_uv_fs_open_threaded(uv_loop_t* loop, const char* path, int flags, int mode){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;

    int r = uv_fs_open(NULL, req, path, flags, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_close_threaded(uv_loop_t* loop, int32_t file){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_close(loop, req, (uv_file)file, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_read_threaded(uv_loop_t* loop, int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    int r = uv_fs_read(loop, req, (uv_file)file, &buf, 1, offset, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_write_threaded(uv_loop_t* loop, int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    int r = uv_fs_write(loop, req, (uv_file)file, &buf, 1, offset, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_unlink_threaded(uv_loop_t* loop, char* path){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_unlink(loop, req, path, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_mkdir_threaded(uv_loop_t* loop, char* path, int mode){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop);
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_mkdir(loop, req, path, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop, slot);
        return (HsInt)r;
    } else return slot;
}
