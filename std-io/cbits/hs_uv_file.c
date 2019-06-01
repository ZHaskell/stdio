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
    uv_fs_open(NULL, &req, path, flags, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (int32_t)req.result;
}

HsInt hs_uv_fs_close(int32_t file){
    uv_fs_t req;
    uv_fs_close(NULL, &req, (uv_file)file, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_read(int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    uv_fs_read(NULL, &req, (uv_file)file, &buf, 1, offset, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_write(int32_t file, char* buffer, HsInt buffer_size, int64_t offset){
    uv_fs_t req;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    uv_fs_write(NULL, &req, (uv_file)file, &buf, 1, offset, NULL);
    return (HsInt)req.result;
}

HsInt hs_uv_fs_unlink(const char* path){
    uv_fs_t req;
    uv_fs_unlink(NULL, &req, path, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_mkdir(const char* path, int mode){
    uv_fs_t req;
    uv_fs_mkdir(NULL, &req, path, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_mkdtemp(const char* tpl, HsInt tpl_size, char* temp_path){
    uv_fs_t req;
    strcpy(temp_path, tpl);
    strcpy(temp_path + tpl_size, "XXXXXX");
    uv_fs_mkdtemp(NULL, &req, temp_path, NULL);
    strcpy(temp_path, req.path);    // save the temp path
    uv_fs_req_cleanup(&req);        // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_rmdir(const char* path){
    uv_fs_t req;
    uv_fs_rmdir(NULL, &req, path, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

void hs_uv_fs_scandir_cleanup(uv_dirent_t** dents, HsInt n){
    int i;
    for (i=0; i<n; i++){
        uv__fs_scandir_free(dents[i]);
    }
    uv__fs_scandir_free(dents);
}

HsInt hs_uv_fs_scandir(const char* path, uv_dirent_t*** dents){
    uv_fs_t req;
    uv_fs_scandir(NULL, &req, path, 0, NULL);
    *dents = req.ptr;
#if defined(_WIN32)
    uv__free(req.file.pathw);      //  we clean up dents later
#endif
    return (HsInt)req.result;
}

HsInt hs_uv_fs_stat(const char* path, uv_stat_t* stat){
    uv_fs_t req;
    uv_fs_stat(NULL, &req, path, NULL);
    memcpy(stat, &req.statbuf, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_fstat(int32_t file, uv_stat_t* stat){
    uv_fs_t req;
    uv_fs_fstat(NULL, &req, file, NULL);
    memcpy(stat, &req.statbuf, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_lstat(const char* path, uv_stat_t* stat){
    uv_fs_t req;
    uv_fs_lstat(NULL, &req, path, NULL);
    memcpy(stat, &req.statbuf, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_rename(const char* path, const char* path2){
    uv_fs_t req;
    uv_fs_rename(NULL, &req, path, path2, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_fsync(int32_t file){
    uv_fs_t req;
    uv_fs_fsync(NULL, &req, file, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_fdatasync(int32_t file){
    uv_fs_t req;
    uv_fs_fdatasync(NULL, &req, file, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_ftruncate(int32_t file, int64_t off){
    uv_fs_t req;
    uv_fs_ftruncate(NULL, &req, file, off, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_copyfile(const char* path, const char* path2, int flag){
    uv_fs_t req;
    uv_fs_copyfile(NULL, &req, path, path2, flag, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_access(const char* path, int mode){
    uv_fs_t req;
    uv_fs_access(NULL, &req, path, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_chmod(const char* path, int mode){
    uv_fs_t req;
    uv_fs_chmod(NULL, &req, path, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_fchmod(int32_t file, int mode){
    uv_fs_t req;
    uv_fs_fchmod(NULL, &req, file, mode, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_utime(const char* path, double atime, double mtime){
    uv_fs_t req;
    uv_fs_utime(NULL, &req, path, atime, mtime, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_futime(int32_t file, double atime, double mtime){
    uv_fs_t req;
    uv_fs_futime(NULL, &req, file, atime, mtime, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_link(const char* path, const char* path2){
    uv_fs_t req;
    uv_fs_link(NULL, &req, path, path2, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

HsInt hs_uv_fs_symlink(const char* path, const char* path2, int flag){
    uv_fs_t req;
    uv_fs_symlink(NULL, &req, path, path2, flag, NULL);
    uv_fs_req_cleanup(&req);    // maybe not neccessary
    return (HsInt)req.result;
}

void hs_uv_fs_readlink_cleanup(char* path){
    if (path != NULL) uv__free(path);
}

HsInt hs_uv_fs_readlink(const char* path, char** result_path){
    uv_fs_t req;
    uv_fs_readlink(NULL, &req, path, NULL);
    *result_path = req.ptr;
#if defined(_WIN32)
    uv__free(req.file.pathw);      //  we clean up result_path later
#endif
    return (HsInt)req.result;
}

HsInt hs_uv_fs_realpath(const char* path, char** result_path){
    uv_fs_t req;
    uv_fs_realpath(NULL, &req, path, NULL);
    *result_path = req.ptr;
#if defined(_WIN32)
    uv__free(req.file.pathw);      //  we clean up result_path later
#endif
    return (HsInt)req.result;
}

////////////////////////////////////////////////////////////////////////////////
// fs, thread pool version
//
void hs_uv_fs_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    // push the slot to event queue
    loop_data->buffer_size_table[slot] = (HsInt)req->result;
    loop_data->event_queue[loop_data->event_counter] = slot;
    loop_data->event_counter += 1;
    uv_fs_req_cleanup(req);
    free_slot(loop_data, slot);  // free the uv_req_t
}

HsInt hs_uv_fs_open_threaded(const char* path, int flags, int mode, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;

    int r = uv_fs_open(loop, req, path, flags, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_close_threaded(int32_t file, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_close(loop, req, (uv_file)file, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_read_threaded(int32_t file, char* buffer, HsInt buffer_size, int64_t offset, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    int r = uv_fs_read(loop, req, (uv_file)file, &buf, 1, offset, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_write_threaded(int32_t file, char* buffer, HsInt buffer_size, int64_t offset, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    uv_buf_t buf = { .base = buffer, .len = (size_t)buffer_size };
    int r = uv_fs_write(loop, req, (uv_file)file, &buf, 1, offset, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_unlink_threaded(const char* path, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_unlink(loop, req, path, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_mkdir_threaded(const char* path, int mode, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_mkdir(loop, req, path, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

void hs_uv_fs_mkdtemp_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    char* path = loop_data->buffer_table[slot];
    if (path != NULL) {
        // push the slot to event queue
        loop_data->buffer_size_table[slot] = (HsInt)req->result;
        loop_data->event_queue[loop_data->event_counter] = slot;
        loop_data->event_counter += 1;
        strcpy(path, req->path);  // save the temp path
    }
    uv_fs_req_cleanup(req);
    free_slot(loop_data, slot);  // free the uv_req_t
}

HsInt hs_uv_fs_mkdtemp_threaded(const char* tpl, HsInt tpl_size, char* temp_path, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    strcpy(temp_path, tpl);
    strcpy(temp_path + tpl_size, "XXXXXX");
    loop_data->buffer_table[slot] = temp_path;
    int r = uv_fs_mkdtemp(loop, req, temp_path, hs_uv_fs_mkdtemp_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_rmdir_threaded(const char* path, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_rmdir(loop, req, path, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

void hs_uv_fs_scandir_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    uv_dirent_t*** dents = (uv_dirent_t***)loop_data->buffer_table[slot];
    if (dents != NULL) {
        *dents = req->ptr;
        // save the dent struct array pointer
        // push the slot to event queue
        loop_data->buffer_size_table[slot] = (HsInt)req->result;
        loop_data->event_queue[loop_data->event_counter] = slot;
        loop_data->event_counter += 1;
    }
    //  we can't cleanup request here, because doing that will
    //  destory our dents array, which we haven't copied in Haskell yet. 
    //  so we manually break down uv_fs_req_cleanup here:
    //  we free path buffer first, then clean up dents later using
    //  hs_uv_fs_scandir_cleanup, or hs_uv_fs_scandir_extra_cleanup 
    //  in case of async exception.
#if defined(_WIN32)
    if (req->file.pathw != NULL)
        uv__free(req->file.pathw);  
#else
    if (req->path != NULL)
        uv__free((void*) req->path);
#endif
    free_slot(loop_data, slot);  // free the uv_req_t
}

void hs_uv_fs_scandir_extra_cleanup(uv_dirent_t*** dents_p, HsInt n){
    int i;
    uv_dirent_t** dents = *dents_p;
    if (dents != NULL) {
        for (i=0; i<n; i++){
            uv__fs_scandir_free(dents[i]);
        }
        uv__fs_scandir_free(dents);
    }
}

HsInt hs_uv_fs_scandir_threaded(const char* path, uv_dirent_t*** dents, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)dents;
    int r = uv_fs_scandir(loop, req, path, 0, hs_uv_fs_scandir_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

void hs_uv_fs_stat_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    uv_stat_t* stat = (uv_stat_t*)loop_data->buffer_table[slot];
    if (stat != NULL) {
        // push the slot to event queue
        loop_data->buffer_size_table[slot] = (HsInt)req->result;
        loop_data->event_queue[loop_data->event_counter] = slot;
        loop_data->event_counter += 1;
        memcpy(stat, &req->statbuf, sizeof(uv_stat_t));  // save the temp path
    }
    uv_fs_req_cleanup(req);
    free_slot(loop_data, slot);  // free the uv_req_t
}

HsInt hs_uv_fs_stat_threaded(const char* path, uv_stat_t* stat, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)stat;
    int r = uv_fs_stat(loop, req, path, hs_uv_fs_stat_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_fstat_threaded(int32_t file, uv_stat_t* stat, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)stat;
    int r = uv_fs_fstat(loop, req, file, hs_uv_fs_stat_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_lstat_threaded(const char* path, uv_stat_t* stat, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)stat;
    int r = uv_fs_lstat(loop, req, path, hs_uv_fs_stat_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_rename_threaded(const char* path, const char* path2, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_rename(loop, req, path, path2, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_fsync_threaded(int32_t file, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_fsync(loop, req, file, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_fdatasync_threaded(int32_t file, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_fdatasync(loop, req, file, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_ftruncate_threaded(int32_t file, int64_t off, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_ftruncate(loop, req, file, off, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_copyfile_threaded(const char* path, const char* path2, int flag, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_copyfile(loop, req, path, path2, flag, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_access_threaded(const char* path, int mode, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_access(loop, req, path, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_chmod_threaded(const char* path, int mode, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_chmod(loop, req, path, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_fchmod_threaded(int32_t file, int mode, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_fchmod(loop, req, file, mode, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_utime_threaded(const char* path, double atime, double mtime, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_utime(loop, req, path, atime, mtime, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_futime_threaded(int32_t file, double atime, double mtime, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_futime(loop, req, file, atime, mtime, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_link_threaded(const char* path, const char* path2, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_link(loop, req, path, path2, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

HsInt hs_uv_fs_symlink_threaded(const char* path, const char* path2, int flag, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    int r = uv_fs_symlink(loop, req, path, path2, flag, hs_uv_fs_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

void hs_uv_fs_readlink_callback(uv_fs_t* req){
    uv_loop_t* loop = req->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = (HsInt)req->data; 
    char** path = (char**)loop_data->buffer_table[slot];
    if (path != NULL) {
        *path = req->ptr;  // save the result path
        // push the slot to event queue
        loop_data->buffer_size_table[slot] = (HsInt)req->result;
        loop_data->event_queue[loop_data->event_counter] = slot;
        loop_data->event_counter += 1;
    }
    //  for the same reason with 'scandir', we can't cleanup request here
#if defined(_WIN32)
    if (req->file.pathw != NULL)
        uv__free(req->file.pathw);  
#else
    if (req->path != NULL)
        uv__free((void*) req->path);
#endif
    free_slot(loop_data, slot);  // free the uv_req_t
}

void hs_uv_fs_readlink_extra_cleanup(char** path){
    if (*path != NULL) uv__free(*path);
}

HsInt hs_uv_fs_readlink_threaded(const char* path, char** result_path, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)result_path;
    int r = uv_fs_readlink(loop, req, path, hs_uv_fs_readlink_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}

// share cleanup and callback with readlink
HsInt hs_uv_fs_realpath_threaded(const char* path, char** result_path, uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_fs_t* req = 
        (uv_fs_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;
    loop_data->buffer_table[slot] = (char*)result_path;
    int r = uv_fs_realpath(loop, req, path, hs_uv_fs_readlink_callback);
    if (r < 0) {
        free_slot(loop_data, slot);
        return (HsInt)r;
    } else return slot;
}
