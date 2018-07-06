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
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// loop
//
// initialize a loop with its data to give slot size. return NULL on fail.
uv_loop_t* hs_uv_loop_init(HsInt siz){
    int r; 
    HsInt i;

    uv_loop_t* loop = malloc(sizeof(uv_loop_t));
    r = uv_loop_init(loop);

    if (r < 0) { 
        free(loop);
        return NULL;
    }

    hs_loop_data* loop_data = malloc(sizeof(hs_loop_data));
        
    HsInt* event_queue = malloc(siz*sizeof(HsInt));
    char** buffer_table = malloc(siz*sizeof(char*));
    HsInt* buffer_size_table = malloc(siz*sizeof(HsInt));
    HsInt* slot_table = malloc(siz*sizeof(HsInt));
    HsInt* free_slot_queue = malloc(siz*sizeof(HsInt));
    hs_uv_struct** uv_struct_table = malloc(sizeof(void*));
    hs_uv_struct* uv_struct_table_block = malloc(siz*sizeof(hs_uv_struct));

    uv_async_t* async = malloc(sizeof(uv_async_t));
    uv_timer_t* timer = malloc(sizeof(uv_timer_t));

    if (loop_data == NULL || event_queue == NULL || buffer_table == NULL ||
            buffer_size_table == NULL || slot_table == NULL || free_slot_queue == NULL ||
                uv_struct_table == NULL || uv_struct_table_block == NULL ||
                    async == NULL || timer == NULL ||
                        uv_timer_init(loop, timer) < 0 ||
                            uv_async_init(loop, async, NULL) < 0){
        free(event_queue);
        free(loop_data);
        free(buffer_table);
        free(buffer_size_table);
        free(slot_table);
        free(free_slot_queue);
        free(uv_struct_table);
        free(uv_struct_table_block);

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
        loop_data->free_slot_queue      = free_slot_queue;
        loop_data->free_slot_counter    = 0;
        loop_data->uv_struct_table      = uv_struct_table;
        uv_struct_table[0]              = uv_struct_table_block;
        loop_data->resize               = 0;
        loop_data->size                 = siz;
        loop_data->async                = async;
        loop_data->timer                = timer;
        loop->data = loop_data;
        return loop;
    }
}

// resize a loop's data to given slot size, return NULL on fail.
hs_loop_data* hs_uv_loop_resize(hs_loop_data* loop_data, HsInt siz){
    HsInt i;
    loop_data->resize += 1;
    HsInt* event_queue_new       = realloc(loop_data->event_queue, (siz*sizeof(HsInt)));
    char** buffer_table_new       = realloc(loop_data->buffer_table, (siz*sizeof(char*)));
    HsInt* buffer_size_table_new = realloc(loop_data->buffer_size_table, (siz*sizeof(HsInt)));
    HsInt* slot_table_new = realloc(loop_data->slot_table, (siz*sizeof(HsInt)));
    HsInt* free_slot_queue_new = realloc(loop_data->free_slot_queue, (siz*sizeof(HsInt)));
    hs_uv_struct** uv_struct_table_new = realloc(loop_data->uv_struct_table, (loop_data->resize+1)*sizeof(void*));
    hs_uv_struct* uv_struct_table_block = malloc((loop_data->size)*sizeof(hs_uv_struct));

    if (event_queue_new == NULL || buffer_table_new == NULL ||
            buffer_size_table_new == NULL || slot_table_new == NULL || free_slot_queue_new == NULL ||
                uv_struct_table_new == NULL || uv_struct_table_block == NULL){
        // release new memory
        if (event_queue_new != loop_data->event_queue) free(event_queue_new);
        if (buffer_table_new != loop_data->buffer_table) free(buffer_table_new);
        if (buffer_size_table_new != loop_data->buffer_size_table) free(buffer_size_table_new);
        if (slot_table_new != loop_data->slot_table) free(slot_table_new);
        if (free_slot_queue_new != loop_data->free_slot_queue) free(free_slot_queue_new);
        if (uv_struct_table_new != loop_data->uv_struct_table) free(uv_struct_table_new);
        free(uv_struct_table_block);
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
        loop_data->free_slot_queue    = free_slot_queue_new;
        loop_data->uv_struct_table    = uv_struct_table_new;
        uv_struct_table_new[loop_data->resize] = uv_struct_table_block;
        loop_data->size               = siz;
        return loop_data;
    }
}

// allocate free slot, resize loop data if neccessary 
// return -1 on resize failure, slot otherwise.
HsInt alloc_slot(hs_loop_data* loop_data){
    HsInt r = loop_data->free_slot;
    loop_data->free_slot = loop_data->slot_table[r];
    // the slot exceed range, we should resize
    if (r == loop_data->size-1 &&
        hs_uv_loop_resize(loop_data, (loop_data->size) << 1) == NULL) {
        return -1;
    }
    return r;
}

void free_slot(hs_loop_data* loop_data, HsInt slot){
    loop_data->free_slot_queue[loop_data->free_slot_counter] = slot;
    loop_data->free_slot_counter++;
}

int hs_uv_run(uv_loop_t* loop, uv_run_mode mode){
    hs_loop_data* loop_data = loop->data;
    HsInt* q = loop_data->free_slot_queue;
    HsInt i = loop_data->free_slot_counter;
    HsInt slot;
    // do the real slot release, see notes on slot allocation in hs_uv.h
    for (i--; i >= 0; i--){
        slot = q[i];
        loop_data->slot_table[slot] = loop_data->free_slot;
        loop_data->free_slot = slot;
    }
    loop_data->free_slot_counter = 0;
    return uv_run(loop, mode);
}

hs_uv_struct* fetch_uv_struct(hs_loop_data* loop_data, HsInt slot){
    int bits = 0;
    HsInt slot2 = slot >> INIT_LOOP_SIZE_BIT;
    // __builtin_clz may be a good idea
    while (slot2 > 0){ bits += 1; slot2 = slot2 >> 1; }
    if (bits == 0)
        return loop_data->uv_struct_table[bits] + slot;
    else
        return loop_data->uv_struct_table[bits] + 
            slot - (1<<(INIT_LOOP_SIZE_BIT+bits-1));
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

void uv_timer_wake_cb(uv_timer_t* timer){ uv_timer_stop(timer); }

int hs_uv_wake_up_timer(hs_loop_data* loop_data){
    return uv_timer_start(loop_data->timer, uv_timer_wake_cb, 1, 1);
}
int hs_uv_wake_up_async(hs_loop_data* loop_data){
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

// Initialize a uv_handle_t, with data field set to an unique slot
uv_handle_t* hs_uv_handle_alloc(uv_loop_t* loop){
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return NULL;
    uv_handle_t* handle = 
        (uv_handle_t*)fetch_uv_struct(loop_data, slot);
    handle->loop = loop;
    handle->data = (void*)slot;
    return handle;
}

// Free uv_handle_t only, used when handle initialization failed.
void hs_uv_handle_free(uv_handle_t* handle){
    uv_loop_t* loop = handle->loop;
    free_slot(loop->data, (HsInt)handle->data);
}

// Close and free uv_handle_t
void hs_uv_handle_close(uv_handle_t* handle){
    uv_close(handle, hs_uv_handle_free);
}

////////////////////////////////////////////////////////////////////////////////
// request

// Cancel an ongoing uv request, so that the slot can be freed ASAP.
void hs_uv_cancel(uv_loop_t* loop, HsInt slot){
    hs_loop_data* loop_data = loop->data;
    uv_req_t* req = 
        (uv_req_t*)fetch_uv_struct(loop_data, slot);
    switch (req->type) {
        case UV_CONNECT:
            hs_uv_handle_close((uv_handle_t*)((uv_connect_t*)req)->handle);
            break;
        case UV_WRITE:
            hs_uv_handle_close((uv_handle_t*)((uv_write_t*)req)->handle);
            break;
        case UV_SHUTDOWN:
            hs_uv_handle_close((uv_handle_t*)((uv_shutdown_t*)req)->handle);
            break;
        case UV_UDP_SEND:
            hs_uv_handle_close((uv_handle_t*)((uv_udp_send_t*)req)->handle);
            break;
        default:
            // we do it in best effort basis
            uv_cancel(req);
    }
}
