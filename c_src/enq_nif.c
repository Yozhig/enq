#define _GNU_SOURCE

#include <erl_nif.h>

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include "fifo.h"
#include "lifo.h"

typedef struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM fifo;
    ERL_NIF_TERM lifo;
    ERL_NIF_TERM ttl;
    ERL_NIF_TERM max_size;
} atoms_t;

typedef struct {
    ErlNifResourceType *queue;
    atoms_t atoms;
} priv_t;

typedef struct {
    union {
        fifo_handle_t fifo;
        lifo_handle_t lifo;
    } handle;
    ErlNifBinary data;
    struct timespec added;
} item_t;

typedef enum {
    QTYPE_FIFO = 0,
    QTYPE_LIFO
} queue_type_t;

typedef struct queue {
    union {
        fifo_t fifo;
        lifo_t lifo;
    } queue;
    uint64_t ttl;
    uint64_t max_size;
    void (*push) (struct queue *inst, item_t *item);
    item_t* (*pop) (struct queue *inst);
    void (*free) (struct queue *inst);
    uint64_t (*size) (struct queue *inst);
    void (*cleanup) (struct queue *inst);
} queue_t;

// returns tuple {error, atom()}
static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char *error) {
    priv_t *priv = (priv_t *) enif_priv_data(env);

    return enif_make_tuple2(env, priv->atoms.error, enif_make_atom(env, error));
}

// returns time diff in milliseconds
static inline int64_t
tdiff(struct timespec *t2, struct timespec *t1) {
    return (t2->tv_sec * 1000 + t2->tv_nsec / 1000000UL) -
           (t1->tv_sec * 1000 + t1->tv_nsec / 1000000UL);
}

static inline void
gettime(struct timespec *tp) {
    int rc = clock_gettime(CLOCK_MONOTONIC_RAW, tp);
    assert(rc == 0);
}

/******************************************************************************/
/* FIFO callbacks */
/******************************************************************************/

static void
cleanup_fifo(queue_t *inst) {
    struct timespec now;

    gettime(&now);

    for (;;) {
        item_t *item = NULL;
        __fifo_peak(&inst->queue.fifo, item, handle.fifo);

        if (item == NULL)
            return;

        int64_t diff = tdiff(&now, &item->added);
        if (diff < inst->ttl) {
            return;
        } else {
            __fifo_pop(&inst->queue.fifo, item, handle.fifo);
            enif_release_binary(&item->data);
            enif_free(item);
        }
    }
}

static void
push_fifo(queue_t *inst, item_t *item) {
    __fifo_push(&inst->queue.fifo, item, handle.fifo);
}

static item_t *
pop_fifo(queue_t *inst) {
    item_t *item = NULL;

    if (inst->ttl > 0) {
        struct timespec now;

        gettime(&now);

        for (;;) {
            __fifo_pop(&inst->queue.fifo, item, handle.fifo);

            if (item == NULL)
                return NULL;

            int64_t diff = tdiff(&now, &item->added);
            if (diff < inst->ttl) {
                return item;
            } else {
                enif_release_binary(&item->data);
                enif_free(item);
            }
        }
    } else {
        __fifo_pop(&inst->queue.fifo, item, handle.fifo);
    }

    return item;
}

static void
free_fifo(queue_t *inst) {
    item_t *item;

    for(;;) {
        __fifo_pop(&inst->queue.fifo, item, handle.fifo);

        if (item == NULL)
            return;

        enif_release_binary(&item->data);
        enif_free(item);
    }
}

static uint64_t
size_fifo(queue_t *inst) {
    return fifo_length(&inst->queue.fifo);
}

/******************************************************************************/
/* LIFO callbacks */
/******************************************************************************/

static void
cleanup_lifo(queue_t *inst) {
    struct timespec now;

    gettime(&now);

    for(;;) {
        item_t *item = inst->queue.lifo.tail;

        if (item == NULL)
            return;

        int64_t diff = tdiff(&now, &item->added);
        if (diff < inst->ttl) {
            return;
        } else {
            item_t *prev = item->handle.lifo.prev;

            if (prev != NULL)
                prev->handle.lifo.next = NULL;

            inst->queue.lifo.tail = prev;

            enif_release_binary(&item->data);
            enif_free(item);
        }
    }
}

static void
push_lifo(queue_t *inst, item_t *item) {
    __lifo_push(&inst->queue.lifo, item, handle.lifo);
}

static item_t *
pop_lifo(queue_t *inst) {
    item_t *item = NULL;

    if (inst->ttl > 0)
        cleanup_lifo(inst);

    __lifo_pop(&inst->queue.lifo, item, handle.lifo);

    return item;
}

static void
free_lifo(queue_t *inst) {
    item_t *item;

    for(;;) {
        __lifo_pop(&inst->queue.lifo, item, handle.lifo);

        if (item == NULL)
            return;

        enif_release_binary(&item->data);
        enif_free(item);
    }
}

static uint64_t
size_lifo(queue_t *inst) {
    return lifo_length(&inst->queue.lifo);
}

/******************************************************************************
** NIFs
*******************************************************************************/

static ERL_NIF_TERM
new_queue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_list(env, argv[0]))
        return enif_make_badarg(env);

    priv_t *priv = (priv_t *) enif_priv_data(env);

    queue_type_t qtype = QTYPE_FIFO;
    unsigned long ttl = 0;
    unsigned long max_size = 0;

    ERL_NIF_TERM settings_list = argv[0];
    ERL_NIF_TERM head;

    // parses proplist [fifo, lifo, {ttl, non_neg_integer()}, {max_size, non_neg_integer()}]
    while(enif_get_list_cell(env, settings_list, &head, &settings_list))
    {
        const ERL_NIF_TERM *items;
        int arity;

        if (enif_is_atom(env, head)) {
            if (enif_is_identical(head, priv->atoms.fifo)) {
                qtype = QTYPE_FIFO;
            } else if (enif_is_identical(head, priv->atoms.lifo)) {
                qtype = QTYPE_LIFO;
            } else {
                return enif_make_badarg(env);
            }
        } else if (enif_get_tuple(env, head, &arity, &items) && arity == 2) {
            if (enif_is_identical(items[0], priv->atoms.ttl)) {
                if (!enif_get_ulong(env, items[1], &ttl)) {
                    return enif_make_badarg(env);
                }
            } else if (enif_is_identical(items[0], priv->atoms.max_size)) {
                if (!enif_get_ulong(env, items[1], &max_size)) {
                    return enif_make_badarg(env);
                }
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    queue_t *inst = (queue_t *) enif_alloc_resource(priv->queue, sizeof(*inst));

    if (inst == NULL)
        return make_error(env, "enif_alloc_resource");

    inst->ttl = ttl;
    inst->max_size = max_size;

    switch (qtype) {
        case QTYPE_FIFO:
            fifo_init(&inst->queue.fifo);
            inst->push = &push_fifo;
            inst->pop = &pop_fifo;
            inst->free = &free_fifo;
            inst->size = &size_fifo;
            inst->cleanup = &cleanup_fifo;
            break;
        case QTYPE_LIFO:
            lifo_init(&inst->queue.lifo);
            inst->push = &push_lifo;
            inst->pop = &pop_lifo;
            inst->free = &free_lifo;
            inst->size = &size_lifo;
            inst->cleanup = &cleanup_lifo;
            break;
    }

    ERL_NIF_TERM result = enif_make_resource(env, inst);
    enif_release_resource(inst);

    return enif_make_tuple2(env, priv->atoms.ok, result);
}

static ERL_NIF_TERM
push_item(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    priv_t *priv = (priv_t *) enif_priv_data(env);

    queue_t *inst;

    if (!enif_get_resource(env, argv[0], priv->queue, (void**) &inst))
        return enif_make_badarg(env);

    // todo: check an owner of the queue

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    if (inst->ttl > 0) {
        inst->cleanup(inst);
    }

    if (inst->max_size > 0 && inst->size(inst) >= inst->max_size) {
        return enif_make_tuple2(env, priv->atoms.error, priv->atoms.max_size);
    }

    item_t *item = (item_t *) enif_alloc(sizeof(*item));

    if (item == NULL)
        return make_error(env, "enif_alloc");

    if (!enif_alloc_binary(bin.size, &item->data)) {
        enif_free(item);
        return make_error(env, "enif_alloc_binary");
    }

    memcpy(item->data.data, bin.data, bin.size);

    if (inst->ttl > 0) {
        gettime(&item->added);
    }

    inst->push(inst, item);
    return priv->atoms.ok;
}

static ERL_NIF_TERM
pop_item(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    priv_t *priv = (priv_t *) enif_priv_data(env);

    queue_t *inst;
    item_t *item;

    if (!enif_get_resource(env, argv[0], priv->queue, (void**) &inst))
        return enif_make_badarg(env);

    // todo: check an owner of the queue

    item = inst->pop(inst);

    if (item == NULL)
        return enif_make_list(env, 0);

    ERL_NIF_TERM result = enif_make_binary(env, &item->data);

    enif_free(item);

    return enif_make_list1(env, result);
}

static ERL_NIF_TERM
queue_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    priv_t *priv = (priv_t *) enif_priv_data(env);

    queue_t *inst;

    if (!enif_get_resource(env, argv[0], priv->queue, (void**) &inst))
        return enif_make_badarg(env);

    return enif_make_uint64(env, inst->size(inst));
}

/******************************************************************************
** NIF initialization
*******************************************************************************/

static void
enq_queue_free(ErlNifEnv* env, void* obj) {
    queue_t *inst = obj;
    inst->free(inst);
}

static priv_t *
make_priv(ErlNifEnv *env) {
    priv_t *priv = enif_alloc(sizeof(*priv));

    if (priv == NULL)
        return NULL;

    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    priv->queue = enif_open_resource_type(env, NULL, "enq_queue", enq_queue_free, flags, NULL);

    priv->atoms.ok = enif_make_atom(env, "ok");
    priv->atoms.error = enif_make_atom(env, "error");
    priv->atoms.fifo = enif_make_atom(env, "fifo");
    priv->atoms.lifo = enif_make_atom(env, "lifo");
    priv->atoms.ttl = enif_make_atom(env, "ttl");
    priv->atoms.max_size = enif_make_atom(env, "max_size");

    return priv;
}

static int
enq_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    *priv_data = make_priv(env);

    return 0;
}

static int
enq_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
    *priv_data = make_priv(env);

    return 0;
}

static ErlNifFunc enq_nif_funcs[] = {
    {"new", 1, new_queue},
    {"push", 2, push_item},
    {"pop", 1, pop_item},
    {"size", 1, queue_size},
};

ERL_NIF_INIT(enq_nif, enq_nif_funcs, enq_nif_load, NULL, enq_nif_upgrade, NULL)