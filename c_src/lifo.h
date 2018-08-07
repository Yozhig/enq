#ifndef _LIFO_H
#define _LIFO_H

typedef struct lifo_t {
  void *head;
  void *tail;
  unsigned long long count;
} lifo_t;

typedef struct lifo_handle_t {
  void *next;
  void *prev;
} lifo_handle_t;

#define lifo_init(lifo) \
do {                    \
  lifo_t *__q = lifo;   \
  __q->head = NULL;     \
  __q->tail = NULL;     \
  __q->count = 0;       \
} while (0)

#define __lifo_push(lifo, p, h)   \
do {                              \
  lifo_t *__q = lifo;             \
  __typeof__ (p) e = p;           \
  e->h.next = __q->head;          \
  e->h.prev = NULL;               \
  if (__q->head == NULL) {        \
    __q->tail = e;                \
  } else {                        \
    __typeof__ (e) t = __q->head; \
    t->h.prev = e;                \
  }                               \
  __q->head = e;                  \
  __q->count++;                   \
} while (0)

#define lifo_push(lifo, p) __lifo_push (lifo, p, lifo_handle)

#define __lifo_pop(lifo, p, h)      \
do {                                \
  lifo_t *__q = lifo;               \
  p = __q->head;                    \
  if (p != NULL) {                  \
    __q->count--;                   \
    __q->head = p->h.next;          \
    if (__q->head != NULL) {        \
      __typeof__ (p) t = __q->head; \
      t->h.prev = NULL;             \
    } else {                        \
      __q->tail = NULL;             \
    }                               \
  }                                 \
} while (0)

#define lifo_pop(lifo, p) __lifo_pop (lifo, p, lifo_handle)

#define lifo_length(lifo) ((lifo)->count)

#define lifo_empty(lifo) ((lifo)->count == 0)

#endif /* _LIFO_H */
