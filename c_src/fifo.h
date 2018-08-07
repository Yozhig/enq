#ifndef _FIFO_H
#define _FIFO_H

/* Main FIFO structure. Allocate memory for it yourself. */
typedef struct fifo_t {
  void *head;
  void *tail;
  unsigned long long count;
} fifo_t;

typedef struct fifo_handle_t {
  void *next;
} fifo_handle_t;

/* Initializes fifo structure. */
#define fifo_init(fifo) \
do {                    \
  fifo_t *__q = fifo;   \
  __q->head = NULL;     \
  __q->tail = NULL;     \
  __q->count = 0;       \
} while (0)

#define __fifo_push(fifo, p, h)   \
do {                              \
  fifo_t *__q = fifo;             \
  __typeof__ (p) e = p;           \
  e->h.next = NULL;               \
  if (__q->tail == NULL) {        \
    __q->head = e;                \
  } else {                        \
    __typeof__ (e) t = __q->tail; \
    t->h.next = e;                \
  }                               \
  __q->tail = e;                  \
  __q->count++;                   \
} while (0)

/* Puts an element to the queue. */
#define fifo_push(fifo, p) __fifo_push (fifo, p, fifo_handle)

#define __fifo_pop(fifo, p, h) \
do {                           \
  fifo_t *__q = fifo;          \
  p = __q->head;               \
  if (p != NULL) {             \
    __q->count--;              \
    __q->head = p->h.next;     \
    if (__q->tail == p)        \
      __q->tail = NULL;        \
  }                            \
} while (0)

/* Pops the first element out of the queue. */
#define fifo_pop(fifo, p) __fifo_pop (fifo, p, fifo_handle)

#define __fifo_peak(fifo, p, h) \
do {                            \
  p = (fifo)->head;             \
} while (0)

/* Returns the first elemnt of the queue without removing. */
#define fifo_peak(fifo, p) __fifo_peak (fifo, p, fifo_handle)

/* Returns the length of the queue. */
#define fifo_length(fifo) ((fifo)->count)

/* Returns true if the queue is empty. */
#define fifo_empty(fifo) ((fifo)->count == 0)

#endif /* _FIFO_H */
