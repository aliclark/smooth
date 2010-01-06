
#include "smoothlang/anc2020/_smooth.h"

#ifndef __inline__
#  if __STDC_VERSION__ >= 199901L
#    define __inline__ inline
#  else
#    define __inline__ /*inline*/
#  endif
#endif

#ifndef __restrict__
#  if __STDC_VERSION__ >= 199901L
#    define __restrict__ restrict
#  else
#    define __restrict__ /*restrict*/
#  endif
#endif

void smooth_execute (void);

/*---------------------------------------------------------------------------*/

smooth_t smooth_argc;
smooth_t smooth_argv;

smooth_t smooth_pc = 0;

#ifdef SMOOTH_FIXED_STACK
static smooth_t smooth_stack[SMOOTH_STACK_SIZE];
smooth_t* __restrict__ smooth_sp = smooth_stack;
#else
smooth_linked_array_t* __restrict__ smooth_stack;
smooth_t smooth_sp = 0;
#endif

/* The space for closures */
smooth_closure_t smooth_closures[SMOOTH_CLOSURE_MEM];

static smooth_closure_t* smooth_cp = smooth_closures + SMOOTH_CLOSURE_MEM;

/*---------------------------------------------------------------------------*/

static __inline__ void smooth_call_lambda    (smooth_t x);
static __inline__ void smooth_call_closure   (smooth_t x);
static __inline__ void smooth_call_primitive (smooth_t x);
static __inline__ void smooth_thread_free    (smooth_th_t th);

static smooth_th_t smooth_thread_alloc (void);

static void smooth_gc (void);

/*---------------------------------------------------------------------------*/

#ifndef SMOOTH_FIXED_STACK

#define EXIT_FAILURE 1

typedef unsigned long int size_t;

void exit (int s);
void perror (const char* s);
void* malloc (size_t s);

static long int smooth__memory_msb (unsigned long int number) __attribute__((const));
static unsigned long int smooth__memory_ceil_base2 (const unsigned long int x) __attribute__((const));


/*
 * Initialises the members on the head of the structure.
 * size is the size of each element, and max is the number
 * of items we want to allocate for this head.
 */
static __inline__ void smooth__linked_array_initialise (smooth_linked_array_t *list,
							smooth_t max);

/*
 * Frees the top half of the link_array array memory
 * and the structure of the linked_array head.
 */
static __inline__ void smooth__linked_array_free_head (smooth_linked_array_t *list);

/*---------------------------------------------------------------------------*/

static long int smooth__memory_msb (unsigned long int n) {
  long int pos = 0;
  long int tmp = 0;
  tmp = n >> 16;
  
  if (tmp != 0) {
    n = tmp;
    pos = pos + 16;
  }
  tmp = n >> 8;
  
  if (tmp != 0) {
    n = tmp;
    pos = pos + 8;
  }
  tmp = n >> 4;
  
  if (tmp != 0) {
    n = tmp;
    pos = pos + 4;
  }
  tmp = n >> 2;
  
  if (tmp != 0) {
    n = tmp;
    pos = pos + 2;
  }
  tmp = n >> 1;
  
  if (tmp != 0) {
    n = tmp;
    pos = pos + 1;
  }
  return pos + n - 1;
}

static unsigned long int smooth__memory_ceil_base2 (const unsigned long int x) {
  return (x & (x - 1)) ? ((unsigned long int) 1) << (smooth__memory_msb( x) + 1) : x;
}

/*---------------------------------------------------------------------------*/

static __inline__ void smooth__linked_array_initialise (smooth_linked_array_t *list,
							smooth_t max) {
  list->array  = malloc(sizeof(smooth_t) * max);
  list->max    = max;
  list->length = max * 2;
  list->rest   = NULL;
}

static __inline__ void smooth__linked_array_free_head (smooth_linked_array_t *list) {
  FREE(list->array);
  FREE(list);
}

smooth_linked_array_t *smooth__linked_array_allocate (smooth_t length) {
  smooth_t max;
  smooth_linked_array_t* a = malloc(sizeof(smooth_linked_array_t));
  smooth_linked_array_t* b = malloc(sizeof(smooth_linked_array_t));

  length = (smooth_t) smooth__memory_ceil_base2( length);
  max    = (smooth_t) ((length == 1) ? 1 : (length / 2));
  
  smooth__linked_array_initialise( a, max);
  smooth__linked_array_initialise( b, max);
  
  b->rest = a;
  return b;
}

static void smooth__linked_array_free (smooth_linked_array_t *list) {
  if (list->rest) {
    smooth__linked_array_free(list->rest);
  }
  smooth__linked_array_free_head(list);
}

static smooth_linked_array_t *smooth__linked_array_grow (smooth_linked_array_t *list) {
  smooth_linked_array_t* a = malloc(sizeof(smooth_linked_array_t));
  smooth__linked_array_initialise(a, list->length);
  a->rest = list;
  return a;
}

static smooth_linked_array_t *smooth__linked_array_shrink (smooth_linked_array_t *list) {
  smooth_linked_array_t *rest = list->rest;
  
  if (!rest || !rest->rest) {
    perror( "Error: Tried to shrink linked_array "
            "beyond initial size.");
    exit( EXIT_FAILURE);
  }
  smooth__linked_array_free_head( list);
  return rest;
}

/*---------------------------------------------------------------------------*/


/*
 * The following functions have the same general code inlined in 3 places,
 * because pop() and push() are such frequent calls they must be maximally quick.
 */


/* This is the "1" in our O(1). It must be zippidy zip! */
smooth_t smooth_la_spoff (smooth_t index) {
  smooth_linked_array_t *list = smooth_stack;
  while (index < list->max) {
    if (list->rest) {
      list = list->rest;
    } else {
      return *(list->array + index);
    }
  }
  return *(list->array + (index - list->max));
}

smooth_t smooth_la_pop (void)
{
  smooth_linked_array_t *list = smooth_stack;
  smooth_t index = --smooth_sp;
  while (index < list->max) {
    if (list->rest) {
      list = list->rest;
    } else {
      return *(list->array + index);
    }
  }
  return *(list->array + (index - list->max));
}

void smooth_la_push (smooth_t x) {
  smooth_linked_array_t* list;
  smooth_t index = smooth_sp++;

  if (smooth_sp >= smooth_stack->length) {
    smooth_stack = smooth__linked_array_grow(smooth_stack);
  }
  list = smooth_stack;

  while (index < list->max) {
    if (list->rest) {
      list = list->rest;
    } else {
      *(list->array + index) = x;
      return;
    }
  }
  *(list->array + (index - list->max)) = x;
}

#endif /* !SMOOTH_FIXED_STACK */

/*---------------------------------------------------------------------------*/

static __inline__ void smooth_call_lambda (smooth_t x) {
  SMOOTH_PUSH(NULL);
  smooth_pc = x;
  smooth_execute();
}

/*
This does not permit having another closure as the code part of the closure.
I'm not sure that we would ever want a closure as the code part.
*/
static __inline__ void smooth_call_closure (smooth_t x) {
  smooth_t code = SMOOTH__CLOSURE_CODE(x);
  smooth_t local;
  if (SMOOTH_LAMBDA_P(code)) {
    SMOOTH_PUSH(x);
    smooth_pc = code;
    smooth_execute();
  } else {
    local = SMOOTH_POP();
    SMOOTH_PUSH(((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, local));
  }
}

static __inline__ void smooth_call_primitive (smooth_t fn) {
  smooth_t local = SMOOTH_POP();
  SMOOTH_PUSH(((smooth_t (*)(smooth_t)) fn)(local));
}

void smooth_call (smooth_t x) {
  if (SMOOTH_CLOSURE_P(x)) {
    smooth_call_closure(x);
  } else if (SMOOTH_LAMBDA_P(x)) {
    smooth_call_lambda(x);
  } else {
    smooth_call_primitive(x);
  }
}

/*---------------------------------------------------------------------------*/

smooth_t smooth_apply (smooth_t x, smooth_t y) {
  smooth_t code;

  if (smooth_cp == smooth_closures) {
    /* Free some memory in case the call wants to allocate a closure. */
  }

  if (SMOOTH_CLOSURE_P(x)) {
    code = SMOOTH__CLOSURE_CODE(x);
    if (SMOOTH_LAMBDA_P(code)) {
      SMOOTH_PUSH(y);
      SMOOTH_PUSH(x);
      smooth_pc = code;
      smooth_execute();
      return SMOOTH_POP();
    } else {
      return ((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, y);
    }
    return smooth_apply_closure(x, y);
  } else if (SMOOTH_LAMBDA_P(x)) {
    SMOOTH_PUSH(y);
    SMOOTH_PUSH(NULL);
    smooth_pc = x;
    smooth_execute();
    return SMOOTH_POP();
  } else {
    return ((smooth_t (*)(smooth_t)) x)(y);
  }
}

smooth_t smooth_spark_apply (smooth_t x, smooth_t y) {
  smooth_th_t th = smooth_thread_alloc();
  smooth_t rv    = smooth_apply(x, y);
  smooth_thread_free(th);
  return rv;
}

/*---------------------------------------------------------------------------*/

static smooth_th_t smooth_thread_alloc (void) {
  return 0;
}

static __inline__ void smooth_thread_free (smooth_th_t th) {

  // good opportunity to also call gc here.
}

/*---------------------------------------------------------------------------*/

smooth_t smooth_closure_create (smooth_t lambda, smooth_t local, smooth_closure_t* parent) {
  smooth_closure_t* rv;
  if (smooth_cp == smooth_closures) {
    // gc time!
  }
  rv = --smooth_cp;
  rv->lambda = lambda;
  rv->local  = local;
  rv->parent = parent;
  return (smooth_t) rv;
}

smooth_t smooth_closure_local (smooth_closure_t* x) {
  return x->local;
}

smooth_closure_t* smooth_closure_parent (smooth_closure_t* x) {
  return x->parent;
}

/*---------------------------------------------------------------------------*/

static smooth_t smooth_refcount;
static void (*smooth_freeptr)(smooth_t);

/*
 * Use to add destructors to memory addresses allocated elsewhere
 * It is possible to have more than one freeptr per address
 * since this can by used for all types of things, eg. the id of a database handle.
 */
void smooth_gc_register (smooth_t ptr, void (*freeptr)(smooth_t)) {
  smooth_freeptr = freeptr;
}

void smooth_gc_incref (smooth_t ptr) {
  ++smooth_refcount;
}

void smooth_gc_decref (smooth_t ptr) {
  if (smooth_refcount < 0) {
    return;
  }
  if (--smooth_refcount == 0) {
#if 0
    smooth_freeptr(ptr);
#endif
  }
}

/*
 * Clean up some space. World must be stopped or
 * an address registered in C may be cleared before the C code has time to push it to stack.
 */
static void smooth_gc (void) {
#if 0
  foreach (n in registered_addresses) {
    if ((refcount == 0) && not_in_stack(n)) {
      smooth_freeptr(n);
    }
  }
#endif
}

