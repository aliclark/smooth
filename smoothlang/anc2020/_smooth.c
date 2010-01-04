
#include "smoothlang/anc2020/_smooth.h"

#ifndef __include__
#  define __include__ include
#endif

#ifndef __restrict__
#  define __restrict__ restrict
#endif


/* Temporary defn until we properly implement an array of gc'd memory */
void* malloc (smooth_t s);
void smooth_execute (void);


/**********************************/

smooth_t smooth_pc = 0;

#ifdef SMOOTH_FIXED_STACK
static smooth_t smooth_stack[SMOOTH_STACK_SIZE];
smooth_t* __restrict__ smooth_sp = smooth_stack;
#else
smooth_linked_array_t* __restrict__ smooth_stack;
smooth_t  smooth_sp = 0;
#endif

/* The space for closures */
smooth_closure_t smooth_closures[SMOOTH_CLOSURE_MEM];

static smooth_closure_t* smooth_cp = smooth_closures + SMOOTH_CLOSURE_MEM;



/**********************************/

static __inline__ void smooth_call_lambda    (smooth_t x);
static __inline__ void smooth_call_closure   (smooth_t x);
static __inline__ void smooth_call_primitive (smooth_t x);
static __inline__ void smooth_thread_free    (smooth_th_t th);

static smooth_t smooth_apply_lambda    (smooth_t x, smooth_t y);
static smooth_t smooth_apply_closure   (smooth_t x, smooth_t y);
static smooth_t smooth_apply_primitive (smooth_t x, smooth_t y);

static smooth_th_t smooth_thread_alloc (void);

static void smooth_gc      (void);

/**********************************/

static __inline__ void smooth_call_lambda (smooth_t x) {
  SMOOTH__PUSH(NULL);
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
    SMOOTH__PUSH(x);
    smooth_pc = code;
    smooth_execute();
  } else {
    local = SMOOTH__POP();
    SMOOTH__PUSH(((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, local));
  }
}

static __inline__ void smooth_call_primitive (smooth_t fn) {
  smooth_t local = SMOOTH__POP();
  SMOOTH__PUSH(((smooth_t (*)(smooth_t)) fn)(local));
}

void smooth__call (smooth_t x) {
  if (SMOOTH_CLOSURE_P(x)) {
    smooth_call_closure(x);
  } else {
    smooth_call_primitive(x);
  }
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


/**********************************/


static smooth_t smooth_apply_lambda (smooth_t x, smooth_t y) {
  SMOOTH__PUSH(y);
  SMOOTH__PUSH(NULL);
  smooth_pc = x;
  smooth_execute();
  return SMOOTH__POP();
}

/*
This does not permit having another closure as the code part of the closure.
I'm not sure that we would ever want a closure as the code part.
*/
static smooth_t smooth_apply_closure (smooth_t x, smooth_t y) {
  smooth_t code = SMOOTH__CLOSURE_CODE(x);

  if (SMOOTH_LAMBDA_P(code)) {
    SMOOTH__PUSH(y);
    SMOOTH__PUSH(x);
    smooth_pc = code;
    smooth_execute();
    return SMOOTH__POP();
  } else {
    return ((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, y);
  }
}

static smooth_t smooth_apply_primitive (smooth_t x, smooth_t y) {
  return ((smooth_t (*)(smooth_t)) x)(y);
}


/**********************************/


static smooth_th_t smooth_thread_alloc (void) {
  return 0;
}

/*
 * This runs when there are no more calls to make on a thread.
 */
static __inline__ void smooth_thread_free (smooth_th_t th) {

}

/*
 * Clean up some space. World must be stopped or
 * an address registered in C may be cleared before the C code has time to push it to stack.
 */
static void smooth_gc (void) {

}


/**********************************/


smooth_t smooth_apply (smooth_t x, smooth_t y) {
  if (SMOOTH_CLOSURE_P(x)) {
    return smooth_apply_closure(x, y);
  } else if (SMOOTH_LAMBDA_P(x)) {
    return smooth_apply_lambda(x, y);
  } else {
    return smooth_apply_primitive(x, y);
  }
}

smooth_t smooth_spark_apply (smooth_t x, smooth_t y) {
  smooth_th_t th = smooth_thread_alloc();
  smooth_t rv    = smooth_apply(x, y);
  smooth_thread_free(th);
  return rv;
}


/**********************************/


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


/**********************************/


/*
 * Use to add destructors to memory addresses allocated elsewhere
 * It is possible to have more than one freeptr per address
 * since this can by used for all types of things, eg. the id of a database handle.
 */
void smooth_gc_register (smooth_t ptr, void (*freeptr)(smooth_t)) {

}

/* Causes GC to allocate us some memory from it's own heap memory. */
void* smooth_gc_allocate (smooth_t size, void (*freeptr)(smooth_t)) {

  void* rv = malloc(size); /* todo: change this to allocate from heap mem */
  smooth_gc_register((smooth_t) rv, freeptr); /* This requires all cleanup fns to run before heap moving. */
  return rv;
}

void smooth_gc_incref (smooth_t ptr) {

}

void smooth_gc_decref (smooth_t ptr) {

}


/**********************************/


void smooth_push (smooth_t x) {
#ifdef SMOOTH_FIXED_STACK
  SMOOTH__PUSH(x);
#else
  if (smooth_sp >= smooth_stack->length) {
    smooth_stack = smooth__linked_array_grow(smooth_stack);
  }
  *smooth__linked_array_get(smooth_stack, smooth_sp++) = x;
#endif
}

smooth_t smooth_pop (void) {
  return SMOOTH__POP();
}
