
#ifndef __restrict__
#  if __STDC_VERSION__ >= 199901L
#    define __restrict__ restrict
#  else
#    define __restrict__ /*restrict*/
#  endif
#endif

/*
 * Whilst all memory is growable on demand, the start size can be changed to suit the application.
 *
 * All sizes _must_ be specified as a power of two and greater than 0.
 * Also, these are no. of items, so its more like 16 bytes per closure and
 * 4 bytes each for stack element and gc table entry
 *
 * Default settings of 1 item per structure are only reasonable for applications
 * which want to be able to use a very tiny amount of bss where possible.
 * Most applications do not care about that and should increase these values to
 * probably at least 1024,1024,1024,16
 *
 * These are manifests rather than constants so they can be defined on command line easily.
 */
#ifndef STACK_SIZE
#  define STACK_SIZE 1024
#endif
#ifndef CLOSURES_FREELIST_SIZE
#  define CLOSURES_FREELIST_SIZE 32
#endif
#ifndef CLOSURES_SIZE
#  define CLOSURES_SIZE 1024
#endif
#ifndef CLOSURES_BODY_FREELIST_SIZE
#  define CLOSURES_BODY_FREELIST_SIZE 32
#endif
#ifndef CLOSURES_BODY_SIZE
#  define CLOSURES_BODY_SIZE 1024
#endif
#ifndef GC_TABLE_SIZE
#  define GC_TABLE_SIZE 1024
#endif
#ifndef CALL_REGISTER_SIZE
#  define CALL_REGISTER_SIZE 512
#endif

/*
 * Each memory area should have an option to comile statically so that it doesn't grow dynamically at run time.
 * This will allow code to run closer to the metal, but at the risk of safety.
 */

#ifdef STACK_STATIC
  #ifdef STACK_NO_BOUNDS_CHECK
  #endif
#endif
#ifdef CLOSURES_FREELIST_STATIC
  #ifdef CLOSURES_FREELIST_NO_BOUNDS_CHECK
  #endif
#endif
#ifdef CLOSURES_STATIC
  #ifdef CLOSURES_NO_BOUNDS_CHECK
  #endif
#endif
#ifndef GC_TABLE_STATIC
  #ifdef GC_TABLE_NO_BOUNDS_CHECK
  #endif
#endif
#ifdef CALL_REGISTER_STATIC
  #ifdef CALL_REGISTER_NO_BOUNDS_CHECK
  #endif
#endif

/*
 * If using RTS_STATIC, the compiler implementation does not need to
 * preserve an ABI between the generated C code and the RTS -
 * it may provide RTS definitions as macros which are burnt into the program code at compile time.
 * This allows the code to run closer to the metal,
 * but at the expense of the ability to switch out run time engine after the app has been compiled
 */

#ifdef RTS_STATIC
#endif

/*
 * The native stack may be used entirely instead of the homemade smooth stack for normal function calls
 * (note that tail calls will still be optimised)
 *
 * This will be preferable in the case where you would otherwise have defined a STACK_SIZE
 * much smaller than the native stack.
 *
 * Of course, the native stack will not have quite as much space for calls in your own code
 * because the run time itself will need to use it for calling primitives
 * and they will need to use it for calling smooth_apply, which may add up.
 *
 * In other words, Know What You Are Doing when setting this option.
 */
#ifdef NATIVE_STACK
#endif

/* OLD NOTES ON IMPLEMENTATION. NOTE THAT THREADING IS NO LONGER BEING CONSIDERED.

Falling through the cracks?
The current ideology is that we will keep watch on all the interfaces to maintain count.
these are PUSH, POP, CALL, APPLY, INCREF, DECREF, and GC_REGISTER

The data we need to keep track of are the registered addresses.
A C module may be returning a registered address, or otherwise just a number.

- GC_REGISTER will set the thread-specific expected flag to true
  and incref the address.

Herein still lies a problem as to whether the value returned is the closure or some other value,
so we may instead need an array of expected addresses to look at.


That was for adding data - now for removing it.
The "for x in args" loop after the call will notice if we passed in any registered data that was not returned,
since only rv is incref'd on return. This will cause that data to be freed if it is not used elsewhere.

// Here is the function push:
push(x):
  if is_already_followed(x):
    incref(x)
  just_push(x)

// similarly,
pop():
  x = just_pop()
  if is_already_followed(x):
    decref(x)
  return x

// We use internal pop to get the values
c = just_pop()
b = just_pop()
a = just_pop()
rv = c_call(a, b, c)

push(rv) // this perfoms incref.

// There is one following bit per address per thread.
if th_was_expected():
  decref(rv)

// This is slightly delayed from popping off the stack.
// We delay it until after we have pushed rv from the call.
for arg in [a, b, c]:
  if is_already_followed(arg)
    decref(arg)




So the above seems to have irreconcilable problems.
But would it be possible to have per-thread counting instead then?
So instead of having a universal follow count,
we could instead have a per-thread "expecting" flag.

Simpler yet, since we know only one call is happening at a time on the thread,
we just have one flag saying whether or not the rv of the current call is to be followed.


smooth c_func (smooth a) {
  smooth b = create_closure(...);
  return a; // b is lost in the abyss
}

// but fear not, the rts knows b has been created, and,
// if it hasn't been returned, it can be decref'd.
// now only if the user has explicitly incref'd b can it be saved!


//per thread variables
smooth *call_closure_creations;  // a mallocd array of all closures made in the most recent call.
smooth *call_register_creations; // likewise for any addresses which were registered to gc.

If is probably possible to just implement closure_create, by calling gc_register with the closure.
This would greatly simplify stuff so I'd like to do that.


rv = spark_apply(closure, local);

PER PROCESS
[linked_array]  closures
[hashed_array]  gc refs

PER THREAD (each thread will read a different value accessing these - one per thread)
[realloc]       threadstack



It may be even more complicated - we may need to use a stack of expected addresses,
since a call may be made from within another call.

 */

#include <stdlib.h> /* size_t, EXIT_FAILURE, NULL, exit, malloc, realloc, free */
#include <stdio.h> /* perror */

#include "smoothlang/anc2020/smooth_core.h"

#ifdef __cplusplus
#  define TRUE  true
#  define FALSE false
#else
#  define TRUE  1
#  define FALSE 0
typedef byte bool;
#endif

/*---------------------------------------------------------------------------*/

int smooth_argc;
char** smooth_argv;

/*---------------------------------------------------------------------------*/
/************************** MEMORY ALLOCATION ********************************/

static void die_message (const char* s) {
  fprintf(stderr, "%s\n", s);
  exit(EXIT_FAILURE);
}

static void memory_ensure (void* p, const char* s) {
  if (!p) {
    perror(s);
    exit(EXIT_FAILURE);
  }
}

static void* memory_ensure_alloc (size_t sz, const char* s) {
  void* a = malloc(sz);
  memory_ensure(a, s);
  return a;
}

static void* memory_ensure_realloc (void* p, size_t sz, const char* s) {
  void* a = realloc(p, sz);
  memory_ensure(a, s);
  return a;
}

/*---------------------------------------------------------------------------*/
/**************************** REALLOC_ARRAY **********************************/

/*
 * This is your vanilla growable array memory.
 */

typedef struct realloc_array {
  void* __restrict__ array;
  size_t data_size;
  size_t size;
  size_t initial_size;
} realloc_array;

static realloc_array* realloc_array_allocate (size_t data_size, size_t size) {
  realloc_array* ra = (realloc_array*) memory_ensure_alloc(sizeof(realloc_array), "Could not allocate realloc array");
  ra->array = memory_ensure_alloc(data_size * size, "Could not allocate realloc array memory");
  ra->data_size = data_size;
  ra->size = size;
  ra->initial_size = size;
  return ra;
}

static realloc_array* realloc_array_grow (realloc_array* ra) {
  ra->size *= 2;
  ra->array = memory_ensure_realloc(ra->array, ra->size * ra->data_size,
                                    "Realloc array grow failed");
  return ra;
}

static realloc_array* realloc_array_shrink (realloc_array* ra) {
  if (ra->size > ra->initial_size) {
    ra->size /= 2;
    ra->array = memory_ensure_realloc(ra->array, ra->size * ra->data_size,
                                      "Realloc array shrink failed");
  }
  return ra;
}

static void* realloc_array_get (realloc_array* ra, size_t index) {
  return (void*) (((byte*) ra->array) + (index * ra->data_size));
}

static void realloc_array_free (realloc_array* ra) {
  free(ra->array);
  free(ra);
}

/*---------------------------------------------------------------------------*/
/**************************** REALLOC_STACK **********************************/

/*
 * Unlike a linked list stack, realloc stack returns a pointer to the stack
 * allowing the implementation to use it for larger data structures.
 *
 * Memory access is more localised since we are using the same chunk of memory
 * and push and pop only rarely impose a malloc.
 *
 * One warning though: When you get a pointer into the stack,
 * you must use that pointer before getting another one.
 * Otherwise, the array may have moved and the first pointer will be dangling!
 *
 * Note that this means using the data structure in multi-threaded code
 * means you must lock all of the usage of the returned pointer,
 * not just the initial get.
 */

typedef struct realloc_stack {
  realloc_array* ra;
  size_t rp;
} realloc_stack;

static realloc_stack* realloc_stack_allocate (size_t data_size, size_t size) {
  realloc_stack* rv = (realloc_stack*) memory_ensure_alloc(sizeof(realloc_stack), "Realloc stack initialise failed");
  rv->ra = realloc_array_allocate(data_size, size);
  rv->rp = 0;
  return rv;
}

static void realloc_stack_grow (realloc_stack* rs) {
  rs->ra = realloc_array_grow(rs->ra);
}

static void realloc_stack_shrink (realloc_stack* rs) {
  rs->ra = realloc_array_shrink(rs->ra);
}

static void* realloc_stack_top (realloc_stack* rs) {
  if (rs->rp == rs->ra->size) {
    realloc_stack_grow(rs);
  }
  return realloc_array_get(rs->ra, rs->rp);
}

static void* realloc_stack_up (realloc_stack* rs) {
  void* rv = realloc_stack_top(rs);
  ++rs->rp;
  return rv;
}

static void* realloc_stack_down (realloc_stack* rs) {
  if ((rs->rp * 4) <= rs->ra->size) {
    realloc_stack_shrink(rs);
  }
  return realloc_array_get(rs->ra, --(rs->rp));
}

static bool realloc_stack_null (realloc_stack* rs) {
  return rs->rp == 0;
}

static void realloc_stack_free (realloc_stack* rs) {
  realloc_array_free(rs->ra);
  free(rs);
}

/*---------------------------------------------------------------------------*/
/********************************* LINKED_LIST *******************************/

typedef struct linked_list {
  void* head;
  struct linked_list* tail;
} linked_list;

static linked_list* linked_list_cons (void* head, linked_list* tail) {
  linked_list* rv = (linked_list*) memory_ensure_alloc(sizeof(linked_list), "Linked list cons failed");
  rv->head = head;
  rv->tail = tail;
  return rv;
}

static void* linked_list_head (linked_list* list) {
  return list->head;
}

static linked_list* linked_list_tail (linked_list* list) {
  return list->tail;
}

static void linked_list_free_head (linked_list* l) {
  free(l);
}

static linked_list* linked_list_tail_free (linked_list* l) {
  linked_list* rest = linked_list_tail(l);
  linked_list_free_head(l);
  return rest;
}

static void linked_list_map_free (linked_list* list, void(*freeptr)(void*)) {
  for (; list; list = linked_list_tail_free(list)) {
    freeptr(linked_list_head(list));
  }
}

static void linked_list_free (linked_list* l) {
  linked_list* l_next = l;
  while (l_next) {
    l = l_next;
    l_next = l_next->tail;
    linked_list_free_head(l);
  }
}

/*---------------------------------------------------------------------------*/
/******************************** LINKED_ARRAY *******************************/

/*
 * This is like a realloc'd array, but it only makes new allocations,
 * and so does not use realloc for grow/shrink.
 * This is useful because whereas realloc may move the array,
 * previous allocations made using a linked_array will never be moved,
 * so pointers into it don't need to be updated when growing/shrinking the array.
 */

typedef struct linked_array_section {
  void* __restrict__ array; /* The block of allocated memory for array contents. */
  size_t data_size;
  size_t size;  /* The max capacity of the array memory for this head. */
} linked_array_section;

typedef linked_list linked_array;

static size_t linked_array_size (linked_array* list) {
  linked_array_section* h = (linked_array_section*) linked_list_head(list);
  return linked_list_tail(list) ? h->size * 2 : h->size;
}

static void linked_array_initialise (linked_array_section *list, size_t data_size, size_t size) {
  list->array     = memory_ensure_alloc(data_size * size, "Linked array chunk malloc failed");
  list->data_size = data_size;
  list->size      = size;
}

static linked_array *linked_array_allocate (size_t data_size, size_t length) {
  linked_array_section* a = (linked_array_section*) memory_ensure_alloc(sizeof(linked_array_section),
                                                                        "Linked array initial malloc failed");
  linked_array_initialise(a, data_size, length);
  return linked_list_cons(a, NULL);
}

static void linked_array_free_section (linked_array_section* h) {
  free(h->array);
  free(h);
}

static void linked_array_free_head (linked_array *list) {
  linked_array_free_section((linked_array_section*) linked_list_head(list));
  linked_list_free_head(list);
}

static void linked_array_free (linked_array *list) {
  linked_list_map_free(list, ((void(*)(void*))linked_array_free_section));
}

static linked_array *linked_array_grow (linked_array *list) {
  linked_array_section* h = (linked_array_section*) linked_list_head(list);
  linked_array_section* a = (linked_array_section*) memory_ensure_alloc(sizeof(linked_array_section),
                                                                        "Linked array grow failed");
  linked_array_initialise(a, h->data_size, linked_array_size(list));
  return linked_list_cons(a, list);
}

static linked_array *linked_array_shrink (linked_array *list) {
  linked_array *rest = linked_list_tail(list);
  linked_array_free_head(list);
  return rest;
}

static void* linked_array_get (linked_array* list, size_t index) {
  linked_array_section* h = (linked_array_section*) linked_list_head(list);
  while (index < h->size) {
    if (list->tail) {
      list = list->tail;
      h = (linked_array_section*) linked_list_head(list);
    } else {
      return (void*) (((byte*) h->array) + (h->data_size * index));
    }
  }
  return (void*) (((byte*) h->array) + (h->data_size * (index - h->size)));
}

static bool linked_array_has (linked_array* list, void* addr) {
  linked_array_section* h = (linked_array_section*) linked_list_head(list);
  for (; list; list = list->tail, h = list ? (linked_array_section*) linked_list_head(list) : NULL) {
    if ((addr >= h->array) && (((byte*) addr) < (((byte*)h->array) + (h->data_size * h->size)))) {
      return TRUE;
    }
  }
  return FALSE;
}

/*---------------------------------------------------------------------------*/
/**************************** GROWABLE FREELIST ******************************/

/*
 * This is like a linked_array, with the specialisation that memory slots
 * can be identified as unused by calling "put", and those will be held in a freelist
 * which is used for allocations in preference of growing the linked_array unnecessarily.
 */

typedef struct growable_freelist {
  linked_array* la;
  size_t lp;
  realloc_stack* fl;
} growable_freelist;

static growable_freelist* growable_freelist_allocate (size_t data_size, size_t size, size_t fl_size) {
  growable_freelist* rv = (growable_freelist*) memory_ensure_alloc(sizeof(growable_freelist),
                                                                   "Could not allocate growable freelist");
  rv->la = linked_array_allocate(data_size, size);
  rv->lp = 0;
  rv->fl = realloc_stack_allocate(data_size, fl_size);
  return rv;
}

static void* growable_freelist_get (growable_freelist* ls) {
  void* rv;

  if (!realloc_stack_null(ls->fl)) {
    rv = *((void**) realloc_stack_down(ls->fl));
  } else {
    if (ls->lp == linked_array_size(ls->la)) {
      ls->la = linked_array_grow(ls->la);
    }
    rv = linked_array_get(ls->la, (ls->lp)++);
  }
  return rv;
}

/* This currently doesn't shrink memory back if a chunk becomes unused. */
static void growable_freelist_put (growable_freelist* ls, void* addr) {
  *((void**) realloc_stack_up(ls->fl)) = addr;
}

static bool growable_freelist_has (growable_freelist* ls, void* addr) {
  return linked_array_has(ls->la, addr);
}

static void growable_freelist_free (growable_freelist* ls) {
  realloc_stack_free(ls->fl);
  linked_array_free(ls->la);
  free(ls);
}

/*---------------------------------------------------------------------------*/
/****************************** GROWABLE TABLE *******************************/

typedef struct table_bucket {
  size_t key;
  void* value;
} table_bucket;

typedef realloc_array growable_table;

static growable_table* growable_table_allocate (size_t size) {
  return realloc_array_allocate(sizeof(linked_list*), size);
}

/* Not growable yet... */
static growable_table* growable_table_grow (growable_table* gt) {
  return gt;
}

/* or shrinkable... */
static growable_table* growable_table_shrink (growable_table* gt) {
  return gt;
}

static linked_list** growable_table_bucket_lookup (growable_table* gt, size_t key) {
  linked_list** bp = (linked_list**) realloc_array_get(gt, (key % gt->size));
  table_bucket* b;
  while (*bp) {
    b = (table_bucket*) linked_list_head(*bp);
    if (b->key == key) {
      return bp;
    }
    bp = &((*bp)->tail);
  }
  return bp;
}

static void** growable_table_lookup_generic (growable_table* gt, size_t key, bool create) {
  linked_list** bp = growable_table_bucket_lookup(gt, key);
  table_bucket* b;
  if (*bp) {
    b = (table_bucket*) linked_list_head(*bp);
    return &(b->value);
  }
  if (create) {
    b = (table_bucket*) memory_ensure_alloc(sizeof(table_bucket), "Failed to create table bucket");
    b->key = key;
    b->value = NULL;
    *bp = linked_list_cons(b, *bp);
    return &(b->value);
  }
  return NULL;
}

static void growable_table_set (growable_table* gt, size_t key, void* value) {
  *growable_table_lookup_generic(gt, key, TRUE) = value;
}

static void* growable_table_get (growable_table* gt, size_t key) {
  void** rp = growable_table_lookup_generic(gt, key, FALSE);
  return rp ? *rp : rp;
}

static void growable_table_remove (growable_table* gt, size_t key) {
  linked_list** bp = growable_table_bucket_lookup(gt, key);
  linked_list*  r  = *bp;

  if (r) {
    free(linked_list_head(r));
    *bp = linked_list_tail_free(r);
  }
}

static void growable_table_free (growable_table* gt) {
  size_t i, len = gt->size;
  for (i = 0; i < len; ++i) {
    linked_list_map_free(*((linked_list**) realloc_array_get(gt, i)), free);
  }
  realloc_array_free(gt);
}

/*---------------------------------------------------------------------------*/
/***************************** LAMBDA TYPE ***********************************/

static const smooth_size* lambda_sizes;


/*
 * TODO:
 * We can possibly get rid of this flipping between pointer and case label
 * by making the case label the same as the pointer value.
 *
 * This would be like:
 * case smooth_lambdas_start + 3: // code here
 *
 * Just need to make sure that the switch block outputs the same fast code jumps
 * by subtracting smooth_lambdas_start and then jumping into the table.
 *
 * Even though there is a subtract there, it will likely end up being faster,
 * because we don't have all of these comparisons every time when checking function type,
 * only when we have checked function type and found it to be a lambda.
 */




#ifdef RTS_STATIC
static
#endif
smooth smooth_lambda (smooth x) {
  return (smooth) (x + lambda_sizes);
}

#ifdef RTS_STATIC
static
#endif
smooth smooth_unlambda (smooth x) {
  return (smooth) (((smooth_size*) x) - lambda_sizes);
}

/*---------------------------------------------------------------------------*/
/******************************** BOXING *************************************/

/*
 * Not needed for this implementation, but perhaps sometime in the future people will want these stubs
 * to be present so another implementation can use the same module procedures while actually providing
 * definitions for boxing and unboxing (which might look something like these ones).
 */
#if 0

#ifdef RTS_STATIC
static smooth box (smooth x) {
#else
smooth smooth_box (smooth x) {
#endif
   return x;
}
#ifdef RTS_STATIC
smooth smooth_box (smooth x) {
  return box(x);
}
#endif


#ifdef RTS_STATIC
static smooth unbox (smooth x) {
#else
smooth smooth_unbox (smooth x) {
#endif
   return x;
}
#ifdef RTS_STATIC
smooth smooth_unbox (smooth x) {
  return unbox(x);
}
#endif

#endif /* 0 */

/*---------------------------------------------------------------------------*/
/******************************** STACK **************************************/

#ifndef NATIVE_STACK

#ifdef STACK_STATIC

static smooth smooth_stack[STACK_SIZE];

static smooth* __restrict__ smooth_stack_sp = smooth_stack;

#else

static realloc_stack* smooth_stack;

#endif


static void stack_allocate (void) {
#ifndef STACK_STATIC
  smooth_stack = realloc_stack_allocate(sizeof(smooth), STACK_SIZE);
#endif
}


#ifdef RTS_STATIC
static
#endif
void smooth_stack_push (smooth x) {
#ifdef STACK_STATIC
#  ifndef STACK_NO_BOUNDS_CHECK
  if (smooth_stack_sp == (smooth_stack + STACK_SIZE)) {
    die_message("Stack overflow");
  }
#  endif
  *smooth_stack_sp++ = x;
#else
  *((smooth*) realloc_stack_up(smooth_stack)) = x;
#endif
}


#ifdef RTS_STATIC
static
#endif
smooth smooth_stack_pop (void) {
#ifdef STACK_STATIC
#  ifndef STACK_NO_BOUNDS_CHECK
  if (smooth_stack_sp == smooth_stack) {
    die_message("Stack underflow");
  }
#  endif
  return *--smooth_stack_sp;
#else
  return *((smooth*) realloc_stack_down(smooth_stack));
#endif
}


static void stack_free (void) {
#ifndef STACK_STATIC
  realloc_stack_free(smooth_stack);
  smooth_stack = NULL;
#endif
}

#endif /* NATIVE_STACK */

/*---------------------------------------------------------------------------*/
/********************************* CALL REGISTER *****************************/

/*
 * The call mechanism pushes a new frame onto the call register for every call it makes
 * it is to this most recent frame that pointers are registered.
 */

static realloc_stack* call_register_frames;

static void call_register_allocate (void) {
  call_register_frames = realloc_stack_allocate(sizeof(linked_list*), CALL_REGISTER_SIZE);
}

static void call_register_add_frame (void) {
  *((linked_list**) realloc_stack_up(call_register_frames)) = NULL;
}

static void call_register_put (smooth ptr) {
  linked_list** lb = (linked_list**) realloc_stack_top(call_register_frames);
  *lb = linked_list_cons((void*) ptr, *lb);
}

/* Returns the top most register frame addresses in a linked list */
static linked_list* call_register_receive (void) {
  return *((linked_list**) realloc_stack_down(call_register_frames));
}

/* Pop off the topmost frame and use it along with arg and rv to decref any lost references. */
static void call_register_check_frame (smooth rv) {
  linked_list* frame;
  for (frame = call_register_receive(); frame;
       frame = linked_list_tail_free(frame)) {
    if (((smooth) linked_list_head(frame)) != rv) {
      smooth_gc_decref(((smooth) linked_list_head(frame)));
    }
  }
}

/* Pop off the topmost frame and use it along with arg and rv to decref any lost references. */
static void call_register_check_frame_and_arg (smooth rv, smooth arg) {
  if (rv != arg) {
    smooth_gc_decref(arg);
  }
  call_register_check_frame(rv);
}

/* This assumes you have already received and deleted your frames */
static void call_register_free (void) {
  realloc_stack_free(call_register_frames);
  call_register_frames = NULL;
}

/*---------------------------------------------------------------------------*/
/******************************** GC TABLE ***********************************/

typedef struct gc_entry {
  size_t refcount;
  linked_list* freeptrs;
} gc_entry;

static growable_table* gc_table;

static void gc_table_allocate (void) {
  gc_table = growable_table_allocate(GC_TABLE_SIZE);
}

static gc_entry* gc_entry_create (void) {
  gc_entry* x = (gc_entry*) memory_ensure_alloc(sizeof(gc_entry), "GC entry create failed");
  x->refcount = 0;
  x->freeptrs = NULL;
  return x;
}

static void gc_entry_add_freeptr (gc_entry* e, void (*freeptr)(smooth)) {
  e->freeptrs = linked_list_cons((void*) freeptr, e->freeptrs);
}

static gc_entry* gc_table_get (smooth ptr) {
  gc_entry* entry = (gc_entry*) growable_table_get(gc_table, ptr);

  if (!entry) {
    entry = gc_entry_create();
    growable_table_set(gc_table, ptr, entry);
  }
  return entry;
}

/* If ptr is already under gc, this will still add a freeptr and incref the ptr */
void smooth_gc_register (smooth ptr, void (*freeptr)(smooth)) {
  gc_entry* entry = gc_table_get(ptr);
  gc_entry_add_freeptr(entry, freeptr);
  ++entry->refcount;
  call_register_put(ptr);
}

void smooth_gc_incref (smooth ptr) {
  gc_entry* entry = gc_table_get(ptr);
  ++entry->refcount;
}

void smooth_gc_decref (smooth ptr) {
  gc_entry* entry;
  void (*freeptr)(smooth);
  linked_list* freeptrs;

  entry = (gc_entry*) growable_table_get(gc_table, ptr);

  if (entry && (--(entry->refcount) == 0)) {
    freeptrs = entry->freeptrs;
    growable_table_remove(gc_table, ptr);

    for (; freeptrs; freeptrs = linked_list_tail_free(freeptrs)) {
      freeptr = (void (*)(smooth)) linked_list_head(freeptrs);
      freeptr(ptr);
    }
  }
}

static void gc_table_free (void) {

  /* TODO: loop across table calling the freeptrs */

  growable_table_free(gc_table);
  gc_table = NULL;
}

/*---------------------------------------------------------------------------*/
/******************************** CLOSURES ***********************************/

typedef struct closure_body {
  smooth* locals;
  int     numlocals;
  int     curpos;
} closure_body;

typedef struct closure {
  smooth code;
  int    curpos;
  closure_body* body;
  struct closure* parent; /* Allows access to more closed variables. */
} closure;


#ifdef CLOSURES_STATIC

static smooth closures_body_memory[CLOSURES_BODY_SIZE];
static smooth* __restrict__ closures_body_sp = closures_body_memory;
static smooth closures_memory[CLOSURES_SIZE];
static smooth* __restrict__ closures_sp = closures_memory;

#  ifdef CLOSURES_FREELIST_STATIC
static smooth closures_body_freelist[CLOSURES_BODY_FREELIST_SIZE];
static smooth* __restrict__ closures_body_freelist_sp = closures_body_freelist;
static smooth closures_freelist[CLOSURES_FREELIST_SIZE];
static smooth* __restrict__ closures_freelist_sp = closures_freelist;
#  else

/*
 * No need for fancy growable memory, but could be faster
 * if we suddenly get lots of closures being freed at once.
 * Very possible this will change to realloc_array at some point
 */
static linked_list* closures_body_freelist;
static linked_list* closures_freelist;
#  endif

#else

static growable_freelist* closures_body_memory;
static growable_freelist* closures_memory;

#endif

static void closures_allocate (void) {
#ifdef CLOSURES_STATIC
#  ifndef CLOSURES_FREELIST_STATIC
  closures_freelist = realloc_stack_allocate(sizeof(closure), CLOSURES_FREELIST_SIZE);
#  endif
#else
  closures_memory = growable_freelist_allocate(sizeof(closure), CLOSURES_SIZE, CLOSURES_FREELIST_SIZE);
#endif

#ifdef CLOSURES_BODY_STATIC
#  ifndef CLOSURES_BODY_FREELIST_STATIC
  closures_body_freelist = realloc_stack_allocate(sizeof(closure_body), CLOSURES_BODY_FREELIST_SIZE);
#  endif
#else
  closures_body_memory = growable_freelist_allocate(sizeof(closure_body), CLOSURES_BODY_SIZE, CLOSURES_BODY_FREELIST_SIZE);
#endif
}

static void closure_free (smooth addr) {
  smooth_size i = 0;
  closure* c = (closure*) addr;
  smooth_gc_decref((smooth) c->parent);

  for (i = 0; i < c->curpos; ++i) {
    smooth_gc_decref(c->body->locals[i]);
  }

  free(c->body->locals);

#ifdef CLOSURES_BODY_STATIC
#  ifdef CLOSURES_BODY_FREELIST_STATIC
#    ifndef CLOSURES_BODY_FREELIST_NO_BOUNDS_CHECK
  if (closures_body_freelist_sp == (closures_body_freelist + CLOSURES_BODY_FREELIST_SIZE)) {
    die_message("Closures freelist overflow");
  }
#    endif
  *closures_body_freelist_sp++ = c->body;
#  else
  *((smooth*) realloc_stack_up(closures_body_freelist)) = c->body;
#  endif
#else
  growable_freelist_put(closures_body_memory, (void*) c->body);
#endif

#ifdef CLOSURES_STATIC
#  ifdef CLOSURES_FREELIST_STATIC
#    ifndef CLOSURES_FREELIST_NO_BOUNDS_CHECK
  if (closures_freelist_sp == (closures_freelist + CLOSURES_FREELIST_SIZE)) {
    die_message("Closures freelist overflow");
  }
#    endif
  *closures_freelist_sp++ = addr;
#  else
  *((smooth*) realloc_stack_up(closures_freelist)) = addr;
#  endif
#else
  growable_freelist_put(closures_memory, (void*) addr);
#endif

}

static closure_body* closure_get_body (void) {
  closure_body* rv;

#ifdef CLOSURES_BODY_STATIC
#  ifdef CLOSURES_BODY_FREELIST_STATIC
  if (closures_body_freelist_sp != closures_body_freelist) {
    rv = *--closures_body_freelist_sp;
  }
#  else
  if (!realloc_stack_null(closures_body_freelist)) {
    rv = *((smooth*) realloc_stack_down(closures_body_freelist_sp));
  }
#  endif
  else {
#  ifndef CLOSURES_BODY_NO_BOUNDS_CHECK
    if (closures_body_sp == (closures_body_memory + CLOSURES_BODY_SIZE)) {
      die_message("Closures memory overflow");
    }
#  endif
    rv = closures_body_sp++;
  }
#else
  rv = (closure_body*) growable_freelist_get(closures_body_memory);
#endif

  return rv;
}

static closure* closure_get_head (void) {
  closure* rv;

#ifdef CLOSURES_STATIC
#  ifdef CLOSURES_FREELIST_STATIC
  if (closures_freelist_sp != closures_freelist) {
    rv = *--closures_freelist_sp;
  }
#  else
  if (!realloc_stack_null(closures_freelist)) {
    rv = *((smooth*) realloc_stack_down(closures_freelist_sp));
  }
#  endif
  else {
#  ifndef CLOSURES_NO_BOUNDS_CHECK
    if (closures_sp == (closures_memory + CLOSURES_SIZE)) {
      die_message("Closures memory overflow");
    }
#  endif
    rv = closures_sp++;
  }
#else
  rv = (closure*) growable_freelist_get(closures_memory);
#endif

  return rv;
}

static closure_body* closure_body_create (smooth_size numlocals, smooth* args, smooth_size curpos) {
  smooth_size i = 0;
  closure_body* b = closure_get_body();
  smooth* locals = (smooth*) memory_ensure_alloc(sizeof(smooth) * numlocals, "Closure variable allocation failed");

  for (i = 0; i < curpos; ++i) {
    smooth_gc_incref(args[i]);
    locals[i] = args[i];
  }

  b->locals    = locals;
  b->numlocals = numlocals;
  b->curpos    = curpos;

  return b;
}

#ifdef RTS_STATIC
static smooth closure_create (smooth code, smooth_size n, smooth* args, smooth_size args_count, smooth parent) {
#else
smooth smooth_closure_create (smooth code, smooth_size n, smooth* args, smooth_size args_count, smooth parent) {
#endif
  closure_body* body = closure_body_create(n, args, args_count);
  closure*      rv   = closure_get_head();

  smooth_gc_register((smooth) rv, closure_free);
  smooth_gc_incref((smooth) parent);

  rv->code   = code;
  rv->curpos = args_count;
  rv->parent = (closure*) parent;
  rv->body   = body;

  return (smooth) rv;
}
#ifdef RTS_STATIC
smooth smooth_closure_create (smooth code, smooth_size n, smooth* args, smooth_size args_count, smooth parent) {
  return closure_create(code, n, args, args_count, parent);
}
#endif


static bool is_closure (smooth x) {
  bool rv;
#ifdef CLOSURES_STATIC
  rv = (x >= closures_memory) && (x < (closures_memory + CLOSURES_SIZE));
#else
  rv = growable_freelist_has(closures_memory, (void*) x);
#endif
  return rv;
}


#ifdef RTS_STATIC
static smooth closure_lookup (smooth c, smooth_size i) {
#else
smooth smooth_closure_lookup (smooth c, smooth_size i) {
#endif

  closure* x = (closure*) c;

  while (x->curpos <= i) {
    i -= x->curpos;
    x = x->parent;
  }

  return x->body->locals[i];
}
#ifdef RTS_STATIC
smooth smooth_closure_lookup (smooth c, smooth_size i) {
  return closure_lookup(c, i);
}
#endif

static void closures_free (void) {
#ifdef CLOSURES_STATIC
#  ifndef CLOSURES_FREELIST_STATIC
  realloc_array_free(closures_body_freelist);
  closures_body_freelist = NULL;
  realloc_array_free(closures_freelist);
  closures_freelist = NULL;
#  endif
#else
  growable_freelist_free(closures_body_memory);
  closures_body_memory = NULL;
  growable_freelist_free(closures_memory);
  closures_memory = NULL;
#endif
}

static void closure_body_copy (closure* x) {
  x->body = closure_body_create(x->body->numlocals, x->body->locals, x->curpos);
}

/*---------------------------------------------------------------------------*/
/***************************** FUNCTION TYPE *********************************/

extern smooth_size smooth_lambdas_length;

/*
When you use SMOOTH_*_P, exactly one of the predicates will be true, no matter what you pass.
In other words, don't use SMOOTH_*_P unless you know you have some kind of function/procedure.
*/

static bool is_lambda (smooth x) {
  return ((((smooth_size*) x) >= lambda_sizes) &&
          (((smooth_size*) x) < (lambda_sizes + smooth_lambdas_length)));
}

/*---------------------------------------------------------------------------*/
/********************************* CALL **************************************/

/*
 * These are just versions of apply() which are ever so slightly quicker if using our own stack
 * hence defined only in that case.
 * For everything else, and when using native stack, it is preferable to use apply()
 */


/*
 * Unless it is possible to share * a lot * of the code from smooth_apply
 * with this one, it will be necessary to scrap this one to keep bloat manageable.
 */


#ifdef NATIVE_STACK

smooth smooth_execute (smooth pc, smooth self, smooth* locals, smooth_size numlocals);

#else

void smooth_execute (smooth pc);

static void call_lambda (smooth x) {
  PUSH(NULL);
  smooth_execute(UNLAMBDA(x));
}

/*
This does not permit having another closure as the code part of the closure.
I'm not sure that we would ever want a closure as the code part.
*/
static void call_closure (smooth x) {
  smooth code = ((closure*) x)->code;
  if (is_lambda(code)) {
    PUSH(x);
    smooth_execute(UNLAMBDA(code));
  } else {
    PUSH(((smooth (*)(closure*, smooth)) code)((closure*) x, POP()));
  }
}

static void call_primitive (smooth fn) {
  smooth rv, arg = POP();
  call_register_add_frame();
  rv = ((smooth (*)(smooth)) fn)(arg);
  call_register_check_frame_and_arg(rv, arg);
  PUSH(rv);
}

#ifdef RTS_STATIC
static
#endif
void smooth_call (smooth x) {
  if (is_closure(x)) {
    call_closure(x);
  } else if (is_lambda(x)) {
    call_lambda(x);
  } else {
    call_primitive(x);
  }
}

#endif /* NATIVE_STACK */

/*---------------------------------------------------------------------------*/
/********************************* APPLY *************************************/

/*

r1=(\ x r2=(\ y y)) ; x loses reference here.

v1=push(closure_create(r2, local, null))
// self and local went in
v2=apply(v1, 0)
  v3=push(local)
// local came out. decref self.

*/

/* TODO:
Warning: it is not this simple.
In reality we need chains so frames so that a smooth_apply within a smooth_apply
Does not leak data references.
*/
#ifdef RTS_STATIC
static smooth apply (smooth x, smooth* args, smooth_size n) {
#else
smooth smooth_apply (smooth x, smooth* args, smooth_size n) {
#endif

  smooth code;
  smooth rv;
#ifndef NATVIE_STACK
  smooth j;
#endif

/* If doing tail call on NATIVE_STACK, we jump here to save frame space */
jump:

  if (is_closure(x)) {

    int i = 0;
    closure* nex;
    closure* xc = (closure*) x;

    /* only need to do stuff on the closure if we are still filling args. */
    if (xc->curpos != xc->body->numlocals) {

      /* if we don't have write access to the closure, first make a copy of the body. */
      if (xc->curpos != xc->body->curpos) {
        closure_body_copy(xc);
      }

      /* now copy in from args until we hit the limit or run out of args. */
      for (i = 0; (i < n) && (xc->body->curpos != xc->body->numlocals); ++i) {
        xc->body->locals[xc->body->curpos++] = args[i];
      }

      /* now we make a new head to match this. */
      nex = closure_get_head();
      nex->code = xc->code;
      nex->curpos = xc->body->curpos;
      nex->body = xc->body;
      nex->parent = xc->parent;

      xc = nex;
    }

/*

(lambda (x1...x50)
  x4
)

 */

    /* if there are still args for consumption, they get applied to the code. */
    if (i != n) {

      code = xc->code;
      if (is_lambda(code)) {
#ifdef NATIVE_STACK
        return smooth_execute(UNLAMBDA(code), (smooth) xc, args, n);
#else
        for (j = n - 1; j >= i; --j) {
          PUSH(args[j]);
        }
        PUSH(n - i);

        PUSH(xc);
        smooth_execute(UNLAMBDA(code));
        rv = POP();
#endif
      } else {
        call_register_add_frame();
        rv = ((smooth (*)(smooth, smooth)) code)((smooth) xc, args[i]);
        call_register_check_frame(rv);

        if ((n - i) == 1) {
          return rv;
        }

        x = rv;
        args += (i + 1);
        n    -= (i + 1);
        goto jump;
      }

    } else {
      /* args ran out */
      return (smooth) xc;
    }

  } else if (is_lambda(x)) {

    // if we're not quite there, we create a closure instead.
    if (n < (*((smooth_size*) x))) {
      return smooth_closure_create(x, *((smooth_size*) x), args, n, NULL);
    }

#ifdef NATIVE_STACK
    return smooth_execute(UNLAMBDA(x), (smooth) NULL, args, n);
#else
    for (i = n - 1; i >= 0; --i) {
      PUSH(args[i]);
    }
    PUSH(n);
    PUSH(NULL);
    smooth_execute(UNLAMBDA(x));
    return POP();
#endif
  } else {
    call_register_add_frame();
    rv = ((smooth (*)(smooth)) x)(args[0]);
    call_register_check_frame(rv);

    if (n == 1) {
        return rv;
    }

    x = rv;
    ++args;
    --n;
    goto jump;
  }

}
#ifdef RTS_STATIC
smooth smooth_apply (smooth x, smooth* args, smooth_size n) {
  return apply(x, args, n);
}
#endif

/*---------------------------------------------------------------------------*/
/********************************* INIT **************************************/

#ifdef RTS_STATIC
static
#endif
void smooth_core_init (const smooth_size* ls) {
  lambda_sizes = ls;

#ifndef NATIVE_STACK
  stack_allocate();
#endif
  call_register_allocate();
  gc_table_allocate();
  closures_allocate();
}

static void smooth_core_free (void) {
#ifndef NATIVE_STACK
  stack_free();
#endif
  call_register_free();
  gc_table_free();
  closures_free();
}

/*---------------------------------------------------------------------------*/
