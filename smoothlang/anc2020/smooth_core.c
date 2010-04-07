

/*

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


smooth_t c_func (smooth_t a) {
  smooth_t b = create_closure(...);
  return a; // b is lost in the abyss
}

// but fear not, the rts knows b has been created, and,
// if it hasn't been returned, it can be decref'd.
// now only if the user has explicitly incref'd b can it be saved!


//per thread variables
smooth_t *call_closure_creations;  // a mallocd array of all closures made in the most recent call.
smooth_t *call_register_creations; // likewise for any addresses which were registered to gc.

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

#include "smoothlang/anc2020/smooth_core.h"

#include "pthread.h"
#include "stdlib.h"
#include "stdio.h"

#define TRUE  1
#define FALSE 0

extern void smooth_execute (smooth_t pc);

extern byte*  smooth_lambdas_start;
extern size_t smooth_lambdas_length;

/*---------------------------------------------------------------------------*/

smooth_t smooth_argc;
smooth_t smooth_argv;

/*---------------------------------------------------------------------------*/

static void call_lambda    (smooth_t x);
static void call_closure   (smooth_t x);
static void call_primitive (smooth_t x);

/*---------------------------------------------------------------------------*/
/************************** MEMORY ALLOCATION ********************************/

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
  realloc_array* ra = memory_ensure_alloc(sizeof(realloc_array), "Could not allocate realloc array");
  ra->array = memory_ensure_alloc(data_size * size, "Could not allocate realloc array memory");
  ra->data_size = data_size;
  ra->size = size;
  ra->initial_size = size;
  return ra;
}

static void* realloc_array_grow (realloc_array* ra) {
  ra->size *= 2;
  ra->array = memory_ensure_realloc(ra->array, ra->size * ra->data_size,
                                    "Realloc array grow failed");
  return ra;
}

static void* realloc_array_shrink (realloc_array* ra) {
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
 */

typedef struct realloc_stack {
  realloc_array* ra;
  size_t rp;
} realloc_stack;

static realloc_stack* realloc_stack_allocate (size_t data_size, size_t size) {
  realloc_stack* rv = memory_ensure_alloc(sizeof(realloc_stack), "Realloc stack initialise failed");
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
  linked_list* rv = memory_ensure_alloc(sizeof(linked_list), "Linked list cons failed");
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
  linked_array_section* h = linked_list_head(list);
  return linked_list_tail(list) ? h->size * 2 : h->size;
}

static void linked_array_initialise (linked_array_section *list, size_t data_size, size_t size) {
  list->array     = memory_ensure_alloc(data_size * size, "Linked array chunk malloc failed");
  list->data_size = data_size;
  list->size      = size;
}

static linked_array *linked_array_allocate (size_t data_size, size_t length) {
  linked_array_section* a = memory_ensure_alloc(sizeof(linked_array_section),
                                                "Linked array initial malloc failed");
  linked_array_initialise(a, data_size, length);
  return linked_list_cons(a, NULL);
}

static void linked_array_free_section (linked_array_section* h) {
  free(h->array);
  free(h);
}

static void linked_array_free_head (linked_array *list) {
  linked_array_free_section(linked_list_head(list));
  linked_list_free_head(list);
}
 
static void linked_array_free (linked_array *list) {
  linked_list_map_free(list, linked_array_free_section);
}

static linked_array *linked_array_grow (linked_array *list) {
  linked_array_section* h = linked_list_head(list);
  linked_array_section* a = memory_ensure_alloc(sizeof(linked_array_section),
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
  linked_array_section* h = linked_list_head(list);
  while (index < h->size) {
    if (list->tail) {
      list = list->tail;
      h = linked_list_head(list);
    } else {
      return (void*) (((byte*) h->array) + (h->data_size * index));
    }
  }
  return (void*) (((byte*) h->array) + (h->data_size * (index - h->size)));
}

static bool linked_array_has (linked_array* list, void* addr) {
  linked_array_section* h = linked_list_head(list);
  for (; list; list = list->tail, h = linked_list_head(list)) {
    if ((addr >= h->array) && (addr < (h->array + h->size))) {
      return 1;
    }
  }
  return 0;
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
  linked_list* fl;
} growable_freelist;

static growable_freelist* growable_freelist_allocate (size_t data_size, size_t size) {
  growable_freelist* rv = memory_ensure_alloc(sizeof(growable_freelist),
                                              "Could not allocate growable freelist");
  rv->la = linked_array_allocate(data_size, size);
  rv->lp = 0;
  rv->fl = NULL;
  return rv;
}

static void* growable_freelist_get (growable_freelist* ls) {
  linked_list* fl = ls->fl;
  void* rv;
  if (fl) {
    rv = linked_list_head(fl);
    ls->fl = linked_list_tail_free(fl);
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
  ls->fl = linked_list_cons(addr, ls->fl);
}

static bool growable_freelist_has (growable_freelist* ls, void* addr) {
  return linked_array_has(ls->la, addr);
}

static void growable_freelist_free (growable_freelist* ls) {
  linked_list_free(ls->fl);
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

static growable_table* growable_table_shrink (growable_table* gt) {
  return gt;
}

static linked_list** growable_table_bucket_lookup (growable_table* gt, size_t key) {
  linked_list** bp = realloc_array_get(gt->array, (key % gt->size));
  table_bucket* b;
  while (*bp) {
    b = linked_list_head(*bp);
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
    b = linked_list_head(*bp);
    return &(b->value);
  }
  if (create) {
    b = memory_ensure_alloc(sizeof(table_bucket), "Failed to create table bucket");
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
  table_bucket* b;
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
/******************************** STACK **************************************/

static realloc_stack* smooth_stack;

static void stack_allocate (void) {
  smooth_stack = realloc_stack_allocate(sizeof(smooth_t), STACK_SIZE);
}

void smooth_push (smooth_t x) {
  *((smooth_t*) realloc_stack_up(smooth_stack)) = x;
}

smooth_t smooth_pop (void) {
  return *((smooth_t*) realloc_stack_down(smooth_stack));
}

void smooth_up (void) {
  realloc_stack_up(smooth_stack);
}

void smooth_down (void) {
  realloc_stack_down(smooth_stack);
}

smooth_t smooth_top (void) {
  return *((smooth_t*) realloc_stack_top(smooth_stack));
}

static void stack_free (void) {
  realloc_stack_free(smooth_stack);
  smooth_stack = NULL;
}

/*---------------------------------------------------------------------------*/
/********************************* CALL REGISTER *****************************/

/*
 * The call mechanism pushes a new frame onto the call register for every call it makes
 * it is to this most recent frame that pointers are registered.
 */

static realloc_stack* call_register_frames; /* Later this will be thread specific */

static void call_register_allocate (void) {
  call_register_frames = realloc_stack_allocate(sizeof(linked_list*), CALL_REGISTER_SIZE);
}

static void call_register_add_frame (void) {
  *((linked_list**) realloc_stack_up(call_register_frames)) = NULL;
}

static void call_register_put (smooth_t ptr) {
  linked_list** lb = realloc_stack_top(call_register_frames);
  *lb = linked_list_cons(ptr, *lb);
}

/* Returns the top most register frame addresses in a linked list */
static linked_list* call_register_receive (void) {
  return *((linked_list**) realloc_stack_down(call_register_frames));
}

/* Pop off the topmost frame and use it along with arg and rv to decref any lost references. */
static void call_register_check_frame (smooth_t rv) {
  linked_list* frame;
  for (frame = call_register_receive(); frame;
       frame = linked_list_tail_free(frame)) {
    if (linked_list_head(frame) != rv) {
      smooth_gc_decref(linked_list_head(frame));
    }
  }
}

/* Pop off the topmost frame and use it along with arg and rv to decref any lost references. */
static void call_register_check_frame_and_arg (smooth_t rv, smooth_t arg) {
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
static pthread_mutex_t gc_table_mutex = PTHREAD_MUTEX_INITIALIZER;

static void gc_table_allocate (void) {
  pthread_mutex_lock(&gc_table_mutex);
  gc_table = growable_table_allocate(GC_TABLE_SIZE);
  pthread_mutex_unlock(&gc_table_mutex);
}

static gc_entry* gc_entry_create (void) {
  gc_entry* x = memory_ensure_alloc(sizeof(gc_entry), "GC entry create failed");
  x->refcount = 0;
  x->freeptrs = NULL;
  return x;
}

static void gc_entry_add_freeptr (gc_entry* e, void (*freeptr)(smooth_t)) {
  e->freeptrs = linked_list_cons(freeptr, e->freeptrs);
}

static gc_entry* gc_table_get (smooth_t ptr) {
  gc_entry* gc_entry = growable_table_get(gc_table, ptr);
  if (!gc_entry) {
    gc_entry = gc_entry_create();
    growable_table_set(gc_table, ptr, gc_entry);
  }
  return gc_entry;
}

/* If ptr is already under gc, this will still add a freeptr and incref the ptr */
void smooth_gc_register (smooth_t ptr, void (*freeptr)(smooth_t)) {
  gc_entry* gc_entry;
  pthread_mutex_lock(&gc_table_mutex);
  gc_entry = gc_table_get(ptr);
  gc_entry_add_freeptr(gc_entry, freeptr);
  ++gc_entry->refcount;
  call_register_put(ptr);
  pthread_mutex_unlock(&gc_table_mutex);
}

void smooth_gc_incref (smooth_t ptr) {
  gc_entry* gc_entry;
  pthread_mutex_lock(&gc_table_mutex);
  gc_entry = gc_table_get(ptr);
  ++gc_entry->refcount;
  pthread_mutex_unlock(&gc_table_mutex);
}

void smooth_gc_decref (smooth_t ptr) {
  gc_entry* gc_entry;
  void (*freeptr)(smooth_t);
  linked_list* freeptrs;
  pthread_mutex_lock(&gc_table_mutex);
  gc_entry = growable_table_get(gc_table, ptr);
  if (gc_entry && (--(gc_entry->refcount) == 0)) {
    freeptrs = gc_entry->freeptrs;
    growable_table_remove(gc_table, ptr);
    pthread_mutex_unlock(&gc_table_mutex);
    for (; freeptrs; freeptrs = linked_list_tail_free(freeptrs)) {
      freeptr = linked_list_head(freeptrs);
      freeptr(ptr);
    }
  } else {
    pthread_mutex_unlock(&gc_table_mutex);
  }
}

static void gc_table_free (void) {
  pthread_mutex_lock(&gc_table_mutex);
  growable_table_free(gc_table);
  gc_table = NULL;
  pthread_mutex_unlock(&gc_table_mutex);
}

/*---------------------------------------------------------------------------*/
/******************************** CLOSURES ***********************************/

static growable_freelist* closures_memory;
static pthread_mutex_t closures_memory_mutex = PTHREAD_MUTEX_INITIALIZER;

static void closures_allocate (void) {
  pthread_mutex_lock(&closures_memory_mutex);
  closures_memory = growable_freelist_allocate(sizeof(smooth_closure_t), CLOSURES_SIZE);
  pthread_mutex_unlock(&closures_memory_mutex);
}

static void closure_free (smooth_t addr) {
  smooth_closure_t* c = (smooth_closure_t*) addr;
  smooth_gc_decref(c->local);
  smooth_gc_decref(c->parent);
  pthread_mutex_lock(&closures_memory_mutex);
  growable_freelist_put(closures_memory, (void*) addr);
  pthread_mutex_unlock(&closures_memory_mutex);
}

smooth_t smooth_closure_create (smooth_t lambda, smooth_t local, smooth_closure_t* parent) {
  smooth_closure_t* rv;
  pthread_mutex_lock(&closures_memory_mutex);
  rv = growable_freelist_get(closures_memory);
  pthread_mutex_unlock(&closures_memory_mutex);

  smooth_gc_register(rv, closure_free);
  smooth_gc_incref(local);
  smooth_gc_incref(parent);

  rv->lambda = lambda;
  rv->local  = local;
  rv->parent = parent;

  return (smooth_t) rv;
}

bool smooth_closure_p (smooth_t x) {
  bool rv;
  pthread_mutex_lock(&closures_memory_mutex);
  rv = growable_freelist_has(closures_memory, (void*) x);
  pthread_mutex_unlock(&closures_memory_mutex);
  return rv;
}

smooth_t smooth_closure_local (smooth_closure_t* x) {
  return x->local;
}

smooth_closure_t* smooth_closure_parent (smooth_closure_t* x) {
  return x->parent;
}

static void closures_free (void) {
  pthread_mutex_lock(&closures_memory_mutex);
  growable_freelist_free(closures_memory);
  closures_memory = NULL;
  pthread_mutex_unlock(&closures_memory_mutex);
}

/*---------------------------------------------------------------------------*/
/********************************* CALL **************************************/

static void call_lambda (smooth_t x) {
  PUSH(NULL);
  smooth_execute(UNLAMBDA(x));
}

/*
This does not permit having another closure as the code part of the closure.
I'm not sure that we would ever want a closure as the code part.
*/
static void call_closure (smooth_t x) {
  smooth_t code = CLOSURE_CODE(x);
  if (SMOOTH_LAMBDA_P(code)) {
    PUSH(x);
    smooth_execute(UNLAMBDA(code));
  } else {
    PUSH(((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, POP()));
  }
}

static void call_primitive (smooth_t fn) {
  smooth_t rv, arg = POP();
  call_register_add_frame();
  rv = ((smooth_t (*)(smooth_t)) fn)(arg);
  call_register_check_frame_and_arg(rv, arg);
  PUSH(rv);
}

void smooth_call (smooth_t x) {
  if (SMOOTH_CLOSURE_P(x)) {
    call_closure(x);
  } else if (SMOOTH_LAMBDA_P(x)) {
    call_lambda(x);
  } else {
    call_primitive(x);
  }
}

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


smooth_t smooth_apply (smooth_t x, smooth_t y) {
  smooth_t code;
  smooth_t rv;
  if (SMOOTH_CLOSURE_P(x)) {
    code = CLOSURE_CODE(x);
    if (SMOOTH_LAMBDA_P(code)) {
      PUSH(y);
      PUSH(x);
      smooth_execute(UNLAMBDA(code));
      return POP();
    } else {
      call_register_add_frame();
      rv = ((smooth_t (*)(smooth_closure_t*, smooth_t)) code)((smooth_closure_t*) x, y);
      call_register_check_frame(rv);
      return rv;
    }
    return smooth_apply_closure(x, y);
  } else if (SMOOTH_LAMBDA_P(x)) {
    PUSH(y);
    PUSH(NULL);
    smooth_execute(UNLAMBDA(x));
    return POP();
  } else {
    call_register_add_frame();
    rv = ((smooth_t (*)(smooth_t)) x)(y);
    call_register_check_frame(rv);
    return rv;
  }
}

/*---------------------------------------------------------------------------*/
/******************************* SPARK APPLY *********************************/

typedef struct spark_call_data {
  smooth_t f;
  smooth_t x;
} spark_call_data;

/* Threaad count including start thread. */
static unsigned long int threading_count = 1;

/* Linked array of thread specific data. */
static smooth_t* thread_data;

/* STUB */
pthread_t thread_alloc (void) {
  return 0;
}

static void* spark_run (void* d) {
  spark_call_data* data = d;
  return (void*) smooth_apply(data->f, data->x);
}

smooth_t smooth_spark_apply (smooth_t x, smooth_t y) {
  pthread_t th = thread_alloc();
  spark_call_data* data = memory_ensure_alloc(sizeof(spark_call_data), "Spark create failed");
  data->f = x;
  data->x = y;
  smooth_t rv = pthread_create(&th, NULL, spark_run, data);
  free(data);
  pthread_detach(th);
  return rv;
}

/*---------------------------------------------------------------------------*/
/********************************* INIT **************************************/

void smooth_core_init (void) {
  stack_allocate();
  call_register_allocate();
  gc_table_allocate();
  closures_allocate();
}

/*---------------------------------------------------------------------------*/
