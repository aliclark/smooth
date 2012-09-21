
; In each file, the final expression is considered to be The Expression
;
; When someone writes (__import__ "filename.smc") , The Expression is copied in
; at that position.
;
; It is mightily preferred that only a single file at the root
; of the project does this, and all other files should receive
; their imports as arguments.
;
; If however, you still wish to provide a self-contained module
; which itself does not require any arguments to be passed,
; please write the module implementation requiring arguments,
; and then a smaller module which takes the implementation module
; and fills in those arguments automatically.
;
; This module style requires that all things of use be first class.
; Macros are first class:  (__macro__ [lambda expression])
; Externs are first class: (__extern__ "file:///usr/local/src/somefile.o/the_symbol")
;
; Suppose that inside pair.smc is a lambda which takes as its
; first argument a module implementing a Boolean interface,
; and which then returns a Pair interface.
; The eventual code might look something like this in the root (main) file,
; which handles all dependency injection.
;
; (let ((bool_impl (__import__ "boolean.smc"))
;       (pair_abs  (__import__ "pair.smc"))
;       (pair_impl (pair_abs bool_impl)))
;
;   ... supply pair_impl to modules which need it ...)

; If a macro is not evaluated away at compile-time, an error is thrown.
; We have the benefits of first-class macros in many instances,
; without the down-side of run-time overhead.
