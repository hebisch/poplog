#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/sequence-compiler-macros.lsp
 | Purpose:         Some compiler macros for commonly used sequence functions
 | Author:          John Williams, Jul  6 1995 (see revisions)
 | Documentation:   HELP * SEQUENCE-COMPILER-MACROS
 | Related Files:   C.all/lisp/src/seq.p, C.all/lisp/src/seq.lsp
 |#

(cl:provide :sequence-compiler-macros)

(poplog:pop11)

section $-lisp;

define simple_remove_if(test, seq);
    lvars item, removed = false, cons_p;
    checkr_function(test) -> test;
    if ispair(seq) then
        conslist -> cons_p;
        lblock;
            lvars list = seq;
            (#| repeat
                    fast_destpair(list) -> (item, list);
                    if lisp_apply(item, test, 1, 1) == nil then
                        item
                    else
                        true -> removed
                    endif;
                    quitif(endp(list))
                endrepeat
            |#)
        endlblock
    elseif seq == [] then
        return([])
    else
        lblock;
            lvars lo, hi, subscr, i;
            Vseq_bounds(seq) -> (seq, lo, hi);
            lisp_fast_subscr_p(seq) -> subscr;
            class_cons(fast_datakey(seq)) -> cons_p;
            (#| for i from lo to hi do
                    fast_apply(i, seq, subscr) -> item;
                    if lisp_apply(item, test, 1, 1) == nil then
                        item;
                        if subscr == lisp_subscrs then fast_char_code() endif
                    else
                        true -> removed
                    endif
                endfor
            |#);
        endlblock
    endif;
    if removed then
        fast_apply(cons_p)
    else
        erasenum();
        seq
    endif
enddefine;


define simple_delete_if(test, seq) -> seq;
    lvars l;
    fastprocs front, back;
    if ispair(seq) then
        checkr_function(test) -> test;
        until lisp_apply(front(seq), test, 1, 1) == nil do
            back(seq) -> seq;
            returnif(endp(seq))
        enduntil;
        seq -> l;
        until endp(back(l)) do
            if lisp_apply(front(back(l)), test, 1, 1) /== nil then
                back(back(l)) -> back(l)
            else
                back(l) -> l
            endif
        enduntil
    else
        simple_remove_if(test, seq) -> seq
    endif
enddefine;


lisp_export(simple_remove_if,   @SYS:SIMPLE-REMOVE-IF,  [2 2 1]);
lisp_export(simple_delete_if,   @SYS:SIMPLE-DELETE-IF,  [2 2 1]);

endsection;

lisp

(in-package "COMMON-LISP")


(define-compiler-macro REMOVE-IF (&whole form pred seq &rest keys
                                  &key (key #'identity k-supp)
                                  &allow-other-keys)
    (if (null keys)
        `(sys:simple-remove-if ,pred ,seq)
        (if (and k-supp (eq (length keys) 2))
            `(sys:simple-remove-if
                #'(lambda (#1=#:arg) (funcall ,pred (funcall ,key #1#)))
                ,seq)
            form)))


(define-compiler-macro DELETE-IF (&whole form pred seq &rest keys
                                  &key (key #'identity k-supp)
                                  &allow-other-keys)
    (if (null keys)
        `(sys:simple-delete-if ,pred ,seq)
        (if (and k-supp (eq (length keys) 2))
            `(sys:simple-remove-if
                #'(lambda (#1=#:arg) (funcall ,pred (funcall ,key #1#)))
                ,seq)
            form)))


(define-compiler-macro REMOVE-IF-NOT (&whole form pred seq &rest keys
                                      &key (key #'identity k-supp)
                                      &allow-other-keys)
    (if (null keys)
        `(sys:simple-remove-if (complement ,pred) ,seq)
        (if (and k-supp (eq (length keys) 2))
            `(sys:simple-remove-if
                #'(lambda (#1=#:arg) (not (funcall ,pred (funcall ,key #1#))))
                ,seq)
            form)))


(define-compiler-macro DELETE-IF-NOT (&whole form pred seq &rest keys
                                      &key (key #'identity k-supp)
                                      &allow-other-keys)
    (if (null keys)
        `(sys:simple-delete-if (complement ,pred) ,seq)
        (if (and k-supp (eq (length keys) 2))
            `(sys:simple-delete-if
                #'(lambda (#1=#:arg) (not (funcall ,pred (funcall ,key #1#))))
                ,seq)
            form)))


(define-compiler-macro REMOVE (&whole form item seq &rest keys
                               &key (key #'identity k-supp)
                                    (test #'eql t-supp)
                               &allow-other-keys)
    (let ((n (length keys)))
        (if (or (zerop n)
                (and (eq n 2) (or k-supp t-supp))
                (and (eq n 4) k-supp t-supp))
            `(sys:simple-remove-if
                    #'(lambda (#1=#:arg)
                        (funcall ,test
                            ,item
                            ,(if key `(funcall ,key #1#) `#1#)))
                    ,seq)
            form)))


(define-compiler-macro DELETE (&whole form item seq &rest keys
                               &key (key #'identity k-supp)
                                    (test #'eql t-supp)
                               &allow-other-keys)
    (let ((n (length keys)))
        (if (or (zerop n)
                (and (eq n 2) (or k-supp t-supp))
                (and (eq n 4) k-supp t-supp))
            `(sys:simple-delete-if
                    #'(lambda (#1=#:arg)
                        (funcall ,test
                            ,item
                            ,(if key `(funcall ,key #1#) `#1#)))
                    ,seq)
            form)))


#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 |#
