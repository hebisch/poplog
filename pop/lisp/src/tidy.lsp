#| --- Copyright University of Sussex 1993. All rights reserved. ----------
 | File:            C.all/lisp/src/tidy.lsp
 | Purpose:         Tidy up prior to building Clisp saved image
 | Author:          John Williams, Dec 7 1986 (see revisions)
 | Documentation:
 | Related Files:   C.all/lisp/src/clisp.p
 |#


;;; Remove all internal symbols from lisp package
;;;     since these should all be garbage


(do-symbols (sym :lisp (values))
    (multiple-value-bind
        (sym key)
        (intern (symbol-name sym) :lisp)
        (if (eq key :INTERNAL)
            (unintern sym :lisp))))


;;; Reset *gensym-counter* (so that next gensym is G0)

(setq *gensym-counter* 0)



#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1993
        Now uses *gensym-counter*
 |#
