#| --- Copyright University of Sussex 1994. All rights reserved. ----------
 | File:            C.all/lisp/src/types.lsp
 | Purpose:         Expansions for built-in type specifiers
 | Author:          John Williams, Feb 9 1987 (see revisions)
 | Documentation:   CLtL, chapters 2 & 4
 | Related Files:   C.all/lisp/src/types.p, C.all/lisp/src/clos-types.p
 |#


;;;; The type expansions for logical, numeric and array type specifiers
;;;; are defined in this file, for the benefit of the function SUBTYPEP.

;;; Logical types.

(deftype ATOM ()
    '(not cons))


(deftype EQL (object)
    `(member ,object))


(deftype NIL ()
    '(or))


(deftype NULL ()
    '(and symbol (member nil)))


;;; Numeric types.
;;; These are all expanded to type specifiers involving the core numeric
;;; types, i.e. INTEGER, RATIO, SINGLE-FLOAT and DOUBLE-FLOAT.


(deftype FIXNUM ()
    `(integer ,most-negative-fixnum ,most-positive-fixnum))


(deftype BIGNUM ()
    `(or (integer ,(1+ most-positive-fixnum) *)
         (integer * ,(1- most-negative-fixnum))))


(deftype BIT ()
    '(integer 0 1))


(deftype MOD (&optional n)
    `(integer 0 (,n)))


(deftype SIGNED-BYTE (&optional (s nil))
    (if s
        `(integer ,(- (ash 1 (1- s))) ,(1- (ash 1 (1- s))))
        '(integer -128 127)))


(deftype UNSIGNED-BYTE (&optional (s nil))
    (if s
        `(integer 0 ,(1- (ash 1 s)))
        '(integer 0 255)))


(deftype RATIONAL (&rest args)
    (if args
        `(or (integer ,@args) (ratio ,@args))
        '(or integer ratio)))


(deftype SHORT-FLOAT (&rest args)
    (if args
        `(single-float ,@args)
        'single-float))


(deftype LONG-FLOAT (&rest args)
    (if args
        `(double-float ,@args)
        'double-float))


(deftype FLOAT (&rest args)
    (if args
        `(or (single-float ,@args) (double-float ,@args))
        '(or single-float double-float)))


(deftype REAL (&rest args)
    (if args
        `(or (rational ,@args) (float ,@args))
        '(or rational float)))


;;; Duplicate character types.
;;; These are for compatibility between CLtL 1, CLtL 2, and ANSI Common Lisp.


(deftype BASE-CHARACTER ()
    'base-char)


(deftype EXTENDED-CHARACTER ()
    'extended-char)


(deftype STRING-CHAR ()
    'character)


;;; Array types.
;;; These all expand (ultimately) to type specifiers involving
;;; ARRAY or SIMPLE-ARRAY


(deftype BIT-VECTOR (&optional size)
    `(array bit (,size)))


(deftype SIMPLE-BIT-VECTOR (&optional size)
    `(simple-array bit (,size)))


(deftype STRING (&optional size)
    `(array character (,size)))


(deftype SIMPLE-STRING (&optional size)
    `(simple-array character (,size)))


(deftype BASE-STRING (&optional size)
    `(array base-char (,size)))


(deftype SIMPLE-BASE-STRING (&optional size)
    `(simple-array base-char (,size)))


(deftype SIMPLE-VECTOR (&optional size)
    `(simple-array t (,size)))


(deftype VECTOR (&optional (etype t) size)
    `(array ,etype (,size)))



#| --- Revision History ---------------------------------------------------
--- John Williams, Apr 26 1994
        Added expansions for new character and string types, also added type
        specifiers REAL and EQL, and removed type COMMON and function COMMONP,
        as per Steele 1990 ch. 6.
--- John Williams, Dec 21 1993
        Added REAL and EQL, removed COMMON and COMMONP. (Steele 1990 p50-1).
 |#
