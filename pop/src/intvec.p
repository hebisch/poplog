/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/intvec.p
 > Purpose:
 > Author:          Aled Morris Sep 18 1986 (see revisions)
 > Documentation:   REF *INTVEC
 */

;;; ----------------- SIGNED (INTEGER) VECTORS -----------------------------

#_INCLUDE 'declare.ph'

;;; ------------------------------------------------------------------------

defclass intvec :int;

defclass untvec :uint;

defclass bytevec :byte;

defclass sbytevec :sbyte;

defclass shortvec :short;

defclass ushortvec :ushort;

defclass longvec :long;

defclass ulongvec :ulong;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  5 1995
        Replaced whole lot with defclass
--- John Gibson, Sep  2 1992
        Added M_K_NO_FULL_FROM_PTR to key flags
--- John Gibson, Apr  2 1990
        Changed K_SPEC to "int"
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  2 1989
        Changes for new pop pointers
--- John Gibson, Mar 28 1988
        Moved out of vectors.p
 */
