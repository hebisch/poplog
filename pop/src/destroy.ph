/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/destroy.ph
 > Purpose:         destroy_action declarations
 > Author:          Roger Evans, Aug 11 1988 (see revisions)
 > Documentation:   REF props HELP sys_destroy_action
 > Related Files:   destroy_prop.p gcmain.p
 */

;;; size of sys_destroy_action property table (MUST BE POWER OF 2)
lconstant DASIZE = 32;


;;; destroy_prop_entry - like any other, but with an extra field
struct DESTROY_PROP_ENTRY
  { full    PTE_ARG,
            KEY,
>->         PTE_VALUE,
            PTE_NEXT;       ;;; next entry in chain
    word    PTE_LINK;       ;;; has various uses...
    full    DPTE_DLINK,     ;;; next entry in destroy chain
            DPTE_OLD;       ;;; pointer to old value during gc
    int     DPTE_FLAGS;     ;;; destroy entry flags
  };

;;; flag values in DPTE_FLAGS
lconstant
    _DPTE_DEPEND    = _2:1e0,   ;;; dependent destroy action
    _DPTE_VSCANNED  = _2:1e1,   ;;; value field scanned
    _DPTE_KEYSAV    = _2:1e2,   ;;; arg key saved in OLD
    _DPTE_SCHEDULED = _2:1e3,   ;;; destroy action scheduled
    ;


global constant
        procedure sys_destroy_isdependent
    ;

global vars
    sys_destroy_dependent
    ;

section $-Sys;

global constant
        destroy_prop_entry_key, destroy_mark_key,
    ;

endsection;

;;; weakref macros
lconstant macro (
        DPTEST  = [testdef destroy_prop_entry_key],
        DPWEAK  = [weakref %"["% destroy_prop_entry_key %"]"%],
    );



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 11 1990
        Replaced PTE for destroy entries with DPTE
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- Roger Evans, Aug 31 1988
        added sys_destroy_dependent and sys_destroy_isdependent
--- Roger Evans, Aug 30 1988
        modifications for new destroy marking and independent destroy actions
 */
