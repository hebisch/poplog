/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/syntax.ph
 > Purpose:         Prolog: syntax words, macros etc. for compiling Prolog
 > Author:          Robert John Duncan, Jul  9 1993
 > Documentation:
 > Related Files:
 */

;;; This shouldn't be recompiled if it's already built in to the system
#_TERMIN_IF DEF _PROLOG_SYNTAX_PH_
iconstant _PROLOG_SYNTAX_PH_ = true;

section prolog;

;;; Syntax for choice points

define iconstant syntax SAVE;
    sysPLOG_SAVE();
enddefine;

define iconstant syntax RESTORE;
    sysPLOG_RESTORE();
enddefine;

;;; Syntax for switching on the Prolog type of an object

include vm_flags;
define iconstant syntax PROLOG_GO_ON;
    pop11_comp_expr();
    sysCALL("prolog_type");
    ;;; prolog_type should always return a value in range
    dlocal pop_vm_flags = pop_vm_flags || VM_NO_CHECK_GO_ON_INT;
    sysGO_ON([
        PT_VAR
        PT_PAIR
        PT_TERM
        PT_NIL
        PT_CONST
        PT_OTHER
    ], false);
enddefine;
;;;
;;; Dummy definitions for all the labels prevent autoloading and improve
;;; compilation time considerably
iconstant
    PT_VAR      = 0,
    PT_PAIR     = 0,
    PT_TERM     = 0,
    PT_NIL      = 0,
    PT_CONST    = 0,
    PT_OTHER    = 0,
;

;;; Abbreviations for fast procedures

#_IF DEF prolog_debugging

iconstant macro (
    Front       = "front",
    Back        = "back",
    Destpair    = "destpair",
    Subscrv     = "subscrv",
    Subscrs     = "subscrs",
    Subscrw     = "subscrw",
    Subscrl     = "subscrl",
    Cont        = "cont",
    For         = "for",
    Lmember     = "lmember",
);

#_ELSE

iconstant macro (
    Front       = "fast_front",
    Back        = "fast_back",
    Destpair    = "fast_destpair",
    Subscrv     = "fast_subscrv",
    Subscrs     = "fast_subscrs",
    Subscrw     = "fast_subscrw",
    Subscrl     = "fast_subscrl",
    Cont        = "fast_cont",
    For         = "fast_for",
    Lmember     = "fast_lmember",
);

#_ENDIF

;;; Weak references to VED identifiers for systems without VED

iconstant macro (
    VEDLOADED   = [testdef vedprocess],
    VEDWEAK     = [weakref %"["% vedprocess %"]"%],
);

;;; Flag controlling whether functors may be variables: not so in the
;;; normal system, allowing functor matching to be optimised

iconstant macro
    PROLOG_FIXED_FUNCTORS = true,
;

;;; Operator precedences:

iconstant macro (
    MAXPREC = 2000,
    NOPREC  = 9999,
);

endsection;     /* prolog */
