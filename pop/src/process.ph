/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/process.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------- DEFINITIONS FOR PROCESSES --------------------------

    ;;; stack structure (used for userstack_stack)
struct STACK
  { word    RAW_SIZE;       ;;; word offset size of whole record
    full    KEY;
>-> (word)  STK_PTR,        ;;; pointer to next free word
            STK_LIM;        ;;; limit for STK_PTR
    word    STK_DATA[];     ;;; start of stack data
  };


    ;;; process state structure
struct PROCESS_STATE
  { word    RAW_SIZE;           ;;; word offset size of whole record
    full    KEY;
>-> word    PSS_PROCESS;        ;;; pointer to process during non-copy gc
    ;;; pointer from process record points here
    word    PSS_DATA[];         ;;; saved callstack, followed by saved userstack
  };

    ;;; prolog process state record
struct PLOGPROC_STATE
  { word    RAW_SIZE;           ;;; word offset size of whole record
    full    KEY;
>-> short   PLGPS_FLAGS,        ;;; copy of process' flags during GC
            PLGPS_PERM_FLAGS;   ;;; permanent flags as for process
    int     PLGPS_COUNT,        ;;; number of prolog_barrier applies
            PLGPS_TRAIL_SIZE,   ;;; trail word offset size
            PLGPS_CONTN_SIZE;   ;;; continuation stack word offset size
    word    PLGPS_TRAIL[];      ;;; trail, followed by continuation stack
  };

struct PROCESS
  { word    PS_RUN_NARGS;       ;;; nargs run with (pop integer)
    full    KEY;
>-> short   PS_FLAGS,           ;;; ephemeral flags
            PS_PERM_FLAGS;      ;;; permanent flags
    int     PS_USERSTACK_SIZE;  ;;; size of userstack in state
    (word)  PS_STATE,           ;;; ptr to state PSS_DATA (callstack area first)
            PS_CALLSTACK_LIM,   ;;; ptr to after callstack, i.e. user stack
            PS_CALLSTACK_PARTIAL;   ;;; ptr to partial callstack or NULL
    word    PS_PARTIAL_RETURN;  ;;; for saving RELATIVE return addr during swapping
    full    PS_PLOG_STATE;      ;;; prolog state record or false
  };


lconstant macro (
    ;;; PS_FLAGS mask values
    M_PS_ACTIVE         = 2:1e0,    ;;; set if being run
    M_PS_SUSPENDING     = 2:1e1,    ;;; set if in the middle of suspending
    M_PS_SUBSUMED       = 2:1e2,    ;;; set if suspended inside another process
    M_PS_DEAD           = 2:1e3,    ;;; set if dead

    ;;; PS_PERM_FLAGS mask values
    M_PS_NON_VOLATILE   = 2:1e0,    ;;; set if not volatile
    M_PS_SYSTEM         = 2:1e1,    ;;; set if proc made by system (e.g. for ved)

    PSWEAK      = [weakref %"["% process_key %"]"%],
    );



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 29 1995
        Revised layout of struct PROCESS
--- John Gibson, Dec 11 1992
        Removed perm declarations
--- John Gibson, Jun 26 1991
        Renamed M_PS_V*ED_IM as M_PS_SYSTEM
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Sep 15 1989
        Changed layout of process structure, added new fields; process
        flags revised.
--- John Gibson, Jul  5 1989
        Made flag macros pop integers
--- John Gibson, Jul  3 1988
        Replaced double word field PS_PARTIAL_SAVE with single word field
        PS_PARTIAL_RETURN holding the relative return address (i.e.
        return address minus owner procedure).
--- John Gibson, Feb 25 1988
        Changes for weakref'ing processes, etc
--- John Williams, Dec  2 1987
        Added M_PS_VED_IM
--- John Gibson, Sep 26 1987
        Changes for garbage collector reorganisation
 */
