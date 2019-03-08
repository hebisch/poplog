/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 * File:            C.all/extern/lib/callback.h
 * Purpose:         Definitions for callback functions
 * Author:          John Gibson, May 19 1990 (see revisions)
 * Documentation:   REF *EXTERNAL
 */

#ifndef __pop_callback_h
#define __pop_callback_h

#if defined(vms) && defined(__alpha)
#define __ALPHA_VMS 1
#endif

/*
 *  Define these for Unix so we can use them in VMS for non-functions (only).
 *  (In (non-open) VMS the usual "extern" or nothing for a non-function causes a symbol
 *  to be defined as a program section, not an ordinary global symbol)
 */
#if (!defined(vms) || defined(__ALPHA_VMS)) && !defined(globalref)
#define globalref extern
#define globaldef
#endif

#ifndef NULL
#define NULL 0
#endif

/*
 *  Types for generic pop word/pop object (type POPOBJ merely indicates that
 *  the value is an encoded Pop object)
 */
typedef unsigned long POPWORD;  /* 64-bit in Alpha OSF */
typedef long POPOBJ;

/*
 *  Declarations of internal things (user code musn't use these)
 */
globalref POPWORD
    _pop_signals_pending,   /* pop _trap */
    _pop_disable_flags,     /* pop _disable */
    _pop_external_flags;    /* pop Sys$- _external_flags */
globalref POPOBJ
    pop_exfunc_arg;
extern int _pop_external_callback();


/*
 *  Function codes for _pop_external_callback
 */
#define PEC_FUNC_MISHAP    1
#define PEC_FUNC_CALL      2
#define PEC_FUNC_GET_IDENT 3
#define PEC_FUNC_FREE      4
#define PEC_FUNC_CHECKINTR 5
#define PEC_FUNC_SYSINIT   6


/*
 *  Standard location to receive exfunc_closure argument
 */
globalref POPOBJ *pop_exfunc_closure_arg;


/*
 *  C Interface functions -- all except pop_get_ident return 1 on
 *  normal exit, and all return 0 on abnormal exit
 */
extern int pop_mishap();
extern int pop_call();
extern POPOBJ pop_get_ident();
extern int pop_free_fixed_hold();
extern int pop_check_interrupt();
extern int pop_sys_init();


/*
 *  Flag bits in *pop_external_flags, which is local to every block of
 *  external calls (and set to zero when in pop).
 *  Also defined in $usepop/pop/lib/include/external_flags.ph
 */

globalref POPWORD *pop_external_flags;

    /* status flags */
#define PEF_UNUSED              0x0001
#define PEF_DOING_ABEXIT        0x0002  /* curr block of extern calls set for abnormal exit */
    /* user control over abnormal exits */
#define PEF_RETURN_ABEXIT_NEXT  0x0100  /* return abexit on next callback only */
#define PEF_RETURN_ABEXIT_ANY   0x0200  /* return abexit on any callback (in this block) */
#define PEF_CATCH_ABEXIT_NEXT   0x0400  /* catch abexit on next callback only */
#define PEF_CATCH_ABEXIT_ANY    0x0800  /* catch abexit on any callback (in this block) */
    /* asynchronous callback */
#define PEF_ASYNC_CALLBACK      0x1000  /* allow asynchronous callback (in this block) */
#define PEF_ASYNC_RETURN_ABEXIT 0x2000  /* set RETURN_ABEXIT_NEXT for async cb */
#define PEF_ASYNC_CATCH_ABEXIT  0x4000  /* set CATCH_ABEXIT_NEXT  for async cb */
   /* for system use */
#define PEF_DO_USER_MALLOC  0x8000

#endif  /* !__pop_callback_h */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  6 1995
    POPWORD/POPOBJ now "long" (64 bits on Alpha OSF, 32 on everything else)
--- John Gibson, Mar  7 1995
    Added POPWORD type and changed POPOBJ to "int".
    Added #ifndef __pop_callback_h around file
--- John Gibson, Nov 12 1993
    Added PEC_FUNC_SYSINIT
--- John Gibson, Sep 15 1993
    Added PEF_DO_USER_MALLOC
--- John Gibson, Jul 13 1991
    Added new ASYNC flags
--- John Gibson, Jan 22 1991
    Added definitions for globalref/def in Unix
--- John Gibson, Jan 19 1991
    Added PEF_ASYNC_CALLBACK and replaced PEF_D*OING_USER_EXTERN with
    PREF_UNUSED.
--- John Gibson, Nov 13 1990
    Added declarations for pop_external_flags and PEF_ flags.
    Revised declarations for callback functions.
--- John Gibson, Sep  6 1990
    Added pop_exfunc_arg
--- Roger Evans, Jun 26 1990
    Added CHECKINTR function
--- Simon Nichols, Jun  1 1990
    Changed type of POPOBJ from void * to char *, because MIPS and
    VAXSTATION C compilers are pre-ANSI standard.
 */
