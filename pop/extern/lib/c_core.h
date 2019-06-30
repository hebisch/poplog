/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/extern/lib/c_core.h
 * Purpose:         Header file for core POPLOG C routines
 * Author:          John Gibson, Mar 14 1991 (see revisions)
 */

#ifndef __c_core_h
#define __c_core_h

#if defined(__linux__)
#define _GNU_SOURCE 1
#endif

/*
 *  Work out which system we're on: VMS, UNIX or WIN32
 */
#if defined(vms) || defined(__vms) || defined(__vms__)
#ifndef VMS
#define VMS
#endif
#endif
#if defined(unix) || defined(__unix) || defined(__unix__)
#ifndef UNIX
#define UNIX
#endif
#endif
#if defined(win32) || defined(__win32) || defined(__win32__)
#ifndef WIN32
#define WIN32
#endif
#endif

#ifdef UNIX
/*
 *  Fine tuning
 */
#if defined(_SVR4) || defined(__SVR4) || defined(__svr4__) \
 || defined(_SVR4_SOURCE)
#ifndef SVR4
#define SVR4
#endif
#endif
#endif  /* UNIX */

#if !defined(VMS) || defined(__alpha)
/*
 *  Define these so we can use them for VAX VMS (for non-functions in VAX VMS,
 *  the usual "extern" or nothing causes a symbol to be defined as a
 *  program section, not an ordinary global symbol)
 */
#ifndef globalref
#define globalref extern
#define globaldef
#endif
#endif  /* !VMS */

#if defined(UNIX) && !defined(linux)
/*
 *  This must be done early on, because it overrides a definition from a
 *  standard header file (<sys/types.h>?). The value must be the same as
 *  in src/unix_select.ph
 */
#define FD_SETSIZE 256
#endif  /* UNIX */

#ifdef UNIX
/*
 *  Save errno during signal handling (not done by default)
 */
#include <errno.h>
extern int errno;   /* for systems which don't define it in <errno.h> */
#define SAVE_ERRNO      int save_errno = errno
#define RESTORE_ERRNO   errno = save_errno

#else   /* !UNIX */
/*
 *  Define dummy versions of same. NB: the first might generate a warning
 *  about an unused variable, but an empty declaration is illegal in ANSI C
 */
#define SAVE_ERRNO      int _not_used
#define RESTORE_ERRNO
#endif  /* UNIX */

/*
 *  Block delivery of signals (VMS & UNIX only)
 */
#ifdef VMS

#include <starlet.h>
#include <ssdef.h>
typedef int sigsave_t;

#define BLOCK_SIG_ALL(save)     (save) = sys$setast(0)
#define BLOCK_SIG(save, sig)    BLOCK_SIG_ALL(save)
#define UNBLOCK_SIG_ALL(save)   (save) = sys$setast(1)
#define RESTORE_SIG(save)       sys$setast((save)==SS$_WASSET)

#endif  /* VMS */
#ifdef UNIX

#include <signal.h>
#ifndef SIG_BLOCK
/*
 *  Non-posix: simulate with definitions from here
 */
#include "posix_signal.h"
#endif  /* !SIG_BLOCK */

#ifndef SIGEMT
#define SIGEMT SIGSYS           /* For Linux */
#endif

typedef sigset_t sigsave_t;

#define BLOCK_SIG_ALL(save)     { sigset_t tmp; sigfillset(&tmp);   \
                                  sigprocmask(SIG_BLOCK, &tmp, &(save)); }
#define BLOCK_SIG(save, sig)    { sigset_t tmp; sigemptyset(&tmp);  \
                                  sigaddset(&tmp, (sig));           \
                                  sigprocmask(SIG_BLOCK, &tmp, &(save)); }
#define UNBLOCK_SIG_ALL(save)   { sigset_t tmp; sigfillset(&tmp);   \
                                  sigprocmask(SIG_UNBLOCK, &tmp, &(save)); }
#define RESTORE_SIG(save)       sigprocmask(SIG_SETMASK, &(save), NULL)

/* Pop wrapper for POSIX sigaction */
void (*_pop_sigaction())();

#endif  /* UNIX */

#ifdef VMS
/*
 *  Additional struct definitions
 */

#ifdef __alpha

 /* union with double ensures type "quad" is double aligned on Alpha */
typedef union
  { struct { unsigned long lo; long hi; } q;
    double  d;
  } quad;
#define q_lo q.lo
#define q_hi q.hi

#else

typedef struct { unsigned long q_lo; long q_hi; } quad;

#endif  /* __alpha */

typedef struct timeval
  { long tv_sec, tv_usec;
  } timeval;

/* Event flag for use by AST routines (23 = last one in cluster 0) */
#define AST_EFN 23

#endif  /* VMS */


/*
 *  Poll handler indexes for _pop_set_poll_state
 */
#define XT_POLL_NUM     0
#define IO_POLL_NUM     1

/*
 *  I/O Sets
 */
#define RDSET   0
#define WRSET   1
#define EXSET   2

/*
 *  AST types (see signals.ph)
 */
#define AST_SIGNAL          0
#define AST_TIMER           1
#define AST_APP_PENDING     2
#define AST_QUEUE_CHECK     3
#define AST_HANDLE          4
#define AST_DEV             5
#define AST_DEV_READ        (AST_DEV+RDSET)
#define AST_DEV_WRITE       (AST_DEV+WRSET)
#define AST_DEV_EXCEPT      (AST_DEV+EXSET)

/*
 * For POPWORD, POPOBJ types
 */
#include "callback.h"

globalref POPWORD _pop_in_X_call;         /* pop Sys$- _in_X_call */

extern unsigned _pop_rem_ast();
extern void
    _pop_add_ast(),
    _pop_do_interrupt(),
    _pop_set_poll_state(),
    _pop_stop_polling(),
    _pop_set_xt_wakeup();

/*
 *  Struct used by _pop_set_xt_wakeup
 */
typedef struct
  { unsigned char
            XWK_ON,             /* interrupt has occurred */
            XWK_FLAG_ENABLED,   /* flag mechanism enabled */
            XWK_FLAG_ON,        /* flag is on */
            XWK_FD_IN,          /* pipe input fd/event flag number */
            XWK_FD_OUT,         /* output fd */
            XWK_SPARE[3];
  } XTWAKE;

globalref XTWAKE __pop_xt_wakeup_struct;

/*
 *  Bits in flags arg to pop_timer
 */
#define TF_VIRT     1
#define TF_NOCANC   2
#define TF_ABS      4
#define TF_REPEAT   8

extern long pop_timer();

/*
 *  Common definitions
 */

typedef unsigned bool;

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif
#ifndef NULL
#define NULL    0
#endif

#define MILL    1000000

#endif  /* !__c_core_h */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 24 1996
        Added AST_HANDLE for Win32
--- John Gibson, Mar  9 1996
        Added AST_EFN for VMS
--- John Gibson, Aug  7 1995
        Added #define SIGEMT = SIGUNUSED for Linux
--- John Gibson, Mar  7 1995
        Added include for callback.h
--- John Gibson, Oct 24 1994
        _pop_signals_pending, _pop_disable_flags and _pop_in_X_call
        are now the actual pop variables rather than pointers to them
--- John Gibson, Sep 28 1994
        Moved in VMS quad and timeval struct defs from pop_timer.c
--- Robert John Duncan, Sep  9 1994
        Modified to support Win32 as well as Unix & VMS, and to be more
        careful about which system it's being compiled for. Moved in some
        typedefs and preprocessor definitions from "c_core.h".
--- John Gibson, Apr 25 1994
        Added new AST_ values
--- John Gibson, May  6 1993
        Moved this file to $popexternlib
--- Robert John Duncan, Nov 27 1991
        Added include for "posix_signal.h"
 */
