/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 * File:            C.all/extern/lib/c_callback.c
 * Purpose:         Functions for C interface to Poplog callback
 * Author:          John Gibson, May 19 1990 (see revisions)
 * Documentation:   REF *EXTERNAL
 * Related Files:
 */

#include "callback.h"

#ifdef __ALPHA_VMS

/*
 *  The corresponding pointers get assigned to these variables by
 *  pop on startup. (Allows this file to be a shareable image without
 *  any dangling references.)
 */
POPWORD
    *pop_external_flags  = NULL,
    *pop_signals_pending = NULL,
    *pop_disable_flags   = NULL;
POPOBJ
    *pop_exfunc_closure_arg = NULL;
int
    (*pop_external_callback)() = NULL;

#define POP_EXTERNAL_CALLBACK (*pop_external_callback)

#else

/*
 *  The pointers can be assigned statically
 */
globaldef POPWORD
    *pop_external_flags  = &_pop_external_flags,
    *pop_signals_pending = &_pop_signals_pending,
    *pop_disable_flags   = &_pop_disable_flags;
globaldef POPOBJ
    *pop_exfunc_closure_arg = &pop_exfunc_arg;

#define POP_EXTERNAL_CALLBACK _pop_external_callback

#endif  /* __ALPHA_VMS */


/*  Generate pop mishap with message message.
 */
int pop_mishap(message)
register char *message;
  { struct { POPWORD func, message; } args;
    args.func   = PEC_FUNC_MISHAP;
    args.message= (POPWORD)message;
    return(POP_EXTERNAL_CALLBACK(&args));
  }


/*  Call obj on argp. obj can be a procedure, an identifier
 *  containing one, or a ref containing a procedure/identifier (e.g.
 *  as returned by pop_get_ident, etc). The procedure is called with
 *  argp in an external pointer as argument.
 */
int pop_call(obj, argp)
register POPOBJ obj;
register char *argp;
  { struct { POPWORD func; POPOBJ obj; POPWORD argp; } args;
    args.func   = PEC_FUNC_CALL;
    args.obj    = obj;
    args.argp   = (POPWORD)argp;
    return(POP_EXTERNAL_CALLBACK(&args));
  }


/*  Get identifier with (relative/absolute) pathname idname from dictionary.
 *  Returns a fixed-address (held) ref containing either the identifier of
 *  idname or its idval for an assigned constant, i.e.
 *          cons_fixed(item, ref_key, true)
 */
POPOBJ pop_get_ident(idname)
register char *idname;
  { struct { POPWORD func, idname; POPOBJ obj; } args;
    args.func   = PEC_FUNC_GET_IDENT;
    args.idname = (POPWORD)idname;
    return( POP_EXTERNAL_CALLBACK(&args) ? args.obj : 0);
  }


/*  Call free_fixed_hold on obj (e.g. to free a ref returned by
 *  pop_get_ident).
 */
int pop_free_fixed_hold(obj)
register POPOBJ obj;
  { struct { POPWORD func; POPOBJ obj; } args;
    args.func   = PEC_FUNC_FREE;
    args.obj    = obj;
    return(POP_EXTERNAL_CALLBACK(&args));
  }


/*  Check and service pop interrupts (we use *pop_signals_pending
 *  -- a pop variable -- to check pop's interrupt flag without the
 *  overhead of full callback)
 */

int pop_check_interrupt()
  { struct { POPWORD func; } args;
    args.func   = PEC_FUNC_CHECKINTR;
    return( *pop_signals_pending && !(*pop_disable_flags&1) ?
                POP_EXTERNAL_CALLBACK(&args)
                : 1);
  }


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1995
        Replaced type unsigned with POPWORD
--- John Gibson, Nov 29 1994
        Made all references to pop stuff indirect thru pointer variables.
        For Alpha VMS, these pointers are assigned by pop on startup, but
        statically assigned for all other systems. Moved pop_sys_init to
        a separate file c_sysinit.c, since this cannot be referenced by
        externally loaded code in Alpha VMS (only linked into a standalone
        program).
--- John Gibson, Oct 24 1994
        _pop_signals_pending and _pop_disable_flags are now the actual
        pop variables rather than pointers to them
--- John Gibson, Nov 12 1993
        Added pop_sys_init
--- John Gibson, Jul 31 1991
        Moved _pop_exfunc_callback to c_core.c so that core system
        doesn't need this file.
--- John Gibson, Jul 30 1991
        Added ; after POPOBJ proc
--- Roger Evans, Jun 28 1991 improved _pop_exfunc_callback
--- Roger Evans, Jun 23 1991 added _pop_exfunc_callback
--- John Gibson, Jan 22 1991
        Replaced "extern" with "globalref" for vars.
--- John Gibson, Nov 19 1990
        Replaced pop_check_interrupt() and pop_interrupt_disabled()
        with *_pop_signals_pending and *_pop_disable_flags
--- John Gibson, Nov 13 1990
        All now return 0 on abnormal exit
--- Roger Evans, Jul  9 1990
        corrected embarassingly stupid syntax error in pop_check_interrupt
--- Roger Evans, Jul  3 1990
        added pop_interrupt_disabled check to pop_check_interrupt
--- Roger Evans, Jun 26 1990 added pop_check_interrupt
--- Simon Nichols, Jun  1 1990
        Changed use of void * to char *, because MIPS and VAXSTATION
        C compilers are pre-ANSI standard.
 */
