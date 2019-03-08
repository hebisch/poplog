/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 * File:            C.all/extern/lib/c_sysinit.c
 * Purpose:         Initialise pop from external code
 * Author:          John Gibson, Nov 28 1994 (see revisions)
 * Documentation:   REF *EXTERNAL
 * Related Files:
 */

#include "callback.h"

/*  Initialise Pop from outside, i.e. when called from controlling
 *  external code in a standalone-linked program.
 */
int pop_sys_init(plog_nwords, unused)
POPWORD plog_nwords, unused;
  { struct { POPWORD func, plog_nwords, unused; } args;
    args.func       = PEC_FUNC_SYSINIT;
    args.plog_nwords= plog_nwords;
    args.unused     = unused;
    return(_pop_external_callback(&args));
  }



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1995
        Changed type unsigned to POPWORD
 */
