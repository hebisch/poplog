/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:            C.unix/extern/lib/posix_signal.h
 * Purpose:         Simulate POSIX signal-handling with BSD 4.3 facilities
 *                  (included in c_core.h)
 * Author:          John Gibson, Jan  5 1991 (see revisions)
 */

typedef int sigset_t;

struct sigaction
{
    struct sigvec sgvec;
};

#define sa_handler  sgvec.sv_handler
#define sa_mask     sgvec.sv_mask
#define sa_flags    sgvec.sv_flags

#define sigfillset(sp)      (*(sp) = -1)
#define sigemptyset(sp)     (*(sp) = 0)
#define sigaddset(sp,sig)   (*(sp) |= sigmask(sig))

#define SA_INTERRUPT SV_INTERRUPT

static int
sigaction(sig,act,oact)
int sig;
struct sigaction *act, *oact;
{
    return sigvec(sig, &act->sgvec, &oact->sgvec);
}

#define SIG_BLOCK   0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

static int
sigprocmask(how, set, oset)
int how;
sigset_t *set, *oset;
{
    int old;
    if (how == SIG_BLOCK)
        old = sigblock(*set);
    else if (how == SIG_SETMASK)
        old = sigsetmask(*set);
    else
        sigsetmask((old = sigblock(0)) & ~*set);
    if (oset != NULL) *oset = old;
    return(0);
}


/* --- Revision History ---------------------------------------------------
--- John Gibson, May  6 1993
        Moved this file to $popexternlib
--- Robert John Duncan, Nov 28 1991
        Made -sigaction- "static" to allow inclusion in multiple files
--- Robert John Duncan, Nov 25 1991
        Renamed and reformatted from the VAX version for general use.
 */
