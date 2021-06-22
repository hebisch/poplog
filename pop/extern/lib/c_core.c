/* --- Copyright University of Sussex, University of Birmingham 2008. All rights reserved. ----------
 * File:            C.all/extern/lib/c_core.c
 * Purpose:         C routines for core POPLOG system
 * Author:          John Gibson, Aug 14 1987 (see revisions)
 * Modified by Waldek Hebisch for new version of GCC, April 2006
 * Modified by Waldek Hebisch to include linux_setper, 2 Dec 2008
 * http://www.math.uni.wroc.pl/~hebisch/poplog/c_core.c
 */

#include "c_core.h"



/*
    Under SCO, compile with:

        cc -c -O -DSVR3 -DSCO -DSIM_ASYNC_IO c_core.c
*/

/*
 *  Bss space for the pop no-restore segment. Must be large enough to contain
 *  all process args/env vars, etc.
 */
globaldef POPWORD
    __pop_nr_seg[20000],
    __pop_nr_seg_nbytes = sizeof(__pop_nr_seg);


/*
 *      Struct used by _pop_set_xt_wakeup
 */
globaldef XTWAKE __pop_xt_wakeup_struct;


/*
 *  Struct for returning signal context info to pop Error_signal
 */
globaldef struct
    { int       PSC_SIG,
            PSC_CODE;
    char   *PSC_PC,
             *PSC_ADDR;
    int     PSC_SIGARRAY[16];   /* for VMS only */
    }  __pop_sigcontext;

/*
 *  This is non-zero when in user external calls (set by _call_external),
 *  and enables asynchronous callback by signals.
 *  It has the value -1 when completely outside of Pop.
 */
globalref POPWORD __pop_in_user_extern;


#if defined(__alpha)
/*
 *  fp value of most recent Pop invocation
 */
globaldef POPWORD __pop_invocation_fp;

/*
 *  Floating-point trap -- check pc within Pop float routines.
 *  __pop_fpe_table in afloat.s is a table of PC ranges and addresses to
 *  continue at.
 */
static bool in_pop_float(pc_ptr)
    char **pc_ptr;
    { POPWORD pc = (POPWORD) *pc_ptr;
    typedef struct { POPWORD starta, enda, conta; } fpe_entry;
    extern fpe_entry __pop_fpe_table[];
    fpe_entry *entry = __pop_fpe_table;

    while (entry->starta)
        { if (entry->starta <= pc && pc <= entry->enda)
            {   /* within range -- set return */
            *pc_ptr = (char *) entry->conta;
            return(TRUE);
            }
        entry++;
        }
    return(FALSE);
    }

#endif  /* __alpha */



/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<        VMS ONLY            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

#ifdef VMS

#include <chfdef.h>
#include <ssdef.h>
#include <iodef.h>
#include <jpidef.h>
#include <dibdef.h>
#include <dcdef.h>
#include <descrip.h>
#include <psldef.h>
#include <lib$routines.h>


/**************************************************************************
 *                          AST Routines                                  *
 **************************************************************************/

/*  These are absolute values (defined in asignals.s) -- the array
 *  declarations are just dummies.
 */
globalref int
    __SIG_INT  [],
    __SIG_ALRM [],
    __SIG_CHLD [],
    __SIG_IO   [];

globalref int _pop_read_wait_status; /* Sys$- _read_wait_status */

typedef struct
    {   unsigned short  IOSB_STATUS;
    unsigned short  IOSB_COUNT;
    unsigned int    IOSB_INFO;
    } IOSB;


#define INTR_EFN (AST_EFN-1)    /* = 22 */

static unsigned efs_allocated;

int pop$get_clust0_ef()
    { register unsigned n, efs = efs_allocated;
    /* use 21 - 8 (allow pop to use 0 - 7) */
    for (n = INTR_EFN-1; n >= 8; n--)
        if (!(efs & (1<<n))) { efs_allocated |= (1<<n); return(n); };
    return(-1);
    }

void pop$free_clust0_ef(n)
    register unsigned n;
    { efs_allocated &= ~(1<<n); }


static int qiow(chan, func, iosb, p1, p2)
    register int chan, func, p1, p2;
    register IOSB *iosb;
    { return(sys$qiow(
                /* efn    */    AST_EFN,
                /* chan   */    chan,
                /* func   */    func,
                /* iosb   */    iosb,
                /* astadr */    0,
                /* astprm */    0,
                /*  p1    */    p1,
                /*  p2    */    p2,
                /*  p3    */    0,
                /*  p4    */    0,
                /*  p5    */    0,
                /*  p6    */    0));
    }

static int set_chan_ast(chan, func, ast)
register int chan, func;
register void (*ast)();
    {   return(qiow(chan, func|IO$_SETMODE, 0, ast, chan)); }


/*
 *  AST that gets run when a subprocess dies.
 *  Arg is address of PROC_COND for entry in proc table.
 *  (see sysspawn.p)
 */
void _pop_spawn_ast(condptr)
register int *condptr;
    {   *condptr = -1;  /* set PROC_COND -1 to say dead */
    _pop_add_ast(AST_SIGNAL, __SIG_CHLD);
    _pop_do_interrupt();
    }

/*
 *  AST for Control C -- receives channel as parameter
 *  (see vmsio.p)
 */
void _pop_ctrlc_ast(chan)
register int chan;
    { /* re-enable myself */
    set_chan_ast(chan, IO$M_CTRLCAST, _pop_ctrlc_ast);
    _pop_add_ast(AST_SIGNAL, __SIG_INT);
    _pop_do_interrupt();
    }

/*
 *  AST for devices allowing direct WRTATTN (e.g. mailboxes)
 *  Receives mailbox unit num as parameter -- re-enabled after actually
 *  reading the device (see vmsio.p)
 */
void _pop_wrtattn_ast(unit)
register int unit;
    { _pop_add_ast(AST_DEV_READ, unit);
    _pop_do_interrupt();
    }

/*
 *  AST for reading messages from a device-associated mailbox, each
 *  message generating a DEV_READ ast. Receives mailbox channel as parameter.
 */
void _pop_devmbx_ast(chan)
register int chan;
    {   IOSB iosb;
#define MSG_SIZE 256
    char buf[MSG_SIZE];
    typedef struct { unsigned short TYPE, UNIT; } MSG;

    for (;;)
        {   iosb.IOSB_STATUS = 0;
        qiow(chan, IO$_READVBLK|IO$M_NOW, &iosb, buf, MSG_SIZE);

        if (iosb.IOSB_STATUS == SS$_NORMAL)
            /* message read -- raise DEV_INPUT ast for unit number
             */
            _pop_add_ast(AST_DEV_READ, ((MSG*) buf)->UNIT);
        else
            /* re-enable myself (can't be done until after read) */
            { set_chan_ast(chan, IO$M_WRTATTN, _pop_devmbx_ast);
            _pop_do_interrupt();
            break;
            }
        };
    }


/* cancel any current POPLOG (possibly asynchronous) read (e.g. terminal
 *  or mailbox)
 */
void _pop_cancel_input_read()
    { register int chan = _pop_read_wait_status;
    if (chan <= 0) return;
    _pop_read_wait_status = 0;
    sys$cancel(chan);       /* will set event flag */
    }


/*
 *  Set/unset wakeup for XtAppProcessEvent. FD_IN is in an event flag
 *  number, added as alternate input to every appcontext. Thus to set
 *  wakeup we set the event flag.
 */
void _pop_set_xt_wakeup(on)
bool on;
    { register XTWAKE *xwk = &__pop_xt_wakeup_struct;
    if (xwk->XWK_FD_IN == 0) return;    /* not set up */
    if (on)
        { if (xwk->XWK_FLAG_ENABLED) sys$setef(xwk->XWK_FD_IN); }
    else
        { xwk->XWK_FLAG_ENABLED = FALSE;
        sys$clref(xwk->XWK_FD_IN);
        }
    xwk->XWK_ON = on;
    }

/*
 *  wake up pop and/or X toolkit
 */
static void wakeup()
    { _pop_set_xt_wakeup(TRUE);
    _pop_cancel_input_read();
    sys$setef(INTR_EFN);            /* set interrupt event flag */
    }

/*
 * Wait for interrupt
 */
int pause_popintr()
    { sys$clref(INTR_EFN);          /* clear interrupt event flag */
    if (! _pop_signals_pending)
        sys$waitfr(INTR_EFN);       /* then wait for it to be set */

    return(SS$_CANCEL);
    }

/*
 * Wait for interrupt or read
 */
bool _pop_read_wait(efn)
int efn;
    { unsigned efmask = 1 << efn, flstat;
    sys$clref(INTR_EFN);                /* clear interrupt event flag */
    if (! _pop_signals_pending)
        /* wait for efn or interrupt to be set */
        sys$wflor(0, efmask | (1<<INTR_EFN));

    sys$readef(0, &flstat);             /* read clust0 event flags */
    return((flstat & efmask) != 0);     /* true if input waiting */
    }

int _pop_sigmask(block)
int block;
    {   return(sys$setast(block ? 0 : 1));
    }


/**************************************************************************
 *              Condition (= error signal) Handler                        *
 **************************************************************************/

static void copybytes();

static bool in_math_lib = FALSE;

unsigned _pop_errsig_handler(sigarglst, mcharglst)
struct chf$signal_array *sigarglst;
struct chf$mech_array *mcharglst;
    { int sig, *pcp, *pslp;
    extern __pop_errsig();

    if (in_math_lib)
        { /* doing library math function -- assume float error */
        in_math_lib = FALSE;        /* signal error */
        return(SS$_CONTINUE);       /* continue */
        };

    /* get signal code */
    sig = sigarglst->chf$l_sig_name;

#ifdef __alpha
    if (sig == SS$_UNWIND) return(SS$_CONTINUE);
#endif

    /* get pointer to pc (psl follows) */
    pcp = &(((int*) sigarglst)[sigarglst->chf$l_sig_args-1]);
    pslp = pcp+1;

#ifdef __alpha
    if (sig == SS$_HPARITH && in_pop_float(pcp)) return(SS$_CONTINUE);
#else
    /* clear First Part Done in psl in case it was in a movc3 etc */
    *pslp &= ~PSL$M_FPD;

    if (sig == SS$_FLTOVF_F || sig == SS$_FLTDIV_F
            || sig == SS$_FLTOVF || sig == SS$_FLTDIV)
        { /* floating-point fault/trap -- all fp instructions in afloat.s put
         * the address of the next instruction in __pop_fpe_handler
         * (pop Sys$- _fpe_handler)
         */
        globalref int __pop_fpe_handler;
        int diff = __pop_fpe_handler - *pcp;
        if (0 < diff && diff < 16)
            {   /* assume OK if less than 16 bytes after instruction */
            *pcp = __pop_fpe_handler;
            *pslp |= PSL$M_V;   /* set overflow bit in psl */
            return(SS$_CONTINUE);
            }
        };
#endif

    __pop_in_user_extern = FALSE;   /* clear this as soon as possible */
    wakeup();

    /* copy the details into a memory structure that pop can access */
    __pop_sigcontext.PSC_SIG    = sig;
    __pop_sigcontext.PSC_CODE   = sigarglst->chf$l_sig_arg1;
    __pop_sigcontext.PSC_PC     = (char *) *pcp;
    __pop_sigcontext.PSC_ADDR   = (char *) *((&sigarglst->chf$l_sig_arg1)+1);

    /* save the whole sigarray for pop to call sys$putmsg on */
    copybytes(sigarglst, &__pop_sigcontext.PSC_SIGARRAY,
                            sizeof(__pop_sigcontext.PSC_SIGARRAY));

    /* return to routine that cleans up and calls Error_signal */
#ifdef __alpha
        {   int handle = (__pop_invocation_fp<<1) | 0x1f,
            new_pc = (int) __pop_errsig;
        /*  unwind VMS procedures back to last pop invocation and then
         *  goto __pop_errsig
         */
        sys$goto_unwind(&handle, &new_pc, NULL, NULL);
        }
#else
    *pcp = (int) __pop_errsig;
    return(SS$_CONTINUE);
#endif
    }


/**************************************************************************
 *                  Calling Math Library Functions                        *
 **************************************************************************/

/*
 *  Call a library math function given the address(es) of 1 or 2
 *  double float args, returning nonzero if OK, zero otherwise.
 *  If OK, result double goes into first arg.
 */

#define MATH_FUNC(ARGS)                                         \
    { double res;                                                   \
    in_math_lib = TRUE;                                         \
    res = (*func)ARGS;                                          \
    if (!in_math_lib) return(FALSE);                            \
    in_math_lib = FALSE;                                        \
    *dfptr = res;                                               \
    return(TRUE);                                               \
    }

bool __pop_math_1(dfptr, func)
register double *dfptr, (*func)();
MATH_FUNC((dfptr))          /* mth$gsin etc take pointers to doubles */

bool __pop_math_2(dfptr, dfptr2, func)
register double *dfptr, *dfptr2, (*func)();
MATH_FUNC((dfptr, dfptr2))


/**************************************************************************
 *                  Unix <-> VMS Time Conversion                          *
 **************************************************************************/

/*
 *  UNIX format time is secs+usecs since 00:00 on 1 Jan 1970
 *  This is that time as a VMS quadword time in 100ns
 */
static quad unix_base = {0x4c178020, 0x007c9567};

void pop$timeval_to_quadtime(tvp, quadp, isabs)
    timeval *tvp;
    quad *quadp;
    bool isabs;
    { long tenmill = 10000000, h_ns = (tvp->tv_usec)*10;    /* usec -> 100ns */

    /* quad = (tvp->tv_sec) * 10000000 + h_ns */
    lib$emul(&tenmill, &tvp->tv_sec, &h_ns, quadp);

    if (isabs)
        {   /* add Unix base time to an absolute time */
        lib$addx(quadp, &unix_base, quadp);
        }
    }

void pop$timeval_from_quadtime(tvp, quadp, isabs)
    timeval *tvp;
    quad *quadp;
    bool isabs;
    {   long tenmill = 10000000, h_ns;
    quad q;

    if (isabs)
        {   /* subtract Unix base time from an absolute time */
        lib$subx(quadp, &unix_base, &q);
        quadp = &q;
        }

    /* quad / 10000000, tvp->tv_sec = quotient, h_ns = remainder */
    lib$ediv(&tenmill, quadp, &tvp->tv_sec, &h_ns);
    tvp->tv_usec = h_ns/10;
    }


#endif  /* VMS */


/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<        UNIX ONLY           >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

#ifdef UNIX

#if defined(__STDC__) || defined(__hpux)
#include <unistd.h>
#endif
#include <sys/time.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <string.h>

#if defined(SVR4) || defined(AIX)
#define HAS_STREAMS
/* additional ioctls */
#include <stropts.h>
#endif


typedef struct timeval timeval;

#ifdef SCO

void sco_setstack();

typedef struct itimerval itimerval;

int setitimer (int iWhich, itimerval *pitvValue, itimerval *pitvOldValue)
{
    /* Dummy setitimer - comes as close as I can with alarm() */
    /* iWhich is ignored */

    static itimerval itvLast;

    if (pitvOldValue)
        *pitvOldValue = itvLast;

    itvLast = *pitvValue;

    alarm(pitvValue->it_value.tv_sec);
}

#endif      /* SCO */


/**************************************************************************
 *                          Signals                                       *
 **************************************************************************/

void _pop_errsig_handler();

globaldef sigset_t * _pop_exclude_sigset = NULL;

void (* _pop_sigaction(sig, handler))()
int sig;
void (*handler)();
    { struct sigaction sa, osa;

    /*  if sig is in this set, don't change handler (allows controlling
     *  external code to prevent Pop installing its signals)
     */
    if (_pop_exclude_sigset && sigismember(_pop_exclude_sigset, sig))
        return((void (*)()) -2);

    if (handler == _pop_errsig_handler)
        sigfillset(&sa.sa_mask);
    else
        sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = handler;
    sa.sa_flags = 0;
#ifdef SA_INTERRUPT
    sa.sa_flags |= SA_INTERRUPT;
#endif
#if defined(SVR4) && !defined(__sgi) || defined(linux)
    /* pass signal context information to handler */
    sa.sa_flags |= SA_SIGINFO;
#endif
    return( sigaction(sig, &sa, &osa) == -1 ? (void (*)()) -1
                                            : (void (*)()) osa.sa_handler);
    }

int _pop_sigmask(block)
int block;
    {   sigset_t set;

    sigfillset(&set);
    if (block) {
        /* don't block these error signals */
        sigdelset(&set, SIGILL);
        sigdelset(&set, SIGEMT);
        sigdelset(&set, SIGFPE);
        sigdelset(&set, SIGBUS);
        sigdelset(&set, SIGSEGV);
    }
    return(sigprocmask(block ? SIG_BLOCK : SIG_UNBLOCK, &set, NULL));
    }


/**************************************************************************
 *                      Pop Signal Handlers                               *
 **************************************************************************/

/*
        29 Jun 2003
        Fix for 'errno' problem on redhat 9 added by A.Sloman
        Suggested by Waldek Hebisch
    Updated: 30 Jun 2003 (made the set function return 0);
*/

long get_libc_errno(void)
{
        return errno;
}

int set_libc_errno(int x)
{
        errno = x;
    return 0;
}

/*
 *  Handler for error-type signals, i.e. QUIT, ILL, IOT, EMT, FPE, BUS, SEGV
 */

#if defined(SVR4) || defined(linux)
/* Default case for SVR4, but not (yet) used for SG IRIX */
#if !defined(__sgi)
#define _POP_ERRSIG_HANDLER_

#if !defined(linux)
#include <siginfo.h>
#endif
#include <ucontext.h>

#if defined(sparc) && !defined(REG_PC)
/* old style */
#define REG_PC PC
#define REG_nPC nPC
#endif


#if defined(__linux__)
#if defined(__x86_64__)
#define REG_PC REG_RIP
#else
#if defined(__i386__)
#define REG_PC REG_EIP
#endif
#endif
#else
#if defined(i386)
#if defined(R_EIP)
#define REG_PC R_EIP
#else
#define REG_PC EIP
#endif
#endif
#endif


void _pop_errsig_handler(int sig, siginfo_t *info, ucontext_t *context)
{
    extern void __pop_errsig();

    int code = 0;
    caddr_t addr = NULL;

    if (__pop_in_user_extern == -1) _exit(1);   /* outside Pop */

#if defined(i386)||defined(__x86_64__)
    if (sig == SIGFPE) {
        extern greg_t __pop_fpe_handler;
        if (__pop_fpe_handler)
        {   /* return to the handler and zero it */
            context->uc_mcontext.gregs[REG_PC] = __pop_fpe_handler;
            __pop_fpe_handler = 0;
            return;
        }
    }
#endif

    __pop_in_user_extern = FALSE;

    if (info != NULL) {
        /* see man siginfo(5) */
        code = info->si_code;
        switch (info->si_signo) {
            case SIGILL:
            case SIGFPE:
            case SIGSEGV:
            case SIGBUS:
                addr = info->si_addr;
                break;
        }
    }

    if (sig == SIGBUS && code == BUS_OBJERR) sig = SIGSEGV;

    /* copy the details into a memory structure that pop can access */
    __pop_sigcontext.PSC_SIG    = sig;
    __pop_sigcontext.PSC_CODE   = code;
#if defined(__arm__)
        __pop_sigcontext.PSC_PC = (char *) context->uc_mcontext.arm_pc;
#else
    __pop_sigcontext.PSC_PC = (char *) context->uc_mcontext.gregs[REG_PC];
#endif
    __pop_sigcontext.PSC_ADDR   = (char *) addr;

    /* return to routine that cleans up and calls Error_signal */
#if defined(__arm__)
        context->uc_mcontext.arm_pc = (greg_t) __pop_errsig;
#else
    context->uc_mcontext.gregs[REG_PC] = (greg_t) __pop_errsig;
#endif
#if defined(REG_nPC)
    /* for RISC processors */
    context->uc_mcontext.gregs[REG_nPC] = (greg_t) __pop_errsig + sizeof(int);
#endif
}

#endif /* !__sgi */
#endif /* SVR4 */

#if  0 /* (defined (linux) && !defined(__alpha)) || defined(SCO) */
#define _POP_ERRSIG_HANDLER_

void
_pop_errsig_handler(sig)
int sig;
{
    /* these are in "asignals.s" */
    extern void __pop_error_return(), __pop_errsig();

    if (__pop_in_user_extern == -1) _exit(1);   /* outside Pop */

    _pop_sigaction(sig, _pop_errsig_handler);

    if (sig == SIGFPE)
    {
        extern int __pop_fpe_handler;
        if (__pop_fpe_handler)
        {   /* return to the handler and zero it */
            __pop_error_return(__pop_fpe_handler);
            __pop_fpe_handler = 0;
            return;
        }
    }

    __pop_in_user_extern = FALSE;   /* clear this as soon as possible */

    /* copy the details into a memory structure that pop can access */
    __pop_sigcontext.PSC_SIG    = sig;
    __pop_sigcontext.PSC_CODE   = 0;
    __pop_sigcontext.PSC_PC     = 0;
    __pop_sigcontext.PSC_ADDR   = 0;

    /* return to __pop_errsig */
    __pop_error_return(__pop_errsig);
}

#endif /* (linux && !alpha) || SCO */

#if defined (__hpux)
#define _POP_ERRSIG_HANDLER_

void
_pop_errsig_handler(sig, code, scp)
int sig, code;
struct sigcontext *scp;
{
    /* __pop_errsig is defined in "asignals.s" */
    extern void __pop_errsig();
    void* sp;

    if (__pop_in_user_extern == -1) _exit(1);   /* outside Pop */

#if defined(__hp9000s300)
    if (sig == SIGFPE)
    {
        extern int __pop_fpe_flag;
        __pop_fpe_flag = 1;
        return;
    }
#endif

    __pop_in_user_extern = FALSE;   /* clear this as soon as possible */

    /* copy the details into a memory structure that pop can access */
    __pop_sigcontext.PSC_SIG    = sig;
    __pop_sigcontext.PSC_CODE   = code;
#if defined(__hp9000s300)
    __pop_sigcontext.PSC_PC     = (char *) scp->sc_pc;
    sp                          = (void*)scp->sc_sp;
#else
#if defined(SS_WIDEREGS)
    /* new-style context */
#define sc_ss_64 sc_sl.sl_ss.ss_wide.ss_64
#define sc_ss_32 sc_sl.sl_ss.ss_narrow
    if (scp->sc_flags & SS_WIDEREGS) {
        __pop_sigcontext.PSC_PC = (char*)scp->sc_ss_64.ss_pcoq_head;
        sp                      = (void*)scp->sc_ss_64.ss_sp;
    }
    else {
        __pop_sigcontext.PSC_PC = (char*)scp->sc_ss_32.ss_pcoq_head;
        sp                      = (void*)scp->sc_ss_32.ss_sp;
    }
#else
    __pop_sigcontext.PSC_PC     = (char*)scp->sc_pcoq_head;
    sp                          = (void*)scp->sc_sp;
#endif
#endif
    __pop_sigcontext.PSC_ADDR   = 0;

    /*  call __pop_errsig to abort, passing the stack pointer from the point
        at which the signal arrived
    */
    __pop_errsig(sp);
}

#endif /* __hpux */

#if !defined(_POP_ERRSIG_HANDLER_)
/* default case, BSD-style */
#define _POP_ERRSIG_HANDLER_

void _pop_errsig_handler(sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;
    { extern __pop_errsig();

    /* specials for FPE */
    if (sig == SIGFPE)
        {
#if defined(sun) && defined(mc68000)
        if (code == FPE_FPA_ERROR)
            {   fpa_handler(sig, code, scp, addr); return; };
#else
#if defined(__alpha)
        if (in_pop_float(&scp->sc_pc)) return;
#endif /* __alpha */
#endif /* sun && mc68000 */

        if (__pop_in_user_extern == -1) _exit(1);   /* outside Pop */
#if defined(vax)
        if (code == FPE_FLTOVF_FAULT || code == FPE_FLTDIV_FAULT)
            {   /* all fp instructions in afloat.s put the address of the
             * next instruction in __pop_fpe_handler (pop Sys$- _fpe_handler)
             */
            extern int __pop_fpe_handler;
            int diff = __pop_fpe_handler - scp->sc_pc;
            if (0 < diff && diff < 16)
                {   /* assume OK if less than 16 bytes after instruction */
                scp->sc_pc = __pop_fpe_handler;
                scp->sc_ps |= 2;    /* set overflow bit in psl */
                return;
                }
            }
        else if (code == FPE_FLTOVF_TRAP || code == FPE_FLTDIV_TRAP)
            /* nothing to do for these */
            return;
#else
#if defined(i386)
        {   extern int __pop_fpe_handler;
            if (__pop_fpe_handler)
            {   /* return to the handler and zero it */
                scp->sc_pc = __pop_fpe_handler;
                __pop_fpe_handler = 0;
                return;
            }
        }
#endif  /* i386 */
#endif  /* vax */
        };

    if (__pop_in_user_extern == -1) _exit(1);   /* outside Pop */

    __pop_in_user_extern = FALSE;   /* clear this as soon as possible */

#if defined(vax)
    /* clear First Part Done in psl in case it was in a movc3 etc */
    scp->sc_ps &= ~(1<<27);
#endif

    /* copy the details into a memory structure that pop can access */
#if defined(sun)
    if (sig == SIGBUS && FC_CODE(code) == FC_OBJERR) sig = SIGSEGV;
#endif

    __pop_sigcontext.PSC_SIG    = sig;
    __pop_sigcontext.PSC_CODE   = code;
#if defined(AIX)
    /* see <sys/mstsave.h> */
    __pop_sigcontext.PSC_PC     = (char *) scp->sc_jmpbuf.jmp_context.iar;
#else
    __pop_sigcontext.PSC_PC     = (char *) scp->sc_pc;
#endif

#if defined(AIX)
    __pop_sigcontext.PSC_ADDR   = (char *) scp->sc_jmpbuf.jmp_context.o_vaddr;
#else
#if defined(mips)
    /* fault address is part of the sigcontext structure */
    __pop_sigcontext.PSC_ADDR   = (char *) scp->sc_badvaddr;
#else
#if defined(__alpha)
    /* ditto */
    __pop_sigcontext.PSC_ADDR   = (char *) scp->sc_traparg_a0;
#else
    __pop_sigcontext.PSC_ADDR   = addr;
#endif  /* __alpha */
#endif  /* mips */
#endif  /* AIX */

    /* return to routine that cleans up and calls Error_signal
     * (On a system where setting the return from the signal isn't
     * possible, this would just have to called instead)
     */
#if defined(AIX)
    { extern POPWORD __pop_errsig_adrtab[];
      scp->sc_jmpbuf.jmp_context.iar = (ulong_t) __pop_errsig;
      /* pass its address table in register 3 */
      scp->sc_jmpbuf.jmp_context.gpr[3] = (ulong_t) __pop_errsig_adrtab;
    }
#else
    scp->sc_pc = (long) __pop_errsig;

#if defined(sparc)
    scp->sc_npc = scp->sc_pc + sizeof(int);
#else
#if defined(__alpha)
     {  /* pass its address table in register 0 */
        extern POPWORD __pop_errsig_adrtab[];
        scp->sc_regs[0] = (long) __pop_errsig_adrtab;
     }
#endif  /* __alpha */
#endif  /* sparc */
#endif  /* AIX */
    }

#endif /* !_POP_ERRSIG_HANDLER_ */
#undef _POP_ERRSIG_HANDLER_


/*
 *  Handler for user-handled signals
 */
void _pop_usersig_handler(sig)
int sig;
    { SAVE_ERRNO;
    _pop_add_ast(AST_SIGNAL, sig);
    _pop_do_interrupt();
    RESTORE_ERRNO;
    }


/*
 *  Set/unset wakeup for XtAppProcessEvent. FD_IN and FD_OUT are pipe
 *  read/write file descriptors, where the read one is added as alternate
 *  input to every appcon. Thus to set wakeup we write a single char to the
 *  pipe, or remove it to unset.
 */
void _pop_set_xt_wakeup(on)
bool on;
    { register XTWAKE *xwk = &__pop_xt_wakeup_struct;
    char c;
    if (xwk->XWK_FD_IN == 0) return;    /* not set up */
    if (on)
        { if (xwk->XWK_FLAG_ENABLED && !xwk->XWK_FLAG_ON)
            { sigsave_t savesig;
            BLOCK_SIG_ALL(savesig);
            if (!xwk->XWK_FLAG_ON)
                { write(xwk->XWK_FD_OUT, &c, 1);
                xwk->XWK_FLAG_ON = TRUE;
                }
            RESTORE_SIG(savesig);
            }
        }
    else
        { xwk->XWK_FLAG_ENABLED = FALSE;
        if (xwk->XWK_FLAG_ON)
            { read(xwk->XWK_FD_IN, &c, 1);
            xwk->XWK_FLAG_ON = FALSE;
            }
        }
    xwk->XWK_ON = on;
    }

/* Set Xt wakeup in case needed (does more in VMS) */
#define wakeup()    _pop_set_xt_wakeup(TRUE)


#ifdef AIX
    /*  Code to determine whether processor needs instruction cache
     *  flushing (POWERPC) or not (POWER). The routine _pop_try_cache_flush
     *  executes an "icbi" instruction, which will produce SIGILL on a POWER
     *  machine.
     */

#include <setjmp.h>

static bool probe_failed = FALSE;
static jmp_buf env;

static void sigill_handler(sig)
int sig;
  { probe_failed = TRUE;
    siglongjmp(env, 1);
  }

bool _pop_needs_cache_flush()
  { struct sigaction sa, osa;
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = sigill_handler;
    sa.sa_flags = 0;
    sigaction(SIGILL, &sa, &osa);
    if (sigsetjmp(env, TRUE) == 0) _pop_try_cache_flush();
    sigaction(SIGILL, &osa, NULL);
    return(!probe_failed);
  }

#endif  /* AIX */


/**************************************************************************
 *                      Async I/O                                         *
 **************************************************************************/

#if defined(__hpux) || defined(sequent)
#define SIM_ASYNC_IO
#endif

static fd_set async_fds[3];     /* sets of async file descriptors */
static fd_set async_on_fds[3];  /* sets for which signal currently on */
static int    async_ctr[3];     /* numbers of bits set in async_on_fds */
static int    async_maxfd[3]    /* max file desc in async_fds */
                    = {-1, -1, -1};

#define TOT_ASYNC   (async_ctr[RDSET]+async_ctr[WRSET]+async_ctr[EXSET])

static void copybytes();
bool _pop_set_async_check();
static timeval zero_tim;

static int sigio_handler()
    {   SAVE_ERRNO;
    register int n, fd, res, maxfd = -1;
    fd_set on_fds[3], *setptr[3];

#define SETUP_SET(set)                                          \
    if (async_ctr[set])                                         \
        { if (async_maxfd[set] > maxfd) maxfd = async_maxfd[set];   \
        setptr[set] = &on_fds[set];                             \
        }                                                           \
    else                                                        \
        setptr[set] = NULL;

    SETUP_SET(RDSET); SETUP_SET(WRSET); SETUP_SET(EXSET);
    if (maxfd < 0) return(0);

    copybytes((char *) async_on_fds, (char *) on_fds, sizeof(on_fds));
    res = n = select(maxfd+1, setptr[RDSET], setptr[WRSET], setptr[EXSET],
                                                            &zero_tim);

    /* produce DEV_INPUT/OUTPUT ast for each with waiting input/output */

#ifdef SIM_ASYNC_IO
#define DISABLE_SET(set)    FD_CLR(fd, &async_on_fds[set]);  async_ctr[set]--;
#else
#define DISABLE_SET(set)    _pop_set_async_check(FALSE, fd, set);
#endif

#define CHECK_SET(set)                                          \
        if (setptr[set] && FD_ISSET(fd, setptr[set]))           \
            { _pop_add_ast(AST_DEV+set, fd);  n--;              \
            /* disable it until after a write/read etc */       \
            DISABLE_SET(set);                                   \
            }

    for (fd = 0; n > 0; fd++)
        {   CHECK_SET(RDSET); CHECK_SET(WRSET); CHECK_SET(EXSET); }

    if (n != res)
        {
#ifdef SIM_ASYNC_IO
        if (TOT_ASYNC == 0) _pop_stop_polling(IO_POLL_NUM);
#endif
        _pop_do_interrupt();
        RESTORE_ERRNO;
        return(2);
        }
    else
        { RESTORE_ERRNO;
        return(0);
        }
    }

bool _pop_set_async_check(on, fd, set)
    bool on;
    int fd, set;
    { register int n;
    register fd_set *on_fds;
    register bool ison;
    int tot_async, seln, flags, res;

    if (!FD_ISSET(fd, &async_fds[set])) return (TRUE);

    on_fds = &async_on_fds[set];
    ison = FD_ISSET(fd, on_fds);

    if (on)
        { if (ison) return(TRUE);
        FD_SET(fd, on_fds); n = 1;
        }
    else
        { if (!ison) return(TRUE);
        FD_CLR(fd, on_fds); n = -1;
        };

    async_ctr[set] += n;

#ifdef SIM_ASYNC_IO
    tot_async = TOT_ASYNC;
    if (tot_async == 0 || (tot_async == 1 && on))
        _pop_set_poll_state(IO_POLL_NUM, on ? sigio_handler : NULL);
    return(TRUE);

#else
    res = seln = 0;
    if (on == 2)
        { /* being turned on from _pop_set_async_fd */
        fd_set fdset; int nfds = fd+1;
        FD_ZERO(&fdset); FD_SET(fd, &fdset);
        if (set == RDSET)
            seln = select(nfds, &fdset, NULL, NULL, &zero_tim);
        else if (set == WRSET)
            seln = select(nfds, NULL, &fdset, NULL, &zero_tim);
        else
            seln = select(nfds, NULL, NULL, &fdset, &zero_tim);
        on = TRUE;
        }

#define STREAMS_CODE                                            \
    flags = 0;                                                  \
    if (FD_ISSET(fd, &async_on_fds[RDSET])) flags |= S_RDNORM;  \
    if (FD_ISSET(fd, &async_on_fds[WRSET])) flags |= S_WRNORM;  \
    if (FD_ISSET(fd, &async_on_fds[EXSET])) flags |= S_RDBAND;  \
    res = ioctl(fd, I_SETSIG, flags);

#define NON_STREAMS_CODE                                \
    if (on || !(    FD_ISSET(fd, &async_on_fds[RDSET])  \
                 || FD_ISSET(fd, &async_on_fds[WRSET])  \
                 || FD_ISSET(fd, &async_on_fds[EXSET])  \
                 ))                                     \
        res = ioctl(fd, FIOASYNC, &on);

#ifdef HAS_STREAMS
#ifdef FIOASYNC
    if (isastream(fd)) { STREAMS_CODE } else { NON_STREAMS_CODE }
#else
    STREAMS_CODE
#endif
#else
    NON_STREAMS_CODE
#endif

    if (res >= 0 || !on)
        { if (seln > 0) sigio_handler();
        return(TRUE);
        }

    /* ioctl failed */
    FD_CLR(fd, on_fds);
    async_ctr[set] -= n;
    return(FALSE);
#endif
    }

long _pop_set_async_fd(on, fd, set)
    bool on;
    int fd, set;
    {   register fd_set *fds = &async_fds[set];

    if (on)
        { static bool setup_done;
        int save;
        if (FD_ISSET(fd, fds)) return(0);

#ifndef SIM_ASYNC_IO
        if (!setup_done)
            { _pop_sigaction(SIGIO, sigio_handler);
            _pop_sigaction(SIGURG, sigio_handler);
            setup_done = TRUE;
            };

#if defined(linux) || defined(AIX) || defined(__sgi) || defined(__DGUX__)
#define GETPGRP getpgrp()
#else
#define GETPGRP getpgrp(0)
#endif  /* linux */

#if defined(HAS_STREAMS) || defined(SCO)
#ifdef FIOASYNC
        if (!isastream(fd) && fcntl(fd, F_SETOWN, -GETPGRP) == -1) return(-1);
#else
        if (!isastream(fd)) { errno = ENOSTR; return(-1); }
#endif
#else
        if (fcntl(fd, F_SETOWN, -GETPGRP) == -1) return(-1);
#endif  /* HAS_STREAMS || SCO */

#endif  /* !SIM_ASYNC_IO */

        FD_SET(fd, fds);
        save = async_maxfd[set];
        if (fd > save) async_maxfd[set] = fd;
        if (_pop_set_async_check(2, fd, set)) return(0);
        /* failed */
        FD_CLR(fd, fds);
        async_maxfd[set] = save;
        return(-1);
        }
    else
        { if (!FD_ISSET(fd, fds)) return(0);
        _pop_set_async_check(FALSE, fd, set);
        FD_CLR(fd, fds);
        if (fd == async_maxfd[set])
            { int f;
            async_maxfd[set] = -1;
            for (f = fd-1; f >= 0; f--)
                if (FD_ISSET(f, fds)) { async_maxfd[set] = f; break; }
            }
        return(0);
        }
    }

bool _pop_get_async_fd(fd, set)
    int fd, set;
    {  return(FD_ISSET(fd, &async_fds[set]));
    }


/**************************************************************************
 *                      Async-related System Calls                        *
 **************************************************************************/

int pop_close(fd)
    int fd;
    { _pop_set_async_fd(FALSE, fd, RDSET);
    _pop_set_async_fd(FALSE, fd, WRSET);
    _pop_set_async_fd(FALSE, fd, EXSET);
    return(close(fd));
    }

long pop_fork() {
        int fd;
        long pid;
    sigset_t all, save;
    sigfillset(&all);
    sigprocmask(SIG_BLOCK, &all, &save);

    if (pid = fork())
        { sigprocmask(SIG_SETMASK, &save, NULL);
        return(pid);
        };

    /* child */
    /* cancel all timers */
    pop_timer(0, 0, NULL, NULL);        /* real */
    pop_timer(TF_VIRT, 0, NULL, NULL);  /* virtual */
    /* cancel async I/O */
    for (fd = 0; async_ctr[RDSET]; fd++) _pop_set_async_fd(FALSE, fd, RDSET);
    for (fd = 0; async_ctr[WRSET]; fd++) _pop_set_async_fd(FALSE, fd, WRSET);
    for (fd = 0; async_ctr[EXSET]; fd++) _pop_set_async_fd(FALSE, fd, EXSET);

    sigprocmask(SIG_SETMASK, &save, NULL);
    return(0);
}


/**************************************************************************
 *              Pop-Interrupt Versions of System Calls                    *
 **************************************************************************/

#define SYSCALL_POPINTR(call)   \
        { int res;                                                      \
        if (_pop_signals_pending && !(_pop_disable_flags & 1))          \
            { errno = EINTR; return(-1); };                             \
        for (;;)                                                        \
            { if ((res = (call)) != -1 || errno != EINTR) return(res);  \
            if (_pop_signals_pending) { errno = EINTR; return(-1); };   \
            }                                                               \
        }

long read_popintr(fd, buf, nbyte)
    int fd; char *buf; int nbyte;
    SYSCALL_POPINTR(read(fd, buf, nbyte))

long pause_popintr()
    SYSCALL_POPINTR(pause())


/****************************************************************************
 * Special version of malloc etc that allocates memory either               *
 * (a) from a single block supplied by Poplog for internal system calls, or *
 * (b) with sbrk when called from inside external_apply.                    *
 ****************************************************************************/

#if defined(SVR4)
#define GETPAGESIZE()   ((int)sysconf(_SC_PAGESIZE))
#else
#if defined(SVR3)
/* System V.3.2 lacks getpagesize system call, so replace with hardwired value.
*/
#define GETPAGESIZE() (0x1000)
#else
#if defined(__hpux)
/* undocumented OSF feature -- see <sys/unistd.h> */
#define GETPAGESIZE()   ((int)sysconf(_SC_PAGE_SIZE))
#else
#define GETPAGESIZE()   getpagesize()
#endif  /* defined(__hpux) */
#endif  /* defined(SVR3) */
#endif  /* defined(SVR4) */


#ifndef AIX
#if ! (defined(__STDC__) || defined(__hpux))
extern char *sbrk();
#endif
#endif

#if defined(SVR4)
/* SVR4 getpwnam needs lots of space */
#define POP_MEM_BLOCK_SIZE 200000
#else
#define POP_MEM_BLOCK_SIZE 40000
#endif

/*
 * Structure of the chains of blocks
 */
typedef struct BLOCK_HEADER
    { struct BLOCK_HEADER * next_block;
    unsigned                block_size; /* in bytes, includes header */
    } *BLOCKP;


/*
 * This is a fast but fairly crude storage allocator.  It allocates blocks of
 * a number of different sizes, and keeps free lists of each size.
 * Requests that don't exactly fit are passed up to the next larger size.
 * In this implementation, the available block sizes are based on the
 * Fibonacci-like recurrence relation
 *
 *      S(n) = S(n-1) + S(n-4)
 *
 *      where S(0)=8, S(1)=16, S(2)=24, S(3)=32.
 *
 * (This gives 62 block sizes less than 2**32.)
 * The overhead on a block is 8 bytes. When free, the first word contains a
 * pointer to the next free block. When in use, the first int is set to MAGIC,
 * and the second is the size index.
 */

typedef union OVERHEAD
    { union OVERHEAD *  next_p;     /* when free */
    struct
        { unsigned int  magic;      /* magic number */
        unsigned int    bucket;     /* bucket # */
        } used;
#define magic_num       used.magic
#define bucket_index    used.bucket
    } *ALLOCP;


#define MAGIC       0xff00fe01  /* magic # on accounting info */
#define NBUCKETS    62          /* number of buckets in table */

/*
 * A freeset block_chain is a pointer to a chain of blocks
 * A freeset free_table is a table of free blocks taken by malloc,
 * whose i'th element is a pointer to the next free block * of size 2^(i+3).
 * The smallest allocatable block is 8 bytes.
 * The OVERHEAD information precedes the data area returned to the user.
 */

struct FREESET
    { BLOCKP    block_chain;            /* block chain */
    BLOCKP  (*get_mem_block)();     /* get mem procedure */
    ALLOCP  free_table[NBUCKETS];   /* freetable */
    };

unsigned __pop_malloc_exhausted;


/* Freeset structure for internal Poplog system calls.
 */
static int pop_mem_block[POP_MEM_BLOCK_SIZE];   /* the block of memory */


static BLOCKP get_pop_mem_block(nbytes)
    unsigned nbytes;
    { static bool allocated;
    if (allocated)
        return(NULL);
    else
        { /* first time -- allocate static block (doubleword aligned) */
        BLOCKP blkp = (BLOCKP) ((((long)pop_mem_block)+7) &~ 7);
        blkp->block_size = sizeof(pop_mem_block) - 8;
        allocated = TRUE;
        return(blkp);
        }
    }

struct FREESET popsys_freeset = {NULL, get_pop_mem_block};


/* Freeset structure for use inside external calls etc.
 */
unsigned __pop_malloc_min_alloc = 0;

static BLOCKP get_ext_mem_block(nbytes)
    register unsigned nbytes;
    { register BLOCKP blkp;
    register unsigned mask, over;
    static unsigned pagesize = 0;

    if (pagesize == 0) pagesize = GETPAGESIZE();
    mask = pagesize-1;

    if (over = ((unsigned long)sbrk(0) & mask)) sbrk(pagesize-over);

    if (nbytes < __pop_malloc_min_alloc) nbytes = __pop_malloc_min_alloc;
    nbytes = (nbytes+mask) & ~mask;
    blkp = (BLOCKP) sbrk(nbytes);
    if (blkp == (BLOCKP) -1) return(NULL);

    blkp->block_size = nbytes;
    return(blkp);
    }

static struct FREESET extern_freeset = {NULL, get_ext_mem_block};


/*
 * The current freeset to use is then determined by whether we are in user
 * external calls (or by PEF_DO_USER_MALLOC if not).
 */
#define CURR_FREESET \
    ( __pop_in_user_extern || (_pop_external_flags&PEF_DO_USER_MALLOC) \
                    ? &extern_freeset \
                    : &popsys_freeset)


/* Find nbytes of memory from a given freeset
*/
static char *find_mem(nbytes, freeset)
    unsigned nbytes;
    struct FREESET *freeset;
    { register BLOCKP blkp, newblkp, *last_addr;
    register long newsize;

    last_addr = &(freeset->block_chain);
    blkp = freeset->block_chain;

    for ( ; ; )
        {
        if (blkp == NULL)
            /*  tried all blocks, none of sufficient size
                call the get_mem_block procedure for the freeset in use
            */
            { blkp = (freeset->get_mem_block)(nbytes+sizeof(struct BLOCK_HEADER));
            if (blkp == NULL)
                return(NULL);
            else
                { blkp->next_block = NULL;
                *last_addr = blkp;      /* chain new block on end */
                }
            }

        if (blkp->block_size >= nbytes)
            /* this block is big enough */
            { newsize = blkp->block_size - nbytes;
            if (newsize > sizeof(struct BLOCK_HEADER))
                /* reduce block size by nbytes */
                { newblkp = (BLOCKP)(((char *)blkp) + nbytes);
                newblkp->block_size = newsize;
                newblkp->next_block = blkp->next_block;
                *last_addr = newblkp;
                }
            else
                /* discard it */
                *last_addr = blkp->next_block;

            /* return pointer to space */
            return((char *)blkp);
            }
        else
            /* try next block */
            { last_addr = &blkp->next_block;
            blkp = blkp->next_block;
            }
        }
    }


void *malloc(size_t nbytes)
    {
    register ALLOCP p;
    register unsigned bucket, s1, s2, s3, size;
    struct FREESET *freeset = CURR_FREESET;
    ALLOCP *freetab = freeset->free_table;

    /* Stop poll_appcons getting called asynchronously, since the X event
     * stuff may use malloc
     */
    POPWORD save_X_call = _pop_in_X_call;
    _pop_in_X_call = TRUE;


    /*
     * Convert amount of memory requested into
     * closest block size stored in hash buckets
     * which satisfies request.  Account for
     * space used per block for accounting.
     */

    nbytes += sizeof (union OVERHEAD);  /* = 8 bytes */

    /* Block sizes are based on the Fibonacci-like recurrence relation
     *      S(n) = S(n-1) + S(n-4)
     */
    bucket = 0;
    s1 = s2 = s3 = size = 8;
    while (nbytes > size)
        { register unsigned tmp = s1;
        s1 = s2; s2 = s3; s3 = size; size = size+tmp;
        bucket++;
        }

    if (bucket >= NBUCKETS) return(NULL);

    /*
     * If nothing in hash bucket right now, request more memory
     */
    if ((p = freetab[bucket]) == NULL)
        { p = (ALLOCP) find_mem(size, freeset);
        if (p == NULL)
            { /* no new mem -- try bigger blocks */
            while (++bucket < NBUCKETS && (p = freetab[bucket]) == NULL) ;
            if (bucket < NBUCKETS)
                freetab[bucket] = p->next_p;    /* remove from linked list */
            else
                { /* no more space */
                _pop_in_X_call = save_X_call;
                if (freeset == &popsys_freeset)
                    {   /* get pop mishap, by sending SIGEMT with
                     * __pop_malloc_exhausted set to the required amount
                     * (tested for by System_error)
                     */
                    __pop_malloc_exhausted = nbytes;
                    kill(getpid(), SIGEMT);
                    pause();                    /* allow sig to be sent */
                    }
                errno = ENOMEM;
                return(NULL);
                }
            }
        }
    else
        freetab[bucket] = p->next_p;    /* remove from linked list */

    p->magic_num = MAGIC;
    p->bucket_index = bucket;

    _pop_in_X_call = save_X_call;
    return ((char *)(p + 1));
    }


void free(void *cp)
    { register unsigned bucket;
    register ALLOCP p, *freetab = CURR_FREESET->free_table;
    POPWORD save_X_call = _pop_in_X_call;

    if (cp == NULL) return;
    p = (ALLOCP)(cp - sizeof (union OVERHEAD));
    if (p->magic_num != MAGIC) return;

    /* Stop poll_appcons getting called asynchronously, since the X event
     * stuff may use free
     */
    _pop_in_X_call = TRUE;

    bucket = p->bucket_index;
    p->next_p = freetab[bucket];
    freetab[bucket] = p;

    _pop_in_X_call = save_X_call;
    }

/*
 * When a program attempts "storage compaction" as mentioned in the
 * old malloc man page, it realloc's an already freed block.  Usually
 * this is the last block it freed; occasionally it might be farther
 * back.  We have to search all the free lists for the block in order
 * to determine its bucket: 1st we make one pass thru the lists
 * checking only the first block in each; if that fails we search
 * ``realloc_srchlen'' blocks in each list for a match (the variable
 * is extern so the caller can modify it).  If that fails we just copy
 * however many bytes was given to realloc() and hope it's not huge.
 */
int realloc_srchlen = 4;    /* 4 should be plenty, -1 =>'s whole list */

/*
 * Search ``srchlen'' elements of each free list for a block whose
 * header starts at ``freep''.  If srchlen is -1 search the whole list.
 * Return bucket number, or -1 if not found.
 */
static int findbucket(freep, srchlen)
    ALLOCP freep;
    int srchlen;
    { register ALLOCP p;
    register int i, j;
    ALLOCP *freetab = CURR_FREESET->free_table;

    for (i = 0; i < NBUCKETS; i++)
        { j = 0;
        for (p = freetab[i]; p && j != srchlen; p = p->next_p)
            { if (p == freep) return (i);
            j++;
            }
        }
    return (-1);
    }

void *realloc(void * cp, size_t nbytes)
    {
    register size_t size, s1, s2, s3;
    ALLOCP p;
    char *res;
    register int bucket;
    int was_alloced = 0;

    if (cp == NULL) return(malloc(nbytes));

    p = (ALLOCP)(cp - sizeof(union OVERHEAD));
    if (p->magic_num == MAGIC)
        { was_alloced++;
        bucket = p->bucket_index;
        }
    else
        { /*
         * Already free, doing "compaction".
         *
         * Search for the old block of memory on the
         * free list.  First, check the most common
         * case (last element free'd), then (this failing)
         * the last ``realloc_srchlen'' items free'd.
         * If all lookups fail, then assume the size of
         * the memory block being realloc'd is the
         * smallest possible.
         */
        if ((bucket = findbucket(p, 1)) < 0 &&
            (bucket = findbucket(p, realloc_srchlen)) < 0)
            bucket = 0;
        }

    /* convert bucket to size */
    s1 = s2 = s3 = size = 8;
    while (bucket != 0)
        { register unsigned tmp = s1;
        s1 = s2; s2 = s3; s3 = size; size = size+tmp;
        bucket--;
        }

    size -= sizeof(union OVERHEAD);

    /* avoid the copy if same size block */
    if (was_alloced && nbytes <= size
        && (nbytes > s3-sizeof(union OVERHEAD) || size == 0))
        return(cp);

    if ((res = malloc(nbytes)) == NULL) return (NULL);
    if (cp != res) copybytes(cp, res, (nbytes < size) ? nbytes : size);
    if (was_alloced) free(cp);
    return(res);
    }



/*  calloc - allocate and clear memory block
 */
#define CHARPERLONG (sizeof(long)/sizeof(char))

void *calloc(size_t num, size_t size)
    {
    register char *mp;
    register long *q;
    register int m;

    num *= size;
    mp = malloc(num);
    if (mp == NULL) return(NULL);
    q = (long *) mp;
    m = (num+CHARPERLONG-1)/CHARPERLONG;
    while (--m >= 0) *q++ = 0;
    return(mp);
    }

void cfree(p, num, size)
    char *p;
    unsigned num, size;
    { free(p); }



#ifdef AIX

/****************************************************************************
 * Special redefinitions of mmap and munmap that record the highest address
 * allocated, and which are then used to implement _pop_brk and _pop_sbrk.
 ****************************************************************************/

#include <sys/types.h>
#include <sys/mman.h>

/* Just returns its argument -- needed to cast int[] as a procedure */
extern char * (*(__pop_return_arg()))();

/* Highest mmapped address -- mmapped memory starts at 0x30000000 */
static char * current_break = (char *) 0x30000000;

void *mmap(void *addr, size_t len, int prot, int flags, int fildes, off_t off)
  {
    int desc[3], *d = (int*) mprotect;
    char *res;
    desc[0] = d[0];
    desc[1] = d[1]+(224-223);       /* mprotect -> mmap */
    desc[2] = 0;
    res = __pop_return_arg(desc)(addr, len, prot, flags, fildes, off);
    if (res != (char *)-1)
      { char *lim = res+len;
        if (lim > current_break && res < (char *)0x40000000)
            current_break = lim;
      }
    return((void *) res);
  }

int munmap(void *addr, size_t len)
  {
    int desc[3], *d = (int*) mprotect;
    int res;
    desc[0] = d[0];
    desc[1] = d[1]+(221-223);       /* mprotect -> munmap */
    desc[2] = 0;
    res = (int) __pop_return_arg(desc)(addr, len);
    if (res != -1)
      { char *a = (char*)addr, *lim = a+len;
        if (lim >= current_break && a < (char *)0x40000000)
            current_break = a;
      }
    return(res);
  }

int _pop_brk(char *new_break)
  { if (new_break > current_break)
        return(mmap(current_break, new_break-current_break,
                    PROT_READ|PROT_WRITE|PROT_EXEC,
                    MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
                == (void *) -1 ? -1 : 0);
    else if (new_break < current_break)
        return(munmap(new_break, current_break-new_break));
    else
        return(0);
  }

char * _pop_sbrk(int nbytes)
  { char *new_break = current_break+nbytes;
    if (nbytes != 0 && _pop_brk(new_break) == -1)
        return((char *) -1);
    else
        return(new_break);
  }

#endif  /* AIX */



/**************************************************************************
 *                  Calling Math Library Functions                        *
 **************************************************************************/

/*
 *  Call a library math function given the address(es) of 1 or 2
 *  double float args, returning nonzero if OK, zero otherwise.
 *  If OK, result double replaces first arg.
 */

#define MATH_FUNC(ARGS)                                         \
    { sigset_t all, save;                                           \
    int en;                                                     \
    double res;                                                 \
                                                                \
    errno = 0;                                                  \
    res = (*func)ARGS;                                          \
    if (errno == 0) { *dfptr = res; return(TRUE); }             \
                                                                \
    /* block signals and try it again */                        \
    sigfillset(&all); sigprocmask(SIG_BLOCK, &all, &save);      \
    errno = 0;                                                  \
    res = (*func)ARGS;                                          \
    en = errno;                                                 \
    sigprocmask(SIG_SETMASK, &save, NULL);                      \
    if (en == 0)                                                \
        { *dfptr = res; return(TRUE); }                         \
    else                                                        \
        return(FALSE);                                          \
    }

bool __pop_math_1(dfptr, func)
register double *dfptr, (*func)();
MATH_FUNC((*dfptr))

bool __pop_math_2(dfptr, dfptr2, func)
register double *dfptr, *dfptr2, (*func)();
MATH_FUNC((*dfptr, *dfptr2))




/**************************************************************************
 *                          Miscellaneous                                 *
 **************************************************************************/

#include <netinet/in.h>

/*
 *  Call a procedure (e.g. inet_ntoa) which takes an in_addr struct by value
 *  as arg. (Used by lib unix_sockets.)
 */
char * pop_call_in_addr_arg(inp, func)
    struct in_addr *inp;
    char * (*func)();
    { return( (*func)(*inp) ); }

/*
 *  Call a procedure (inet_makeaddr) which takes two ints and returns
 *  an in_addr struct by value
 */
void pop_call_in_addr_res(net, lna, inp, func)
    unsigned int net, lna;
    struct in_addr *inp, (*func)();
    { *inp = (*func)(net, lna); }

#if defined(__hpux) || defined(AIX)
/* These are macros in hpux/AIX */
#ifdef htonl
#undef htonl
#undef htons
#undef ntohl
#undef ntohs
#endif
unsigned long  htonl(h) unsigned long  h; { return(h); }
unsigned short htons(h) unsigned short h; { return(h); }
unsigned long  ntohl(n) unsigned long  n; { return(n); }
unsigned short ntohs(n) unsigned short n; { return(n); }

#ifdef AIX
/* This is needed in crummy AIX because the procedures above are not
 * referenced in poplog itself, and "ld" isn't exporting the procedure
 * descriptor symbols for these in the image symbol table (this ought to
 * fixed by the -bnogc option, but that seems to be buggy).
 * This procedure is never actually called.
 */
__pop_socket_stuff_dummy(dum)
int *dum;
  {
    dum[0] = (int) pop_call_in_addr_arg;
    dum[1] = (int) pop_call_in_addr_res;
    dum[2] = (int) htonl;
    dum[3] = (int) htons;
    dum[4] = (int) ntohl;
    dum[5] = (int) ntohs;
  }
#endif

#endif  /* defined(__hpux) || defined(AIX) */

#endif  /* UNIX */


/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<        COMMON              >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<                            >>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/


static void copybytes(from, to, nbytes)
register char *from, *to;
int nbytes;
    { register char *lim;
    if (from > to)
        { lim = from + nbytes;
        while (from < lim) *to++ = *from++;
        }
    else if (from < to)
        { lim = from;
        from += nbytes;
        to += nbytes;
        while (from > lim) *--to = *--from;
        };
    }


/**************************************************************************
 *                      AST/Signal Queue                                  *
 **************************************************************************/

#if defined(__hpux) && defined(__hppa)
/*  _WEAK_pop_external_callback is a pointer to a plabel
*/
globalref int (**_WEAK_pop_external_callback)();
#define POP_EXTERNAL_CALLBACK (**_WEAK_pop_external_callback)
#else
globalref int (*_WEAK_pop_external_callback)();
#define POP_EXTERNAL_CALLBACK (*_WEAK_pop_external_callback)
#endif

/*
 *  Queue of pending ASynchronous Traps/Signals
 */
#define QLEN 64

typedef struct
  { unsigned AST_TYPE;      /* type, e.g. AST_SIGNAL */
    POPWORD  AST_DATA;      /* data, e.g. signal number */
  } AST;

static AST
    astq[QLEN],                 /* queue */
    *astq_add = astq,           /* add pointer */
    *astq_rem = astq;           /* remove pointer */
#define ASTQ_LIM &astq[QLEN]    /* queue limit pointer */


/*
 *  Add ast to queue
 */
void _pop_add_ast(type, data)
register unsigned type;
register POPWORD data;
    {   register AST *p;
    sigsave_t savesig;
    BLOCK_SIG_ALL(savesig);

    p = astq_add;
    p->AST_TYPE = type; p->AST_DATA = data;

    if (++p == ASTQ_LIM) p = astq;  /* wraparound */
    if ((astq_add = p) == astq_rem)
        /* queue overflowed -- lose oldest one */
        astq_rem = (++p == ASTQ_LIM) ? astq : p;

    _pop_signals_pending = 1;   /* set pop _trap */
    RESTORE_SIG(savesig);
    }

/*
 *  Remove next ast from queue and store type and data thru typep and
 *  datap (returning 1 if there was one and 0 if queue empty).
 */
unsigned _pop_rem_ast(typep, datap)
register unsigned *typep;
register POPWORD *datap;
    {   register AST *p;
    register unsigned result;
    sigsave_t savesig;
    BLOCK_SIG_ALL(savesig);

    if ((p = astq_rem) == astq_add)
        /* queue empty -- return 0 */
        {   _pop_signals_pending = 0;   /* clear pop _trap */
        result = 0;
        }
    else
        /* return next ast in loc */
        {   *typep = p->AST_TYPE; *datap = p->AST_DATA;
        astq_rem = (++p == ASTQ_LIM) ? astq : p;
        result = 1;
        };

    RESTORE_SIG(savesig);
    return(result);
    }

/*
 *  Called after adding one or more asts with _pop_add_ast
 */
void _pop_do_interrupt()
    {
    /* wake up things */
    wakeup();

    if (__pop_in_user_extern)
        { /* async callback is safe */
        POPWORD pef = _pop_external_flags;
        if (pef & PEF_ASYNC_CALLBACK && !(_pop_disable_flags & 1))
            { /* service interrupt by calling back */
            struct { POPWORD func; } args;
            sigsave_t savesig;

            if (pef & PEF_ASYNC_CATCH_ABEXIT)
                pef |= PEF_CATCH_ABEXIT_NEXT;
            else if (pef & PEF_ASYNC_RETURN_ABEXIT)
                pef |= PEF_RETURN_ABEXIT_NEXT;

            _pop_external_flags = pef;
            args.func = PEC_FUNC_CHECKINTR;

            /* restore pop signal handling; if the callback returns, restore
            sigs to what they were (else leave them in the pop state) */
            UNBLOCK_SIG_ALL(savesig);
            POP_EXTERNAL_CALLBACK(&args);
            RESTORE_SIG(savesig);
            }
        }
    }


/*
 *  Handler for pop_timer used by sys_timer
 */
void _pop_timer_trap(ident)
POPWORD ident;
    {   _pop_add_ast(AST_TIMER, ident);
    _pop_do_interrupt();
    }


/*  Wrapper for consexfunc routines. Invoked as an exfunc_closure with
 *  pop_exfunc_arg containing a (pop) procedure, and flag information.
 *  We call the pop procedure passing the address of the arg (hence the
 *  address of an array of all the args). On return, the possibly altered
 *  contents of the (first) arg are returned as result
 */
#include <stdarg.h>
typedef char *OPAQUE;

globaldef POPOBJ pop_exfunc_arg;
#if 0
OPAQUE _pop_exfunc_callback(va_alist)
va_dcl
#else
#if 0
OPAQUE _pop_exfunc_callback(OPAQUE first_arg, ...)
#else
OPAQUE _pop_exfunc_callback(
OPAQUE arg0,
OPAQUE arg1,
OPAQUE arg2,
OPAQUE arg3,
OPAQUE arg4,
OPAQUE arg5,
OPAQUE arg6,
OPAQUE arg7,
OPAQUE arg8,
OPAQUE arg9,
OPAQUE arg10,
OPAQUE arg11,
OPAQUE arg12,
OPAQUE arg13,
OPAQUE arg14,
OPAQUE arg15,
OPAQUE arg16,
OPAQUE arg17,
OPAQUE arg18,
OPAQUE arg19)
#endif
#endif
    {
    struct efdata { POPOBJ flags, pdr; };
    register struct efdata *efdata = (struct efdata *)pop_exfunc_arg;
    struct { POPWORD func; POPOBJ obj; POPWORD argp; } args;
    va_list argp;
#if 0
    va_start(argp, first_arg);
#endif
    _pop_external_flags |= *(int*)(efdata->flags);
    args.func   = PEC_FUNC_CALL;
    args.obj    = efdata->pdr;

/* (don't split next line with backslash -- VAX VMS doesn't like it) */
#if 1 /* defined(__hpux) && defined(__hppa) || defined(__sgi) && defined(_CFE) || defined(__alpha) */
#define MAX_N_ARGS 20
    /*  This is for cases where the gross assumption --- that a va_list
        pointer can be passed to directly Pop as an array of arguments
        --- falls down.

        On HP PA-RISC systems, the stack grows UP, so argp points above
        the function args; on SG systems using the new CFE compiler, the
        low bits of the argument pointer are used to encode alignment
        information; on Alpha, each vararg is a double (i.e. 8 bytes)

        In these cases, the varargs mechanism is used correctly to copy
        the arguments into a temporary area, and a pointer to that is
        passed to Pop. Unfortunately, this imposes an arbitrary (and
        unchecked!) limit of MAX_N_ARGS on the number of arguments passed:
        the exfunc closure mechanism should have required the number of
        arguments to be specified.
    */
    {   OPAQUE tmp[MAX_N_ARGS];
#if 0
        int i;
                tmp[0] = first_arg;
        for (i = 1; i < MAX_N_ARGS; i++) tmp[i] = va_arg(argp,OPAQUE);
#else
tmp[0] = arg0;
tmp[1] = arg1;
tmp[2] = arg2;
tmp[3] = arg3;
tmp[4] = arg4;
tmp[5] = arg5;
tmp[6] = arg6;
tmp[7] = arg7;
tmp[8] = arg8;
tmp[9] = arg9;
tmp[10] = arg10;
tmp[11] = arg11;
tmp[12] = arg12;
tmp[13] = arg13;
tmp[14] = arg14;
tmp[15] = arg15;
tmp[16] = arg16;
tmp[17] = arg17;
tmp[18] = arg18;
tmp[19] = arg19;
        args.argp = (POPWORD)tmp;
        POP_EXTERNAL_CALLBACK(&args);
        return(tmp[0]);
#endif
    }
#else
        args.argp = (POPWORD) &first_arg;
    POP_EXTERNAL_CALLBACK(&args);
        return(*(OPAQUE*)args.argp);
#endif
    va_end(argp);
    }

#if defined(linux)

/* Personality code, stolen from sbcl */

/* Prototype for personality(2). Done inline here since the header file
 *  * for this isn't available on old versions of glibc. */
int personality (unsigned long);

#include <sys/utsname.h>

void
linux_setper(int argc, char * * argv, char * * envp)
{
#if defined(__i386__) || defined(__x86_64__) || defined(__arm__)
    struct utsname name;
    int major_version = 2;
    int minor_version = 6;
    int patch_version = 12;
    char *p;
    uname(&name);
    p = name.release;
    /* Do not want <stdlib.h> here */
    extern int atoi(const char *);
    major_version = atoi(p);
    p = strchr(p,'.')+1;
    minor_version = atoi(p);
    p = strchr(p,'.')+1;
    patch_version = atoi(p);

    if ((major_version == 2
         /* Some old kernels will apparently lose unsupported personality
            flags on exec() */
         && ((minor_version == 6 && patch_version >= 11)
             || (minor_version > 6)))
        || major_version >= 3) {
        int pers = personality(0xffffffffUL);
        /* 0x40000 aka. ADDR_NO_RANDOMIZE */
        if (!(pers & 0x40000)) {
            int retval = personality(pers | 0x40000);
            /* Allegedly some Linux kernels (the reported case was
               "hardened Linux 2.6.7") won't set the new personality,
               but nor will they return -1 for an error. So as a
               workaround query the new personality...  */
            int newpers = personality(0xffffffffUL);
            /* ... and don't re-execute if either the setting resulted
               in an error or if the value didn't change. Otherwise
               this might result in an infinite loop. */
            if (retval != -1 && newpers != pers) {
                /* Use /proc/self/exe instead of trying to figure out
                   the executable path from PATH and argv[0], since
                   that's unreliable. We follow the symlink instead of
                   executing the file directly in order to prevent top
                   from displaying the name of the process as "exe". */
                char runtime[PATH_MAX+1];
                int i = readlink("/proc/self/exe", runtime, PATH_MAX);
                if (i != -1) {
                    runtime[i] = '\0';
                    execve(runtime, argv, envp);
                }
            }
            /* Either changing the personality or execve() failed.
               Either way we might as well continue, and hope that
               the random memory maps are ok this time around. */
            {
                char err_mess[] = "WARNING: Couldn't re-execute Poplog with"
                            " the proper personality flags (maybe /proc"
                            " isn't mounted?). Trying to continue anyway.\n";
                write(2, err_mess, sizeof(err_mess)-1);
            }
        }
        /* 0x0400000 is READ_IMPLIES_EXEC */
        if (!(pers & 0x0400000)) {
            personality(pers|= 0x0400000);
        }
    }
#endif
}
#endif
/* --- Revision History ---------------------------------------------------
--- Aaron Sloman and Waldek Hebisch 2 Dec 2008
    Installed revisions posted by Waldek to poplog-dev on 2 Dec 2008
    adding linux_setper, invoked from modified amain.s
    Making invocation of setarch unnecessary
--- Aaron Sloman and Waldek Hebisch 5 Jan 2007
    Finally installed revisions posted by Waldek to poplog-dev on
    3 April 2006
    http://mailman.cs.bham.ac.uk/archives/poplog-dev/2006q2/000034.html

--- Aaron Sloman and Waldek Hebisch 29 Jun 2003
    The use of 'errno' which has been deprecated for some time is no longer
    supported. So the old use is simulated using these two procedures defined
    in $popexternlib/c_core.c (thanks to Waldek Hebisch):
        int get_libc_errno(void)
        int set_libc_errno(int x)

    The are accessed via an active variable defined in $popsrc/errors.p
    and its updater:

        define active DO_ERRNO_VAL();

        define updaterof active DO_ERRNO_VAL(_x);

    $popsrc/unixdefs.ph was also changed:
    This macro definition is no longer used
        lconstant macro _ERRNO = [_extern errno:data!(int)];
    Instead it is defined thus, to invoke the new active variable
    or its updater:
        lconstant macro _ERRNO = [DO_ERRNO_VAL];

    This also required a change to LIB unix_socets

--- Robert Duncan, Feb 17 1999
        Added an extra level of indirection to _WEAK_pop_external_callback
        for HP-UX on PA-RISC.
--- Robert Duncan, Aug 11 1998
        Modifications for DG/UX
--- Julian Clinton, Aug  7 1998
        Added __sgi case for no arg GETPGRP defn.
--- John Gibson, May 18 1998
        Changed async I/O code to use either STREAMS or SETOWN if both
        are available.
--- John Gibson, May 13 1998
        __pop_call_in_addr_res/arg -> pop_call_in_addr_res/arg (these
        don't get exported in AIX if they begin with _).
--- John Gibson, May  6 1998
        Added AIX stuff
--- John Gibson, Mar 25 1998
        Added AIX cases
--- John Williams, May 20 1997
        Increased SVR4 POP_MEM_BLOCK_SIZE yet again.
--- Robert Duncan, May 12 1997
        Modified _pop_errsig_handler for HP-UX 10.20
--- Robert Duncan, Oct 28 1996
        Moved polling code to new file "pop_poll.c"
--- Robert Duncan, Oct 21 1996
        Added #undef htonl, etc. for HP-UX
--- Robert Duncan, Oct  7 1996
        Increased POP_MEM_BLOCK_SIZE again for SVR4 (following further
        problems with getpwnam() on Solaris 2.4)
--- Robert Duncan, Sep 30 1996
        Reinstalled change lost from Sep 10
--- John Gibson, Sep 10 1996
        Added _pop_retry_Xt_poll
--- Robert Duncan, Sep 10 1996
        In _pop_sigaction, made SA_SIGINFO the default for SVR4 but
        excluded __sgi which still uses the BSD-style handler
--- Robert Duncan, Aug  9 1996
        Added code for NCR SVR4
--- Robert Duncan, Apr 25 1996
        Changed Linux/SCO _pop_errsig_handler not to use inline assembly
        code
--- John Gibson, Mar  9 1996
        Fixed VMS event flag use (so nothing uses 0 - 7).
--- Robert John Duncan, Oct  5 1995
        Changed Unix _pop_sigmask so that it stops error signals only from
        being blocked, not unblocked, otherwise once any one of them has
        been blocked on signal delivery, there's a danger of it staying
        blocked for ever (e.g. on HP)
--- John Gibson, Sep 16 1995
        For HPUX, added missing htonl, htons, ntohl, ntohs.
--- Integral Solutions Ltd, Aug 31 1995 (Julian Clinton)
        Removed extra ";" from in_pop_float (causes warning messages
        which later cause newpop to fail).
        Removed use of #elif (HP-UX still doesn't support it!) and set
        SYM_ASYNC_IO true fpr HPUX.
--- John Gibson, Jun 30 1995
        Made VMS pop$timeval_to/from_quadtime take a bool arg isabs to say
        whether absolute time or not (allows negative absolute times)
--- John Gibson, May 19 1995
        Changed malloc to use block sizes based on a Fibonacci-like sequence
        rather than powers of 2 -- this doubles the number of available sizes
        and allows requested sizes to be fitted more closely. (As a
        consequence, the Solaris POP_MEM_BLOCK_SIZE can now be smaller.)
--- John Gibson, Mar 14 1995
        # Changes to _pop_errsig_handler for Alpha OSF1.
        # Made get_pop_mem_block produce a SIGEMT with var
            __pop_malloc_exhausted set to nbytes, rather than calling
            _pop_malloc_exhausted routine (which is no longer necessary)
        # Added __pop_malloc_min_alloc to control min amount malloc
            requests from O/S for external use.
        # Made _pop_sigmask exclude error-type signals
--- John Gibson, Mar  8 1995
        Revised the types of various things for OSF1
--- John Gibson, Feb 25 1995
        Replaced __pop_m*ath1 and __pop_m*ath2 with __pop_math_1 and
        __pop_math_2 (all systems), which are just _extern callable
        functions taking double pointer(s) and a math library function
        (removes the need for the _m*ath1/2 subroutines in afloat.s).
--- Poplog System, Jan 18 1995
        Added code for Linux (#ifdef linux) and SCO (#ifdef SCO or
        #ifdef SVR3).
--- John Gibson, Nov 22 1994
        Added Alpha VMS to the (ever-growing!) list of cases where
        _pop_exfunc_callback has to copy the varargs
--- Robert John Duncan, Nov  2 1994
        __pop_fpe_handler not used on MIPS
--- John Gibson, Oct 24 1994
        _pop_signals_pending, _pop_disable_flags and _pop_in_X_call
        are now the actual pop variables rather than pointers to them.
        Similarily, _pop_external_flags is the actual pop variable (as
        opposed to pop_external_flags, which remains a pointer, since
        it's documented).
--- John Gibson, Oct 22 1994
        Changed globaldef of __pop_in_user_extern to globalref (now
        initialised in initial.p)
--- John Gibson, Oct 17 1994
        For VAX, replaced __pop_fpe_continue with __pop_fpe_handler
        (pop var Sys$- _fpe_handler).
--- John Gibson, Oct 13 1994
        Removed code for subscript range trap from VMS/Unix errsig handlers
        (no longer needed since arrays don't use the VAX "index" instruction
        any more).
--- John Gibson, Sep 28 1994
        Added VMS pop$timeval_to_quadtime and pop$timeval_from_quadtime
        (previously in assembler in amisc.s)
--- Robert John Duncan, Sep  9 1994
        Moved all definitions for pop_timer out to a new file ("pop_timer.c")
        and some typedefs and preprocessor definitions to "c_core.h".
--- Robert John Duncan, Jun  1 1994
        Added SG IRIX 5 to the cases for which _pop_exfunc_callback doesn't
        really work.
--- John Gibson, May 27 1994
        Added pop_call_in_addr_arg
--- John Gibson, May 23 1994
        Made Unix signal handlers save and restore errno
--- John Gibson, May  5 1994
        Revised async I/O code (for SVR4 etc); got rid of pop_r*ead and
        pop_s*elect.
--- Robert John Duncan, Mar 25 1994
        Changed _pop_sigaction to set the SA_SIGINFO flag only for selected
        systems: on IRIX 5, omitting the flag preserves the old (BSD-style)
        behaviour which works fine.
--- Simon Nichols, Jan 12 1994
        Changed the type of us_ptr in MATH_FUNC to be a ptr to a union of
        the types actually loaded from & stored to the user stack. This was
        prompted by the SG C compiler complaining about converting between
        a void* and a function ptr (which is not allowed in ANSI C).
--- John Gibson, Dec  6 1993
        Added __pop_m*ath1 and __pop_m*ath2
--- Simon Nichols, Nov 19 1993
        Fixed syntax error in _pop_errsig_handler for i386/mips FPE case:
        put the code inside a block, as it starts with a declaration.
--- John Gibson, Nov 19 1993
        Increased malloc's POP_MEM_BLOCK_SIZE for Solaris yet again.
        Added _pop_exclude_sigset
--- Simon Nichols, Nov 12 1993
        Modified the version of _pop_errsig_handler which was previously
        specific to the HP-PA to cope with the HP9000 300/400 series also.
        This was necessary as updating the PC field of the sigcontext
        structure doesn't work for HP9000 400 series machines.
--- Simon Nichols, Oct 13 1993
        Added definition of POP_MEM_BLOCK_SIZE for other (non-Solaris)
        systems.
--- John Gibson, Oct 11 1993
        Increased malloc's POP_MEM_BLOCK_SIZE for Solaris
--- John Gibson, Sep 15 1993
        Changed CURR_FREESET for malloc to use ordinary extern mem if
        PEF_DO_USER_MALLOC set in *pop_external_flags
--- Simon Nichols, Jun 18 1993
        Changed _pop_sigaction to block all signals during handling of
        error signals.
--- Robert John Duncan, Jun  1 1993
        More SVR4 changes
--- Robert Duncan, May 12 1993
        Added code for HP-PA (flagged by __hppa); also various minor mods
        to HP-UX code (flagged by __hpux).
--- John Gibson, May  6 1993
        o  Moved this file to $popexternlib
        o  Removed dummy VMS procedure __pop_sh*rim_start -- now in its own
             file $popexternlib/pop_shrim_start.cc
--- Simon Nichols, Mar  1 1993
        Changes for Solaris 2.1: names of indices into gregset_t array now
        prefixed by REG_ (used in _pop_errsig_handler)
--- John Gibson, Dec 18 1992
        Put def for pop_exfunc_arg in here rather than asignals.s
--- John Gibson, Dec 10 1992
        o Removed wait_popintr
        o Moved VMS pop$get_clust0_ef in from XtPoplog.c
--- Robert John Duncan, Sep 22 1992
        Removed use of #elif (not supported on HP-UX)
--- John Gibson, Aug 12 1992
        Fixed SYSCALL_POPINTR to ignore _pop_signals_pending the first time if
        pop_disable_flags&1
--- Robert John Duncan, Jul 27 1992
        Additional changes for SVR4.
        [NB: must be compiled with -DSVR4]
--- Robert John Duncan, Jun 16 1992
        Removed uses of -syscall- which is no longer supported on all
        systems (e.g. SVR4). System calls which were previously redefined
        here (read, close etc.) are now prefixed with 'pop_' (pop_close etc.)
        and it's these which should be called from Poplog.
--- John Gibson, Apr  8 1992
        Changed -pop_timer- to take TF_REPEAT flag to create a repeating timer
--- Robert John Duncan, Dec  2 1991
        Changes for SGI IRIX 4.0.
--- Robert John Duncan, Nov 27 1991
        Added declaration for -errno- and a (redundant) cast in
        _pop_sigaction for MIPS.
--- Simon Nichols, Nov 27 1991
        Changed the type of the extern declaration of sbrk on HP-UX to
        void *.
--- John Gibson, Jul 31 1991
        Moved in _pop_exfunc_callback from $popexternlib/c_callback.c
        and changed to use _WEAK_pop_external_callback
--- John Gibson, Jul 13 1991
        Changed _pop_do_interrupt to handle new PEF_ASYNC flags
--- Simon Nichols, Jul 11 1991
        Doubled minimum size for get_ext_mem_block and added #define for it.
--- John Gibson, Jul 10 1991
        Added VMS version of _pop_sigmask
--- John Gibson, Apr 15 1991
        Added __pop_shrim_start for VMS.
--- John Gibson, Apr 12 1991
        Added __pop_nr_seg.
--- John Gibson, Mar 26 1991
        Malloc now allocates its own memory block for internal pop use.
--- John Gibson, Mar 14 1991
        Moved some declarations to new header file c_core.h.
        Added _pop_set_poll_state for combined polling mechanism, etc.
--- John Gibson, Feb 12 1991
        Made _pop_errsig_handler return from signal to __pop_errsig
        instead of calling it.
--- John Gibson, Feb  9 1991
        VMS condition handler added (was in asignals.s)
--- Robert John Duncan, Jan 31 1991
        Fixed setting of PSC_ADDR field for MIPS
--- John Gibson, Jan 30 1991
        Added unblocking of signals for callback in -_pop_do_interrupt-
--- John Gibson, Jan 27 1991
        Changed definition of -fork- to -pop_fork-
--- John Gibson, Jan 22 1991
        Added stuff for VMS.
--- John Gibson, Jan 19 1991
        Added _pop_do_interrupt and changed D*OING_USER_EXTERN test to
        __pop_in_user_extern
--- John Gibson, Jan 15 1991
        Added _pop_set_async_fd and various other things.
--- John Gibson, Jan  5 1991
        Added pop_timer, and various other changes.
--- John Gibson, Dec 29 1990
        Added code for Unix signal handlers, replacing code previously
        in asignals.s.
--- John Williams, Dec  5 1990
        Changed <sys/syscall.h> to <syscall.h>
--- John Gibson, Dec  4 1990
        Changes to VMS stuff.
--- John Gibson, Dec  3 1990
        Replaced _pop_add/rem_sig with _pop_add/rem_ast.
--- John Gibson, Nov 22 1990
        Changed from C.unix/src/malloc.c to C.all/src/c_core.c and
        added VMS stuff.
--- Roger Evans, Nov 21 1990
        Changed WEAK_pop_check_interrupt to return -1 if pop handlers were
        not called so select can detect that they were called, and use the
        dummy fd code to return control to XptReadWait
--- Roger Evans, Nov 21 1990 added FD_CLR on dummy fd in select
--- Roger Evans, Nov 20 1990
        added reassignment to errno in select
--- John Gibson, Nov 19 1990
        Added WEAK_pop_check_interrupt
--- John Gibson, Nov 13 1990
        _pop_m*alloc_use_external replaced by pop_external_flags, etc.
--- Roger Evans, Nov  9 1990
        Added select code
--- John Gibson, Aug 21 1990
        Added signal queue routines.
--- John Gibson, May 13 1990
        Changed method of switching between internal and external freesets;
        now done by inspecting bit in Poplog variable
        Sys$-_external_flags (setting this dlocally being easier for Poplog).
--- John Gibson, Mar  7 1990
        Changed OVERHEAD struct to be 2 words instead of 1 and made
        -malloc- round block size being allocated to doubleword multiple,
        so that all blocks returned are doubleword-aligned (cures
        problems with programs that use -malloc- to get space for
        double-float operations on SPARC etc).
 */
