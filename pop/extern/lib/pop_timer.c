/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/extern/lib/pop_timer.c
 * Purpose:         Managing multiple timers
 * Author:          Robert John Duncan, Sep  7 1994 (see revisions)
 * Documentation:
 * Related Files:
 */

#include "c_core.h"


/***************************************************************************
*                                                                          *
*   VMS                                                                    *
*                                                                          *
***************************************************************************/

#ifdef VMS

#include <jpidef.h>

extern void pop$timeval_to_quadtime(), pop$timeval_from_quadtime();

typedef struct itm
  { short   ITM_BUFLEN,
            ITM_FUNC;
    char   *ITM_BUFFER;
    long   *ITM_RETLEN;
  } itm;

/* Doesn't really matter what these are providing they're non-zero */
#define RTIMER_SIG  ((int) &real_timer)
#define VTIMER_SIG  ((int) &virt_timer)

#define HZ 100
#define TIMER_SETUP(handler, clockintp) *(clockintp) = MILL/HZ

static void TIMER_SET_ITIMER(virt, sec, usec, sig, handler)
bool virt;
long sec, usec;
int sig;
void (*handler)();
  { /* must cancel first even if setting, else we get more than one */
    sys$cantim(sig, 0);

    if (sec|usec)
      { /* setting -- have to use the real timer always, but since
         * this runs faster than cpu time it doesn't matter
         */
        timeval tv; quad qt;
        tv.tv_sec = -sec; tv.tv_usec = -usec;   /* negate for delta time */
        pop$timeval_to_quadtime(&tv, &qt, FALSE);   /* FALSE = interval */
        sys$setimr( /* efn    */    AST_EFN,
                    /* daytim */    &qt,
                    /* astadr */    handler,
                    /* reqidt */    sig);
      }
  }

static void TIMER_GET_CLOCK(virt, tvp)
register timeval *tvp;
register bool virt;
  { if (virt)
      { itm itmlst[2];
        long t;
        /* get cpu time */
        itmlst[0].ITM_BUFLEN    = sizeof(long);
        itmlst[0].ITM_FUNC      = JPI$_CPUTIM;
        itmlst[0].ITM_BUFFER    = (char *) &t;
        itmlst[0].ITM_RETLEN    = NULL;
        itmlst[1].ITM_FUNC      = 0;
        sys$getjpiw(/* efn    */    AST_EFN,
                    /* pidadr */    0,
                    /* prcnam */    0,
                    /* itmlst */    itmlst,
                    /* iosb   */    0,
                    /* astadr */    0,
                    /* astprm */    0);

        tvp->tv_sec  = t/HZ;
        tvp->tv_usec = ((t%HZ)*MILL)/HZ;
      }

    else
      { quad qt;
        sys$gettim(&qt);        /* real time */
        pop$timeval_from_quadtime(tvp, &qt, TRUE);  /* TRUE = abs time */
      }
  }

#define TIMER_RAISE_TRAP(sig, handler)  sys$dclast((handler), (sig), 0)

#endif  /* VMS */


/***************************************************************************
*                                                                          *
*   UNIX                                                                   *
*                                                                          *
***************************************************************************/

#ifdef UNIX

#include <unistd.h>
#include <signal.h>
#include <sys/time.h>

typedef struct timeval timeval;

static struct itimerval itv;

#define it_sec  it_value.tv_sec
#define it_usec it_value.tv_usec

#define RTIMER_SIG  SIGALRM
#define VTIMER_SIG  SIGVTALRM

static void TIMER_SETUP(handler, clockintp)
void (*handler)();
long *clockintp;
  { static struct itimerval zero_itv;

    /* get clock interval in usec */
    itv.it_sec = 0; itv.it_usec = 1;
    setitimer(ITIMER_VIRTUAL, &itv, NULL);
    setitimer(ITIMER_VIRTUAL, &zero_itv, &itv);
    *clockintp = itv.it_usec;

    /* set signal handlers */
    _pop_sigaction(RTIMER_SIG, handler);
    _pop_sigaction(VTIMER_SIG, handler);
  }

#define TIMER_SET_ITIMER(virt, sec, usec, sig, handler)         \
  { itv.it_sec = (sec); itv.it_usec = (usec);                   \
    setitimer((virt)?ITIMER_VIRTUAL:ITIMER_REAL, &itv, NULL);   \
  }

#if defined(SVR4) || defined(__hpux)
/* Use times() to get process time */
#include <unistd.h>
#include <sys/times.h>
#define TIMER_VIRT(tvp)                         \
    {   static long hz;                         \
        struct tms tms;                         \
        long t;                                 \
        if (!hz) hz = sysconf(_SC_CLK_TCK);     \
        times(&tms);                            \
        tvp->tv_sec = (t = tms.tms_utime)/hz;   \
        tvp->tv_usec = ((t%hz)*MILL)/hz;        \
    }
#else
/* Use getrusage to get process time */
#include <sys/resource.h>
#define TIMER_VIRT(tvp)                         \
    {   struct rusage rusage;                   \
        getrusage(RUSAGE_SELF, &rusage);        \
        tvp->tv_sec = rusage.ru_utime.tv_sec;   \
        tvp->tv_usec = rusage.ru_utime.tv_usec; \
    }
#endif  /* SVR4 || HP-UX */

#define GETTIMEOFDAY(tvp) gettimeofday(tvp, NULL)

static void TIMER_GET_CLOCK(virt, tvp)
register timeval *tvp;
register bool virt;
  { if (virt)
        TIMER_VIRT(tvp)
    else
        /* real time */
        GETTIMEOFDAY(tvp);
  }

#define TIMER_RAISE_TRAP(sig, handler)  kill(getpid(), sig)

#endif  /* UNIX */


/***************************************************************************
*                                                                          *
*   WIN32                                                                  *
*                                                                          *
***************************************************************************/

#ifdef WIN32

#ifdef __NUTC__
#include <sys/time.h>
#endif
#include <windows.h>
#include <mmsystem.h>

extern DWORD pop_get_process_time(void);
extern DWORD pop_get_real_time(DWORD *usecs);
    /*  These should be in a header file somewhere ...
    */

typedef struct timeval timeval;
    /*  Time as seconds + microseconds
    */

typedef void (*TIMER_HANDLER)();
    /*  Type of handler procedure called on a timer interrupt
    */

#define N_TIMERS        2
    /*  Pop uses two timers -- one for real time and one for virtual
        (process) time. Win32 timers are always real time, but since
        that runs faster than virtual time it doesn't matter.
    */

#define RTIMER_SIG      0
#define VTIMER_SIG      1
    /*  Identifiers for the real and virtual timers (used as indexes
        into the timer arrays)
    */

#define TARGET_RESOLUTION   10
    /*  Target resolution for timers in msec: setting this too low can
        cripple performance (observations from NT 3.51/x86)
    */

static UINT timer_event[N_TIMERS];
    /*  Identifiers of any active timer events: setting one of these to
        zero invalidates the corresponding event.
    */

static HANDLE timer_semaphore[N_TIMERS];
#define TIMER_SEMAPHORE_MAX 64
    /*  Indicates for each timer the number of events which have fired
        but have not yet been processed. Max semaphore value must be >=
        N_TIMENTS (see below).
    */

static CRITICAL_SECTION timer_section;
    /*  Grants exclusive access to timer data structures
    */

static HANDLE timer_thread;
    /*  Handle of thread processing timer events
    */

static TIMECAPS timecaps;
    /*  Min and max resolutions of the timer device
    */

static void CALLBACK TimeProc(
        UINT event,
        UINT reserved1,
        DWORD data,
        DWORD reserved2,
        DWORD reserved3)
    /*  Timer callback function. Indicates the occurrence of a timer event
        by signalling the appropriate semaphore: the timer index number is
        passed through the user data.
    */
{
    timer_event[data] = 0;
    ReleaseSemaphore(timer_semaphore[data], 1, NULL);
}

static DWORD timer_loop(DWORD arg)
    /*  Runs in a separate thread, processing timer events signalled by
        TimeProc.
    */
{
    TIMER_HANDLER handler = (TIMER_HANDLER)arg;

    for (;;)
    {
        DWORD which;

        /* wait for some timer to fire */
        which = WaitForMultipleObjects(
                    N_TIMERS,           /* number of timers */
                    timer_semaphore,    /* one semaphore for each */
                    FALSE,              /* terminate for any one */
                    INFINITE);          /* no timeout */

        /* claim critical section */
        EnterCriticalSection(&timer_section);

        /* call handler with timer index */
        handler(which - WAIT_OBJECT_0);

        /* exit critical section */
        LeaveCriticalSection(&timer_section);
    }

    return 0;
}

static void TIMER_SET_ITIMER(
        BOOL virt,
        long sec,
        long usec,
        int which,
        TIMER_HANDLER handler)
    /*  Set or cancel the timer indicated by WHICH.
    */
{
    /* invalidate any existing timer so there's never more than one running */
    if (timer_event[which] != 0) {
        timeKillEvent(timer_event[which]);
        timer_event[which] = 0;
    }

    /* start new timer for non-zero time */
    if (sec != 0 || usec != 0) {
        UINT msec = (sec * 1000) + ((usec + 500) / 1000);
        if (msec < timecaps.wPeriodMin) {
            msec = timecaps.wPeriodMin;
        }
        else if (msec > timecaps.wPeriodMax) {
            msec = timecaps.wPeriodMax;
        }
        timer_event[which] =
            timeSetEvent(
                msec,                   /* event period */
                timecaps.wPeriodMin,    /* resolution */
                (LPTIMECALLBACK)TimeProc,
                                        /* callback function */
                (DWORD)which,           /* user data */
                TIME_ONESHOT);          /* event type */
    }
}

static void TIMER_RAISE_TRAP(int which, TIMER_HANDLER handler)
    /*  Signal an immediate timer event
    */
{
    ReleaseSemaphore(timer_semaphore[which], 1, NULL);
}

static void TIMER_GET_CLOCK(BOOL virt, struct timeval *tvp)
    /*  Get the current (real or virtual) time
    */
{
    if (virt) {
        /* process time: this is accurate to 1/100th of a second */
        DWORD t = pop_get_process_time();
        tvp->tv_sec = t / 100;
        tvp->tv_usec = (t % 100) * 10000;
    }
    else {
        /* real time */
        tvp->tv_sec = pop_get_real_time(&tvp->tv_usec);
    }
}

static void TIMER_SETUP(TIMER_HANDLER handler, long *clockintp)
    /*  Initialise timer structures and determine the minimum clock
        interval
    */
{
    int i;
    DWORD thread_id;

    /* create timer semaphores */
    for (i = 0; i < N_TIMERS; i++) {
        timer_semaphore[i] = CreateSemaphore(NULL, 0, TIMER_SEMAPHORE_MAX, NULL);
    }

    /* initialise critical section */
    InitializeCriticalSection(&timer_section);

    /* create thread for processing asynchronous timer events */
    timer_thread = CreateThread(
            NULL,                       /* security (default) */
            4096,                       /* initial stack size */
            (LPTHREAD_START_ROUTINE)timer_loop,
                                        /* function to call */
            (LPVOID)handler,            /* argument */
            0,                          /* flags */
            &thread_id);                /* new thread ID */

    /* tweak its priority so that timer events will be handled ASAP */
    SetThreadPriority(timer_thread, THREAD_PRIORITY_ABOVE_NORMAL);

    /* get timer capabilities */
    timeGetDevCaps(&timecaps, sizeof(TIMECAPS));

    /* adjust minimum resolution to target */
    timecaps.wPeriodMin = min(max(timecaps.wPeriodMin, TARGET_RESOLUTION),
                              timecaps.wPeriodMax);

    /* initialise timer device for minimum resolution */
    timeBeginPeriod(timecaps.wPeriodMin);

    /* return that as clock interval (in usecs) */
    *clockintp = timecaps.wPeriodMin * 1000;
}


typedef int sigsave_t;
#define BLOCK_SIG(save, sig)    EnterCriticalSection(&timer_section)
#define RESTORE_SIG(save)       LeaveCriticalSection(&timer_section)
    /*  Ensure exclusive access to the timer data structures. In VMS & UNIX,
        this can be guaranteed only by blocking delivery of all ASTs (signals).
        We use a critical section instead, but the macro names reflect the
        traditional strategy.
    */

#endif  /* WIN32 */


/***************************************************************************
*                                                                          *
*   COMMON                                                                 *
*                                                                          *
***************************************************************************/

typedef struct timentry
  { struct timentry  *E_NEXT;
    void            (*E_HANDLER)();
    POPWORD           E_IDENT;
    timeval           E_VAL;
#define                 E_SEC  E_VAL.tv_sec
#define                 E_USEC E_VAL.tv_usec
    timeval           E_INTERVAL;
#define                 E_INTSEC  E_INTERVAL.tv_sec
#define                 E_INTUSEC E_INTERVAL.tv_usec
  } timentry;

typedef struct timer
  { timentry   *T_QUEUE;
    int         T_SIG;
  } timer;


#define ADDTIM(xs,xu,ys,yu,ds,du) \
        { (ds)=(xs)+(ys); if (((du)=(xu)+(yu))>=MILL) {(du)-=MILL; ++(ds);} }

#define SUBTIM(xs,xu,ys,yu,ds,du) \
        { (ds)=(xs)-(ys); if (((du)=(xu)-(yu))<0) {(du)+=MILL; --(ds);} }

#define VIRT(timerp)    (timerp)->T_SIG == VTIMER_SIG

#define N_TIMENTS 32
static timentry
    timents[N_TIMENTS],
    *free_timents = timents;

static timer
    real_timer = {NULL, RTIMER_SIG},
    virt_timer = {NULL, VTIMER_SIG};

static long half_clockint;
static void timer_handler();

static timentry *next_timer(timerp, handling)
register timer *timerp;
bool handling;
  { register timentry *e;
    timeval clk;
    register int sec, usec;

    if ((e = timerp->T_QUEUE) == NULL)          /* next timer entry */
        /* no timers */
        sec = usec = 0;
    else
      { sec = e->E_SEC; usec = e->E_USEC;
        if (sec | usec)
          { TIMER_GET_CLOCK(VIRT(timerp), &clk);        /* abs time */
            /* get time till expiry */
            SUBTIM(sec, usec, clk.tv_sec, clk.tv_usec, sec, usec);
          };

        if (sec < 0 || (sec == 0 && usec <= half_clockint))
          { /* expired */
            if (handling)
              { timerp->T_QUEUE = e->E_NEXT;    /* make next entry head of queue */

                /* test whether to repeat timer (interval nonzero) */
                sec = e->E_INTSEC; usec = e->E_INTUSEC;
                if (sec | usec)
                  { /* repeat it */
                    register timentry *f, **last;
                    ADDTIM(sec, usec, e->E_SEC, e->E_USEC, sec, usec);
                    e->E_SEC = sec; e->E_USEC = usec;
                    /* add back to queue in order */
                    last = &timerp->T_QUEUE;
                    for (f = timerp->T_QUEUE; f; f = f->E_NEXT)
                        if (sec < f->E_SEC || (sec == f->E_SEC && usec < f->E_USEC))
                            break;
                        else
                            last = &f->E_NEXT;

                    *last = e;
                    e->E_NEXT = f;
                  }
                else
                  { /* no repeat -- free entry */
                    e->E_NEXT = free_timents;   /* chain free entries */
                    free_timents = e;           /* onto old entry */
                  }
                return(e);
              }

            else
              { /* not handling immediately -- generate trap */
                sec = usec = 0;     /* to cancel itimer */
                TIMER_RAISE_TRAP(timerp->T_SIG, timer_handler);
              }
          }
      };

    /* set/cancel timer */
    TIMER_SET_ITIMER(VIRT(timerp), sec, usec, timerp->T_SIG, timer_handler);
    return(NULL);
  }

static void timer_handler(sig)
int sig;
  { SAVE_ERRNO;
    register timer *timerp = (sig == VTIMER_SIG) ? &virt_timer : &real_timer;
    register timentry *e = next_timer(timerp, TRUE);
    if (e != NULL)
      { /* check next before running handler */
        next_timer(timerp, FALSE);
        /* run handler for expired entry */
        (*e->E_HANDLER)(e->E_IDENT);
      }
    RESTORE_ERRNO;
  }

long pop_timer(flags, ident, handler, tvp)
unsigned flags;
POPWORD ident;
void (*handler)();
timeval *tvp;
  {
    register timer *timerp = flags&TF_VIRT ? &virt_timer : &real_timer;
    register timentry *e, *nxt, **last, *org;
    timeval clk;
    int res;
    register int sec, usec;
    sigsave_t savesig;
    static long clockint;
    static bool setup_done;


    if (!setup_done)
      { if (!handler && !ident) return(0);  /* cancelling all timers */
        /* set up free list of timer entries */
        e = free_timents;
        for (nxt = e+N_TIMENTS-1; e < nxt; e++) e->E_NEXT = e+1;
        e->E_NEXT = NULL;
        /* set signal handlers etc, and compute clock interval in usec */
        TIMER_SETUP(timer_handler, &clockint);
        half_clockint = clockint>>1;
        setup_done = 1;
      };

    BLOCK_SIG(savesig, timerp->T_SIG);

    org = e = timerp->T_QUEUE;  /* current timer entry */

    if (handler)

        /* SETTING */
      { if ((nxt = free_timents) == NULL || tvp == NULL)
          { /* no free entries or no time pointer -- return error */
            RESTORE_SIG(savesig);
            return(-1);
          };

        free_timents = nxt->E_NEXT;
        nxt->E_HANDLER = handler;
        nxt->E_IDENT = ident;
        nxt->E_INTSEC = nxt->E_INTUSEC = 0;     /* repeat interval */

        sec = tvp->tv_sec; usec = tvp->tv_usec;

        if (!(flags&TF_ABS) && (sec|usec))
          { /* time interval -- make into abs time */

            /* if nonzero and less than 1 clock interval, make it 1 */
            if (sec == 0 && usec < clockint) usec = clockint;
            /* then record interval if repeating */
            if (flags&TF_REPEAT)
                { nxt->E_INTSEC = sec; nxt->E_INTUSEC = usec; }

            TIMER_GET_CLOCK(VIRT(timerp), &clk);    /* get current time */
            ADDTIM(sec, usec, clk.tv_sec, clk.tv_usec, sec, usec);
          };
        /* elseif zero interval, leave as zero and expires immediately */
        nxt->E_SEC = sec; nxt->E_USEC = usec;

        /* add to queue in order */
        for (last = &timerp->T_QUEUE; e != NULL; e = e->E_NEXT)
            if (sec < e->E_SEC || (sec == e->E_SEC && usec < e->E_USEC))
                break;
            else
                last = &e->E_NEXT;

        *last = nxt;
        nxt->E_NEXT = e;
        res = 0;
      }

    else if (ident)

        /* GETTING/CANCELLING */
      { /* find entry matching ident */
        res = -1;
        for (last = &timerp->T_QUEUE; e != NULL; e = e->E_NEXT)
            if (e->E_IDENT == ident)
              { if (tvp != NULL)
                  { /* return time */
                    TIMER_GET_CLOCK(VIRT(timerp), &clk);    /* current time */
                    sec = e->E_SEC; usec = e->E_USEC;
                    if (!(sec|usec))
                      { sec = clk.tv_sec; usec = clk.tv_usec; };
                    if (!(flags&TF_ABS))
                      { /* return interval */
                        SUBTIM(sec, usec, clk.tv_sec, clk.tv_usec, sec, usec);
                        if ((sec|usec) < 0) sec = usec = 0;
                      };
                    tvp->tv_sec = sec; tvp->tv_usec = usec;
                  };
                if (!(flags&TF_NOCANC))
                  { /* cancel entry */
                    *last = e->E_NEXT;
                    e->E_NEXT = free_timents;   /* chain free entries */
                    free_timents = e;           /* onto old entry */
                  };
                res = 0;
                break;
              }

            else
                last = &e->E_NEXT;
      }

    else

        /* CANCELLING ALL TIMERS */
      { timerp->T_QUEUE = NULL; res = 0; };

    if (timerp->T_QUEUE != org)
        /* head of queue changed -- set timer etc */
        next_timer(timerp, FALSE);

    RESTORE_SIG(savesig);
    return(res);
  }



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct 23 1996
        Fixed definition of GETTIMEOFDAY for Solaris 2.5: it was a mistake
        for SVR4 to define gettimeofday() with only one argument, and most
        systems have now gone back on it
--- Robert Duncan, May 14 1996
        Added special case for Win32 + NuTCRACKER
--- Robert Duncan, Mar 22 1996
        Changed Win32 timer to use a minimum resolution of 10msec. It can
        do better, but is likely to significantly degrade performance.
--- John Gibson, Mar  9 1996
        Changed VMS timer routines to use event flag number AST_EFN
--- John Gibson, Jun 30 1995
        VMS pop$timeval_to/from_quadtime now take a bool arg isabs to say
        whether absolute time or not.
--- John Gibson, Sep 28 1994
        Moved VMS struct definitions for quad and timeval to c_core.h
 */
