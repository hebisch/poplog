/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/extern/lib/pop_poll.c
 * Purpose:         Asynchronous Polling
 * Author:          Robert Duncan, Oct 28 1996
 * Documentation:
 * Related Files:
 */

#include "c_core.h"
#include <sys/types.h>


/***************************************************************************
*                                                                          *
*   VMS                                                                    *
*                                                                          *
***************************************************************************/

#ifdef VMS

#define CLT(clicks) (-200000*clicks)    /* 1 click = 1/50 sec */
static int poll_time[2] = {0, -1};

#define OS_SET_POLL_TIMER(t) \
        { poll_time[0] = (t); \
          sys$setimr(AST_EFN, poll_time, async_poll, (long) poll_time); }
#define CANCEL_POLL_TIMER() \
        sys$cantim((long) poll_time, 0)

#endif  /* VMS */


/***************************************************************************
*                                                                          *
*   UNIX                                                                   *
*                                                                          *
***************************************************************************/

#ifdef UNIX

#include <sys/time.h>
typedef struct timeval timeval;

#define CLT(clicks) (20000*clicks)      /* 1 click = 1/50 sec */
static timeval poll_time  = {0, 0};

#define OS_SET_POLL_TIMER(t) \
        { poll_time.tv_usec = (t); \
          pop_timer(0, (long) &poll_time, async_poll, &poll_time); }
#define CANCEL_POLL_TIMER() \
        pop_timer(0, (long) &poll_time, NULL, NULL);

#ifdef AIX
/* This silly little procedure is used in c_core.c, but must be in a
    different file to work */
void * __pop_return_arg(void *arg) { return(arg); }
#endif

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

typedef struct timeval timeval;

#define CLT(clicks) (20000*clicks)      /* 1 click = 1/50 sec */
static timeval poll_time  = {0, 0};

    /*  This will be called from the timer thread, but polling routine
        must be called from the main thread
    */
static void raise_poll() {
    void async_poll();
    extern void pop_poll_trap();
    pop_poll_trap(async_poll);
}

#define OS_SET_POLL_TIMER(t) \
        { poll_time.tv_usec = (t); \
          pop_timer(0, (long) &poll_time, raise_poll, &poll_time); }
#define CANCEL_POLL_TIMER() \
        pop_timer(0, (long) &poll_time, NULL, NULL);

#endif  /* WIN32 */


/**************************************************************************
 *                          Polling                                       *
 **************************************************************************/

globaldef int (*_pop_Xt_poll_deferred)();

#define MAX_POLL_INDEX 19
static int poll_time_curve[MAX_POLL_INDEX+1]
                        = { CLT(1), CLT(1), CLT(1), CLT(1), CLT(1),
                            CLT(1), CLT(1), CLT(1), CLT(1), CLT(1),
                            CLT(1), CLT(2), CLT(2), CLT(3), CLT(4),
                            CLT(5), CLT(6), CLT(7), CLT(8), CLT(10)};

static int num_polling, poll_index, poll_index_base;
static int (*poll_handlers[2])();

#define SET_POLL_TIMER()  OS_SET_POLL_TIMER(poll_time_curve[poll_index])

#define DO_HANDLER(n)   (p = poll_handlers[n], (p) ? (*p)() : 0)

static void async_poll()
  { register int (*p)();
    register int activity_level = DO_HANDLER(0) | DO_HANDLER(1);
    register int n = poll_index-9;
    if (poll_index_base < n)
        poll_index_base = n;
    else if ((activity_level&2) && poll_index_base > n && poll_index_base != 0)
        poll_index_base--;

    if (num_polling != 0)
      { if (activity_level)
          { if (activity_level&2) poll_index = poll_index_base; }
        else if (poll_index < MAX_POLL_INDEX)
            poll_index++;

        SET_POLL_TIMER();
      }
  }

void _pop_stop_polling(which)
register int which;
  { poll_handlers[which] = NULL;
    num_polling--;
  }

void _pop_set_poll_state(which, handler)
register int which;
register int (*handler)();
  { register int (*old)() = poll_handlers[which];
    poll_handlers[which] = handler;
    if (handler != NULL)
      { if (!(old == NULL && ++num_polling == 1)) CANCEL_POLL_TIMER();
        poll_index = poll_index_base;
        SET_POLL_TIMER();
      }
    else
      { if (old != NULL && --num_polling == 0) CANCEL_POLL_TIMER(); }
  }

static caddr_t poll_other()
  { register int (*p)();
    register int activity_level = DO_HANDLER(0) | DO_HANDLER(1);
    return(num_polling != 0 ? (caddr_t) poll_other : NULL);
  }

/*
 *  Turn normal polling off/back on during X toolkit waits
 */
caddr_t (*_pop_set_Xt_poll(handler))()
register int (*handler)();
{
    if (handler == NULL) {
        _pop_set_poll_state(XT_POLL_NUM, NULL);
        if (num_polling == 0) return(NULL);
        CANCEL_POLL_TIMER();
        return(poll_other);
    } else {
        if (num_polling != 0) {
            poll_index = poll_index_base;
            SET_POLL_TIMER();
        }
        _pop_set_poll_state(XT_POLL_NUM, handler);
        return (NULL);
    }
}

void _pop_retry_Xt_poll()
  { if (poll_handlers[XT_POLL_NUM] != NULL) return;
    if (num_polling != 0) CANCEL_POLL_TIMER();
    poll_handlers[XT_POLL_NUM] = _pop_Xt_poll_deferred;
    num_polling++;
    _pop_Xt_poll_deferred = NULL;
    async_poll();
  }
