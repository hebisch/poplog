/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.unix/lib/auto/pipeout.p
 >  Purpose:        passing characters out to a unix command
 >  Author:         Mark Rubinstein, Aug 21 1985 (see revisions)
 >  Documentation:  HELP * PIPEOUT
 >  Related Files:  LIB * PIPEIN
 */
compile_mode :pop11 +strict;

section;

include ved_declare.ph;

lconstant macro VWEAK = [weakref[vedprocess]];

define pipeout(src, command, args, wait);
    lvars src, command, args, wait, child;
    dlocal VWEAK vedediting, VWEAK vedbufferlist, popexit;

    define lconstant do_pipeout(command, args, src);
        lvars command, args, child, dout, din, nulldev, src, outchars;
        dlocal poprawdevin, poprawdevout;

        syspipe(false) -> din -> dout;
        unless isref(src) then
            discout(dout) -> outchars;
            unless isprocedure(src) then
                discin(src) -> src
            endunless;
        endunless;

        sysopen('/dev/null', 2, false) -> nulldev;

        if sys_vfork(true) ->> child then
            /* this is still parent */
            sysclose(din);
            /* send the data to the exec'ed process */
            if isref(src) then
                ;;; cont(src) is a procedure which takes an output device
                ;;; and writes the data to it
                cont(src)(dout);
                sysclose(dout)
            else
                until outchars(dup(src())) == termin do /* nothing */ enduntil;
            endif;
            sys_wait(child) -> (,); ;;; wait for the child
            sysclose(nulldev);

        else
            /* child - the exec process */
            sysclose(dout);
            din -> popdevin;
            ;;; prevent attempts to read from or write to terminal
            nulldev ->> poprawdevin -> poprawdevout;
            ;;; Should never return from this:
            sysexecute(command, args, false);
            ;;; but just in case we do ...
            fast_sysexit();
        endif;
    enddefine;

    if testdef vedprocess and isdevice(poprawdevout) then
        0 -> VWEAK vedscreencharmode;   ;;; in case there are error messages
        VWEAK vedscr_flush_output();
    endif;

    /* is user willing to wait? */
    if wait then
        /* transfer the stuff to the exec'ed child from top level process */
        do_pipeout(command, args, src);
    else
        ;;; busy user can't wait, so detach a process
        ;;; these values are restored by dlocal
        identfn -> popexit;
        [] -> VWEAK vedbufferlist;
        false -> VWEAK vedediting;
        if sys_vfork(true) ->> child then
            /* top level parent - a quick wait then we're off */
            sys_wait(child) -> (,)
        else
            /* vforked 1st child (to prevent zombie) */
            if sys_fork(false) then
                /* needed a real fork because processes will be running along
                 * side one another.  This is still the 1st child.
                 */
                fast_sysexit();
            endif;
            /* we're in the fully detached process */
            do_pipeout(command, args, src);
            /* exit from the detached parent process */
            fast_sysexit();
        endif;
    endif;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 29 1995
        Guarded vedscr_flush_output() with test for poprawdevout being
        a device
--- John Gibson, Apr 21 1994
        Changed to use new sys_vfork etc
--- John Gibson, Oct  8 1992
        Made all VED references weak
--- John Gibson, Mar  6 1992
        Changed so that the -src- argument can be a ref containing
        a procedure which will write data to the output pipe.
--- Simon Nichols, Jun 12 1991
        Declared -src- as lvars in -global pipeout- (see bugreport
        davidy.43).
--- Aaron Sloman, Jul 28 1990 - assigned null dev to poprawdevout rather
        that popdevout. Also localised it.
--- James Goodlet, Jul 24 1989 - virtually rewrote the function to use vforks
        properly, cribbing from ved_send.
--- James Goodlet, Jun  6 1989 - reinstated use of sys*vfork
--- John Williams, Apr 10 1989 - Now uses -fast_sysexit-
--- John Gibson, Nov 11 1987 - Replaced -popdevraw- with -poprawdevin- and
        -poprawdevout-
--- Aaron Sloman, Dec 12 1986 - Changed not to use SYSVFORK - a source of too
        many problems
 */
