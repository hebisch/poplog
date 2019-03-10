/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sysexecute.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; ---------------------- UNIX EXEC -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (sys_encode_string)
    ;

vars
        popenvlist
    ;

;;; ----------------------------------------------------------------------

section $-Sys => sysexecute;

define sysexecute(execfile, arglist, envlist);
    lvars   s, arglist, string, envlist, execfile, devlist = [], dev,
            rs, _ptr, _argp, _envp, _alen, _exec;
    lstackmem stackbuff _nbuf;

    ;;; shuffle args if given list of devices to remain open
    if ispair(envlist) and isdevice(hd(envlist)) then
        ((), execfile, arglist, envlist)
                -> (execfile, arglist, envlist, devlist)
    endif;

    ;;; add standard files to list of devices to keep open
    if testdef popdevin then
        weakref popdevin :: devlist -> devlist
    endif;
    if testdef popdevout then
        weakref popdevout :: devlist -> devlist
    endif;
    if testdef popdeverr then
        weakref popdeverr :: devlist -> devlist
    endif;

    if isref(execfile) then
        ;;; use execvp
        fast_cont(execfile) -> execfile;
        _extern execvp
    else
        ;;; use execve
        _extern execve
    endif -> _exec;

    sysfileok(execfile) -> execfile;
    unless envlist then popenvlist -> envlist endunless;

    _int(#| applist(arglist, sys_encode_string), _NULL,
            applist(envlist, sys_encode_string), _NULL
        |#) -> _alen;

    ;;; Get a rawstruct to hold args/env values (+ 1 word for possible
    ;;; pointer alignment below)
    Init_rawstruct(##(w)[_alen | (b) ] _add _1) -> rs;

    ;;; unset close-on-exec flag for given devices + standard ones
    ;;; (doesn't matter if any are closed already)
    for dev in devlist do
        _extern[NI] fcntl(dev!D_FILE_DESC, _F_SETFD, _0) ->
    endfor;

    rs@V_WORDS@(w.r -> (b)) -> _argp;   ;;; ensure correctly aligned
    _argp@((b))[_alen] -> _ptr;
    while _ptr >@((b)) _argp do
        if dup() == _NULL then _ptr -> _envp endif;
        () -> _ptr--!((b)) -> _ptr
    endwhile;

    ;;; clear all timers (which would otherwise get inherited)
    _extern pop_timer(_0, _0, _NULL, _NULL) -> ;    ;;; real
    _extern pop_timer(_1, _0, _NULL, _NULL) -> ;    ;;; virtual

    _extern[INDIR,NI] _exec(Encode_sys(execfile,_nbuf), _argp, _envp) -> ;

    ;;; it returned - an error
    Syserr_mishap(execfile, 1, 'FAILED TO EXECUTE FILE')
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 11 1997
        Now uses sys_encode_string to encode arglist and envlist (and
        Encode_sys for filename).
--- John Gibson, Jun 20 1996
        Made it cancel all timers before doing the exec.
--- John Gibson, Apr 29 1996
        Allowed execfile to be given in a ref to say use execvp instead
        of execve.
--- John Gibson, Mar 16 1995
        Rewritten to use a rawstruct to pass the args/envvars (so it works
        correctly in OSF1, where type (b) is a double)
--- John Gibson, Sep 14 1991
        Removed N*ull_end, and removed (now unnecessary) processing of
        arg and env strings
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 17 1988
        Moved out of sysutil.p
 */
