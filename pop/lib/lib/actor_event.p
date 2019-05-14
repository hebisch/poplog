/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/lib/actor_event.p
 >  Purpose:        for use with LIB * ACTOR
 >  Author:         Jonathan Cunningham (see revisions)
 >  Documentation:  HELP * ACTOR
 >  Related Files:  LIB * ACTOR
 */
compile_mode :pop11 +strict;

uses actor;

weak vars procedure (queuelist, actor_receive, actor_send, actor_die,
                    actor_exists, actor_kill, actor_newactor, $-actor$-nameof);

weak vars actor_mymessagequeue, actor_myparent, $-actor$-current;


section $-actor =>  actor_eventhandler, actor_event, actor_waitevent,
                    actor_diewhen;

;;; For getting word identifiers in pattern lists
define lconstant macro WID;
    [ ("ident %readitem()% ") ].dl
enddefine;

define vars actor_eventhandler();
    lvars waiters = [];
    dlvars message, sender, eventlist, eventname;
    repeat
        actor_receive() -> message;
        if message = [=?sender waitsfor =??eventlist]
        then
            message :: waiters -> waiters
        elseif message = [event =?eventname =?message] then
            [% for waiters in waiters do
                 if waiters = [=?sender =* =** ^eventname =**]
                 then
                    erase(actor_send(sender,[event ^message]))
                 else waiters
                 endif
               endfor %] -> waiters
        endif
    endrepeat
enddefine;

define vars actor_event(eventname, message);
    lvars eventname, message;
    unless actor_send("eventhandler1",[event ^eventname ^message]) then
        mishap('No event handler',[[event ^eventname][message ^message]])
    endunless
enddefine;

define vars actor_waitevent(eventlist) -> message;
    lvars mlist, eventlist;
    dlvars message;
    if eventlist.isword then [^eventlist] -> eventlist endif;
    if eventlist == [] then actor_die() endif;
    unless actor_send("eventhandler1",[% nameof(current)% waitsfor ^^eventlist])
        then mishap('No event handler',[[eventlist ^eventlist]])
    endunless;
    [% until (actor_receive() ->> message) = [event =?message] do
        message
        enduntil %] -> mlist;
    mlist <> queuelist(actor_mymessagequeue) -> queuelist(actor_mymessagequeue)
enddefine;

define vars actor_diewhen(eventname);
    lvars eventname;
    actor_newactor(
        procedure (eventname);
            lvars eventname;
            actor_waitevent(eventname);
            if actor_exists(actor_myparent) then
                actor_kill(actor_myparent)
            endif;
        endprocedure, [^eventname])
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 22 1995
        Replaced use of mat*ches with = and new matchvars
--- John Gibson, Aug 24 1993
        Uses actor instead of new*_actor
--- John Gibson, Nov 11 1992
        Now lib actor_event with all exported identifiers prefixed with
        "actor_"

 */
