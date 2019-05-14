/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/lib/actor.p
 >  Purpose:        actors as processes
 >  Author:         Jonathan Cunningham, (see revisions)
 >  Documentation:  HELP *ACTOR
 */
compile_mode :pop11 +strict;

uses newqueue;

section $-actor =>
            actor_apocalypse, actor_askactor, actor_answer, actor_aeons
            actor_die, actor_exists, actor_genesis, actor_gpo, actor_kill,
            actor_mymessagequeue, actor_myname, actor_myparent, actor_newactor,
            actor_receive, actor_say, actor_saytime, actor_send, actor_simtime,
            actor_sleep, actor_wait, actor_wake;

vars
    actor_aeons     = 999999,
    actor_saytime   = true,
    actor_myname,
    actor_mymessagequeue,
    actor_myparent,
    actor_simtime,

    current,
    waiting,
    sleeping,
;

defclass vars actor {
    nextactor,
    waketime,
    nameof,
    processof,
    parentof,
    messagequeue
};

define practor(actor);
    lvars actor;
    pr('(');
    pr(nameof(actor));
    pr(' @');
    pr(waketime(actor));
    pr(')');
    unless nextactor(actor) == [] then practor(nextactor(actor)) endunless
enddefine;
;;;
practor -> class_print(actor_key);

lvars p, q;

define vars actor_apocalypse();
    '\nThere are no more events\n'.pr;
enddefine;

define vars schedule(time, actor);
    lvars time, actor;
    dlocal p, q;
    time -> waketime(actor);
    if waketime(waiting) > time then
        waiting -> nextactor(actor);
        actor -> waiting
    else
        waiting -> q;
        until q == [] or waketime(q) > time do
            q ->> p, nextactor() -> q
        enduntil;
        q -> nextactor(actor);
        actor -> nextactor(p)
    endif;
enddefine;

define vars startactor(abnormal);
    lvars abnormal;
    dlocal actor_myname, actor_mymessagequeue, actor_myparent;
    nameof(current) -> actor_myname;
    messagequeue(current) -> actor_mymessagequeue;
    parentof(current) -> actor_myparent;
    if abnormal then
        mishap('Asking an actor a question before it has been born',
                [% current %])
    endif;
    apply()
enddefine;

;;; calls to actor_newactor have a variety of formats, see help actor
define vars actor_newactor(item);
    lvars item, proc, procname, time, arglist;
    if item.isinteger then
        actor_simtime + item -> time;
            -> item
    else actor_simtime -> time
    endif;
    if item.islist then item -> arglist; -> item else [] -> arglist endif;
    if item.isprocedure then
        item -> proc;
        gensym(pdprops(proc)) -> procname
    else
        item -> procname -> proc
    endif;
    schedule(time,
        consactor([],0,procname,
            consproc(dl(arglist),proc,length(arglist)+1,startactor),
            if current then nameof(current) else "eldest" endif,
            newqueue([])))
enddefine;

define vars nextprocess();
    waiting ->> current, nextactor() -> waiting;
    if deadproc(processof(current)) then
        mishap('Attempting to reincarnate zombie actor',[^current])
    endif;
    waketime(current) -> actor_simtime;
enddefine;

define vars actor_genesis(firstborn);
    lvars firstborn, actor, n;
    dlocal actor_simtime, current, waiting, sleeping;
    0 -> actor_simtime;
    false -> current;
    consactor([],actor_aeons,"apocalypse",
                consproc(actor_apocalypse,1,startactor),[],newqueue([]))
        -> waiting;
    [] -> sleeping;
    1 -> n;
    for actor in firstborn do
        if actor.isinteger then actor -> n
        elseif actor.isword then
            1 -> gensym(actor);
            repeat n times actor_newactor(valof(actor)) endrepeat;
            1 -> n
        elseif actor.islist then
            actor_newactor(dl(actor))
        else mishap('Format error in GENESIS list',firstborn)
        endif
    endfor;
    waiting ->> current, nextactor() -> waiting;
    repeat forever
        erase([% runproc(false,1,processof(current)) %]);
        if nameof(current) == "apocalypse" then quitloop endif;
        nextprocess()
    endrepeat
enddefine;

define vars actor_answer(question);
    dlvars question;
    if question = [popval =??question] then
        pop11_compile(question)
    else
        'Dont know'
    endif
enddefine;

define vars returnanswer();
    suspend(actor_answer(),1)
enddefine;

define vars actor_die();
    nextprocess();
    resume(false,1,processof(current));
    while do returnanswer() endwhile
enddefine;

define vars actor_wait(duration);
    lvars duration;
    schedule(actor_simtime+duration,current);
    actor_die()
enddefine;

define vars actor_sleep();
    sleeping -> nextactor(current);
    current -> sleeping;
    actor_die()
enddefine;

constant dummyactor = writeable consactor([],[],[],[],[],[]);

define vars find(name,actor);
    lvars name, actor;
    actor ->> nextactor(dummyactor) -> q;
    dummyactor -> p;
    until q == [] do
        if nameof(q) == name then return(true) endif;
        q ->> p, nextactor() -> q
    enduntil;
    return(false)
enddefine;

define vars actor_wake(actorname);
    lvars actorname;
    dlocal p, q;
    if find(actorname,sleeping) then
        nextactor(q) -> nextactor(p);
        nextactor(dummyactor) -> sleeping;
        schedule(actor_simtime,q)
    endif;
enddefine;

define vars actor_kill(actorname);
    lvars actorname;
    dlocal p, q;
    if find(actorname,waiting) then
        nextactor(q) -> nextactor(p);
        nextactor(dummyactor) -> waiting
    elseif find(actorname,sleeping) then
        nextactor(q) -> nextactor(p);
        nextactor(dummyactor) -> sleeping
    else mishap('Attempt to kill non-existent actor',[^actorname])
    endif;
enddefine;

define vars actor_exists(name);
    lvars name;
    dlocal p, q;
    if find(name,waiting) or find(name,sleeping) then q else false endif
enddefine;

define vars actor_say(something);
    lvars something;
    syspr(actor_myname);
    if actor_saytime then syspr(' @'); syspr(actor_simtime) endif;
    syspr(' says: ');
    pr(something);
    nl(1)
enddefine;

define vars actor_receive() -> message;
    lvars message;
    while (messagequeue(current)() ->> message) == termin do
        actor_sleep()
    endwhile
enddefine;

define vars actor_send(name, message) -> gotthere;
    lvars name, message, gotthere;
    dlocal p, q;
    if find(name,sleeping) then
        true -> gotthere;
        message -> messagequeue(q)();
        nextactor(q) -> nextactor(p);
        schedule(actor_simtime,q);
        nextactor(dummyactor) -> sleeping
    elseif find(name,waiting) then
        true -> gotthere;
        message -> messagequeue(q)()
    else false -> gotthere
    endif
enddefine;

define vars actor_gpo(name,message,delay);
    lvars name, message, delay;
    actor_newactor(actor_send,[%name,message%],delay)
enddefine;

define vars actor_askactor(name, question) -> ans;
    lvars name, question, ans;
    dlocal current;
    unless (actor_exists(name) ->> current) then
        mishap('Asking non-existent actor',[^name ^question])
    endunless;
    runproc(question,true,2,processof(current)) -> ans
enddefine;

constant $-actor = true;        ;;; for uses

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 22 1995
        Replaced use of mat*ches with = and new matchvars
--- John Gibson, Aug 24 1993
        Moved to lib actor
--- John Gibson, Nov 11 1992
        Now lib new*_actor with all exported identifiers prefixed with
        "actor_"
 */
