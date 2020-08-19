/*  --- Copyright University of Sussex 2009.  All rights reserved. ---------
 >  File:           C.all/lib/lib/super.p
 >  Purpose:        prolog like extenstion to the POP-11 database
 >  Author:         Steve Hardy (see revisions)
 >  Documentation:  HELP * SUPER, HELP * WHICH
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

section $-database =>
    add alladd present allpresent remove allremove
    spy nospy newdatabase fetch trynext
    foreach endforeach forevery endforevery ;

section super =>
    add alladd present allpresent remove allremove
    spy nospy newdatabase fetch trynext
    foreach endforeach forevery endforevery ;


vars argument;      ;;; The argument for the current operand


vars c;             ;;; A temporary variable
vars continuation;  ;;; A list of routines to be performed after current action
vars convertqi;     ;;; To convert a structure from external form to internal


vars deref;         ;;; To remove REFs from a structure
vars procedure do_and;        ;;; Interpreter routine to do 'and'
vars procedure do_convert;    ;;; To convert and AND a list
vars procedure do_cut;        ;;; Interpreter routine to do 'cut'
vars procedure do_done;       ;;; Interpreter routine to signal end of a goal
vars procedure do_find;       ;;; Interpreter routine to satisfy a goal
vars procedure do_quit;       ;;; Interpreter routine to cause the interepreter to quit
vars procedure do_tell_done;  ;;; For use in SPYing
vars procedure do_tell_failed; ;;; For use in SPYing
vars procedure do_tell_retry; ;;; For use in SPYing
vars procedure do_search;     ;;; Interpreter routine to find database entry
vars goal_number;   ;;; An identification number for use by SPY
vars k1;            ;;; A temporary variable
vars k2;            ;;; A temporary variable
vars procedure note_and;      ;;; To add a list of goal to continutaion
vars procedure note_or;       ;;; To save a list of goals as back-track options
vars operand;       ;;; The current interpreter routine
vars r;             ;;; A temporary variable
vars restore;       ;;; To restore the state of the interpreter
vars run;           ;;; The interpreter
vars save;          ;;; To save the state of the interpreter
vars size;          ;;; The stacklength when interpreter starts
vars spylist;       ;;; List of 'spied' procedures
vars store;         ;;; For variable bindings during CONVERTXI
vars t;             ;;; A temporary variable
vars tx;            ;;; A temporary variable
vars ty;            ;;; A temporary variable
vars unified;       ;;; Set false when unification fails
vars unifyii;       ;;; To unify two internal structures
vars unifyxi;       ;;; To unify a external  structure and an internal structure
vars x;             ;;; A temporary variable
vars y;             ;;; A temporary variable
vars z;             ;;; A temporary variable

vars macro newref; [%consref(%false%)%] -> nonmac newref;

unless islist(spylist) then [] -> spylist endunless;

define procedure spyread() -> r;
    vars x, popnewline;
    true -> popnewline;
    [] -> r;
    unless poplastchar == `\n` then
        readitem() -> x;
        until x == termin or x == newline or x == ";" do
            conspair(x, r) -> r;
            readitem() -> x
        enduntil
    endunless
enddefine;

define global macro spy;
    vars x;
    spyread() -> x;
    if x == [] then
        maplist(database, front) -> spylist
    else
        x <> spylist -> spylist
    endif
enddefine;

define global macro nospy;
    vars x;
    spyread() -> x;
    if x == [] then
        [] -> spylist
    else
        until x == [] do delete(destpair(x) -> x, spylist) -> spylist enduntil;
    endif
enddefine;

define procedure deref(x);
    if ispair(x) then
        conspair(deref(fast_front(x)), deref(fast_back(x)))
    elseif isref(x) then
        fast_deref(x) -> x;
        if isref(x) then x else deref(x) endif
    else
        x
    endif;
enddefine;

define global macro save; enddefine;

define global procedure restore();
    while isref(->>continuation) do
        false -> fast_cont(continuation)
    endwhile
        -> argument -> operand
enddefine;


define constant known();
    store -> k2 -> k1;
    while ispair(k2) do
        if k1 == (fast_destpair(k2) -> k2) then
            return(fast_front(k2))
        endif;
        fast_back(k2) -> k2
    endwhile;
    false
enddefine;

define constant convertqi(c);
    vars x, y, tx, ty;
    unless ispair(c) then
        if c == "=" then return(newref()) endif;
        unless isref(c) then return(c) endunless;
        unless known(c) ->> x then
            conspair(c, conspair(newref() ->> x, store)) -> store;
        endunless;
        return(x);
    endunless;
    if (fast_destpair(c) -> y ->> x) == "?" then
        unless known(destpair(y) -> y ->> x) ->> tx then
            conspair(x, conspair(consref(x) ->> tx, store)) -> store;
        endunless;
        return(conspair(tx, convertqi(y)))
    endif;
    if x == "??" then
        if known(front(y) ->> x) ->> tx then return(tx) endif;
        conspair(x, conspair(consref(x) ->> x, store)) -> store;
        return(x);
    endif;
    if x == "==" then return(newref()) endif;
    if (convertqi(x) ->> tx) == x then
        if (convertqi(y) ->> ty) == y then return(c) endif;
        return(conspair(tx, ty))
    endif;
    conspair(tx, convertqi(y))
enddefine;

define constant convertxi(c);
    vars t;
    if ispair(c) then
        return(conspair(convertxi(fast_front(c)), convertxi(fast_back(c))))
    endif;
    unless isref(c) then return(c) endunless;
    if known(c) ->> t then return(t) endif;
    newref() -> t;
    conspair(c, conspair(t, store)) -> store;
    return(t)
enddefine;

define constant unifyii(x, y);
    vars t;
unifyii:
    fast_deref(x) -> x;
    fast_deref(y) -> y;
    if x == y then return endif;
    if isref(x) then y -> fast_cont(x); save(x); return endif;
    if isref(y) then x -> fast_cont(y); save(y); return endif;
    if ispair(x) and ispair(y) then
        unifyii(fast_destpair(x) -> x, fast_destpair(y) -> y);
        if unified then goto unifyii endif;
        return;
    endif;
    false -> unified
enddefine;

define constant unifyxi(x, y);
    vars t;
unifyxi:
    fast_deref(y) -> y;
    if isref(y) then convertxi(x) -> fast_cont(y); save(y); return endif;
    if ispair(x) then
        unless ispair(y) then false -> unified; return endunless;
        unifyxi(fast_destpair(x) -> x, fast_destpair(y) -> y);
        if unified then goto unifyxi endif;
        return;
    endif;
    unless isref(x) then x == y -> unified; return endunless;
    if known(x) ->> t then unifyii(t, y); return endif;
    conspair(x, conspair(y, store)) -> store
enddefine;

define constant note_and(x);
    if ispair(x) then
        note_and(fast_back(x));
        conspair(do_find, conspair(fast_front(x), continuation)) -> continuation
    endif
enddefine;

define constant note_or(x);
    if ispair(x) then
        note_or(fast_back(x));
        save(do_find, fast_front(x), continuation);
    endif
enddefine;

define constant do_tell_done();
    destpair(argument) -> x -> y;
    deref([DONE ^y ^x]) ==>
    save(do_tell_retry, y, []);
    do_done();
enddefine;

define constant do_tell_retry();
    [RETRY ^argument] ==>
    restore();
enddefine;

define constant do_tell_failed();
    [FAILED ^argument] ==>
    restore();
enddefine;

define constant do_and();
    if ispair(argument) then
        fast_destpair(argument) -> x -> argument;
        note_and(x);
        do_find();
        return
    endif;
    fast_destpair(fast_destpair(continuation))
        -> continuation -> argument -> operand
enddefine;

define constant do_quit();
    exitfrom(consvector(stacklength() fi_- size),
                argument,
                run);
enddefine;

define constant do_or();
    unless ispair(argument) then restore(); return endunless;
    fast_destpair(argument) -> x -> argument;
    note_or(x);
    do_find();
enddefine;

define constant do_find();
    if argument = "fail" then restore(); return endif;
    if argument = "cut" then
        continuation -> argument;
        until (fast_destpair(argument) -> argument) == do_done do
            fast_back(argument) -> argument;
        enduntil;
        save(do_cut, fast_front(argument), nil);
        fast_destpair(fast_destpair(continuation))
            -> continuation -> argument -> operand;
        return
    endif;
    if ispair(argument) then
        fast_destpair(argument) -> x -> y;
        if y == "and" then x -> argument; do_and(); return endif;
        if y == "or" then x -> argument; do_or(); return endif;
    endif;
    conspair(stacklength() fi_- size, continuation) -> continuation;
    conspair(do_done, continuation) -> continuation;
    argument ->> operand -> x;
    fast_deref(x) -> x;
    destpair(x) -> y -> x;
    fast_deref(x) -> x;
    unless atom(x) then restore(); return endunless;
    if x == "set" then
        destpair(deref(y)) -> y -> x;
        valof(front(y))(dl(fast_back(y))) -> y;
        true -> unified;
        unifyii(x, y);
        if unified then do_done() else restore() endif;
        return;
    endif;
    if x == "do" then
        deref(y) -> y;
        [%valof(front(y))(dl(back(y)))%] -> y;
        if y == [] or front(y) then do_done() else restore() endif;
        return
    endif;
    if lmember(x, spylist) then
        goal_number + 1 -> goal_number;
        deref([GOAL ^goal_number ^operand]) ==>
        save(do_tell_failed, goal_number, []);
        conspair(do_tell_done, conspair(conspair(goal_number, operand), continuation))
            -> continuation;
    endif;
    database -> argument;
do_find_loop:
    unless ispair(argument) then restore(); return; endunless;
    fast_destpair(argument) -> argument -> y;
    unless fast_front(y) == x then goto do_find_loop endunless;
    fast_back(y) -> argument;
    do_search();
enddefine;

define constant do_done();
    fast_destpair(fast_destpair(continuation))
            -> continuation -> argument -> operand;
enddefine;

define constant do_cut();
    argument -> x;
    x fi_+ size -> x;
    until stacklength() == x do restore() enduntil;
enddefine;

define constant do_search();
do_search:
    unless ispair(argument) then restore(); return endunless;
    fast_destpair(argument) -> argument -> x;
    [] -> store;
    unless argument == [] do save(operand, argument, continuation) endunless;
    if isprocedure(x) then
        operand -> argument;
        x();
        return
    endif;
    if ispair(x) and (fast_destpair(x) -> y) == "ifneeded" then
        true -> unified;
        unifyxi(back(destpair(y) -> y), back(operand));
        if unified then
            unless ispair(y) then do_done(); return endunless;
            convertxi(fast_destpair(y) -> y) -> argument;
            unless atom(y) do
                conspair(do_convert,
                    conspair(conspair(store, y), continuation))
                    -> continuation
            endunless;
            do_find();
            return;
        endif;
        restore();
        unless isprocedure(operand) then goto do_search endunless;
        operand();
        return;
    endif;
    true -> unified;
    unifyxi(back(x), back(operand));
    if unified then do_done(); return endif;
    restore();
    unless isprocedure(operand) then goto do_search endunless;
    operand()
enddefine;

define constant do_convert();
    fast_destpair(fast_destpair(argument)) -> x -> y -> store;
    convertxi(y) -> argument;
    unless atom(x) do
        conspair(do_convert, conspair(conspair(store, x), continuation))
            -> continuation
    endunless;
    do_find()
enddefine;

define procedure run(stack);
    vars store, size, operand, argument, continuation, x, y, z;
    stacklength() -> size;
    explode(stack);
    restore();
    repeat forever
        if isprocedure(operand) then operand() else do_search() endif
    endrepeat
enddefine;

define global procedure fetch(x);
    vars store, y;
    [] -> store;
    convertqi(x) -> x;
    store -> y;
    until y == [] do
        false -> cont(fast_destpair(fast_back(y)) -> y)
    enduntil;
    {%x, store,
         {%do_quit, false, [],
              do_find, x, [%do_done, 0, do_quit, true%]
         %},
        0
    %}
enddefine;

define global procedure trynext(state) -> result;
    vars x, y, store, goal_number;
    state(4) -> goal_number;
    run(state(3)) -> result -> state(3);
    deref(state(1)) -> it;
    if front(it) = "and" then fast_back(it) -> them endif;
    goal_number -> state(4);
    state(2) -> store;
    until store == [] do
        destpair(destpair(store)) -> store -> y -> x;
        if isword(x) then deref(y) -> valof(x) endif;
    enduntil;
enddefine;

define global procedure present(x);
    trynext(fetch(x))
enddefine;

define global procedure allpresent(x);
    trynext(fetch(conspair("and", x)))
enddefine;

define constant finddata(f) -> h -> y -> d;
    vars x, z;
    front(f) -> h;
    if h == "ifneeded" then front(front(back(f))) -> h endif;
    database -> x;
    [] -> z;
    until x == [] do
        destpair(x) -> x -> y;
        if front(y) == h then
            until z == [] do conspair((destpair(z) -> z), x) -> x enduntil;
            back(y) -> y;
            x -> d;
            return
        endif;
        conspair(y, z) -> z
    enduntil;
    [] -> y;
    database -> d;
enddefine;

define global procedure add(f);
    vars d, h, x, store;
    finddata(f) -> h -> x -> d;
    [] -> store;
    convertqi(f) -> f;
    (h :: rev(f :: rev(x))) :: d -> database;
enddefine;

define global procedure remove(f);
    vars d, h, x, y, store;
    finddata(f) -> h -> x -> d;
    [] -> store;
    convertqi(f) -> f;
    [] -> y;
    until x == [] do
        if f = front(x) then
            back(x) -> x;
            until y == [] do conspair((fast_destpair(y) -> y), x) -> x enduntil;
            if x == [] then d else h :: x :: d endif -> database;
            return;
        else
            conspair((destpair(x) -> x), y) -> y
        endif
    enduntil;
    mishap('REMOVE FAILURE', [%f%])
enddefine;

define global procedure newdatabase(x);
    [] -> database;
    alladd(x);
enddefine;

;;;  FOREACH pattern DO actions ENDFOREACH
;;;
;;;      VARS %X;
;;;      FETCH(pattern) -> %X;
;;;      WHILE TRYNEXT(%X) DO actions ENDWHILE
;;;
global vars syntax endforeach;

define global syntax foreach;
    vars x y z;
    sysNEW_LVAR() -> x;
    sysNEW_LABEL() -> y;
    pop11_loop_start(y);
    sysNEW_LABEL() -> z;
    pop11_loop_end(z);
    erase(pop11_comp_expr_to([do then]));
    sysCALL("fetch");
    sysPOP(x);
    sysLABEL(y);
    sysPUSH(x);
    sysCALL("trynext");
    sysIFNOT(z);
    erase(pop11_comp_stmnt_seq_to([endforeach {close}]));
    sysGOTO(y);
    sysLABEL(z);
enddefine;

;;;  FOREVERY patterns DO actions ENDFOREVERY
;;;
;;;      VARS %X;
;;;      FETCH("AND" :: patterns) -> %X;
;;;      WHILE TRYNEXT(%X) DO actions ENDWHILE
;;;
global vars syntax endforevery;

define global syntax forevery;
    vars x y z;
    sysNEW_LVAR() -> x;
    sysNEW_LABEL() -> y;
    pop11_loop_start(y);
    sysNEW_LABEL() -> z;
    pop11_loop_end(z);
    sysPUSHQ("and");
    erase(pop11_comp_expr_to([do then]));
    sysCALL("conspair");
    sysCALL("fetch");
    sysPOP(x);
    sysLABEL(y);
    sysPUSH(x);
    sysCALL("trynext");
    sysIFNOT(z);
    erase(pop11_comp_stmnt_seq_to([endforevery {close}]));
    sysGOTO(y);
    sysLABEL(z);
enddefine;


/* The true second argument to section_cancel
    can be removed for de-bugging purposes
    (it assigns false to the pdprops of all section-local procedures)
*/
section_cancel(current_section, true);  ;;; true means zap pdprops of pdrs

endsection;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 13 1989
        Replaced old sys- compiler procedures with pop11_ ones.
 */
