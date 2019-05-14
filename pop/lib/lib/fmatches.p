/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/fmatches.p
 > Purpose:         Full implementation of matches, with state saving
 >                  Also works with sections and dlvars
 > Author:          Aaron Sloman, Jun  4 1990 (see revisions)
 >              (based partly on work by Jon Cunningham and Tom Khabaza)
 > Documentation: HELP * FMATCHES
 > Related Files: SRC * SYSMATCH.P
 */
compile_mode: pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/pop11_flags.ph'
#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

/*
Note: fmatches as defined here is not an ordinary operator but a
syntactic operator. It can ONLY be followed by list expressions,
and it reads them in and replaces occurrences of variable names
after "?" or "??" with identifiers. This is what makes it work in
a section other than that in which it was compiled.

Also it works with lexical variables as they are treated like dynamic
variables. It is more efficient to use dlvars than lvars, as the latter
will always be of Type 3.

Note: fmatches cannot be traced. Trace fmatches_domatch or fmatches_submatch
instead. However, identifiers will show up in patterns with their current
values.

WARNING:
    All patterns must follow "fmatches" or "fmatching" explicitly.
    They cannot be passed as arguments to functions calling fmatches.
    Any extensions based on the matcher will have to define
    syntax words to read in patterns, using fmatches_listread

    Also patterns should NOT contain evaluated sub-expressions quoting
    "?" or "??", e.g.

        [== [?x b] == [?x h ^(consword("?" >< 'foo')] ==], x =>

    Finally note that patterns containing lexically scoped variables
    cannot be compiled as constants. (See LIB * COMPILE_MODE)
*/

;;; To see how states are saved for backtracking into the matcher look
;;; for occurrences of "contn". It may be possible to restore them to
;;; the free list.


section;

applist([fmatches with where fmatching], sysunprotect);

global vars popmatchvars = [];

define lvars whereclause();
    ;;; The default condition to be used with fmatches
    true
enddefine;

;;; Global variables for fmatches_submatch
lvars savecontn = [], p_restrict = [], contn = [];

;;; Some macros to simplify code (From Jon Cunningham's version,
;;; with extra test).

lconstant macro GETRESTRICT = [
    if ispair(pat) and fast_front(pat) == ":" and ispair(fast_back(pat)) then
        fast_destpair(fast_back(pat)) -> pat -> p_restrict
    else
        false -> p_restrict
    endif];


define lconstant macro RESTRICT;
    lvars varb, action;
    dest(listread()) -> action -> varb;
    dl([unless p_restrict then]);
    dl(action);
    dl([endunless;
        if restrict(^varb, p_restrict) ->> rtemp then
            if rtemp /== true then rtemp -> idval(^varb) endif;]);
    dl(action);
    dl([endif;])
enddefine;


define lconstant restrict(varb, proc);
    ;;; used to run restrictions.
    lvars varb, proc;
    idval(varb) -> varb;
    if proc.isident then
        idval(proc)(varb)
    elseif proc.isword then
        valof(proc)(varb)
    elseif proc.isinteger then
        length(varb) == proc
    elseif proc.isprocedure then
        proc(varb)
    else
        mishap(proc,1,'WORD, INTEGER, OR PROCEDURE SHOULD FOLLOW ":"')
    endif
enddefine;


define lconstant stripfront(sub, list) -> list;
    lvars sub, list;
    while ispair(sub) do
        unless ispair(list)
        and
            (fast_destpair(sub) -> sub) = (fast_destpair(list) -> list)
        then
            false -> list;
            return
        endunless
    endwhile
enddefine;


define lconstant striptail(sub, list);
    lvars sub, list;
    if sub == list then
        []
    elseif list.ispair then
        conspair( fast_front(list), striptail(sub, fast_back(list)))
    else
        list
    endif
enddefine;


define global vars fmatches_submatch(dat, pat);
    ;;; This does the main matching of datum against pattern
    ;;; It uses the procedure -whereclause- nonlocally to test each
    ;;; match

    lvars dat, pat, temp, varb, rtemp, item;

    dlocal popmatchvars, savecontn, p_restrict;

    repeat
        unless ispair(pat) then
            if dat = pat then
                if contn == [] then
                    return(whereclause())
                else
                    destpair(destpair(contn)) -> contn -> pat -> dat;
                    /* pat is a pair, so drop through */
                endif;
            else
                return(false)
            endif
        endunless;
patapair:
        fast_front(pat) -> item;
        if item == "=" and ispair(dat) then
            fast_back(dat) -> dat;
            fast_back(pat) -> pat;
            nextloop;
        elseif item == "?" and ispair(dat) and ispair(fast_back(pat))
;;;     Next bit not needed: should produce a compile-time error
;;;     and isident(fast_front(fast_back(pat)))
        then
            fast_destpair(dat) -> dat -> temp;
            fast_destpair(fast_back(pat)) -> pat -> varb;
            /* check that varb is a suitable pattern variable */
            GETRESTRICT;
            if fast_lmember(varb, popmatchvars) then
                if temp = idval(varb) then
                    RESTRICT[varb,nextloop];
                endif
            else
                temp -> idval(varb);
                [^varb ^^popmatchvars] -> popmatchvars;
                RESTRICT[varb, nextloop];
            endif
        elseif item == "==" then
            contn -> savecontn;
            if ispair(dat)
                and fmatches_submatch(fast_back(dat), pat)
                and whereclause()
            then
                return(true)
            else
                savecontn -> contn;
                fast_back(pat) -> pat;
                nextloop;
            endif
        elseif item == "??" and ispair(fast_back(pat))
;;;     and isident(fast_front(fast_back(pat)))
        then
            fast_destpair(fast_back(pat)) -> pat -> varb;
            /* check that varb is a suitable pattern variable */
            GETRESTRICT;
            if fast_lmember(varb, popmatchvars) then
                if stripfront(idval(varb), dat) ->> dat then
                    RESTRICT[dat, nextloop]
                else
                    return(false)
                endif
            else
                dat -> temp;
                [^varb ^^popmatchvars] -> popmatchvars;
                contn -> savecontn;
                repeat
                    striptail(temp,dat) -> idval(varb);
                    if fmatches_submatch(temp, pat) then
                        RESTRICT[varb, if whereclause() then return(true) endif]
                    endif;
                    savecontn -> contn;
                    unless ispair(temp) then return(false) endunless;
                    fast_back(temp) -> temp
                endrepeat
            endif
        elseif ispair(dat) then
            if ispair(item) then
                if ispair(fast_back(pat)) then
                    ;;; This is where states are saved A.S.
                    [% fast_back(dat), fast_back(pat) % ^^contn] -> contn;
spaghetti:
                    fast_front(dat) -> dat;
                    item -> pat;
                    goto patapair
                else
                    if fast_back(dat) /= fast_back(pat) then return(false) endif;
                    goto spaghetti
                endif
            else
                if fast_front(dat) /= item then return(false) endif;
                fast_back(dat) -> dat;
                fast_back(pat) -> pat;
                nextloop
            endif;
        endif;
        quitloop;
    endrepeat;
    return(false)
enddefine;


define global vars fmatches_domatch(datum, pattern);
    ;;; Defined so that it can either have two arguments, a list and a
    ;;; pattern, or three, a list, a pattern and a procedure. If used with
    ;;; the syntax operator "where" (defined below) the optional procedure
    ;;; can be conveniently provided by simply giving its body.

    lvars datum, pattern;
    dlocal popmatchvars, contn, whereclause;
    [] -> popmatchvars;
    [] -> contn;

    ;;; Check whether procedure argument supplied, and if so set up
    ;;; variables appropriately
    if isprocedure(pattern) then
        pattern -> whereclause;
        datum -> pattern;
        -> datum;
    else
        procedure; true; endprocedure -> whereclause
    endif;

    fmatches_submatch(datum, pattern);
enddefine;


sysunprotect("sysPUSHQ");

lvars procedure oldPUSHQ;

define lconstant readpattern();

    ;;; Read in a list or vector expression minus the closing bracket,
    ;;; replacing words following "?", "??", and ":" with the corresponding
    ;;; identifier, in nested lists, but not inside nested vectors.

    dlvars was_query = false;

    ;;; It would be nice to make patterns constants where possible,
    ;;; but it doesn't work if the pattern contains identifiers.
    ;;; dlocal pop_pop11_flags = pop_pop11_flags || POP11_CONSTRUCTOR_CONSTS;

    ;;; Discount lexical idents pushed in patterns
    dlocal pop_vm_flags = pop_vm_flags || VM_DISCOUNT_LEX_PROC_PUSHES;

    dlocal oldPUSHQ;
    if isundef(oldPUSHQ) then sysPUSHQ -> oldPUSHQ endif;

    define dlocal sysPUSHQ(item);
        lvars idprops item;
        if was_query then
            if isword(item) then
                identprops(item) -> idprops;
                if idprops == undef or isinteger(idprops) then
                    sysIDENT(item)
                else
                    mishap(item, 1, 'UNSUITABLE ITEM AFTER ' sys_>< was_query)
                endif
            elseif was_query == ":" and isinteger(item) then
                oldPUSHQ(item)
            else
                mishap(item, 1, 'NON-WORD AFTER ' sys_>< was_query)
            endif;
            false -> was_query
        else
            oldPUSHQ(item);
            if lmember(item, #_<[? ?? :]>_#)
            and not(lmember(nextreaditem(), #_<[% "]", "}", "%", "^" %]>_#))
            and not(iscaller(nonsyntax {)) then
                item
            else
                false
            endif ->  was_query
        endif
    enddefine;

    apply(nonsyntax [)
enddefine;

sysprotect("sysPUSHQ");


define global fmatches_listread();
    ;;; Check that a list expression follows on proglist
    ;;; Read in its items using readpattern to replace variable
    ;;; names with identifiers.
    lvars item;

    readitem() -> item;
    if item /== "[" then
        mishap(item, 1, 'LIST EXPECTED AFTER fmatches or fmatching...with')
    endif;

    ;;; Now read in the list, replacing pattern variables with
    ;;; identifiers
    readpattern()
enddefine;


global constant syntax where = pop_undef;


define global syntax 8 fmatches;

    ;;; plant call of fmatches_domatch, after reading in a list expression

    pop_expr_inst(pop_expr_item);
    pop11_FLUSHED -> pop_expr_inst;

    fmatches_listread();

    if pop11_try_nextreaditem("where") then
        sysPROCEDURE("where",0);
        ;;; read expression with precedence 11. (It's debatable whether it
        ;;; makes syntactic sense to grab operators of higher precedence
        ;;; than fmatches itself, but it needs to be left this way for backward
        ;;; compatibility. It means that and/or are included in the where
        ;;; expression.)
        pop11_comp_prec_expr(221,false) ->;
        sysPUSHQ(sysENDPROCEDURE())
    endif;

    "fmatches_domatch" -> pop_expr_item;
    sysCALL -> pop_expr_inst;
enddefine;


;;; A new looping construct:
;;;     fmatching <list> with <pattern> do <action> endfmatching

global constant syntax (with = pop_undef, endfmatching = pop_undef);

define global syntax fmatching;
    lvars item, label;
    ;;; read in the datum
    pop11_comp_expr_to("with") ->;

    ;;; Now read in the pattern, replacing pattern variables with
    ;;; identifiers. But first check that it is a list.
    fmatches_listread();

    ;;; check that "where" or "do" follows
    pop11_need_nextreaditem([do where]) -> item;

    sysPROCEDURE("fmatching",0);
    sysNEW_LABEL() -> label;    ;;; may be redundant

    if item == "where" then
        ;;; compile -where- clause
        pop11_comp_expr_to("do") ->;
        sysIFNOT(label);
    endif;

    ;;; now compile the procedure body after "do"
    pop11_comp_stmnt_seq_to("endfmatching") ->;
    sysLABEL(label);
    sysPUSHQ(false);    ;;; procedure always returns false, to force repeat
    sysPUSHQ(sysENDPROCEDURE());
    sysCALL("fmatches_domatch");
    sysERASE(0);
enddefine;

applist([fmatches with where fmatching], sysprotect);

#_INCLUDE '$usepop/pop/lib/include/ved_declare.ph'

[^^(weakref vedopeners) fmatching] -> weakref vedopeners;
[^^(weakref vedbackers) where with] -> weakref vedbackers;
[^^(weakref vedclosers) endfmatching] -> weakref vedclosers;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 29 1992
        Weakref'ed ved assignments
--- John Williams, Aug 26 1992
        Fixed BR isl-fr.4467 - restriction procedures in a pattern can
        also be lvars or section local vars.
--- John Gibson, Jul 13 1992
        Got rid of where as a separate syntax word and incorporated it into
        fmatches. (Stops an fmatches without a where clause trying to read
        following and operators, etc.)
--- John Williams, Jul 13 1992
        Fixed isl-fr.4455
--- Aaron Sloman, Jun 30 1991
    Declared "with" as a syntax word, and protected the words declared
    here. (This fixes a bug reported informally by Steve Knight, i.e.
    redefining "with" can stop it working here.)

--- Aaron Sloman, Nov  3 1990
    Made fmatches_listread available to users, also fmatches_domatch
    and fmatches_submatch

--- Aaron Sloman, Nov  3 1990
    Changed to use sysCALL rather than sysCALLQ so that tracing can be
    turned on and off after compilation.

    Changed fmatching to allow a where clause.

    Wrote the HELP file

--- Aaron Sloman, Oct  7 1990
    Fixed bug reported by Jason Handby - inserted two more tests.
--- Aaron Sloman, Jun  9 1990
    Changed to use nonsyntax [, with locally redefined sysPUSHQ
    Improved error detection
 */
