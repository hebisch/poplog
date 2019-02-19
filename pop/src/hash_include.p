/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/hash_include.p
 > Purpose:         #_INCLUDE macro
 > Author:          John Gibson, Mar  8 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */


#_INCLUDE 'declare.ph'

global constant
        procedure (isincharitem)
    ;

global vars
        procedure (cucharin),
        pop_default_type, Sys$-Prglst$- #_if_stack, Sys$-Prglst$-was_nonmac
    ;

;;; ---------------------------------------------------------------------

section $-Sys$-Prglst =>
                        popfilename, poplinenum, pop_#_include_stack,
                        proglist_state, proglist_new_state,
                        #_INCLUDE
                    ;

protected vars
    pop_#_include_stack = false,    ;;; stack of files (false if empty)
    ;

vars
    popfilename         = false,    ;;; current file being read
    poplinenum          = 0,        ;;; line number in current file
    ;


lconstant macro STATE_EXPR = [
    (proglist, cucharin, popfilename, poplinenum, was_nonmac,
                    weakref #_if_stack, pop_#_include_stack)
    ];

define active:7 proglist_state;
    STATE_EXPR
enddefine;
;;;
define updaterof active proglist_state with_nargs 7;
    () -> STATE_EXPR
enddefine;

define proglist_new_state(stream);
    lvars props, stream;
    if islist(stream) then
        (stream, #_<identfn(%termin%)>_#, popfilename, poplinenum)
    else
        unless isprocedure(stream) then discin(stream) -> stream endunless;
        stream!PD_PROPS -> props;
        (   pdtolist(incharitem(stream)),
            stream,
            if isstring(props) then
                props, 1
            elseif ispair(props) then
                fast_destpair(props)
            else
                false, 0
            endif
        )
    endif,
    (false, [], false)
enddefine;

define lconstant Trans_chartypes(from_rep);
    lvars from_rep, to_rep;
    if from_rep and (isincharitem(readitem) ->> to_rep) then
        fast_frozval(2, from_rep) -> frozval(2, to_rep);
        fast_frozval(3, from_rep) -> frozval(3, to_rep)
    endif
enddefine;

define macro #_INCLUDE filename;
    lvars filename, old_rep = isincharitem(readitem);
    dlocal pop_default_type = pop_default_type sys_>< 'h';
    {% proglist_state %}, proglist_new_state(filename)
                        -> (pop_#_include_stack, proglist_state);
    Trans_chartypes(old_rep)
enddefine;

    ;;; Pop the include stack if not empty -- called when proglist is null
define Include_pop();
    lvars old_rep;
    if isvector(pop_#_include_stack) then
        ;;; unstack stacked proglist, etc
        isincharitem(readitem) -> old_rep;
        explode(pop_#_include_stack) -> proglist_state;
        Trans_chartypes(old_rep);
        true
    else
        false
    endif
enddefine;


endsection;     /* $-Sys$-Prglst */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1991
        Added -proglist_state- and -proglist_new_state-.
--- John Gibson, Jun 24 1989
        Made #_INCLUDE keep item_chartypes same as original proglist,
        and transfer back any changes.
--- John Williams, Jun  6 1989
        Changed >< to sys_><
--- John Gibson, Jun  5 1989
        Exported #_INCLUDE stack as -pop_#_include_stack-.
--- John Williams, Sep 10 1988
        -Setup- now interprets a character repeater with a pair in the
        -pdprops- as specifying -popfilename- and -poplinenum-
 */
