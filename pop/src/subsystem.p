/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/subsystem.p
 > Purpose:         Facilities for having several subsystems accessible
 > Author:          John Williams, June 1990 (see revisions)
 >                      (based on Mellish, Cunningham, Laventhol, 1983)
 > Documentation:   HELP * SUBSYSTEMS, REF * SUBSYSTEM
 */

;;; ------------------- SUBSYSTEM CORE PROCEDURES ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/subsystem.ph'


;;; -----------------------------------------------------------------------

section $-Sys => subsystem, subsystem_changed_warn, subscr_subsystem, app_subsystems;

vars
    subsystem   = false,
    procedure subsystem_changed_warn = erase,
    ;


define Get_ss_procedure(ss, pfield);
    lvars ss, v = ss(SS_PROCEDURES), pfield = pfield fi_>> 8;
    if isword(v) then
        returnunless(isdefined(v)) (false);
        valof(v) -> v
    elseif isident(v) then
        idval(v) -> v
    endif;
    if length(v) fi_>= pfield then
        v(pfield)
    else
        pop_undef_p
    endif
enddefine;

define Apply_ss_procedure(/*ss, pfield*/);
    lvars p = Get_ss_procedure();
    returnunless(p);
    if isident(p) then
        idval(p) -> p
    elseif isword(p) then
        returnunless(isdefined(p));     ;;; might be a weakref
        valof(p) -> p
    endif;
    unless isundef(p) then chain(p) endunless
enddefine;

define Subsystem_find(value, field) -> ss;
    lvars ss, value, field, v, in_pdrs = field fi_>= 16:100;
    for ss in sys_subsystem_table do
        if in_pdrs then
            nextunless(Get_ss_procedure(ss, field) ->> v)
        else
            nextunless(length(ss) fi_>= field);
            ss(field) -> v
        endif;
        returnif(v = value)     ;;; MUST USE = NOT ==
    endfor;
    ;;; not found
    false -> ss
enddefine;

define lconstant Do_subscr(field, ssname, upd);
    lvars field, ssname, upd, ss, p, errms, err_p = false;
    if isprocedure(ssname) then
        (), field, ssname -> (field, ssname, err_p)
    endif;
    Check_integer(field, 1);
    unless Subsystem_find(ssname, SS_NAME) ->> ss then
        'SUBSYSTEM NONEXISTENT OR NOT LOADED' -> errms
    elseif field fi_>= 16:100 then
        unless Get_ss_procedure(ss, field) ->> p then
            'SUBSYSTEM COMPILER NOT LOADED' -> errms
        elseunless upd then
            return(p)
        else
            if isword(ss(SS_PROCEDURES) ->> ss) then
                valof(ss) -> ss
            elseif isident(ss) then
                idval(ss) -> ss
            endif;
            () -> ss(field fi_>> 8);
            return
        endunless
    elseunless upd then
        return(if length(ss) fi_>= field then ss(field) else pop_undef endif)
    else
        () -> ss(field);
        return
    endunless;

    ;;; subsystem or compiler not loaded
    if err_p then       ;;; e.g. vederror
        err_p(ssname sys_>< '\s' sys_>< errms)
    else
        mishap(ssname, 1, errms)
    endif
enddefine;

define subscr_subsystem(/*field, ssname*/) with_nargs 2;
    Do_subscr(false)
enddefine;
;;;
define updaterof subscr_subsystem(/*val, field, ssname*/) with_nargs 3;
    Do_subscr(true)
enddefine;

define app_subsystems(p);
    lvars p, ss;
    for ss in sys_subsystem_table do;
        p(ss(SS_NAME));
    endfor;
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 29 1995
        Changed initialisation of subsystem to false
--- Adrian Howard, May  4 1993
        Added -app_subsystems-
 */
