/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_info.p
 > Purpose:         Prolog: report information about a predicate
 > Author:          Robert John Duncan, Jul  5 1993
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( predicate_record, pred_isdefined, );

weak constant
    procedure ( converter_from_contn, );


;;; =======================================================================

;;; predicate_info:
;;;     returns a list of attributes declared for the predicate -fn/arity-

define predicate_info(fn, arity);
    lvars fn, arity;
    lvars pred = predicate_record(fn, arity, true);
    lvars proc = pred_isdefined(pred);
    lvars tag = pred_tag(pred);
    ;;; create the attributes list
    [%  if not(proc) then
            "undefined"
        elseif testdef converter_from_contn
        and pdpart(proc) == weakref converter_from_contn
        then
            "nonprolog"
        endif;
        if pred_system(pred) == true then
            "system_predicate"
        elseif pred_system(pred) == false then
            "user_predicate"
        endif;
        if pred_dynamic(pred) == true then
            "dynamic"
        elseif pred_dynamic(pred) == false then
            "static"
        endif;
        if pred_notext(pred) == true then
            "no_clauses"
        elseif pred_notext(pred) == false then
            "clauses"
        endif;
        if pred_spied(pred) then
            "spied"
        endif;
        if pred_hidden(pred) then
            "hidden"
        elseif isref(tag) and isstring(Cont(tag)) then
            prolog_maketerm("file", consword(Cont(tag)), "=", 2)
        endif;
    %];
enddefine;

endsection;     /* prolog */

PROLOG

:- module prolog.

predicate_info(Spec, Info) :-
    predicate_spec(Spec, Fn, Arity),
    prolog_eval(predicate_info(quote(Fn, Arity)), Info).

:- endmodule prolog.
