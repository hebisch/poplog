/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/trace.p
 > Purpose:         PML: tracing function calls
 > Author:          Robert John Duncan, Dec  5 1994
 */


section $-ml;

;;; Forward (from "vmml.p"):
constant procedure (
    mlid_arity,
    mlid_tupled,
    val_tracing,
);

vars
    ml_trace_pr_level = 10,
        ;;; maximum print depth while tracing
    ml_trace_names = false,
        ;;; global list of function names to trace
    trace_names = false,
        ;;; local version, set for each evaluation
    trace_depth = 0,
        ;;; trace counter
;

;;; trace_add:
;;;     add a set of names to those already being traced; <true> means
;;;     everything, <false> means nothing

define trace_add(names);
    lvars name, names;
    if not(trace_names) or isboolean(names) then
        names -> trace_names;
    elseif trace_names /== true then
        For name in names do
            unless Lmember(name, trace_names) then
                conspair(name, trace_names) -> trace_names;
            endunless;
        endfor;
    endif;
enddefine;

;;; trace_check:
;;;     test whether the current function is being traced

define trace_check();
    trace_names
    and (trace_names == true or Lmember(pdprops(caller(1)), trace_names));
enddefine;

;;; trace_print:
;;;     print a trace message on function entry and exit

define trace_print(id, ty, on_entry);
    lvars id, ty, on_entry;
    dlocal cucharout = cucharerr, ml_pr_level = ml_trace_pr_level;
    ;;; print trace intro
    lvars name = pdprops(caller(1));
    printf('[');
    pr_field(trace_depth, 4, ` `, false, sys_syspr);
    printf(']%c %P', [% on_entry and `>` or `<`, name %]);
    ;;; print arguments/results
    type_deref(ty) -> ty;
    if mlid_tupled(id) then
        while is_alias_type(ty) do type_expand(ty) -> ty endwhile;
        lvars arg_ty = type_domain(ty);
        type_range(ty) -> ty;
        if on_entry then
            lvars arg = ml_constuple(mlid_arity(id));
            printf('\s');
            compile_toplevel_printer(arg_ty)(arg, false);
            ml_desttuple(arg) -> ;
        endif;
    else
        lvars arg_tys = {%
            repeat mlid_arity(id) times
                while is_alias_type(ty) do type_expand(ty) -> ty endwhile;
                type_domain(ty), type_deref(type_range(ty)) -> ty;
            endrepeat;
        %};
        if on_entry then
            lvars arg, args = consvector(mlid_arity(id));
            for arg, arg_ty in_vector args, arg_tys do
                printf('\s');
                compile_toplevel_printer(arg_ty)(arg, false);
            endfor;
            explode(args);
        endif;
    endif;
    unless on_entry then
        lvars res = ();
        printf('\s');
        compile_toplevel_printer(ty)(res, false);
        res;
    endunless;
    printf('\n');
enddefine;

;;; trace_message:
;;;     print a message re. the trace/untrace command

define lvars trace_message(msg, args);
    lvars msg, args;
    dlocal pr = ml_pr;
    printf(msg, args);
    cucharout(`\n`);
enddefine;

;;; do_trace, do_untrace:
;;;     add/remove a set of names to be traced from the current value of
;;;     id

define lconstant do_trace(id, names);
    lvars id, names, val = idval(id);
    unless val == true then
        if isboolean(names) or not(val) then
            names -> idval(id);
        else
            lvars name;
            For name in names do
                unless Lmember(name, val) then
                    conspair(name, val) -> val;
                endunless;
            endfor;
            val -> idval(id);
        endif;
    endunless;
enddefine;
;;;
define lconstant do_untrace(id, names);
    lvars id, names, val = idval(id);
    if val then
        if isboolean(names) or names == [] then
            not(names) -> idval(id);
        elseif val /== true then
            lvars name;
            [%  For name in val do
                    unless Lmember(name, names) then name endunless;
                endfor;
            %] -> idval(id);
        endif;
    endif;
enddefine;

;;; parse_arguments:
;;;     parse arguments to the trace and untrace commands

define lconstant parse_arguments(args) -> args;
    lvars arg, args;
    ;;; put back as one big string
    consstring(#| For arg in args do explode(arg), `\s` endfor |#)-> args;
    ;;; itemise it
    lvars item, procedure inp = new_itemiser(instream(stringin(args)));
    [%  until (inp() ->> item) == termin do
            if isid(item) then
                item;
            elseif item == RESERVED '[' then
                [%  until (inp() ->> item) == termin or item == RESERVED ']'
                    do  if isshortid(item) then
                            item;
                        elseunless item == RESERVED ','  then
                            quitloop;
                        endif;
                    enduntil;
                %];
                quitunless(item == RESERVED ']');
            elseunless item == RESERVED ',' then
                quitloop;
            endif;
        enduntil;
    %] -> args;
    unless item == termin then
        trace_message('Bad arguments to command', []);
    endunless;
enddefine;

;;; trace_command:
;;;     implement the trace and untrace commands

define trace_command(cmd, args);
    lvars cmd, args, done = false;
    parse_arguments(args) -> args;
    until args == [] do
        lvars id = ident ml_trace_names, (arg, args) = Destpair(args);
        if isid(arg) then
            (arg, []) -> (id, arg);
            if args /== [] and islist(hd(args)) then
                Destpair(args) -> (arg, args);
            endif;
            lvars val = lookup_var(id);
            unless val then
                trace_message('No definition for val %p', [^id]);
                nextloop;
            elseunless val_tracing(val) then
                trace_message('val %p is not a traceable function', [^id]);
                nextloop;
            elseif cmd = 'trace' then
                ;;; include the identifier itself
                conspair(val_name(val), arg) -> arg;
            endunless;
            val_tracing(val) -> id;
        endif;
        if islist(arg) and Lmember("*", arg) then
            ;;; "*" means trace/untrace everything
            true -> arg;
        endif;
        if cmd = 'trace' then do_trace else do_untrace endif(id, arg);
        true -> done;
    enduntil;
    unless done then
        trace_message('Nothing to %S', [^cmd]);
    endunless;
enddefine;

;;; mlved_trace, mlved_untrace:
;;;     trace/untrace from Ved

define lconstant mlved_trace_command(cmd);
    lvars cmd, args;

    define dlocal trace_message(msg, args);
        lvars msg, args;
        dlocal cucharout = identfn, pr = ml_pr;
        vederror(consstring(#| printf(msg, args) |#));
    enddefine;

    lex_command_args(new_itemiser(instream(stringin(vedargument)))) -> args;
    trace_command(cmd, args);
enddefine;

define vars ved_trace =
    mlved_trace_command(% 'trace' %);
enddefine;

define vars ved_untrace =
    mlved_trace_command(% 'untrace' %);
enddefine;

endsection; /* $-ml */
