/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_ui.p
 > Purpose:         Add Prolog options pages to the Poplog UI
 > Author:          Robert Duncan, Jul 12 1996
 */

#_TERMIN_IF not(DEF poplog_ui)

section prolog;

define lconstant default_value(sheet, name);
    idval(propsheet_field_ident(sheet, name)) ->>
        propsheet_field_default(sheet, name);
enddefine;

define lconstant prompt_load(name) -> ok;
    /*  This helpful prompt can lock up the whole UI under OLIT, so
        why bother...
    lvars msg = 'This option requires library(' <>
        (name sys_>< ').\nDo you wish to load that now?');
    returnunless(pop_ui_confirm(msg, [Yes No], 1, true, pop_ui_control_panel) == 1)
        (false -> ok);
    */
    lvars ok = true;
    external_defer_apply(
        procedure();
            dlocal XptBusyCursorOn = true;
            prolog_invoke(prolog_maketerm(name, "library", 1)) -> ;
        endprocedure);
enddefine;

define lconstant save_options(save_p);
    $-poplog_ui$-proptool_save_to_file('init.pl', save_p);
enddefine;

/* General Options */

define lconstant prolog_syntax_accept(sheet, name, value) -> value;
    unless value == "poplog"
    or isdefined("edinsynt") and valof("edinsynt")
    or prompt_load("edinsynt")
    then
        propsheet_undef -> value;
    endunless;
enddefine;

define lvars active prolog_syntax_style;
    lvars style = prolog_newvar();
    if isdefined("edinsynt") and valof("edinsynt")
    and prolog_invoke(prolog_maketerm(style, "prolog_syntax", 1))
    then
        prolog_deref(style)
    else
        "poplog"
    endif;
enddefine;
;;;
define updaterof prolog_syntax_style style;
    unless isdefined("edinsynt") and valof("edinsynt")
    and prolog_invoke(prolog_maketerm(style, "prolog_syntax", 1))
    then
        propsheet_refresh(ident prolog_syntax_style);
    endunless;
enddefine;

define lconstant prolog_unknown_accept(sheet, name, value) -> value;
    unless value == "fail"
    or predicate_isdefined("unknown", 2)
    or prompt_load("unknown")
    then
        propsheet_undef -> value;
    endunless;
enddefine;

define lvars active prolog_unknown_action;
    lvars action = prolog_newvar();
    if predicate_isdefined("unknown", 2)
    and prolog_invoke(prolog_maketerm(action, prolog_newvar(), "unknown", 2))
    then
        prolog_deref(action)
    else
        "fail"
    endif;
enddefine;
;;;
define updaterof prolog_unknown_action action;
    unless predicate_isdefined("unknown", 2)
    and prolog_invoke(prolog_maketerm(prolog_newvar(), action, "unknown", 2))
    then
        propsheet_refresh(ident prolog_unknown_action);
    endunless;
enddefine;

define lconstant prolog_area_accept(sheet, name, value) -> value;
    returnunless(isreal(value))(propsheet_undef -> value);
    max(0, intof(value)) -> value;
    if propsheet_acceptreason == "increment" then
        ;;; already increased by 1; round up to 1024 word boundary
        (value + 1023) &&~~ 1023 -> value;
    elseif propsheet_acceptreason == "decrement" then
        ;;; already decreased by 1; round down to 1024 word boundary
        value &&~~ 1023 -> value;
    endif;
    returnunless(isinteger(value))(propsheet_undef -> value);
    ;;; ensure size <= limit
    if name == "pop_prolog_size" then
        if value > propsheet_field_value(sheet, "pop_prolog_lim") then
            value -> propsheet_field_value(sheet, "pop_prolog_lim");
        endif;
    elseif name == "pop_prolog_lim" then
        if value < propsheet_field_value(sheet, "pop_prolog_size") then
            propsheet_field_value(sheet, "pop_prolog_size") -> value;
        endif;
    endif;
enddefine;

    ;;; Prolog area size can't be changed during external calls so we
    ;;; need an active variable to defer the update
define lvars active prolog_area_size;
    pop_prolog_size;
enddefine;
;;;
define updaterof prolog_area_size newsize;
    external_defer_apply(
        procedure();
            newsize -> pop_prolog_size;
            ;;; the actual sizes may be different
            propsheet_refresh(ident prolog_area_size);
            propsheet_refresh(ident pop_prolog_lim);
        endprocedure);
enddefine;

define lconstant prolog_general_save(title, sheet);
    dlocal pr = sys_syspr, pop_pr_radix = 10;
    unless prolog_syntax_style == "poplog" then
        printf(';;; See PLOGHELP * EDINSYNT\n');
        printf(':- library(edinsynt).\n');
        printf(':- prolog_syntax(edinburgh).\n');
    endunless;
    unless prolog_unknown_action == "fail" then
        printf(';;; See PLOGHELP * UNKNOWN\n');
        printf(':- library(unknown).\n');
        printf(':- unknown(_, %p).\n', [^prolog_unknown_action]);
    endunless;
    printf(';;; See PLOGHELP * SYSTEM\n');
    printf(':- prolog_area_size(%p).\n', [^pop_prolog_size]);
    printf(':- prolog_area_lim(%p).\n', [^pop_prolog_lim]);
enddefine;

pop_ui_add_property('Prolog', [
    [edinsynt menuof [poplog edinburgh]
        (label = 'Syntax style',
         ident = ^(ident prolog_syntax_style),
         accepter = ^prolog_syntax_accept,
         default = ^default_value)]
    [unknown menuof [fail error autoload]
        (label = 'Unknown action',
         ident = ^(ident prolog_unknown_action),
         accepter = ^prolog_unknown_accept,
         default = ^default_value)]
    [pop_prolog_size ^pop_prolog_size
        (label = 'Area size',
         ident = ^(ident prolog_area_size),
         accepter = ^prolog_area_accept,
         default = ^default_value)]
    [pop_prolog_lim ^pop_prolog_lim
        (label = 'Area limit',
         ident = ^(ident pop_prolog_lim),
         accepter = ^prolog_area_accept,
         default = ^default_value)]
], false, save_options(%prolog_general_save%));

/* Spy Port Options */

lconstant
    SPY_PORTS   = [call exit redo fail],
    SPY_ACTIONS = [stop continue ignore user],
;

define lvars active call_port =
    spy_action(%"call"%);
enddefine;
;;;
define lvars active exit_port =
    spy_action(%"exit"%);
enddefine;
;;;
define lvars active redo_port =
    spy_action(%"redo"%);
enddefine;
;;;
define lvars active fail_port =
    spy_action(%"fail"%);
enddefine;

define lconstant prolog_spy_ports_save(title, sheet);
    dlocal pr = sys_syspr;
    lvars port;
    for port in SPY_PORTS do
        printf(':- spy_action(%P, %P).\n', [% port, spy_action(port) %]);
    endfor;
enddefine;

pop_ui_add_property(['Prolog' 'Spy Ports'], [
    [Call menuof ^SPY_ACTIONS
        (ident = ^(ident call_port),
         default = ^default_value)]
    [Exit menuof ^SPY_ACTIONS
        (ident = ^(ident exit_port),
         default = ^default_value)]
    [Redo menuof ^SPY_ACTIONS
        (ident = ^(ident redo_port),
         default = ^default_value)]
    [Fail menuof ^SPY_ACTIONS
        (ident = ^(ident fail_port),
         default = ^default_value)]
], false, save_options(%prolog_spy_ports_save%));

endsection;     /* prolog */
