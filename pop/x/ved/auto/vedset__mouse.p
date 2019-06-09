/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedset__mouse.p
 > Purpose:         Defines action of vedset mouse ...
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:   REF *XVED
 */
compile_mode :pop11 +strict;

uses vedset;
include xved_constants.ph;

section;


lconstant
    valid_types = [
        press
        release
        drag
        dragRelease
        hold
        holdRelease
    ],

    valid_mods = [
        shift       ^XVM_SHIFTMASK
        control     ^XVM_CONTROLMASK
        meta        ^XVM_METAMASK   ;;; same as mod1
        mod1        ^XVM_MOD1MASK
        mod2        ^XVM_MOD2MASK
        mod3        ^XVM_MOD3MASK
        mod4        ^XVM_MOD4MASK
        mod5        ^XVM_MOD5MASK
        anyModifier ^XVM_ANYMODMASK
        noModifier  ^XVM_NOMODMASK
    ],

    valid_btns = [
        btn1    ^XVM_BTN1MASK
        btn2    ^XVM_BTN2MASK
        btn3    ^XVM_BTN3MASK
        btn4    ^XVM_BTN4MASK
        btn5    ^XVM_BTN5MASK
        anyBtn  ^XVM_ANYBTNMASK
    ],
;

lvars mouse_window, mouse_place, requested_types, clear_first;

define lconstant read_vedsetmouse_options();
    lvars item;
    until pop11_try_nextreaditem(")") do
        if pop11_try_nextreaditem("override") then
            true -> clear_first;
        elseif pop11_try_nextreaditem("at") then
            pop11_need_nextreaditem([front back]) -> mouse_place;
        elseif pop11_try_nextreaditem(
                [defaultWindow currentWindow nextWindow])->>item then
            item -> mouse_window;
        endif;
        unless nextreaditem() == ")" then
            pop11_need_nextreaditem(",")->;
        endunless;
    enduntil;
    ;;; changing attributes clears the requested_types list
    [] -> requested_types;
enddefine;

define lconstant read_vedsetmouse_command();
    lvars   varname, pushp, type = "buttonRelease", item, list,
            buttons = 0, modifiers = 0, num_clicks = 0,
            click_verb = false,
        ;
    dlocal  pop_new_lvar_list;

    if pop11_try_nextreaditem("(") then
        read_vedsetmouse_options();
    endif;

    vedset_compile_expr("vedmouse__", true, false) -> (pushp, varname);

    ;;; READ EVENT TYPE
    if nextreaditem() == "click" then   ;;; as a verb
        readitem() ->;
        true -> click_verb
    else
        ;;; get event type
        if nextreaditem().isinteger then
            ;;; n-click adjective
            fi_check(readitem(),1,false) -> num_clicks;
            pop11_need_nextreaditem("-") -> ;
            pop11_need_nextreaditem("click") -> ;
        endif;
        nextreaditem() -> item;
        if fast_lmember(item, valid_types) then
            readitem() -> ;
            consword(#| explode('button'), lowertoupper(item(1)),
                        explode(allbutfirst(1,item)) |#) -> type
        elseif num_clicks == 0 then
            mishap(item, 1, 'vedset: INVALID BUTTON EVENT TYPE')
        endif
    endif;

    ;;; READ BUTTON
    repeat
        readitem() -> item;
        unless (fast_lmember(item, valid_btns) ->> list) then
            mishap(item,1,'vedset: INVALID BUTTON NUMBER');
        endunless;
        fast_front(fast_back(list)) || buttons -> buttons;
        quitunless(pop11_try_nextreaditem("or"))
    endrepeat;

    ;;; READ CLICK TIMES
    if nextreaditem().isinteger then
        fi_check(readitem(),1,false) -> num_clicks;
        pop11_need_nextreaditem("times")->;
        unless click_verb then
            mishap(num_clicks, 1, 'vedset: UNEXPECTED "times" CLAUSE')
        endunless
    elseif num_clicks == 0 then
        if type == "buttonRelease" then 1 -> num_clicks endif
    else
        if type == "buttonPress" then
            mishap(0, 'vedset: CAN\'T SPECIFY NUMBER OF CLICKS WITH "press"')
        endif
    endif;

    ;;; READ MODIFIERS
    if pop11_try_nextreaditem("with") then
        repeat;
            readitem() -> item;
            unless (fast_lmember(item, valid_mods) ->> list) then
                mishap(item,1,'vedset: INVALID BUTTON NUMBER');
            endunless;
            fast_front(fast_back(list)) || modifiers -> modifiers;
            quitunless(pop11_try_nextreaditem("or"));
        endrepeat;
    else
        XVM_NOMODMASK -> modifiers;
    endif;

    ;;; CLEAR ENTRIES FIRST IF NECESSARY
    ;;; does the equiv of
    ;;;     false -> xved_event_handler(window, type)
    ;;; for a type not specified so far
    if clear_first and not(fast_lmember(type, requested_types)) then
        sysPUSHQ(false);
        sysPUSHQ(mouse_window);
        sysPUSHQ(type);
        sysUCALL("ident $-xved$-xved_event_handler");
        type :: requested_types -> requested_types;
    endif;

    pushp(varname);
    sysPUSHQ(type);
    sysPUSHQ(buttons);
    sysPUSHQ(modifiers);
    sysPUSHQ(num_clicks);
    sysPUSHQ(mouse_window);
    sysPUSHQ(mouse_place);
    sysCALL("ident $-xved$-xved_add_button_handler");

enddefine;

define vedset__mouse();
    "front" -> mouse_place;
    "defaultWindow" -> mouse_window;
    [] -> requested_types;
    false -> clear_first;

    until pop11_try_nextreaditem("endvedset") do
        read_vedsetmouse_command();
    enduntil
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 29 1996
        Made it allow things like '2-click btn1' etc
--- John Gibson, May 25 1993
        Took out of section xved. Made it use sysCALL/UCALL on word
        idents for XVed procedures.
--- John Gibson, Sep  9 1992
        Got rid of X*VED_section
--- John Gibson, Aug  5 1992
        Made vedset__mouse global
--- John Gibson, Feb 27 1992
        System -vedset- rewritten to call vedset__<keyword> for
        vedset <keyword>, and "keys" changed to incorporate = (code)
        via -wved_vedset_key_chars-.
        Thus this file now only defines -vedset__mouse-.
--- Simon Nichols, Oct 28 1991
        Fixed bug in -keusym- which prevented LIB * VEDHPXTERMKEYS from
        loading.
--- John Gibson, Aug 19 1991
        Changed read_vedsetmouse_command to allow 'n-click' as an adjective
        before event type
--- John Gibson, Aug 10 1991
        Added modifiers mod1-5
--- Jonathan Meyer, Jun 27 1991
        Allowed strings on lhs of key settings
--- Jonathan Meyer, Jun 19 1991
        Added keysym numbers
 */
