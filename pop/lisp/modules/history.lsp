#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/history.lsp
 | Purpose:         History mechanism for Common Lisp
 | Author:          John Williams, Jan 22 1988 (see revisions)
 | Documentation:   HELP * HISTORY
 | Related Files:   C.all/lisp/src/compile.p
 |#

(cl:provide :history)

(cl:in-package :poplog)

(export '(history-data *history-max*))

(pop11)


section $-lisp;

lisp_compile_mode;

vars
    history_max     =   20,
    history         =   false,
    history_now     =   false,
    ;

lconstant macro (
    INDEX   =   1,
    FORM    =   2,
    RESULTS =   3,
    NEXT    =   4,
    LAST    =   5,
    );


define lconstant syntax 1 !;
    lvars n;
    pop_expr_inst(pop_expr_item);
    sysPUSHQ(itemread());
    sysSWAP(1, 2);
    sysCALLQ -> pop_expr_inst;
    subscrv -> pop_expr_item;
enddefine;


define hs_entry(i) -> h;
    lvars sym;
    history_now -> h;
    if isinteger(i) then
        if i < 0 then
            until i == -1 do
                quitunless(h);
                h!LAST -> h;
                i + 1 -> i
            enduntil
        else
            history -> h;
            while h do
                quitif(h!INDEX == i);
                h!NEXT -> h
            endwhile
        endif
    elseif isstring(i) then
    STRING:
        while h do
            recursive_front(h!FORM) -> sym;
            quitif(issymbol(sym) and isstartstring(i, symbol_string(sym)));
            h!LAST -> h
        endwhile
    else
        while h do
            quitif(recursive_front(h!FORM) == i);
            h!LAST -> h
        endwhile;
        if issymbol(i) then
            symbol_string(i) -> i;
            goto STRING
        endif
    endif
enddefine;


define lconstant Add_hs_entry(form);
    lvars h, i;
    false -> h;
    if history_now then
        history_now!INDEX + 1
    else
        1
    endif -> i;
    if history_max /== nil then
        while history and (i - history!INDEX) >= history_max do
            history -> h;
            h!NEXT -> history
        endwhile
    endif;
    unless h do
        initv(5) -> h
    endunless;
    form -> h!FORM;
    nil -> h!RESULTS;
    i -> h!INDEX;
    false -> h!NEXT;
    history_now -> h!LAST;
    if history_now then
        h ->> history_now!NEXT -> history_now
    else
        h ->> history -> history_now
    endif
enddefine;


define lconstant Hs_data(i);
    lvars h;
    defaults i -1;
    if (hs_entry(i) ->> h) then
        h!FORM, h!RESULTS, h!INDEX
    else
        nil, nil, nil
    endif
enddefine;


/* Patch into top-level loop */

constant Old_before_hook Old_after_hook;

if isundef(Old_before_hook) then
    top_level_before_hook -> Old_before_hook
endif;

if isundef(Old_after_hook) then
    top_level_after_hook -> Old_after_hook
endif;


define vars top_level_before_hook(form);
    if Old_before_hook(form) then
        ;;; a magic word ... ignore
        true
    else
        if history_max == 0 then
            false ->> history -> history_now
        else
            Add_hs_entry(form)
        endif;
        false
    endif
enddefine;


define vars top_level_after_hook(results);
    if Old_after_hook(results) then
        true
    else
        if history_now then
            results -> history_now!RESULTS
        endif;
        false
    endif
enddefine;


/* Top level history commands */

define lconstant Hs_arg() -> h;
    dlocal popprompt = 'Index? ';
    if top_level_listen() then
        hs_entry(dest(top_level_forms) -> top_level_forms)
    else
        history_now
    endif -> h;
    unless h do
        advise('Event not found');
        exitto(true, top_level_loop)
    endunless
enddefine;


define lconstant Hs_redo();
    lvars h;
    Hs_arg() -> h;
    format(true, '~D: ~S~%', h) ->;
    h!FORM :: top_level_forms -> top_level_forms
enddefine;


define lconstant Hs_show();
    format(true, '~D: ~S~%=>~{ ~S~}~%', Hs_arg()) ->
enddefine;


define lconstant Hs_history();
    lvars h;
    history -> h;
    while h do
        format(true, '~D: ~S~%', h) ->;
        h!NEXT -> h
    endwhile
enddefine;


define lconstant Hs_fix();
    lvars text, old, i;

    define lconstant Rdstring(popprompt) -> string;
        dlocal popprompt;
        repeat
            @CLEAR-INPUT(0) ->;
            @READ-LINE(true, nil, termin, 3) -> -> string;
            if string == termin then
                advise('Abandoning :FIX');
                exitfrom(Hs_fix)
            endif;
            quitunless(datalength(string) == 0)
        endrepeat
    enddefine;

    @PRIN1-TO-STRING(Hs_arg()!FORM, 1) -> text;
    npr(text);
    repeat
        repeat
            Rdstring('Old: ') -> old;
            quitif(issubstring(old, 1, text) ->> i);
            advise('Substring not found')
        endrepeat;
        substring(1, i - 1, text)
            <> Rdstring('New: ')
            <> allbutfirst(i + datalength(old) - 1, text) -> text;
        npr(text);
        quitunless(@Y-OR-N-P('Eval?', 1) == nil);
        returnif(@Y-OR-N-P('Continue :FIX?', 1) == nil)
    endrepeat;
    (@READ-FROM-STRING(text, 1) ->) :: top_level_forms -> top_level_forms
enddefine;


lispsynonym(@*HISTORY-MAX*, "history_max");

lisp_export(Hs_data, @HISTORY-DATA, [0 1 3]);

Hs_redo     -> magic_word_handler(@:REDO);
Hs_show     -> magic_word_handler(@:SHOW);
Hs_history  -> magic_word_handler(@:HISTORY);
Hs_fix      -> magic_word_handler(@:FIX);

endsection;


/* Define the ! read macro */

lisp

(in-package :poplog)

(unless (get-macro-character #\!)
        (set-macro-character #\!
            #'(lambda (stream char)
                (values (history-data (read stream t t t))))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 12 1995
        Uses format instead of format_print.
 |#
