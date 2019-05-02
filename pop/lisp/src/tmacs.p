/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/tmacs.p
 > Purpose:         Terminating read macro characters
 > Author:          John Williams, Sep 26 1986 (see revisions)
 > Documentation:   CLtL, p346-351
 > Related Files:   C.all/lisp/src/itemise.p
 */

lisp_compile_mode;

section $-lisp;

;;; Producing backquoted forms

lconstant
    Comma      =  'COMMA',
    Comma_at   =  'COMMA-AT',
    Comma_dot  =  'COMMA-DOT',
    ;

constant procedure bq_list;

define lconstant Bq_atom(item) -> item;
    if issymbol(item) then
        [^@QUOTE ^item] -> item
    elseif isvector(item) and fast_vector_length(item) /== 0 then
        [^@APPLY [^@FUNCTION ^@VECTOR] ^(bq_list(datalist(item)))] -> item
    endif
enddefine;

define bq_list(list) -> result;
    lvars last, next, item, sym;
    conspair(@LIST, list) ->> last -> result;
    repeat
        fast_destpair(list) -> next -> item;
        if ispair(item) then
            fast_front(item) -> sym;
            if sym == Comma then
                fast_back(item) -> fast_front(list)
            elseif sym == Comma_at or sym == Comma_dot then
                if next == [] then
                    @LIST* -> fast_front(result);
                    fast_back(item) -> fast_front(list)
                else
                    [] -> fast_back(last);
                    [% if sym == Comma_dot then @NCONC else @APPEND endif,
                       if fast_back(result) /== [] then result endif,
                       fast_back(item),
                       bq_list(next)
                    %] -> result;
                    return
                endif
            else
                bq_list(item) -> fast_front(list)
            endif
        elseif item == Comma then
            @LIST* -> fast_front(result);
            next -> fast_front(list);
            [] -> fast_back(list);
            return
        else
            Bq_atom(item) -> fast_front(list)
        endif;
        if atom(next) then
            unless next == [] do
                @LIST* -> fast_front(result);
                conspair(Bq_atom(next), []) -> fast_back(list)
            endunless;
            return
        else
            list -> last;
            next -> list
        endif
    endrepeat
enddefine;


;;; quote, comma, and backquote

define quote();
    acons(@QUOTE, lispreaditem(), [])
enddefine;

define backquote();
    lvars form;
    dlocal lexbqdepth = (lexbqdepth fi_+ 1);
    lispreaditem() -> form;
    if atom(form) then
        Bq_atom(form)
    else
        bq_list(copy_tree(form))
    endif
enddefine;

define comma();
    dlocal lexbqdepth;
    if lexbqdepth fi_> 0 then
        lexget();
        lexbqdepth fi_- 1 -> lexbqdepth;
        if lexchar == `@` then
            if lexket then
                return(conspair(Comma_at, lispreaditem()))
            endif
        elseif lexchar == `.` then
            if lexket then
                return(conspair(Comma_dot, lispreaditem()))
            endif
        else
            lexput();
            return(conspair(Comma, lispreaditem()))
        endif
    endif;
    mishap(0, 'Misplaced comma (not inside backquote ?)')
enddefine;


;;; Reading lists ()

define rdlist();
    dlocal lexdot, lex_eof_error = true, lexket;
    #_< mishap(% 0, 'Form needed before "." in list' %) >_# -> lexdot;
    procedure; chainfrom(rdlist, sysconslist) endprocedure -> lexket;

    popstackmark;
    lispreaditem();
    define dlocal lexdot;
        #_< mishap(% 0, 'Only one "." allowed in list' %) >_#  -> lexdot;
        #_< mishap(% 0, 'Form needed after "." in list' %) >_# -> lexket;
        lispreaditem();
        procedure; chainfrom(rdlist, dotconslist) endprocedure -> lexket;
        lispreaditem();     ;;; should be the closer, which exits
        mishap(0, 'Only one form allowed after "." in list')
    enddefine;
    repeat
        lispreaditem()
    endrepeat
enddefine;

define closer();
    lexket()
enddefine;


;;; POP11 escape (@)

define do_pop11();
    lvars endchar;
    lexchar -> endchar;
    acons(
        @POPLOG:POP11-VAL,
        consstring
            (#| repeat
                    lexget();
                    quitif(lexchar == `\n` or lexchar == endchar or lexchar == termin);
                    lexchar
            endrepeat |#),
          []);
enddefine;


;;; End-of-file (termin)

define terminpdr =
    tmac(% eof %)
enddefine;


;;; End-of-line comment (semicolon)

define scolon();
    lvars char;
    until (cucharin() ->> char) == `\n` or char == termin do enduntil;
    chainfrom(lispreaditem, lispreaditem)
enddefine;


;;; String-quote (")

define rdstring();
    lvars endchar;
    lexchar -> endchar;
    until (lexget(), lexchar == endchar) do
        if lextype == single_escape then
            lexget()
        endif;
        if lexchar == termin then
            consstring(stacklength() fi_- lexstacklength) -> endchar;
            mishap(endchar, 1, 'Unterminated string')
        endif;
        lexchar
    enduntil;
    consstring(stacklength() fi_- lexstacklength)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  7 1994
        Symbol POP11-VAL now in POPLOG package.
--- John Williams, Dec  1 1993
        backquote now copies its argument (if a list).
--- John Williams, Jul 12 1993
        No longer uses cons_with.
 */
