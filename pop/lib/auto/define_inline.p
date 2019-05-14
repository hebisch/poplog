/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/define_inline.p
 > Purpose:         In-line expansion macro define form
 > Author:          Jonathan Meyer, Oct  1 1990 (see revisions)
 > Documentation:   HELP *INLINE
 */

compile_mode :pop11 +strict;

section;

uses-by_name define_inline_macro_base;

lconstant
    ;;; temporary symbol table.
    names = newproperty([],8,false,"perm"),
;

define lconstant process_decl(list);
    lvars list, item;
    dlocal  pop_autoload = false,
            proglist_state = proglist_new_state(list);
    [%until proglist = [] do
        itemread() -> item;
        unless is_syntax_word(item) then
            mishap(item,1,'INVALID DECLARATOR')
        elseif item == "updaterof" then
            mishap(item,1,':inline MACROS CANNOT HAVE UPDATERS');
        endunless;
        item;
    enduntil%]
enddefine;

define :define_form global inline;
    lvars   next, items, declarators, inline_expr, id, readpname, readp,
            readplist = [], macro_name, defcount;

    clearproperty(names);

    ;;; GET DECLARATORS AND NAME
    [% until fast_lmember(readitem(),[(;]) do poplastitem enduntil %]
                                            -> declarators;
    poplastitem -> next;
    last(declarators) -> macro_name;
    process_decl(allbutlast(1, declarators)) -> declarators;

    unless next == ";" then
        ;;; GET PARAMETERS
        [] -> items;
        until (readitem() ->> next) == ")" do
            unless next.isword then
                mishap(next, 1, 'WORD NEEDED');
            endunless;
            if (identprops(next)->>id) == "syntax" or id == "macro" then
                mishap(next,1,'INVALID PARAMETER NAME');
            elseif fast_lmember(next, items) then
                mishap(next, 1, 'INVALID (DUPLICATE) PARAMETER');
            endif;
            if pop11_try_nextreaditem("=") then
                readitem() <> "read" -> readpname;
                sys_current_val(readpname) -> readp;
                unless isprocedure(readp) then
                    mishap(readpname,1,'INVALID READER FOR PARAMETER')
                endunless
            else
                ;;; default is exprread
                exprread -> readp
            endif;
            ;;; use readplist pair as parameter 'identifier'
            readp :: readplist ->> readplist -> names(next);
            next :: items -> items;
            if nextreaditem() /== ")" then
                pop11_need_nextreaditem(",") ->
            endif
        enduntil;
        pop11_need_nextreaditem(";") -> ;
        nonwriteable ncrev(readplist) -> readplist

    ;;; else no params
    endunless;

    ;;; GET INLINE EXPRESSION
    1 -> defcount;
    [%  repeat
            if (readitem() ->> next) == "enddefine" then
                quitif((defcount-1 ->> defcount) == 0)
            elseif next == "define" then
                defcount+1 -> defcount
            elseif names(next) ->> id then
                ;;; item should be substituted - replace with id pair
                id -> next
            elseif next == termin then
                mishap("FOUND", termin, "'READING TO'", "enddefine", 4,
                        'msw: MISPLACED SYNTAX WORD', 'pop11-msw:syntax')
            endif;
            next
        endrepeat
    %] -> inline_expr;
    pop11_need_nextreaditem(";") -> ;

    ;;; GENERATE NEW MACRO -- uses define_inline_macro_base
    ;;; use define to decode the declarators - saves us the effort.
    [define ^^declarators macro ^macro_name;
        define_inline_macro_base(^readplist,^inline_expr)
     enddefine; ] nc_<> proglist -> proglist;

    ;;; COMPILE NEW MACRO
    pop11_comp_expr();
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  9 1996
        Fixed BR davidy.99 (doesn't check for end of file when reading
        procedure body).
--- John Gibson, Sep 19 1995
        No longer passes exprvec to define_inline_macro_base -- using this
        stopped the latter working recursively.
--- John Gibson, Jun 19 1993
        Made it cope with nested defines in the inline expression
--- John Gibson, Oct 27 1992
        Changed to use sys_current_val
--- John Gibson, Oct  5 1992
        Rewritten to use pairs of readplist as param identifiers instead of
        a property. Base macro procedure made separately autoloadable.
--- John Gibson, Sep 29 1992
        Changed to use is_syntax_word
--- John Gibson, Sep 16 1991
        Rewritten to allow =<category> after parameter name -- then
        uses procedure <category>read to read actual value
--- Jonathan Meyer, Aug 17 1991
        Removed redeclaration of sysdeclare (exprread now fixed)
--- Jonathan Meyer, Apr 16 1991
        fixed issyntax and added dlocal proglist_state
--- Jonathan Meyer, Apr  9 1991
        Fixed reading of declarators (again)
--- Jonathan Meyer, Apr  5 1991
        Allowed inline macros with 0 args (either () or ; accepted).
        Made it read declarators correctly (back to readitem).
--- Jonathan Meyer, Dec 18 1990
        Changed readitem to itemread for reading declarators. This allows
        INCLUDE_constant inline macros
--- Jonathan Meyer, Nov 14 1990
        Added redefinition of sysdeclare to stop declaration of variables
        during exprread.
--- Jonathan Meyer, Oct 15 1990
        Fixed to work in sections (made define_inline_macro global).
        Removed in_lblock syntax.
 */
