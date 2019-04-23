/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/ved/src/vddefsrch.p
 > Purpose:         Search for procedure heading in current file
 > Author:          Various (see revisions)
 */

;;; ------------------ SEARCHING FOR PROCEDURE HEADERS --------------------

#_INCLUDE 'vddeclare.ph'

global constant
    procedure (vedmoveitem, vednextitem, ved_try_search, ved_set_search)
    ;

global vars
    vvedoldsrchdisplay,
    ;

section $-Sys$-Ved;

constant
    No_search_string, Cannot_find, procedure (Safe_search),
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys$-Ved => vedfindpdheader, vedfstring, ved_f;

vars
    vedfstring = nullstring
    ;


    ;;; search for procedure <name>, returns line no or false.
    ;;; if <exact> is false, <name> can be initial substring
    ;;; of the procedure name

define vars vedfindpdheader(name, _exact);
    lvars _firstoccurence, name, _exact;
    dlocal vedline, vedcolumn, poplastchar;

    define lconstant Get_name(namestr, _exact);
        ;;; Found 'define', so get name of current procedure
        lvars _line = vedline, namestr, item, _exact, list,
            precedence = false;
        dlocal vedline, vedcolumn = 1;
        unless (vedmoveitem() == "define" and vedline = _line) then
            return(false);
        endunless;
        if vednextitem()== ":" then
            ;;; it's a "define : type" definition -- ignore colon and next item
            vedmoveitem() ->; vedmoveitem() -> item;
            if item == "infix" then
                vedmoveitem() -> precedence;
                unless isnumber(precedence) then
                    return(false)
                endunless;
            endif
        endif;
        while lmember(vedmoveitem() ->> item, define_headers) do endwhile;
        if isnumber(item) then
            item -> precedence;
            vedmoveitem() -> item
        endif;
        if item == "$-" then ;;; section pathname starting
            vedmoveitem() -> item;
        endif;
        while vednextitem() == "$-" do
            vedmoveitem() ->; vedmoveitem() -> item
        endwhile;
        if isnumber(item) or precedence then
            ;;; operator
            [%  unless isnumber(item) then item endunless,
                until lmember(vedmoveitem() ->> item, define_terminators) do
                     item;
                enduntil %] -> list;
            ;;; next condition added by A.S. Use item if list empty.
            unless list == [] then
                if listlength(list) == 3 then fast_back(list) -> list endif;
                fast_front(list) -> item;
            endunless;
        endif;
        if _exact then
            unless isword(namestr) do
                consword(namestr) -> namestr
            endunless;
            namestr == item
        else
            isstartstring(namestr, item)
        endif
    enddefine;      /* Get_name */

    vednextline();                  ;;; assume not on current line
    false -> _firstoccurence;
    while Safe_search('define', [noembed], ved_try_search) do
        if _firstoccurence then     ;;; already found one, see if wrapped
            if vedline == _firstoccurence then return(false) endif;
        else                        ;;; not 1st occurence
            vedline -> _firstoccurence
        endif;
        if Get_name(name, _exact) then return(vedline) endif;
    endwhile;
    false;                          ;;; never found any
enddefine;


define vars ved_f();
    lvars exact, line, name;

    if vedargument = nullstring then
        if vedfstring = nullstring then
            vederror(No_search_string)
        endif
    else
        vedargument -> vedfstring
    endif;

    if datalength(vedfstring) fi_> 3
    and isstartstring('-x ', vedfstring) then
        true, allbutfirst(3, vedfstring)
    else
        false, vedfstring
    endif -> name -> exact;

    if (vedfindpdheader(name, exact) ->> line) then
        line -> vedline;
        vedtextleft();
        unless vedline fi_>= vedlineoffset              ;;; near top?
        and (vedline fi_- vedlineoffset) fi_< (vedwindowlength >> 1) then
            vedline fi_- 1 -> vedlineoffset;            ;;; no, then refresh
            vedrefreshwindow(true)
        endunless;
        ;;; record this search string so that REDO looks for it...
        ved_set_search(name, false, []);
    else
        vederror(name <> ' \{b}not found')
    endif
enddefine;


endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 25 1993
        Converted to ved_try_search
--- John Williams, Mar 19 1991
        -vedfindpdheader- now uses -Safe_search-
--- John Williams, Jul  6 1990
        Made -vedfindpdheader- a vars, created new exported variable
        -vedfstring-.
--- John Gibson, Apr 30 1989
        Into section Sys$-Ved
--- Aaron Sloman, Jul 14 1988
        Made -vvedanywhere- dlocal
--- Aaron Sloman, Jul  6 1988
        Re-named last_string as vedfstring
        Fixed bug that left extra stuff on stack (using ->> instead of ->)
        Made vvedsrchstring etc dlocal to vedfindpdheader as requested
            in SFR 4155
        Changed to use define_headers, and define_terminators,
            instead of vedfspecial and op_terminators. Moved their
            declarations to pop11_syntax.p
--- Aaron Sloman, Jul  4 1988
        Fixed to cope with section pathnames in procedure headings
--- Aaron Sloman, Jul  4 1988
        Further fixes to cope with define:infix ...
--- Aaron Sloman, Jul  3 1988
        Fixed to cope with define:type .... by altering Get_name
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Gibson, Aug 18 1987
        Tidied up
--- 18/3/86 A. Sloman
    fixed to cope with infix operators, and used consword for exact match
--- 12/1/86 A.Sloman
    Get_name had a bug. Could not deal with some syntactic operators.
--- Mark Rubinstein October 1985
    Get_name and vedfindpdheader rewritten
 */
