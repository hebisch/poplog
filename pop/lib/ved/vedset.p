/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedset.p
 > Purpose:         Easy setup syntax for VED users
 > Author:          Aaron Sloman, Mar 18 1989 (see revisions)
 > Documentation:   HELP * VEDSET
 > Related Files:   VED terminal conversion files in $popvedlib/term/
 */
compile_mode :pop11 +strict;

section;

uses-by_name (  consstring, explode, pop_character_set, stacklength,
                sysanyvecons, vedset_pcompose, vedsetkey,
                wved_vedset_key_chars);

lconstant macro (
    SCREEN  = 1,
    KEYS    = 2,
    CHARS   = 3,
    GRAPHIC = 4,    ;;; now obsolete
    );


define vedset_compile_expr(prefix, allow_pdr, allow_string)
                                                -> (pushp, varname);
    lvars   prefix, allow_pdr, allow_string, pushp, varname, enter, vedcom,
            compose = false;

    repeat

        if allow_string and isstring(nextreaditem()) then
            readitem() -> varname;
            sysPUSHQ -> pushp;
            pop11_need_nextreaditem("=") -> ;
            return
        endif;

        sysPUSH -> pushp;
        ;;; look for a quoted name or ident
        if pop11_try_nextreaditem(""") then
            sysPUSHQ -> pushp
        elseif pop11_try_nextreaditem("ident") then
            sysIDENT -> pushp
        endif;
        ;;; look for an ENTER prefix
        unless pop11_try_nextreaditem("ENTER") ->> enter then
            pop11_try_nextreaditem("VED") -> vedcom
        endunless;
        ;;; read a VED function name
        unless isword(readitem() ->> varname) then
            mishap(varname, 1, 'vedset: ILLEGAL FUNCTION NAME')
        endunless;
        ;;; check for closing quote
        if pushp == sysPUSHQ and not(pop11_try_nextreaditem(""")) then
            mishap(varname, 1, 'vedset: MISSING CLOSING QUOTE')
        endif;

        if enter and not(allow_pdr) then
            mishap(enter, varname, 2, 'vedset: ILLEGAL FUNCTION')
        endif;

        ;;; construct appropriate variable or procedure name
        consword(if enter then 'ved_'
                 elseif vedcom then 'ved'
                 else prefix
                 endif sys_>< varname) -> varname;

        ;;; stop at "="
        quitif(pop11_try_nextreaditem("="));

        ;;; allow "<>" for procedure composition where appropriate
        unless allow_pdr and pop11_try_nextreaditem("<>") then
            mishap(consword('FOUND:'), readitem(), consword('READING TO:'),
                                    "=", 4, 'vedset: SYNTAX ERROR')
        endunless;
        pushp(varname);
        if compose then sysCALL("vedset_pcompose") endif;
        true -> compose
    endrepeat;

    if compose then
        ;;; complete the procedure composition and save the result
        pushp(varname), sysCALL("vedset_pcompose");
        sysPOP(sysNEW_LVAR() ->> varname);
        sysPUSH -> pushp
    endif
enddefine;

;;; readcode:
;;;     reads a sequence of characters delimited by white space, or else
;;;     enclosed in POP-11 string quotes

define lconstant readcode();
    lvars c, n;

    define lconstant readchar = nextchar(% readitem %) enddefine;

    define lconstant tmpstring(n);
        ;;; There should be n characters on the stack, for -fill- or
        ;;; -consstring- to pick up
        lconstant
            strings = (lvars i; {% for i to 10 do writeable inits(i) endfor %});
        lvars n;
        if n fi_<= 0 then
            nullstring
        elseif n fi_<= datalength(strings) then
            fill(fast_subscrv(n, strings));
        else
            consstring(n);
        endif;
    enddefine;

    ;;; Strip leading tabs and spaces
    while (readchar() ->> c) == `\s` or c == `\t` do endwhile;

    if c == termin then
        mishap(0, 'vedset: UNEXPECTED END OF FILE');
    elseif c == `\n` then
        ;;; stop at end of line
        false
    elseif c == `'` then
        ;;; read a POP-11 string; return a flag to show no more
        ;;; interpretation needed
        c -> readchar();
        readitem(), true
    else
        ;;; test for end of line comment
        if c == `;` then
            readchar() -> c;
            if c == `;` then
                readchar() -> c;
                if c == `;` then
                    until (readchar() ->> c) == `\n` or c == termin do
                    enduntil;
                    return(false);
                else
                    ;;; wasn't end of line comment
                    c -> readchar();
                    `;` -> readchar();
                endif;
            else
                c -> readchar();
            endif;
            `;` -> c;
        endif;
        ;;; read to next delimiter
        c, 1 -> n;
        until (readchar() ->> c) == `\s` or c == `\t` or c == `\n`
        or c == `'` or c == termin
        do
            c, n fi_+ 1 -> n;
        enduntil;
        c -> readchar();
        tmpstring(n);
    endif
enddefine;

;;; read_vedset_command:
;;;     reads a single vedset command

define lconstant read_vedset_command(type) -> old_graph_used;
    lvars   varname, code, type, len, pushp, runtime_len = false,
            stklen, str, old_graph_used = false, n, attr;
    dlocal  pop_new_lvar_list;

    ;;; read the argument to a 'ctrl' or 'graph' code prefix
    define lconstant readarg(code) -> arg;
        lvars arg, code;
        unless readcode() ->> arg then
            mishap(0, 'vedset: MISSING ARGUMENT TO ' <> code <> ' CODE PREFIX');
        endunless;
        if arg == true then /* POP-11 string */ -> arg endif;
        unless datalength(arg) == 1 then
            mishap(arg, 1, 'vedset: ILLEGAL ARGUMENT TO ' <> code <> ' CODE PREFIX');
        endunless;
    enddefine;

    ;;; convert character -c- to a control character
    define lconstant ctrl(c) -> ctrlc;
        lvars c, ctrlc;
        returnif(c == `?`)(`\^?` -> ctrlc);
        if c == `?` then `\^?` else lowertoupper(c) fi_- `@` endif -> ctrlc;
        unless `\^@` fi_<= ctrlc and ctrlc fi_< `\s` or ctrlc == `\^?` then
            mishap(consstring(`^`,c,2), 1, 'vedset: ILLEGAL CONTROL CHARACTER');
        endunless;
    enddefine;

    ;;; convert character -c- to an old-style graphic character
    define lconstant graph(c) -> graphc;
        lvars c, graphc;
        c fi_+ 16:80 -> graphc;
        if graphc fi_> 16:FF then
            mishap(consstring(`g`,c,2), 1, 'vedset: ILLEGAL GRAPHIC CHARACTER');
        endif;
    enddefine;

    ;;; Read the function name
    vedset_compile_expr(if type == SCREEN then
                            'vvedscreen'
                        elseif type == CHARS or type == GRAPHIC then
                            'vedscreen'
                        else
                            'ved'
                        endif, dup(type == KEYS)) -> (pushp, varname);

    stacklength() -> stklen;

    while readcode() ->> code do
        if code /== true then
            0 -> attr;
            datalength(code) -> len;
            if type == CHARS and len > 1 and subscrs(1,code) == `[`
            and (locchar(`]`, 2, code) ->> n) then
                ;;; char attributes
                strnumber('`\\' <> substring(1, n, code) <> '`') -> attr;
                allbutfirst(n, code) -> code;
                datalength(code) -> len
            endif
        endif;

        if code == true then
            ;;; POP-11 string left on stack
        elseif len == 1 then
            ;;; single character stands for itself
            subscrs(1, code)
        elseif len == 2 and subscrs(1, code) == `^` then
            ;;; control character
            ctrl(subscrs(2, code))
        elseif type == GRAPHIC and len == 2 and subscrs(1,code) == `g` then
            ;;; old graphic character
            true -> old_graph_used;
            graph(subscrs(2, code))
        elseif type == CHARS and len > 1 and subscrs(1,code) == `G` then
            ;;; new graphic character
            strnumber('`\\' <> code <> '`')
        elseif type == KEYS
        and subscrs(1,code) = `(` and subscrs(len,code) = `)` then
            ;;; call wved procedure at run-time to deal with this
            unless runtime_len then
                ;;; first occurrence
                sysCALL("stacklength"), sysPOP(sysNEW_LVAR() ->> runtime_len);
                consstring(stacklength() - stklen) -> str;
                unless str = nullstring then
                    sysPUSHQ(str), sysCALL("explode")
                endunless
            endunless;
            sysCALL(sysPUSHQ(substring(2, len-2, code)),
                                            "wved_vedset_key_chars");
            nextloop
        elseif (uppertolower(code) ->> code) = 'bs' or code = '\\b' then
            `\b`
        elseif code = 'lf' or code = '\\n' then
            `\n`
        elseif code = 'cr' or code = '\\r' then
            `\r`
        elseif code = 'sp' or code = 'space' or code = '\\s' then
            `\s`
        elseif code = 'tab' or code = '\\t' then
            `\t`
        elseif code = 'del' then
            `\^?`
        elseif code = 'esc' then
            `\^[`
        elseif code = 'ctrl' then
            ctrl(subscrs(1, readarg(code)));
        elseif type == GRAPHIC and code = 'graph' then
            true -> old_graph_used;
            graph(subscrs(1, readarg(code)));
        else
            mishap(code, 1, 'vedset: ILLEGAL CHARACTER CODE')
        endif -> code;

        if runtime_len then
            sysPUSHQ(code);
            if isstring(code) then sysCALL("explode") endif
        else
            if isstring(code) then explode(code) else code || attr endif
        endif
    endwhile;

    if runtime_len then
        sysCALL(sysPUSH(runtime_len), sysPUSH("consstring"), "sysanyvecons")
    else
        consdstring(stacklength() - stklen) -> code;
        if type == CHARS or type == GRAPHIC then
            ;;; should be exactly one character in the code
            unless datalength(code) == 1 then
                mishap(0, 'vedset chars: SINGLE CHARACTER REQUIRED')
            endunless;
            fast_subscrdstring(1, code) -> code
        endif;
        sysPUSHQ(code)
    endif;

    if type == KEYS then
        ;;; vedsetkey(code, valof(varname))
        pushp(varname), sysCALL("vedsetkey")
    else
        ;;; code -> valof(varname);
        sysPOP(varname)
    endif
enddefine;

define lconstant do_vedset(type);
    lvars type;
    until pop11_try_nextreaditem("endvedset") do
        if read_vedset_command(type) then
            ;;; old-style graphics char used
            sysprmessage(0, 'vedset: OLD-STYLE GRAPHIC CHARS USED -- pop_character_set WILL BE SET FALSE', 'WARNING -', 1);
            sysPUSHQ(false);
            sysPOP("pop_character_set");
        endif
    enduntil
enddefine;

define vedset__keys();
    if pop11_try_nextreaditem("from_defaults") then
        pop11_try_nextreaditem(";") -> ;
        sysCALL("veddefaultkeys")
    endif;
    do_vedset(KEYS)
enddefine;

define vedset__screen   = do_vedset(% SCREEN %) enddefine;
define vedset__chars    = do_vedset(% CHARS %)  enddefine;

define vedset__graphic();
    printf(';;; NOTE: vedset graphic is now superseded by vedscreengraphtrans,\n');
    printf(';;; which translates a standard set of graphics characters on output.\n');
    printf(';;; Please see \'VED Standard Graphics Characters\' in REF VEDPROCS.\n');

    do_vedset(GRAPHIC)
enddefine;

constant syntax endvedset = pop_undef;
;;;
define syntax vedset;
    lvars type, from_defaults, name, p;
    pop11_try_nextreaditem("from_defaults") -> from_defaults;
    readitem() -> type;
    pop11_try_nextreaditem(";") -> ;
    if from_defaults then
        from_defaults :: proglist -> proglist
    endif;

    consword('vedset__' sys_>< type) -> name;
    sys_autoload(name) -> ;

    if isdefined(name) and isprocedure(valof(name) ->> p) then
        p()
    else
        mishap(type, 1, 'vedset: ILLEGAL KEYWORD');
    endif;
    ;;; Don't require a semicolon after -endvedset-
    ";" :: proglist -> proglist;
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1993
        Allowed ident format like
            ident ENTER deof = esc esc P
--- John Gibson, Nov 15 1992
        Moved out of ved/src into lib/ved so as to make it autoloadable
        for POPC.
--- John Gibson, Aug 25 1992
        Fixed bug in read_vedset_command where it was not dealing properly
        with second or subsequent occurrences of (...).
--- John Gibson, Feb 27 1992
        Rewritten so that vedset <keyword> calls vedset__<keyword>.
        Incorporated XVed "keys" feature via -wved_vedset_key_chars-.
--- Aaron Sloman, Jan 14 1990
        Added declarations at top, and put in sections.
        MOVED from library to system.
--- Rob Duncan, Oct 10 1989
        Tidied up a bit; revised mishap messages to a consistent format.
        Allowed for ";" in "vedset <keyword>;"
--- Andreas Schoter, Sep 11 1989
    Extended to allow quoted formats like
    vedset keys
        "ENTER deof" = esc esc P
    endvedset
    can also cope with formats such as
    vedset keys
        "nextline" <> "nextline" <> "linedelete" = esc esc lf
    endvedset
--- Jason Handby, Aug 11 1989
    Removed addition of vedset and endvedset to -vedopeners- and -vedclosers-
--- Jason Handby, Jul 13 1989
    Extended to cope with  vedset from_defaults .... endvedset
--- Rob Duncan, Jul 10 1989
    Heavily revised.
--- Aaron Sloman, Apr  6 1989
    Extended by Jason Handby to allow formats like
    vedset keys
        nextline <> nextline <> linedelete = esc esc lf
    endvedset
*/
