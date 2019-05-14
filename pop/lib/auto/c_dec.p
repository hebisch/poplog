/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 > File:            C.all/lib/auto/c_dec.p
 > Purpose:         C interface for LIB EXTERNAL
 > Author:          Aled Morris and Robert James Duncan (see revisions)
 > Documentation:   HELP * EXTERNAL
 > Related Files:   LIB * FORTRAN_DEC, LIB * EXTERNAL
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF c_dec

uses external;

section $-external => c_dec;

lvars vars_types, variable_name, procedure_header; ;;; used non-locally

define lconstant typedef_ident = newassoc([]) enddefine;

define lconstant read_type();
lvars   w       = nextreaditem(),
        size    = false,
        type    = false,
        ;
lconstant err = mishap(% 2, 'C ERROR: Invalid type specifier' %)
        ;
    if w == "unsigned" then
        npr(';;; C WARNING: "unsigned" not supported - signed is assumed');
        readitem() ->;
        nextreaditem() -> w;
    endif;
    if lmember(w, [short long]) then
        readitem() -> size;
        nextreaditem() -> w;
    endif;
    if lmember(w, [char int float double])
    or (procedure_header and w == "void") then
        readitem() -> type;     ;;; same as "w -> type", i.e. flush proglist
    elseif typedef_ident(w) ->> type then
        readitem() ->;
        if size then err(size, type) else return(type) endif;
    endif;

    unless type then "int" -> type endunless;

    if size == "long" and type == "float" then
        false -> size; "double" -> type;
    endif;

    if size then
        if size == "short" and type /== "int" then
            err("short", type)
        elseif type /== "int" then      ;;; size == "long" is implied
            err("long", type);
        endif;
    endif;

    conspair(type, []);
enddefine;

define lconstant parse_array() -> s;
lvars x, s = false;
    if nextreaditem().isinteger then
        readitem() -> s;            ;;; first dimension (optional in C)
    endif;
    pop11_need_nextreaditem("]") ->;
    while pop11_try_nextreaditem("[") do
        if nextreaditem().isinteger then
            readitem() -> x;
            if s then s * x -> s endif
        else
            mishap(0, 'C ERROR: Underspecified array dimensions');
        endif;
        pop11_need_nextreaditem("]") ->;
    endwhile;
enddefine;

lconstant procedure read_one; ;;; forward ref

define lconstant parse_wordtype();
lvars w = nextreaditem();
    if pop11_try_nextreaditem("(") then
        read_one();
        pop11_need_nextreaditem(")") ->;
    else
        readitem() -> variable_name;
        [];
    endif;
enddefine;

define lconstant read_vartype();
lvars wordtype size;
    unless (parse_wordtype() ->> wordtype) then return(false) endunless;
    if pop11_try_nextreaditem("(") then
        if procedure_header == true then
            [%
                unless nextreaditem() == ")" then
                    repeat
                        readitem();
                    quitunless(pop11_try_nextreaditem(","));
                    endrepeat;
                endunless;
            %] -> procedure_header;
        elseif procedure_header then
            mishap(0, 'C ERROR: Invalid function header');
        endif;
        pop11_need_nextreaditem(")") ->;
        wordtype <> #_< [function] >_#;
    elseif pop11_try_nextreaditem("[") then
        parse_array() -> size; /* or false */
        wordtype <> [array ^size]
    else
        wordtype
    endif;
enddefine;

define lconstant read_one();
lvars var;
    if pop11_try_nextreaditem("*") then
        if read_one() ->> var then var <> #_< [pointer] >_# else false endif;
    else
        read_vartype();
    endif;
enddefine;

define lconstant read_vars();
lvars   type vartype;
dlocal  variable_name, procedure_header = false;
    read_type() -> type;
    repeat
        read_one() -> vartype;
        variable_name :: ((vartype <> type) :: vars_types) -> vars_types;
    quitunless(pop11_try_nextreaditem(","));
    endrepeat;
    pop11_need_nextreaditem(";") ->;
enddefine;

define read_pdr_header() -> procedure_header -> pdrspec -> variable_name;
lvars   pdr_type
        pdrspec
        ;
dlocal  variable_name,
        procedure_header = true,
        ;
    read_type() -> pdr_type;
    read_one() -> pdrspec;
    unless hd(pdrspec) == "function" then
        mishap(0, 'C ERROR: Invalid function header');
    endunless;
    tl(pdrspec) <> pdr_type -> pdrspec;
enddefine;

define read_typedef();
lvars   ttype trest
        ;
dlocal  variable_name,
        procedure_header = false,
        ;
    read_type() -> ttype;
    read_one() -> trest;
    pop11_need_nextreaditem(";") ->;
    trest <> ttype -> typedef_ident(variable_name);
enddefine;

/* the exported main procedure */

define vars c_dec;
lvars   variables returns pdr_name
        varname vartype pos
        ;
dlocal  % item_chartype(`*`) % = 5,
        vars_types,
        ;
    until pop11_try_nextreaditem("endexternal") do
        if pop11_try_nextreaditem("typedef") then
            read_typedef();
        nextloop;
        endif;

        [] -> vars_types;
        read_pdr_header() -> variables -> returns -> pdr_name;

        until pop11_try_nextreaditem("{") do
            read_vars();
        enduntil;
        pop11_need_nextreaditem("}") ->;

        until vars_types == [] do
            destpair(vars_types) -> vars_types -> varname;
            destpair(vars_types) -> vars_types -> vartype;

            if lmember(varname, variables) ->> pos then
                vartype -> front(pos);
            else
                mishap(varname, 1,
                'C ERROR: Redeclaration or attempt to declare non argument');
            endif;
        enduntil;

        {%
            applist(variables,  procedure (word);
                                lvars word;
                                    if word.isword then [int] else word endif;
                                endprocedure)
        %} -> vars_types;

        external_import(#_<consref("C")>_#, pdr_name, vars_types, returns,
                                false, false /* cbr */);

    enduntil;
enddefine;

endsection;     /* $-external */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 15 1994
        Removed buggy casting of "float" to "double" at end of read_type
--- John Gibson, Dec 21 1993
        Changed to work with new lib external. Runtime parts moved to
        lib external_runtime.
--- Robert John Duncan, Aug 11 1992
        Extended last change for SVR4, which doesn't use COFF files any more
        but still doesn't add a leading underscore.
--- Simon Nichols, Dec 11 1991
        Changed -c_dec- not to add a leading underscore to -pdr_name- if
        COFF is defined.
--- John Gibson, Aug 13 1989
        Replaced old sys- procedures with pop11_ ones.
--- Aled Morris, May 31 1988 - SFR 4/89 (Anthony Worrall, Reading)
        bug in parse_array fixed - in multi-dimensional arrays, it is the
        first dimension which is permitted to be absent.  Also fixed long
        standing bug in declarations of C double precision f.p. numbers.
        I think it conforms to K&R now.
--- Aled Morris, Sep 15 1987 - numerous minor Beta test bug fixes. Added
        array of short.
--- Rob Duncan 6th April, 1987 - took out annoying printing.
 */
