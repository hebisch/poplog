/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_translate_exlibs.p
 > Purpose:         Translate exlib envvar & parse xlink options
 > Author:          John Gibson, May  7 1993 (see revisions)
 > Documentation:   REF * X, * SYSTEM
 */
compile_mode :pop11 +strict;

section;

;;; warn:
;;;     display diagnostic messages

define lconstant warn(msg);
    lvars msg, args = [];
    dlocal cucharout = charerr, pr = sys_syspr, pop_pr_quotes = false;
    if islist(msg) then ((), msg) -> (msg, args) endif;
    sysflush(pop_charerr_device); sysflush(pop_charout_device);
    printf('!!! Warning - %s\n', [^msg ^^args]);
    sysflush(pop_charerr_device); sysflush(pop_charout_device);
enddefine;

;;; skipto, skipover:
;;;     string utilities for parsing option lines

define lconstant skipto(cs, i, s) -> i;
    lvars cs, i, s, lim = datalength(s);
    until i > lim or locchar(subscrs(i, s), 1, cs) do
        i + 1 -> i;
    enduntil;
enddefine;

define lconstant skipover(cs, i, s) -> i;
    lvars cs, i, s, lim = datalength(s);
    until i > lim or not(locchar(subscrs(i, s), 1, cs)) do
        i + 1 -> i;
    enduntil;
enddefine;

;;; xlink_options:
;;;     parse a user-specified X link qualifier of the form
;;;         <TYPE> ["/" <VERSION>] [ ":" <ARGUMENTS> ]

define lconstant xlink_options(qualifier) -> (type, version, libs);
    lvars i, j, qualifier, type, version = false, libs = nullstring;

    ;;; <TYPE>
    skipto('\s\t:/', 1, qualifier) -> i;
    uppertolower(substring(1, i-1, qualifier)) -> type;
    if type = nullstring then
        ;;; nothing before the ":" or "/"
        warn('missing X link type defaults to \'%s\'', ['user']);
        'user' -> type;
    endif;
    skipover('\s\t', i, qualifier) -> i;

    ;;; <VERSION>
    if issubstring_lim('/', i, i, false, qualifier) then
        skipover('\s\t', i+1, qualifier) -> i;
        skipto('\s\t:', i, qualifier) -> j;
        unless strnumber(substring(i, j-i, qualifier)) ->> version then
            ;;; nothing after the "/"
            warn('missing X link version number');
        endunless;
        skipover('\s\t', j, qualifier) -> i;
    endif;

    ;;; <ARGUMENTS>
    if issubstring_lim(':', i, i, false, qualifier) then
        skipover('\s\t', i+1, qualifier) -> i;
        allbutfirst(i-1, qualifier) -> libs;
        if libs = nullstring then
            ;;; nothing after the ":"
            warn('missing X link arguments');
        endif;
    elseif i <= datalength(qualifier) then
        ;;; trailing information unaccounted for
        mishap(qualifier, 1, 'badly formed X link qualifier');
    endif;
enddefine;

define sys_translate_exlibs(varname) /* -> (type, version, libs) */;
    lvars   varname, type = false, version = false, libs, trans, varstr,
            cons_p = identfn;
    lconstant cache = newmapping([], 4, false, false);


    if cache(varname) ->> trans then
        returnunless(trans == true) (explode(trans));
        mishap(varname, 1, 'TRANSLATION FOR EXTERNAL LIBLIST ENV VAR IS RECURSIVE')
    endif;

    varname -> varstr;
    if isword(varname) then
        fast_word_string(varname) -> varstr;
        consword -> cons_p
    endif;

    if varstr = 'POP_XLINK_EXLIBDIRS' or varstr = 'POP_XLINK_EXLIBFILES' then
        ;;; map these to the value of XLINK_EXLIBDIRS/FILES
        valof(subword(5, datalength(varstr)-4, varstr)) -> libs;
        if isundef(libs)
        or libs = ['==POP_XLINK_EXLIBDIRS']
        or libs = ['==POP_XLINK_EXLIBFILES']
        then
            mishap(varstr, 1, 'SYSTEM IS NOT LINKED WITH X (trying to translate dummy xlink env var)')
        elseif isprocedure(libs) then
            ;;; run it to get the value -- used by poplink
            libs() -> libs
        endif
    else
        unless systranslate(varstr) ->> trans then
            mishap(varname, 1, 'NO TRANSLATION FOR EXTERNAL LIBLIST ENV VAR')
        endunless;
        if isstartstring('==', trans) then
            ;;; just dereferences to another
            procedure;
                dlocal %cache(varname)% = true;     ;;; catch recursion
                sys_translate_exlibs(cons_p(allbutfirst(2, trans)))
            endprocedure() -> (type, version, libs)
        else
            if isstartstring('x=', trans) then
                ;;; xlink specification
                xlink_options(allbutfirst(2,trans)) -> (type, version, libs)
            else
                trans -> libs
            endif;
            [% sys_parse_string(libs, cons_p) %] -> libs        ;;; liblist
        endif
    endif;

    consvector(type, version, libs, 3) -> cache(varname);
    (type, version, libs)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 14 1993
        Removed all the defaults stuff.
 */
