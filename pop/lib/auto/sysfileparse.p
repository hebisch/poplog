/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:           C.unix/lib/auto/sysfileparse.p
 > Purpose:        Parse Unix filename into parts
 > Author:         Aaron Sloman, October 1982 (see revisions)
 > Documentation:  HELP * SYSFILEPARSE
 > Related Files:  sysfilefield, and sysfile<component> for each component
 >                 also VMS version in C.vms
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

;;; Given a string representing a file name, return a vector of strings:
;;; host, disk, directory, name, type, version


section;

define global sysfileparse(string);
    lvars a, i, z, string, host, dir, name, type, version, hostchar;

    if isinteger(string) then
        string -> hostchar;
        -> string
    else
        `!` -> hostchar
    endif;
    sysfileok(string) -> string;
    datalength(string) -> z;

    define lconstant Substr(a, z, string);
        lvars a, z, string;
        if a fi_> z then
            nullstring
        else
            substring(a, (z fi_- a) fi_+ 1, string)
        endif
    enddefine;

    /* --- LOOK FOR HOST --- */
    if locchar_back(hostchar, locchar(`/`, 1, string) or z, string) ->> a then Substr(1, a, string);
        a fi_+ 1
    else
        nullstring, 1
    endif -> a -> host;

    /* --- LOOK FOR VERSION --- */
    if (skipchar_back(`-`, z, string) ->> i) then
        Substr(i fi_+ 1, z, string);
        i -> z
    else
        nullstring
    endif -> version;

    /* --- LOOK FOR DIR --- */
    if (locchar_back(`/`, z, string) ->> i) then
        if i == a then '/' else Substr(a, i fi_- 1, string) endif,
        i fi_+ 1 -> a
    else
        nullstring
    endif -> dir;

    /* --- LOOK FOR NAME AND TYPE --- */
    if (locchar_back(`.`, z, string) ->> i)
    and (i /== z and i fi_>= a) then
        Substr(a, i fi_- 1, string),
        Substr(i fi_+ 1, z, string)
    else
        Substr(a, z, string),
        nullstring
    endif -> type -> name;

    {% host, nullstring, dir, name, type, version %}
enddefine;


define global sysfilefield(string, n);
    lvars string, n;
    subscrv(n, sysfileparse(string))
enddefine;


endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 28 1989
        Now works correctly when dir = /
--- John Gibson, Jul  1 1989
        Added +strict etc
--- John Williams, Mar 15 1988
        Optional second argument to specify host delimiter
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
 */
