/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/ved/auto/ved_xved.p
 > Purpose:         Set XVed application settings
 > Author:          Jonathan Meyer, Jul  2 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

#_IF not(DEF xved or DEF POPC_COMPILING)
vederror('XVed not loaded');
#_ENDIF

section;

define vars ved_xved;
    lvars   arg = vedargument, name, value, loc = "application",
            queries = nullstring, names = [];
    dlocal  proglist_state;

    define dlocal prmishap(string, culprits);
        lvars string, culprits;
        lvars culprits_string = nullstring;

        unless culprits == [] then
            allbutfirst(1, allbutlast(1, culprits sys_>< nullstring)) sys_>< ': '
                -> culprits_string;
        endunless;

        vederror(culprits_string sys_>< string);

    enddefine;

    if vedusewindows /== "x" then
        vederror('NOT RUNNING XVed');
    elseif vedargument == vednullstring then
        vederror('NAME and VALUE pair needed');
    endif;

    proglist_new_state(sysparse_string(vedargument)) -> proglist_state;
    consword(nextreaditem() sys_>< nullstring) -> arg;

    if arg == "window" then
        ;;; specifying a window setting
        readitem()->;
        consword(nextreaditem() sys_>< nullstring) -> arg;
        if lmember(arg,[current default next]) then
            readitem()->;
            arg <> "Window" -> loc;
        else
            "currentWindow" -> loc;
        endif;
    elseif arg == "application" then
        ;;; ignore this -
        readitem()->;
    endif;

    until null(proglist) do
        consword(readitem() sys_>< nullstring) -> name;
        readitem() -> value;
        if value == termin then
            '?' -> value;
            ;;; vederror('VALUE NEEDED WITH ' sys_>< name);
        endif;
        if value = 'true' or value = 'false' then
            value = 'true' -> value;
        endif;
        if value = '?' then
            queries sys_>< xved_value(loc, name) sys_>< space -> queries
        else
            value;
            name :: names -> names
        endif;
    enduntil -> xved_value(loc, rev(names));
    unless queries == nullstring then
        vedputmessage(allbutlast(1, queries));
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 21 1992
        Changed to use new feature of xved_value allowing multiple
        values to be updated simultaneously.
--- Jonathan Meyer, Jul  7 1991
        Moved code and comments in from ved_window.p.
        Made missing VALUE on last arg print value instead of doing vederror
--- Adrian Howard, Jul  7 1991
        - fixed bug caused when you had a mishap with 0 culprits
        - fixed bug caused by non-strings in sysparse_string(vedargument)
        - added ability to do <ENTER> window ? to get screen length
--- Jonathan Meyer, Jul  2 1991
        Added "application" type
--- Adrian Howard, Jun 26 1991 : Added option to query XVed attributes and
        trapped possible bad value mishaps - NOTE: this should really catch
        Xt warnings as well, but you can't at the moment
 */
