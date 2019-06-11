/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:           C.all/lib/ved/ved_uses.p
 > Purpose:        Ved command to use library
 > Author:         John Gibson, May 15 1993
 > Documentation:  REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_uses;
    lvars old_loadlib = loadlib, loaded = false;

    define dlocal prmishap(item);
        lvars item;
        unless isstring(item) then -> item; endunless;
        vederror(item);
    enddefine;

    define dlocal loadlib();
        true -> loaded;
        vedputmessage('LOADING LIB ' sys_>< vedargument);
        chain(old_loadlib)
    enddefine;

    useslib(vedargument);
    unless loaded then vedputmessage('ALREADY LOADED') endunless;
enddefine;

endsection;
