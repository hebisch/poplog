/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/lib/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, May 30 1993
 */
compile_mode :pop11 +strict;

uses-now popxved;

library_declare_section '$usepop/pop/x/ved/lib/'

section;

weak vars
        vedxvedmenu,
        vedxvedmenubar,
    ;

weak vars procedure (
        vedxvedinit,
        vedxvedkeys,
        vedxvedmouse,
    );

endsection;

section $-xved;

weak constant procedure (
        create_parent_xm,
        create_parent_xol,
    );

weak constant
        menubar_xm,
        menubar_xol,
        scrollbar_xm,
        scrollbar_xol,
    ;

endsection;

end_library_declare_section;
