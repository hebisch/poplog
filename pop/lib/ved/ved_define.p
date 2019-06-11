/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_define.p
 >  Purpose:        Insert a template for a procedure definition.
 >  Author:         A.Sloman 1982
 >  Documentation:  HELP * VEDCOMMS/ved_define
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_define;
    vedlinebelow(); vedlinebelow();
    vedpositionpush();
    vedinsertstring('define NAME (PARAMETERS) -> OUTPUT;\n\tvars VARIABLES;\nenddefine;\n');
    vedpositionpop();
enddefine;

endsection;
