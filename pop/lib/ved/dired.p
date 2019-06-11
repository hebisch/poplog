/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.unix/lib/ved/dired.p
 > Purpose:         Macro to invoke ved_dired from outside VED
 > Author:          Aaron Sloman, Aug 11 1989
 > Documentation:   HELP * DIRED
 > Related Files:   LIB * VED_DIRED and files referred to there
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses ved_dired;

define global macro dired;
    lvars spec;
    readstringline() -> spec;
    if spec = nullstring then
        mishap(0, 'NO ARGUMENT GIVEN TO DIRED')
    else
        ;;; stick procedure to set up and run dired in VED input stream
        vedinput(
            procedure;
                ;;; in temporary file. Run ved_qdired with appropriate args
                'qdired ' sys_>< spec -> vedcommand;
                vedputcommand(vedcommand);
                veddocommand();
            endprocedure);
        vededit($-dired$-dired_tempfile(spec), vedhelpdefaults);
    endif;
enddefine;

endsection;
