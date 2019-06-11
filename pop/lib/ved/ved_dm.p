/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_dm.p
 >  Purpose:        define a ved macro.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * VEDMACROS
 >  Related Files:  LIB * VED_DK
 */

;;; To define a ved macro called foo, which when run will give the command
;;;     ENTER gs/dfine/define
;;; type ENTER DM FOO, then go through the key sequence corresponding to
;;; giving the command. I.e. press ENTER then GS/DFINE/DEFINE then RETURN
;;; then terminate the macro definition by pressing ESC three times.
;;; The file VEDINIT.P will be automatically extended to include your
;;; new macro, unless you say you don't want it to be permanent
section;

define global ved_dm();
lvars vedfile vedflag;
    if vedargument = nullstring then vederror('NO NAME GIVEN TO DM') endif;
    vedputmessage('WANT IT TO BE PERMANENT? y/n');
    vedscreenbell();
    vedinascii() -> vedfile;
    if vedfile == `y` or vedfile == `Y` then
        true -> vedflag;
        sysfileok('$poplib/vedinit.p')
    else
        false -> vedflag;
        systmpfile(false, 'dm', '.p')
    endif -> vedfile;
    vedsetonscreen(vedopen(vedfile),'DEFINING NEW MACRO');
    false -> vedbreak;
    vedendfile();
    ;;; insert procedure heading
    vedinsertstring('\n;;;DEFINED BY ENTER DM\ndefine ved_'); vedinsertstring(vedargument);
    vedinsertstring(';\nPRESS KEY SEQUENCE FOR MACRO. EDIT WITH <ESC> DEL. FINISH WITH <ESC> 3 times ');

    vedpositionpush();
    ;;; start a call of vedinput
    vedinsertstring('\n\tvedinput(\'');
    ;;; read in codes for the string of input
    vedinkeys(false);
    ;;; finish the call of vedinput and finish procedure definition.
    vedinsertstring('\');\nenddefine;\n');
    vedpositionpop();
    vedlinedelete();
    ;;; load the procedure
    ved_lcp();
    ;;; return to previous file
    if vedflag then ved_wq() else false -> vedchanged; ved_clear(); ved_rrq()
    endif;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jun  8 1991
        Uses vedusewindows == "pw*m"
--- Aaron Sloman, Apr  4 1989
    Made vedbreak false to fix bug reported by Jason Handby
--- Aaron Sloman, Feb  5 1989
    added call of -sysfileok- to expand $poplib/vedinit.p
 */
