/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_dounix.p
 > Purpose:         Obey a shell command from the Ved buffer
 > Author:          Aaron Sloman, Oct  8 1997
 > Documentation:   HELP * DOUNIX
 > Related Files:
 */


section;



define ved_dounix;
    ;;; make sure any output goes into a new file, not the status line
    dlocal show_output_on_status = false;

    ;;; Read the current line and send it as a unix command, like
    ;;;     ENTER unix <command>
    veddo('unix ' sys_>< vedthisline())
enddefine;

endsection;
