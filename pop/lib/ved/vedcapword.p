/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/vedcapword.p
 > Purpose:         Capitalise first letter of current word
 > Author:          Aaron Sloman, Jan  3 1988 (see revisions)
 > Documentation:   HELP * VEDEMACS
 > Related Files:   LIB * VEDEMACS
 */
compile_mode :pop11 +strict;

section;

;;; in LIB VEDEMACS this is assigned to ESC-c
define vedcapword;
    ;;; change first character of current word to upper case
    ;;; and remainder to lower case. Move to right of current word.
    lvars char, linea,cola, lineb,colb;
    repeat
        quitif(vedcolumn == 1);
        vedcurrentchar() -> char;
        quitunless(char == `_` or isalphacode(char));
        vedcharleft();
    endrepeat;
    until isalphacode(vedcurrentchar()) do
        vedcharright();
        if vedcolumn > vvedlinesize then
            vedchardown();
            1 -> vedcolumn
        endif;
    enduntil;

    vedline -> linea; vedcolumn -> cola;
    vedwordright();
    vedline -> lineb; vedcolumn -> colb;
    vedjumpto(linea,cola);
    lowertoupper(vedcurrentchar()) -> vedcurrentchar();
    vedcharright();
    until vedcolumn == colb and vedline == lineb do
        uppertolower(vedcurrentchar()) -> vedcurrentchar();
        vedcharright();
    enduntil;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  4 1988
    Fixed to change characters after first one to lower case
 */
