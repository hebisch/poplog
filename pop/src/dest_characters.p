/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/dest_characters.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PRINT
 */

;;; ----------- STACK THE PRINTED CHARACTERS OF AN ITEM ------------------

#_INCLUDE 'declare.ph'

vars
        pop_pr_ratios
    ;

;;; ---------------------------------------------------------------------

    ;;; Stack the characters that would be produced by printing the
    ;;; item in standard format
define dest_characters() with_nargs 1;
    dlocal
        cucharout           =   identfn,
        pop_pr_level        =   1000,
        pop_pr_quotes       =   false,
        pop_pr_radix        =   10,
        weakref pop_pr_places   =   6,
        weakref pop_pr_exponent =   false,
        weakref pop_pr_ratios   =   true,
        ;

    sys_syspr()
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
