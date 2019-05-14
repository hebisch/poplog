/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/pdcomp.p
 *  Purpose:        Joins two procedures.  Superseded by <>
 *  Author:         J.Gibson (see revisions)
 *  Documentation:  HELP * PDCOMP
 *  Related Files:
 */

;;; Given two procedures return a third, which when run calls first one
;;; then the other.
;;; From Version 7.1 onwards, use <> instead of pdcomp

section;

define global 3 pdr1 pdcomp pdr2 -> pdr3;
lvars pdr1 pdr2 pdr3;
    unless pdr1.isprocedure and pdr2.isprocedure then
        mishap(pdr1, pdr2, 2, 'NON-PROCEDURE(S) FOR PDCOMP')
    else
        pdr1 <> pdr2 -> pdr3;
        pdprops(pdr2) -> pdprops(pdr3);
        if updater (pdr2) .isprocedure then
            pdr1 pdcomp updater(pdr2) -> updater(pdr3);
        endif;
    endunless;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectioned.
 */
