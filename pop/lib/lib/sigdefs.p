/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/lib/sigdefs.p
 > Purpose:         Defining signal numbers
 > Author:          Roger Evans, Sep 21 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   LIB SYSDEFS
 */

pop11_compile('$usepop/pop/lib/include/sigdefs.ph');

constant $-sigdefs = true;          ;;; to make 'uses' work


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 20 1989
        Moved definitions of signals to lib/include/sigdefs.ph
--- Roger Evans, Oct 12 1988
        sectioned and made declarations global and added variable -sigdefs-
 */
