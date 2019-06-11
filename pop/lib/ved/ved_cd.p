/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 |  File:           $usepop/master/C.all/lib/ved/ved_cd.p
 |  Purpose:        change directory from within ved
 |  Author:         Aaron Sloman, Jun  3 1985 (see revisions)
 |  Documentation:  HELP * CD
 */

section;

define global ved_cd();
    define dlocal prmishap() with_nargs 2;
        ->; ->;
        vederror('CAN\'T CHANGE DIRECTORY TO: ' <> vedargument)
    enddefine;
    vedargument -> current_directory
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Williams, Sep 22 1987
        Re-written to be compatible with -cd- (cf SD BETA 23)
--- Aaron Sloman, Nov  9 1986 changed to use current_directory
--- Aled Morris, Jul 29 1986 - added call to -sysclose- to close the device
    record returned by -readable-
--- Aaron Sloman, Jun  3 1985 - updated to use vederror if there is no
    appropriate directory.
 */
