/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/mkvedimage.p
 > Purpose:         Create a standard saved image with VED libraries loaded
 > Author:          John Williams, Jan 24 1990 (see revisions)
 > Documentation:   HELP * MKVEDIMAGE
 > Related Files:   C.unix/com/mkv55 etc
 */

#_TERMIN_IF DEF POPC_COMPILING

['-ved' ^^poparglist] -> poparglist;
compile('$popliblib/mkimage.p');

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
--- Robert John Duncan, May 20 1991
        Now just a synonym for LIB MKIMAGE with option '-ved'
--- John Williams, Jan  4 1991
        Now does max(100000, popmemlim) -> popmemlim
--- John Williams, Dec  6 1990
        Added weak declaration of -sysxsetup-
--- John Williams, Oct 29 1990
        Now parses special flags (%x etc)
--- John Williams, May 21 1990
        Now sets -popmemlim- to 100000
 */
