/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/raster_flavour.p
 > Purpose:        a class for representing a raster (part of a window)
 > Author:         Mark Rubinstein, Jul  8 1986
 > Documentation:  HELP WINDOW_FLAVOURS /raster_flavour
 > Related Files:
 */

section;

;;; -- RASTER --------------------------------------------------------------
;;; a flavour definition for a raster of a selected portion of a raster
flavour raster;
ivars width height data_string;
endflavour;

endsection;
