/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XPW_EXLIBS.p
 > Purpose:         External library list for Poplog widgetset
 > Author:          John Gibson, Mar 31 1993 (see revisions)
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

include sysdefs.ph;

section;

vars XPW_EXLIBS = [%

#_IF DEF UNIX
    '-lXpw',

#_ELSEIF DEF VMS
  #_IF DEF ALPHA
    'pop$libxpw:/share',        ;;; "pop$libxpw" is defined as a logical name
  #_ELSE
    'popexternlib:libxpw.olb/library',
  #_ENDIF

#_ELSE_ERROR
#_ENDIF

    ident XLINK_EXLIBS
%];

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 19 1994
        Added Alpha shareable image
--- John Gibson, Nov 15 1993
        Changed Unix version to just use -lXpw (all link commands now specify
        -L$popexternlib automatically)
--- John Gibson, Nov 13 1993
        Changed Unix version to use -L$popexternlib -lXpw since otherwise
        with shareable libraries the value of $popexternlib at exload time
        will be frozen into saved images (instead of being picked up from
        LD_LIBRARY_PATH at restore time).
--- John Gibson, Jul 22 1993
        Changed extension on libXpw
--- John Gibson, Jul 22 1993
        Changed Unix extension of libXpw to .a instead of .olb
 */
