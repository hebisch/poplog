/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/ODBC_EXLIBS.p
 > Purpose:         External libraries needed for ODBC
 > Author:          Robert Duncan, Jan  7 1997
 > Documentation:
 > Related Files:   LIB * ODBC
 */
compile_mode :pop11 +strict;

include sysdefs.ph;

section;

vars ODBC_EXLIBS =
    #_IF DEF UNIX
        ['-lodbc']
    #_ELSEIF DEF WIN32
        [odbc32]
    #_ELSE_ERROR
    #_ENDIF
;

endsection;
