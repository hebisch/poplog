/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/include/sys_match_filename.ph
 > Purpose:         Match codes for SYS_MATCH_FILENAME
 > Author:          John Williams, Aug 14 1983 (see revisions)
 > Documentation:   REF * SYSIO
 > Related Files:   LIB * SYS_FILE_MATCH
 */

#_TERMIN_IF DEF SYS_MATCH_FILENAME_INCLUDED

section;

iconstant macro (
    M_CHAR         =    1,       ;;; single character
    M_STRING       =    2,       ;;; string of characters
    M_NCHARS       =    3,       ;;; fixed number of characters
    M_STAR         =    4,       ;;; zero or more characters
    M_VERS         =    5,       ;;; matching start of version
    M_OR           =    6,       ;;; OR node
    M_XOR          =    7,       ;;; exclusive OR node
    M_NOT          =    8,       ;;; NOT node
    M_CHAR_RANGE   =    9,       ;;; range of characters
    );

iconstant SYS_MATCH_FILENAME_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 30 1992
        Added #_TERMIN_IF
--- John Gibson, Jun  5 1989
        Made iconstants
--- John Gibson, Jan 29 1989
        Put inside normal_compile ... end_normal_compile so useable
        with popc
--- John Williams, Feb  5 1986
         removed 'section' brackets, so macros are only available in
         (and below) the section including them
*/
