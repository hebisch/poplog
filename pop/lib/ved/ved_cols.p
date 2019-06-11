/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_cols.p
 > Purpose:         Toggle value of vedstatusshowcols
 > Author:          John Williams, November 1990 (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * TRYSETVALOF
 */
compile_mode :pop11 +strict;

section;

lconstant macro LOCAL = [is_vedfile_local(ident vedstatusshowcols,
                                                ved_current_file)];

define vars ved_cols;
    vedputmessage(if not(LOCAL) ->> LOCAL ->> vedstatusshowcols then
                    '\{b}column number display'
                  else
                    '\{b}line number display'
                  endif)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 21 1994
        Made vedstatusshowcols be local to each file when true
 */
