/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/mklisprefindex.p
 > Purpose:         Make index for Lisp REF files
 > Author:          John Williams, Jan  5 1990 (see revisions)
 > Documentation:   HELP * MKREFINDEX
 > Related Files:   LIB * MKREFINDEX
 */

section;

uses mkrefindex;

define global mklisprefindex() with_nargs 1;

    define dlocal description_hdr_name(line);
        lvars i, j, len, line;
        if fast_subscrs(1, line) == `(` then
            datalength(line) -> len;
            2 -> i;
            min(locchar(`\s`, 1, line) or len,
                locchar(`)`, 1, line) or len)
                -> j;
        else
            1 -> i;
            locchar(`\s`, 1, line) -> j;
        endif;
        uppertolower(substring(i, j - i, line));
    enddefine;

    mkrefindex();
enddefine;


/* Automatic startup if loaded from the command line */

if poparg1 and sys_fname_nam(poparg1) = 'mklisprefindex' then
    1 ->> popsyscall -> pop_file_versions;
    ['mklisprefindex' ^^poparglist] =>
    applist(poparglist, mklisprefindex);
endif;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 19 1995
        Now declares mklisprefindex as global. Also checks if poparg1 is false.
--- John Williams, Jul  1 1993
        Revised for standard REF file format.
--- John Williams, Jan 20 1993
        Now uses poparg1 in startup code.
--- Robert John Duncan, Apr 30 1991
        Startup code copied from LIB MKREFINDEX.
--- John Williams, Nov 14 1990
        Re-written using the new user-tailorable LIB MKREFINDEX
--- John Williams, Aug 24 1990
        Now only stores filename in index; directory is added at search
        time by -sys_search_doc_index-. Allows 'ref' directories to be
        relocated.
--- John Williams, May  1 1990
        Now tests -poparglist0-
--- John Williams, Apr  3 1990
        Changed -sysfullfilename- to -sys_fname_name-
 */
