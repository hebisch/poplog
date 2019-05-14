/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.unix/lib/auto/sys_search_unix_path.p
 > Purpose:         Search a $PATH-style directory list for file
 > Author:          John Williams, Mar 30 1990 (see revisions)
 > Documentation:   REF * SYS_SEARCH_UNIX_PATH
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define sys_search_unix_path(file, PATH) -> path;
    lvars i, j;
    1 -> i;
    while (locchar(`:`, i, PATH) ->> j) do
        (substring(i, j - i, PATH) dir_>< file) -> path;
        returnif(sys_file_exists(path));
        j + 1 -> i
    endwhile;
    allbutfirst(i - 1, PATH) dir_>< file -> path;
    returnif(sys_file_exists(path));
    false -> path
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar 18 1993
        Now uses sys_file_exists instead of sys_file_stat.
--- Simon Nichols, Nov  5 1991
        Fixed bug where anything after the last colon in PATH was ignored.
 */
