/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_file_exists.p
 > Purpose:         Test whether a given file exists
 > Author:          John Williams, Mar 12 1993 (see revisions)
 > Documentation:   REF * sys_file_exists
 > Related Files:   LIB * NULLVECTOR
 */

section;

compile_mode:pop11 +strict;

define global sys_file_exists(file);
    sys_file_stat(file, nullvector) and true
enddefine;

endsection;
