/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/sysfileowner.p
 > Purpose:         Get/set user id of file
 > Author:          John Williams, May 13 1997
 > Documentation:   REF * sysfileowner
 > Related Files:
 */

section;

compile_mode:pop11 +strict;

define sysfileowner(file);
    lconstant buf = writeable initv(4);
    if sys_file_stat(file, buf) then
        fast_subscrv(4, buf)
    else
        mishap(file, 1, '%CAN\'T GET FILE OWNER (%M)')
    endif
enddefine;


exload sysfileowner
(language C)
    lconstant chown(file, uid, gid) :int;
endexload;


define updaterof sysfileowner(uid, file);
    unless exacc chown(file, uid, -1) == 0 do
        mishap(uid, file, 2, '%CAN\'T SET FILE OWNER (%M)')
    endunless
enddefine;


endsection;
