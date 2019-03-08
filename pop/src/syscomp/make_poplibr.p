/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/make_poplibr.p
 > Purpose:
 > Author:          John Gibson, Dec 24 1987 (see revisions)
 */

/* -------------------------------------------------------------------------

                      POP SYSTEM LIBRARY UTILITY

--------------------------------------------------------------------------*/

printf('Loading POPLIBR Version 14.22\n');
if pop_internal_version < 136601 then
    mishap(0, 'POPLIBR VERSION NEEDS POPLOG VERSION 13.6601 OR LATER')
endif;

unless isboolean(pop_debugging) then true -> pop_debugging endunless;
pop_debugging -> popgctrace;
if pop_debugging then 1 -> popsyscall endif;

sys_unlock_heap();
true -> pop_record_writeable;

load lib.p
compile(pop_architecture >< '/sysdefs.p');
load mcdata.p
load files.p
load wordflags.p
compile(pop_architecture >< '/asmout.p');
load os_comms.p
load w_util.p
load w_module.p
load poplibr_main.p

section $-Popas => make_saved_image;

define make_saved_image(name);
    lvars name;
    if sys_lock_system(name, 2:011, 'POPLIBR') then
        $-Pop$-Main(), sysexit()
    endif
enddefine;

unless pop_debugging then
    section_cancel(current_section, false)
endunless;

endsection;


sysgarbage();
sys_lock_heap();



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 21 1995
        Added load for mcdata.p
--- John Gibson, May 18 1990
        Version 13.8
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Loads os_comms.p
--- John Gibson, Dec  5 1988
        wcommon.p replaced by w_util.p and w_module.p
--- John Gibson, Jun 24 1988
        Renamed make_poplibr.p (previously poplibr.p)
 */
