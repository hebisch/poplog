/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/make_poplink.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                        POP SYSTEM LINKER

--------------------------------------------------------------------------*/

printf('Loading POPLINK Version 14.22\n');
if pop_internal_version < 136601 then
    mishap(0, 'POPLINK VERSION NEEDS POPLOG VERSION 13.6601 OR LATER')
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
load poplink_main.p

section $-Popas => make_saved_image;

define make_saved_image(name);
    lvars name;
    if sys_lock_system(name, 2:011, 'POPLINK') then
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
--- John Gibson, Jan  7 1990
        Version 13.7 for new pop pointers
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.6
--- John Gibson, Dec  5 1988
        wcommon.p replaced by w_util.p and w_module.p
--- John Gibson, Jun 24 1988
        Renamed make_poplink.p (previously poplink.p)
--- John Gibson, Feb  9 1988
        Increased version number
--- John Gibson, Jan 17 1988
        Moved most of code into new file 'poplink_main.p'. This file now
        just builds the program.
 */
