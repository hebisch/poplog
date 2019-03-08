/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/make_popc.p
 > Purpose:
 > Author:          John Gibson, Jan  8 1988 (see revisions)
 */

/* -------------------------------------------------------------------------

                    POPLOG OBJECT COMPILER MAKE FILE

--------------------------------------------------------------------------*/
compile_mode :pop11 +strict;

printf('Loading POPC Version 14.24\n');
if pop_internal_version < 142400 then
    mishap(0, 'POPC VERSION NEEDS POPLOG VERSION 14.24 OR LATER')
endif;

unless isboolean(pop_debugging) then true -> pop_debugging endunless;
pop_debugging -> popgctrace;
if pop_debugging then 1 -> popsyscall endif;

sys_unlock_heap();
true -> pop_record_writeable;
200000 -> popmemlim;

constant macro POPC = true;


load lib.p
compile(pop_architecture >< '/sysdefs.p');
load mcdata.p
load files.p
load wordflags.p
compile(pop_architecture >< '/asmout.p');
load os_comms.p
load sysint.p
load syspop.p
load ident_labs.p
load genfloat.p
load genstruct.p
compile(pop_architecture >< '/genproc.p');
load pas_optimise.p
load m_optimise.p
load m_trans.p
load vm_genp.p
load pas_interpret.p
load w_util.p
load syspop_mode.p
load do_asm.p
load popc_main.p

cancel POPC;

section $-Popas => make_saved_image;

define make_saved_image(name);
    lvars name;
    if sys_lock_system(name, 2:011, 'POPC') then
        $-Pop$-Main(), sysexit()
    endif
enddefine;

#_IF pop_debugging

500000 -> popmemlim;

#_ELSE

section_cancel(current_section, false);

#_ENDIF

endsection;     /* $-Popas */

sysgarbage();
sys_lock_heap();



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 26 1995
        Added temporary code for old corepop where popc_is_array_upd
        is not present
--- John Gibson, Mar 21 1995
        Added load for mcdata.p
--- John Gibson, Oct 10 1994
        Added POPC macro
--- John Gibson, Jun  8 1993
        Removed load for popc_pdrs.p (code moved into pas_interpret.p)
--- John Gibson, Mar 13 1993
        Removed uses pop*xlib
--- John Gibson, Oct 13 1992
        Requires V14.22
--- John Gibson, Oct  1 1992
        Added uses pop*xlib
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Sep 22 1990
        Changed -popc- from macro to syntax
--- John Gibson, Jan  7 1990
        Version 13.7 for new pop pointers
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Loads os_comms.p
--- John Gibson, Jun  7 1989
        Version 13.65
--- John Gibson, May 17 1989
        Version 13.6403
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Jan 29 1989
        Version 13.62 required
--- John Gibson, Nov 23 1988
        POPC Version 13.61 (handles lexical blocks in the VM).
--- John Gibson, Jul 12 1988
        Added -popc- macro for testing
--- John Gibson, Jun 24 1988
        Renamed make_popc.p (previously popas.p)
--- John Gibson, Feb  9 1988
        Increased version number
--- John Gibson, Jan 17 1988
        Moved bulk of code into 'popas_main.p'
 */
