/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sys_os_type.p
 > Purpose:
 > Author:          John Gibson, Feb 28 1988 (see revisions)
 > Documentation:   REF *SYSTEM
 */

#_INCLUDE 'declare.ph'

    /*  These are in a separate file because they probably use
        floating-point constants (the macros defining them are in
        syscomp/sysdefs.p)
    */

constant
    sys_os_type         = OPERATING_SYSTEM,
    sys_processor_type  = PROCESSOR,
    sys_machine_type    = MACHINE,
    ;



/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Apr  4 1989
        Added -sys_machine_type-
--- John Gibson, Feb 28 1988
        Pulled these out of initial.p
 */
