/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.all/src/initial.p
 > Purpose:         Global initialisation
 > Author:          John Gibson (see revisions)
 */


#_INCLUDE 'declare.ph'

weak constant
    Sys$- _external_callback_func
    ;


constant
    newline     = "\n",
    space       = "\s",
    tab         = "\t",
    undef       = "undef",
    sys_word_bits = WORD_BITS,
;

    ;;; Internal version number, for allowing library files etc to
    ;;; know about new versions. Used as the version number in the string
    ;;; popversion.
    ;;; (See declaration in declare.ph -- in AIX this is a vars.)
    160001 -> pop_internal_version;


    ;;; Initialise variables
vars
    database        = [],
    pop_debugging   = true,
    ;;; A.S. 1 Nov 2003
    ;;; syfileok will translate environment variables
    pop_translate_envvars = true,

    ;;; virtual machine control flags word -- see 'lib/include/vm_flags.ph'
    pop_vm_flags    = 0,
    pop_autoload    = true,

    ;;; set to "popc" when running in Popc compile mode
    Sys$-vm_pas_mode = false,
    ;


constant
    popcopyright = 'Copyright (c) 1982-1999 University of Sussex. All rights reserved.',

    ;;; version number string -- gets concatenated with Sys$-image_date set up
    ;;; by "link" to produce pop_version.
    Sys$-image_version = '(Version ' >< pop_internal_version/10000.0,

    ;;; dummy initialisations
    _special_var_block = _special_var_block,    ;;; set up by poplink
    ;

vars
    _trap           = _0,           ;;; interrupt flag
    _disable        = _DISABLE_ALL, ;;; interrupt/stack check disable flags

    Sys$- _external_flags   = _0,
    Sys$- _in_X_call        = _0,

    ;;; This is non-zero when in user external calls (set by _call_external),
    ;;; and enables asynchronous callback by signals.
    ;;; It has the value -1 when completely outside of Pop.
    Sys$- _in_user_extern   = _-1,
    ;


    ;;; Define C synonyms for things

define_extern_name
    __pop_in_user_extern    = ident Sys$- _in_user_extern,
    _pop_signals_pending    = ident _trap,
    _pop_disable_flags      = ident _disable,
    _pop_in_X_call          = ident Sys$- _in_X_call,
    _pop_external_flags     = ident Sys$- _external_flags,

    ;;; This can't be set equal to the weak constant value, so has to be
    ;;; a pointer to it.
    _WEAK_pop_external_callback = ident weakref Sys$- _external_callback_func,
    ;



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 1 Nov 2003
        Added pop_translate_envvars
        syfileok will translate environment variables if it is true (default)
--- John Gibson, Sep 29 1998
        pop_internal_version = 155210 (various changes)
--- John Gibson, Dec  2 1997
        pop_internal_version = 155200 (various changes)
--- John Gibson, Apr 30 1997
        pop_internal_version = 155101 (various changes)
--- John Gibson, Feb 14 1997
        pop_internal_version = 155100 (16-bit chars)
--- John Gibson, Oct 17 1996
        pop_internal_version = 155001 (slight change to Popc interface)
--- Robert Duncan, Oct  7 1996
        pop_internal_version = 155000 (general release)
--- John Williams, Sep  6 1996
        pop_internal_version = 154000 (installing in COGS).
--- John Gibson, Aug 15 1996
        pop_internal_version = 150301 (external names not translated for Popc)
--- John Williams, May 30 1996
        pop_internal_version = 150300 (installing in Cogs)
--- John Gibson, Apr  1 1996
        pop_internal_version = 150220 (slight change to Popc interface)
--- John Gibson, Mar 26 1996
        pop_internal_version = 150210 (dlocal allowed at execute level)
--- John Gibson, Mar  5 1996
        Changed default for pop_debugging to true.
--- Robert John Duncan, Jan  8 1996
        Copyright extended to 1996.
--- John Williams, Nov 30 1995
        pop_internal_version = 150200 (no warnings from vars in procedures).
        (Note 150100 used by ISL as version number for their re-release
         of V15 with the vars change).
--- John Williams, Aug  7 1995
        pop_internal_version = 150001
--- John Williams, Jul 31 1995
        pop_internal_version = 150000
--- Robert John Duncan, Jul 12 1995
        Updated and exported the copyright string.
--- John Gibson, May 23 1995
        pop_internal_version = 145300 (new lvars default for procedure formals, etc)
--- John Gibson, May 20 1995
        pop_internal_version = 145200 (first Alpha OSF version)
--- John Gibson, Apr 10 1995
        pop_internal_version = 145102 (procedure header and key layouts change)
--- John Williams, Feb  2 1995
        pop_internal_version = 145101 (fix to I_LBLOCK for Clisp/CLX)
--- John Gibson, Dec  8 1994
        pop_internal_version = 145100 (first Alpha VMS version)
--- John Gibson, Nov 28 1994
        Moved initialisation of pointer variable pop_external_flags to
        c_callback.c (pop must refer only to _pop_external_flags)
--- John Gibson, Oct 24 1994
        Added new Popc define_extern_name construct for defining
        external names
--- John Gibson, Jun  2 1994
        Removed _d*ouble
--- John Williams, Apr 26 1994
        Upped pop_internal_version to 145003 after copying 145002
        as COGS popversion "new".
--- John Gibson, Feb  7 1994
        pop_internal_version = 145002 (various Ved changes)
--- Simon Nichols, Nov 29 1993
        pop_internal_version = 145001 (after copying 14.5)
--- Simon Nichols, Nov 29 1993
        pop_internal_version = 145000 (prior to copying 14.5)
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Williams, Jul 13 1993
        pop_internal_version upped to 142400, just after copying 14.2301
--- John Gibson, May 21 1993
        Added init for pop_vm_flags
--- John Gibson, May  2 1993
        pop_internal_version = 142301 (slight change in POPC interface)
--- John Williams, Apr 23 1993
        pop_internal_version upped to 142300, just after copying 14.2201
--- John Williams, Jan  8 1993
        pop_internal_version upped to 142201 (new vedsetup_actions)
--- John Gibson, Dec 18 1992
        Moved in initialisations of _external_flags and _in_X_call
--- John Gibson, Oct 12 1992
        pop_internal_version = 142200 (change in POPC interface)
--- John Gibson, Jul 16 1992
        pop_internal_version = 142100 (change in POPC interface)
--- John Williams, Jul  2 1992
        pop_internal_version = 142000
--- John Williams, May 22 1992
        pop_internal_version = 141200
            (fix to 'sig_stop.p' to prevent runaway VED processes)
--- John Gibson, Jan 21 1992
        pop_internal_version = 141100 (dstrings and VED char attributes)
--- Simon Nichols, Nov  6 1991
        pop_internal_version = 141000
--- Simon Nichols, Oct  7 1991
        pop_internal_version = 140810
--- Simon Nichols, Sep 17 1991
        pop_internal_version = 140800
--- Simon Nichols, Sep  2 1991
        pop_internal_version = 140700
--- Simon Nichols, Aug 13 1991
        pop_internal_version = 140600
--- John Gibson, Jun 20 1991
        pop_internal_version = 140500
--- Robert John Duncan, Jun  6 1991
        pop_internal_version = 140400
        (Lots of XVed fixes)
--- Simon Nichols, May 30 1991
        pop_internal_version = 140300
--- John Gibson, Mar 14 1991
        pop_internal_version = 140200
--- John Williams, Jan 14 1991
        pop_internal_version = 140100
            (because frozen master version number is now 140000)
--- John Gibson, Jan  5 1991
        pop_internal_version = 139200 (changes to signals, sys_timer etc).
--- John Williams, Nov 21 1990
        pop_internal_version = 139100 (beta release to ISL)
--- John Williams, Oct 22 1990
        pop_internal_version = 139000 (alpha release to ISL)
--- John Gibson, Sep 21 1990
        pop_internal_version = 138400 (changes to signals, external_do_load)
--- John Gibson, Aug 31 1990
        pop_internal_version = 138300 (sub-lists in poparglist)
--- John Williams, Jul 19 1990
        pop_internal_version = 138200 (new LIB SUBSYSTEM)
--- John Gibson, Jun 23 1990
        pop_internal_version = 138100 (callback, fix SunOS 4.1 stack empty)
--- John Gibson, May 10 1990
        pop_internal_version = 138000
--- John Gibson, Mar  3 1990
        pop_internal_version = 137100 (fixed-address structures)
--- John Gibson, Jan  7 1990
        pop_internal_version = 137000 (new pop pointers)
--- John Gibson, Sep 15 1989
        pop_internal_version = 136610 (new process stuff)
--- John Gibson, Sep  4 1989
        pop_internal_version = 136602
--- John Gibson, Jul 31 1989
        pop_internal_version = 136601
--- John Gibson, Jun 25 1989
        pop_internal_version = 136600
--- John Gibson, Jun  4 1989
        Added -pop_debugging-
--- John Gibson, May 18 1989
        pop_internal_version = 136500 (nonpop identifiers begin with Ctrl-_)
--- John Gibson, May  9 1989
        pop_internal_version = 136403 (-sysLBLOCK- takes bool argument)
--- John Gibson, May  2 1989
        pop_internal_version = 136402
--- John Gibson, Apr 27 1989
        pop_internal_version = 136401
--- John Gibson, Mar 26 1989
        pop_internal_version = 136400 (most key identifiers exported)
--- John Gibson, Mar 12 1989
        pop_internal_version = 136300 (more changes for POPC)
--- John Gibson, Jan 29 1989
        pop_internal_version = 136200 (for new version of POPC)
--- John Williams, Jan 23 1989
        re-installed after accidentally replacing with frozen version
--- John Gibson, Nov 23 1988
        pop_internal_version = 136100  (lexical blocks in VM/POP-11)
--- John Gibson, Nov  6 1988
        pop_internal_version = 136001
--- Aaron Sloman, Sep 16 1988
        pop_internal_version = 136000
--- John Gibson, Jul 25 1988
        Changed _d*ouble to be initialised as double float.
--- John Gibson, Jun 24 1988
        pop_internal_version = 135200
--- John Gibson, May  5 1988
        Made -image_version- contain the full version number, i.e.
        pop_internal_version/10000.0.
        Also set pop_internal_version = 131000
--- John Gibson, Apr  6 1988
        pop_internal_version = 135007
--- John Gibson, Mar  7 1988
        pop_internal_version = 135006
--- John Gibson, Feb 28 1988
        Moved -sys_os_type- and -sys_processor_type- to sys_os_type.p
        (because they use floating-point constants).
--- John Gibson, Feb 14 1988
        Added -nullstring-
        pop_internal_version = 135005
--- John Gibson, Feb 11 1988
        pop_internal_version = 135004 (number files split up, section Sys
        and weakrefs introduced).
--- John Gibson, Feb  9 1988
        pop_internal_version = 135003
--- John Gibson, Jan 19 1988
        pop_internal_version = 135002
--- John Gibson, Jan 17 1988
        pop_internal_version = 135001
 */
