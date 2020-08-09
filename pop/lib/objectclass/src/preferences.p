/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/preferences.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; == PREFERENCE VARIABLES =================================================

compile_mode :pop11 +strict;

section $-objectclass =>
    pop_oc_trace
    pop_oc_reuse
    pop_oc_sensitive_methods
    pop_oc_writeable_default
    pop_oc_inheritance_algorithm
    pop_oc_vars_default
;

;;; -- pop_oc_trace ------------------------------------------------
;;; This is a debugging flag that might as well be left in for tutorial
;;; purposes.  When it is switched on, a trace output is produced.
;;; I plan this to become a multi-level debugging.  At the moment it is
;;; just a single level.

;;; pop_oc_trace
;;;     false       :   no output
;;;     cancel      :   cancelling or unprotecting identfiers
;;;     link        :   method creation, linking and unlinking notification
;;;     replace     :   notify on key-replacement
;;;     upgrade     :   upgrading a method
;;;     true        :   all traced

global vars pop_oc_trace = false;

define trace_matches( level ); lvars level;
    define lconstant procedure Match( flag, level ); lvars flag, level;
        if flag.isboolean then
            flag
        elseif flag.isword then
            level == flag
        elseif flag.islist and not( null( flag ) ) then
            Match( hd( flag ), level ) or
            Match( tl( flag ), level )
        else
            false
        endif;
    enddefine;

    Match( pop_oc_trace, level )
enddefine;

;;; -- pop_oc_reuse ------------------------------------------------
;;; This flag determines whether or not to attempt to reuse classes when
;;; recompiling objectclass definitions which haven't changed.

global vars pop_oc_reuse = true;

;;; -- pop_oc_sensitive_methods ------------------------------------
;;; This flag determines what you want to happen to methods when a class
;;; is relinked with a different key.
;;;     "both",     make all methods work on both old and new instances
;;;     "new",      make all methods only work with new instances
;;;     false,      ignore the new classes until relinkd

global vars pop_oc_sensitive_methods = "new";

;;; -- pop_oc_writeable_default -------------------------------------------
;;; Determines the writeable/nonwriteable class default applied to classes
;;; (keys) created by objectclass; this can be overridden by specifying
;;; attributes in a class definition. Possible values are:
;;;     "writeable"         -- default writeable
;;;     "nonwriteable"      -- default nonwriteable
;;;     <false>             -- unspecified: decided by sys_lock_system
;;; The default is "writeable" on the assumption that objects have mutable
;;; state

global vars pop_oc_writeable_default = "writeable";

;;; -- pop_oc_inheritance_algorithm ---------------------------------------
;;; Selects the algorithm used for determining the order of inheritance
;;; from superclasses. Possible values:
;;;     "dfs"       -- depth-first search (old objectclass behaviour)
;;;     "clos"      -- same as CLOS
;;;     "dylan"     -- the default, as used by Dylan
;;; NB: if this is to be changed, it must be done before any classes are
;;; defined

global vars pop_oc_inheritance_algorithm = "dylan";

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  7 1995
        Got rid of pop_oc_v*ars_default!
--- Robert John Duncan, Dec  1 1995
        Added pop_oc_v*ars_default.
--- Robert John Duncan, Nov 14 1995
        Changed default inheritance algorithm to "dylan"
--- Robert John Duncan, Oct 11 1995
        Added pop_oc_inheritance_algorithm
--- Robert John Duncan, Oct  9 1995
        Added pop_oc_writeable_default
;;; -------------------------------------------------------------------------
;;; Modified, 6/10/93, JJC
;;;     *   removed checking for pop_oc_version > 6.
;;;     *   removed definition of pop_oc_v600_recognisers.
;;; -------------------------------------------------------------------------
;;; Modified, 15/05/93, sfk
;;;     *   pop_oc_v600_recognisers is now initialised according
;;;         to the value of pop_oc_version.
;;; -------------------------------------------------------------------------
;;; Modified, 6/12/92, sfk & jonm
;;;     *   Made all exported variables global.
;;; -------------------------------------------------------------------------
 */
