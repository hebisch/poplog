/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/globals.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

;;; -- Global variables for the Objectclass package. ------------------------

section $-objectclass;

;;; This variable is used to optimise unlinking.
;;;
vars all_classed_methods_are_unlinked = true;

;;; This variable indicates whether or not it is safe to trash method
;;; tables when we optimise methods.  It is set to false, for example,
;;; by loading -method_path-.
;;;
vars can_trash_method_table = true;

;;; This variable is used to extend the compilation process.  This allows
;;; us to make method tracing autoloadable.
vars relink_hook = erase;

;;; -- invokes_call_next_method
;;; Flag set on method parts to allow the method linker to generate
;;; suitable code for parts that use call_next_method.
;;;
define c_invoke_call_next_method() with_props cICNM;
    internal_error();
enddefine;
;;;
define u_invoke_call_next_method() with_props uICNM;
    internal_error();
enddefine;
;;;
define lconstant invokes_call_next_method_table =
    newproperty([
        [^c_invoke_call_next_method ^CALL_MODE]
        [^u_invoke_call_next_method ^UCALL_MODE]
    ], 20, false, "tmparg");
enddefine;
;;;
;;; Recursively invoked on closures.
define get_invokes_call_next_method( p );
    invokes_call_next_method_table( p ) or
    p.isclosure and get_invokes_call_next_method( frozval( 1, p ) )
enddefine;
;;;
;;; Made explicit as part of a systematic renaming.
define set_invokes_call_next_method( v, p );
    v -> invokes_call_next_method_table( p )
enddefine;

;;; -- isobsolete
;;; Table flagging obsolete classes.
;;;
define isobsolete =
    newproperty( [], 8, false, "tmparg" );
enddefine;

;;; -- class_extern_posn
;;; Table describing where the >-> is inside a class.

define class_extern_posn =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false, false
    );
enddefine;

;;; -- fields_of_class
;;; Table describing the format of a class - similar to key_of_dataword.
;;; Returns a list of slots.
;;;
define fields_of_class =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false, false
    );
enddefine;

;;; -- supers_of_class
;;; Table describing the "super" relationship.  Note that it *must* be
;;; temporary to avoid store clogging up during recompilation.
;;; The supers are stored in order of dominance ie. dominant classes come
;;; early in the list.
;;;
define supers_of_class =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        [], false
    );
enddefine;

;;; -- infs_of_class
;;; At some points we need the inverse to supers -- namely inferiors.
;;;
define infs_of_class =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false, false
    );
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 12 1995
        Moved code for managing superclasses and inferiors to new file
        "inheritance.p"
 */
