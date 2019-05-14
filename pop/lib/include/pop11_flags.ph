/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/include/pop11_flags.ph
 >  Purpose:        Definitions of POP11 compiler control flags
 >                  (These are bits in the integer variable -pop_pop11_flags-)
 >  Author:         John Gibson, May 31 1989 (see revisions)
 >  Documentation:  REF * POPCOMPILE
 >  Related Files:  LIB COMPILE_MODE, $usepop/pop/lib/include/vm_flags.ph
 */

#_TERMIN_IF DEF POP11_FLAGS_INCLUDED

;;; N.B. compile_mode is a POP-11 syntax construct for compile-time
;;; manipulation of the flags defined in this file; the 'compile_mode:pop11'
;;; option corresponding to SETTING each flag appears on the right after
;;; its definition.

;;; Note also that pop_pop11_flags is localised to each lexical block,
;;; procedure or compilation stream.

section;

iconstant macro (

    POP11_DEFINE_CONSTANT           = 2:1e0,        ;;; +defcon
            /*  Sets the default declaration for a top-level define to be
                constant, i.e. when no explicit declaration keyword is
                specified. Thus when set, 'define foo() ...' at top-level
                will declare foo as a permanent constant.
                    Note that the default for nested defines is always
                vars (providing one is allowed, see POP11_VARS_CHECK
                below).
                    N.B. The action of this flag is turned off when
                pop_debugging == true (since this turns off all permanent
                constants).
            */

    POP11_DEFINE_PROCEDURE          = 2:1e1,        ;;; +defpdr
            /*  Sets the default identprops value for define to be
                "procedure", i.e. when no explicit identprops is specified.
                Thus when set, 'define foo() ...' will declare foo as a
                procedure-type identifier.
            */

    POP11_NO_LEX_PDPROPS            = 2:1e2,        ;;; -lprops
            /*  When set, a define for a lexical identifier (i.e.
                'define lconstant ...' or 'define lvars ...') will give
                the procedure being defined a pdprops value of false
                by default (only by default, i.e. this does not affect an
                explicit with_props value).
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    POP11_VARS_CHECK                = 2:1e3,        ;;; +varsch
            /*  From Version 14.53, this flag only affects explicit vars
                statements in procedures, unless POP11_OLD_VARS is set.
                When POP11_OLD_VARS is clear, formal argument and result
                variables default to lvars and nested defines default to
                lconstant.

                With POP11_OLD_VARS set, formal argument and result variables
                of procedures default to vars rather than lvars, and nested
                defines default to vars rather than lconstant. However, if
                POP11_VARS_CHECK is set also, the (variable) procedure
                pop11_vars_default_check is called with the name of the
                identifier concerned as argument. The standard version of this
                procedure for ordinary interactive compilation then prints a
                warning message (but still defaults the identifier to vars);
                in Popc compilation, on the other hand,
                pop11_vars_default_check is redefined to treat the
                condition as an error.

                Also when POP11_VARS_CHECK is set, it is an error to have an
                explicit vars statement inside a procedure (dlocal must
                be used instead).
            */

    POP11_CONSTRUCTOR_CONSTS        = 2:1e4,        ;;; +constr
            /*  Setting this flag allows list and vector constructor
                expressions containing no 'evaluate' keywords to be
                produced as compile-time constants (as opposed to
                producing new structures each time the code is  run).
                See pop11_comp_constructor in REF *POPCOMPILE.
            */

    POP11_PERM_GLOBAL               = 2:1e5,        ;;; +global
            /*  Makes global be the default for all permanent vars and
                constant declarations. (An explicit nonglobal must be used
                to override this flag.)
            */

    POP11_OLD_VARS                  = 2:1e6,        ;;; +oldvar
            /*  Restores the pre-Version 14.53 behaviour of the compiler, i.e.

                1.  procedure input and output locals default to vars
                    (rather than lvars);

                2.  nested procedures default to vars (rather than lconstant);

                3.  variables declared as vars inside procedures do not
                    produce a warning message;
            */


    /*  Flags that should be propagated through a call of syslibcompile.
        (This should only include flags that don't affect the declaration
        of identifiers, which currently is just NO_LEX_PDPROPS.)
    */
    POP11_SLC_PROPAGATE = POP11_NO_LEX_PDPROPS,

    );

iconstant POP11_FLAGS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 23 1995
        Added POP11_OLD_VARS
--- John Gibson, Oct 27 1992
        Added POP11_PERM_GLOBAL etc
 */
