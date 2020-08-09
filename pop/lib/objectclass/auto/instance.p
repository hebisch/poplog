/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/instance.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; -- instance -------------------------------------------------------------
;;; syntax word for constructing instances in a convenient way.
;;;     instance CLASS [;]    ;;; this semi is optional
;;;         METHOD = EXPR;
;;;         METHOD = EXPR;
;;;         ...
;;;         METHOD = EXPR[;]        ;;; last semi is optional
;;;     endinstance
;;;
;;; define-extension for constructing instances in a convenient way.
;;;     define :instance [IDENTSPEC] VAR:CLASS [;]
;;;         METHOD = EXPR;
;;;         METHOD = EXPR;
;;;         ...
;;;         METHOD = EXPR[;]
;;;     enddefine

section $-objectclass =>
    define_instance
    instance
    endinstance;

define lconstant pop_new(class, Lvar);
    lvars keyname = derive_key_name(class);
    lvars id = sys_current_ident(keyname);
    if id
    and isconstant(id)
    and (not(isassignable(id)) or pop_pas_mode == "popc")
    then
        lvars key = sys_current_val(keyname);
        if isclass(key) == "object" then
            ;;; we can safely do the class_new call now, which is
            ;;; marginally more efficient but more importantly avoids a
            ;;; run-time dependency which would upset Popc
            sysCALLQ(class_new(key)), sysPOP(Lvar);
            return;
        endif;
    endif;
    sysPUSH(keyname), sysCALL("class_new"), sysCALLS("undef"), sysPOP(Lvar);
enddefine;

define lconstant process_method_init( Lvar, closer );
    lvars Lvar, closer;
    lvars name = itemread();
    unless name.isword do
        mishap( 'OBJECT: invalid method name', [^name] )
    endunless;
    pop11_need_nextitem( "=" ).erase;
    pop11_comp_expr();
    sysPUSH( Lvar );
    sysUCALL( name );
    unless
        pop11_try_nextreaditem( ";" ) or
        null( proglist ) or
        hd( proglist ) == closer
    do
        ;;; This will mishap with the right message.
        pop11_need_nextreaditem( closer ).erase;
    endunless;
enddefine;

define lconstant bindings( Lvar, closer ); lvars Lvar, closer;
    until pop11_try_nextreaditem( closer ) do
        process_method_init( Lvar, closer );
    enduntil;
enddefine;

vars syntax endinstance;

define lconstant instance_to( closer ); lvars closer;
    dlocal pop_new_lvar_list;

    lvars class = readword();
    lvars Lvar = sysNEW_LVAR();

    pop_new(class, Lvar);

    ;;; optional semi-colon.
    pop11_try_nextreaditem( ";" ).erase;

    bindings( Lvar, closer );
    sysPUSH( Lvar );
enddefine;

define syntax instance;
    instance_to( "endinstance" )
enddefine;

define lconstant define_instance_to( closer ); lvars closer;
    dlocal pop_new_lvar_list;

    lvars idspec = read_identspec( "vars" );
    lvars Lvar = readword();
    pop11_need_nextreaditem( ":" ).erase;

    identspec_declare( idspec )( Lvar, 0 );
    lvars class = readword();

    pop_new(class, Lvar);

    ;;; optional semi-colon.
    pop11_try_nextreaditem( ";" ).erase;

    bindings( Lvar, closer );
enddefine;

define :define_form instance;
    define_instance_to( "enddefine" )
enddefine;

#_IF DEF vedprocess
unless lmember( "instance", vedopeners ) do
    [instance ^^vedopeners] -> vedopeners;
    [endinstance ^^vedclosers] -> vedclosers;
endunless;
#_ENDIF

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Robert John Duncan, Oct  4 1995
        Popc changes
--- Integral Solutions Ltd (Julian Clinton), Feb 16 1994
        Replaced readitem with itemread to allow macro expansion
        within "instance...endinstance" in -process_method_init-.
 */
