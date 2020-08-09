/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/readutils.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

;;; -- Utilities to help parsing and compilation ----------------------------

section $-objectclass;
include pop11_flags.ph


define read_optional_semi( opening_keyword ); lvars opening_keyword;
    if opening_keyword == ";" then
        pop11_try_nextreaditem
    else
        pop11_need_nextreaditem
    endif( opening_keyword ).erase;
enddefine;

;;; Is the word W a lexical constant?
define islconstant( W ); lvars W, id;
    isident( sys_current_ident( W ) ->> id ) /== "perm" and
    isconstant( id )
enddefine;


;;; -- Dealing with words ---------------------------------------------------

;;; Iterate over a list of values and obtain the n'th element from each
;;; value.  Just dump them onto the stack.  Boring service function.
;;; Check that a presumed local variable is actually a word.
define lconstant check_word( w ) -> w; lvars w;
    unless w.isword do
        mishap( 'WORD NEEDED', [^w] )
    endunless
enddefine;

define readword();
    readitem().check_word
enddefine;


;;; -- Dealing with FIELD SPECS ---------------------------------------------

lconstant single_specs =
    [
        full
        int uint long ulong short ushort sbyte byte pint
        sfloat dfloat float
        exptr
    ];

;;; Checks whether or not we have a valid class spec element.  See REF * KEYS.
define check_spec( s ); lvars s;
    unless
        lmember( s, single_specs ) or
        ( s.isinteger and -32 <= s and s <= 32 )
    then
        if s.isclosure and s.length == 2 and frozval( 2, s ) == true then
            frozval( 1, s ).check_spec
        else
            mishap( s, 1, 'NEWOBJECTCLASS : invalid field spec.' )
        endif
    endunless
enddefine;

define get_spec();
    dlocal pop_autoload = false;
    if nextreaditem() == ":" then
        $-typespec_utils$-read_typespec( false, false ).erase.erase
    else
        false
    endif;
enddefine;


;;; -- Dealing with IDENT-SPECS ---------------------------------------------

define sysGVARS( w, a ); lvars w, a;
    sysSYNTAX( w, a, false );
    sysGLOBAL( w );
enddefine;

define sysGCONSTANT( w, a ); lvars w, a;
    sysSYNTAX( w, a, true );
    sysGLOBAL( w );
enddefine;

define sysLVARS0( /*w*/ ) with_nargs 1;
    sysLVARS( /*w*/, 0 );
enddefine;

;;; Only checks whether or not its parameter is defined.  Obviously
;;; respects weak declarations.
define sysNONLOCAL( w ); lvars w;
    unless w.isdefined do
        mishap( 'NON-LOCAL DEFINITION OF AN UNDEFINED VARIABLE', [^w] )
    endunless
enddefine;

;;; Look out for the artifical identspecs "updaterof" and "lupdaterof"
;;; which are used to get the defaults for updater-only declarations
;;; correct.
;;;
define identspec_declare( f ); lvars f;
    if f == "constant" then
        sysGCONSTANT
    elseif f == "lconstant" then
        sysLCONSTANT
    elseif f == "lvars" then
        sysLVARS
    elseif f == "vars" then
        sysGVARS
    elseif f == "dlocal" then
        procedure( w, props ); lvars w, props;
            sysLOCAL( w )
        endprocedure
    else
        internal_fault( [spec ^f] )
    endif;
enddefine;

define mode_identspec_declare( ids, mode ); lvars ids, mode;
    if mode.check_mode == UCALL_MODE then
        procedure( w, props ); lvars w, props; endprocedure
    else
        identspec_declare( ids )
    endif
enddefine;

define islexical_identspec( x ); lvars x;
    x == "lvars" or x == "lconstant"
enddefine;


;;; Read an optional identspec off the input stream, defaulting to -d- if it
;;; is not there.
define read_identspec( d ); lvars d;
    lconstant readable_idspecs = [constant lvars vars lconstant dlocal];
    pop11_try_nextreaditem( readable_idspecs ) or d
enddefine;


define default_default_idspec();
    if pop_vm_compiling_list == [] then
        if pop_debugging /== true and POP11_DEFINE_CONSTANT then
            "constant"
        else
            "vars"
        endif
    else
        "lconstant"
    endif
enddefine;



;;; -- Dealing with PROCEDURE HEADERS ---------------------------------------

define which_pdprops( props, defprops, lexidspec ); lvars props, defprops, lexidspec;

    ;;; Deal with the trivial case.
    returnif( props )( props );

    if lexidspec.isword then
        islexical_identspec( lexidspec ) -> lexidspec
    elseunless lexidspec.isboolean then
        mishap( 'INVALID FLAG', [^lexidspec] )
    endif;
    if lexidspec then
        POP11_NO_LEX_PDPROPS && pop_pop11_flags == 0 and defprops
    else
        defprops
    endif
enddefine;

define get_declare( type, idprops ); lvars type, idprops;
    if type == "lvars" then
        if idprops == 0 then
            sysLVARS0
        elseif idprops == "procedure" then
            procedure() with_nargs 1; sysLVARS( "procedure" ) endprocedure
        else
            sysLVARS(% idprops %)
        endif;
    else
        unless idprops == 0 then
            mishap( 'invalid arguments', [^type ^idprops] )
        endunless;
        if type == "dlocal" then
            sysLOCAL
        elseif type == "nonlocal" then
            sysNONLOCAL
        else
            internal_fault( [type ^type] );
        endif
    endif
enddefine;

;;; declarations of input and output locals look like this:
;;;     define foo( [lvars dlocal nonlocal] [procedure] <name>, ... )
;;; e.g.
;;;     define appdata( x, procedure p )
;;;     define foo( dlocal procedure cucharout, nonlocal lastchar )
;;;
define read_local();
    lvars t = pop11_try_nextreaditem( [lvars dlocal nonlocal] );
    lvars p = pop11_try_nextreaditem( "procedure" );
    get_declare( t or "lvars", p or 0 );
    readword()
enddefine;

define read_olocals() -> ( odecs, olocals ); lvars odecs, olocals;

    define lconstant procedure read_target();
        if pop11_try_nextreaditem( "(" ) then
            if pop11_try_nextreaditem( ")" ) then
                ;;; nothing returned
            else
                read_target();
                while pop11_try_nextreaditem( "," ) do
                    read_target()
                endwhile;
                pop11_need_nextreaditem( ")" ).erase;
            endif
        else
            read_local()
        endif;
    enddefine;

    [] -> odecs;
    [] -> olocals;
    while pop11_try_nextreaditem( "->" ) do
        lvars DS = [], LS = [];
        lvars DLs = [% read_target() %];
        until DLs.null do
            lvars ( D, L ) = DLs.dest.dest -> DLs;
            D :: DS -> DS;
            L :: LS -> LS;
        enduntil;
        [% DS.revdl % ^^odecs] -> odecs;
        [% LS.revdl % ^^olocals] -> olocals;
    endwhile;
enddefine;

;;; This is a generalised read of with_nargs, with_props, with_combination
;;; syntax that is designed to be extensible.  It avoids allocating
;;; any store in the common case that the user actually uses no properties.
;;;
define read_with_stuff() -> answer;
    lvars procedure answer = procedure( x ); lvars x; false endprocedure;

    define lconstant table( x ); lvars x;
        answer( x )
    enddefine;

    define updaterof table( v, x ); lvars v, x;
        unless answer.isproperty do
            newproperty( [], 8, false, "perm" ) -> answer
        endunless;
        v -> answer( x )
    enddefine;

    lconstant with_stuff = [ with_nargs with_props with_combination ];
    lconstant combinations = [ standard before after ];
    repeat
        lvars tok = pop11_try_nextreaditem( with_stuff );
        if table( tok ) then
            mishap( 'REPEATED USE OF ' sys_>< tok, [] )
        elseif tok then
            lvars item = readitem() ->> table( tok );
            if tok == "with_nargs" then
                unless item.isinteger or item == "variadic" do
                    mishap( 'INVALID ARGUMENT FOR with_nargs', [^item] )
                endunless;
            elseif tok == "with_combination" do
                unless lmember( item, combinations ) do
                    mishap( 'INVALID ARGUMENT FOR with_combination', [^item] )
                endunless
            endif
        else
            quitloop
        endif
    endrepeat
enddefine;

define read_nargs_and_props() -> ( nargs, props ); lvars nargs, props;
    lvars t = read_with_stuff();
    ( t( "with_nargs" ), t( "with_props" ) ) -> ( nargs, props );
    if t( "with_combination" ) then
        mishap( 'INVALID USE OF with_combination', [] )
    endif
enddefine;

;;; define read_nargs_and_props() -> ( nargs, props ); lvars nargs, props;
;;;     false.dup -> ( nargs, props );
;;;     repeat
;;;         if pop11_try_nextreaditem( "with_nargs" ) then
;;;             if nargs then
;;;                 mishap( 'REPEATED USE OF with_nargs', [^nargs] )
;;;             else
;;;                 readitem() -> nargs;
;;;                 unless nargs.isinteger do
;;;                     mishap( 'INVALID ARGUMENT FOR with_nargs', [^nargs] )
;;;                 endunless
;;;             endif;
;;;         elseif pop11_try_nextreaditem( "with_props" ) then
;;;             if props then
;;;                 mishap( 'REPEATED USE OF with_props', [^props] )
;;;             else
;;;                 readitem() -> props
;;;             endif;
;;;         else
;;;             quitloop
;;;         endif;
;;;     endrepeat
;;; enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 28 1995
        Fix to read_with_stuff
;;; -------------------------------------------------------------------------
;;; Modified, 8/10/93, JJC
;;;     *   Made check_word an lconstant.
;;; -------------------------------------------------------------------------
 */
