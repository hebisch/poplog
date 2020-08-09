/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/trace_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass =>
    trace_method
    untrace_method
    untrace_every_method
    trace_every_method
;

;;; We have both utrace & ctrace in an attempt to improve performance.
;;;
define lconstant procedure update_list( L, mode ); lvars L, mode;

    define lconstant procedure adjust_name( P, level ); lvars P, level;
        lvars name = P.pdprops;
        repeat level times
            '* ' sys_>< name -> name
        endrepeat;
        return( name )
    enddefine;

    define lconstant procedure utrace( P, level ); lvars P, level;
        systrace_proc( P, adjust_name( P, level ), true )
    enddefine;

    define lconstant procedure ctrace( P, level ); lvars P, level;
        systrace_proc( P, adjust_name( P, level ), false )
    enddefine;

    lvars T = mode and utrace or ctrace;

    lvars n = 0;
    while L.ispair do
        T(% front( L ), n %) -> front( L );
        n + 1 -> n;
        back( L ) -> L;
    endwhile;
enddefine;

define systrace_method( FM ); lvars FM;
    dlocal poptraceindent = poptraceindent + 1;
    dlocal CallNextMethodProcs = find_all_parts( FM );
    update_list( CallNextMethodProcs, false );
    ( destpair( CallNextMethodProcs ) -> CallNextMethodProcs )();
enddefine;

define updaterof systrace_method( FM ); lvars FM;
    dlocal poptraceindent = poptraceindent + 1;
    dlocal UCallNextMethodProcs = find_all_parts( FM.updater );
    update_list( UCallNextMethodProcs, true );
    ( destpair( UCallNextMethodProcs ) -> UCallNextMethodProcs )();
enddefine;

define has_traced_parts( m ); lvars m;
    if m.isgeneric then
        lvars p = m.pdpart;
        p.isclosure and p.datalength == 1 and
        p.pdpart == systrace_method
    else
        false
    endif
enddefine;

constant procedure trace_method_table =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false, false
    );

;;; This is the cunning bit.  We add a hook to method-linking in order
;;; to make the tracing work cleanly.
;;;
vars relink_hook =
    procedure( M ); lvars M;
        if trace_method_table( M ) then
            systrace_method(% M.full_method %) -> M.pdpart;
            M.pdpart.updater -> M.updater;
        endif;
    endprocedure;

define lconstant trace_me( w, id, trace_flag );
    lvars w, id, trace_flag;
    lvars p = idval( id );
    if p.isgeneric then
        unlink_method( p );
        trace_flag -> trace_method_table( p );
    else
        mishap( 'METHOD NEEDED', [^w] )
    endif;
enddefine;

define lconstant do_trace_method( trace_flag ); lvars trace_flag;
    repeat
        lvars w = readitem();
        quitif( w == ";" );
        nextif( w == "," );
        sysPUSHQ( w );
        sysIDENT( w );
        sysPUSHQ( trace_flag );
        sysCALLQ( trace_me );
    endrepeat;
    ";" :: proglist -> proglist;
enddefine;

define syntax trace_method;
    do_trace_method( true )
enddefine;

define syntax untrace_method;
    do_trace_method( false )
enddefine;

define syntax untrace_every_method;
    procedure( M ); lvars M;
        unlink_method( M );
        true -> trace_method_table( M );
    endprocedure.app_all_methods;
    ";" :: proglist -> proglist;
enddefine;

define syntax trace_every_method;
    clearproperty( trace_method_table );
    procedure( M ); lvars M;
        unlink_method( M );
    endprocedure.app_all_methods;
    ";" :: proglist -> proglist;
enddefine;

endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 11/12/92, sfk
;;;     *   Corrected the way systrace_method worked so that
;;;         it is possible to trace call_next_method.
;;; -------------------------------------------------------------------------

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
