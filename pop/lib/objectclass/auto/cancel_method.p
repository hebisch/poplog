/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/cancel_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => cancel_method;

sysunprotect( "cancel_method" );

define global constant syntax cancel_method;

    define lconstant procedure check_method( m ) -> m; lvars m;
        unless m.isgeneric do
            mishap( 'METHOD NEEDED', [^m] )
        endunless
    enddefine;

    define lconstant procedure cancel_all( m ); lvars m;
        unlink_method( m.check_method );
        lvars mt = m.method_table;
        [] -> uEntriesMethodTable( mt );
        [] -> cEntriesMethodTable( mt );
        "unbound" -> cArityMethodTable( mt );
        "unbound" -> uArityMethodTable( mt );
    enddefine;

    define lconstant procedure cancel_it( m, rpath ); lvars m, rpath;
        unlink_method( m.check_method );
        lvars mt = m.method_table;
        lvars path = rpath.rev;
        updateMethodTable( false, CALL_MODE, path, mt );
        updateMethodTable( false, UCALL_MODE, path, mt );
    enddefine;

    define lconstant procedure Locals();
        lvars continue = true;
        until pop11_try_nextreaditem( ")" ) do
            unless continue do
                ;;; This raises an error message with the right format.
                pop11_need_nextreaditem( "," ).erase
            endunless;
            lvars n = readitem();
            if pop11_try_nextreaditem( ":" ) then
                readitem() -> n
            endif;
            if n == "_" then
                "false"
            else
                n <> "_key";
            endif;
            pop11_try_nextreaditem( "," ) -> continue;
        enduntil
    enddefine;

    lvars want_comma = false;
    repeat
        lvars item = readitem();
        quitif( item == ";" or item == termin );
        if want_comma then
            pop11_need_nextreaditem( "," ).erase
        endif;
        sys_read_path( item, false, false ) -> item;
        if item.isprotected then
            mishap( item, 1, 'ATTEMPT TO CANCEL PROTECTED METHOD' )
        elseif pop11_try_nextreaditem( "(" ) then

            ;;; We need to ask whether or not this code is being executed
            ;;; immediately, with no prospect of being captured inside processes
            ;;; etc etc.
            unless popexecute do
                ;;; If this is compiled inside procedure we must protect the
                ;;; method tables.
                true -> can_trash_method_table
            endunless;

            lvars path = [% Locals() %];
            sysPUSH( item );
            sysPUSH( "popstackmark" );
            applist( path, sysPUSH );
            sysCALL( "sysconslist" );
            sysCALLQ( cancel_it );
        else
            ;;; Cancel all the method-parts.
            sysPUSH( item );
            sysCALLQ( cancel_all );
        endif;
        true -> want_comma;
    endrepeat;
    ";" :: proglist -> proglist
enddefine;

sysprotect( "cancel_method" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Integral Solutions Ltd, Feb 16 1994
        Added SteveK's fix for method cancelling in -cancel_it-.
 */
