/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/create_instance.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => create_instance;

sysunprotect( "create_instance" );

define global procedure create_instance( arg ) -> inst with_nargs 3;
    lvars modify = false;
    if arg.isprocedure then
        arg -> modify;
        () -> arg;
    endif;

    lvars slot_inits =
        if arg.isinteger then
            arg.conslist
        elseif arg.islist then
            arg
        elseif arg.isvector then
            arg.destvector.conslist
        else
            mishap( 'NEEDED SLOT INITIALISER', [^arg] )
        endif;

    lvars item = ();
    if item.iskey then
        lvars new = class_new( item );
        if new then
            new()
        else
            mishap( 'CLASS NEEDED', [^item] )
        endif
    else
        ;;; Assume it is a prototypical instance -- copy it.  Should
        ;;; this be checked?  I think it shouldn't on the basis of
        ;;; generality.
        copy( item )
    endif -> inst;

    set_slots( inst, slot_inits );

    if modify then
        unless #| modify( inst ) |# == 0 do
            mishap( 'EXTRA ITEMS AFTER MODIFIER FINISHED', [^modify] )
        endunless
    endif
enddefine;

sysprotect( "create_instance" );

endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 12/04/93, sfk
;;;     *   Removed illegal sys_grbg_list.  Unfortunately one cannot prevent
;;;         suspend's from occuring in the region between list allocation
;;;         and deallocation.
;;; -------------------------------------------------------------------------

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul  6 1999
        Changed not to use instance syntax word as a variable name
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
