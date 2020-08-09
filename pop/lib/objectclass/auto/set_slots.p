/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/set_slots.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section;

sysunprotect( "set_slots" );

define set_slots( /*item,*/ slot_inits ) with_nargs 2;
    unless slot_inits.islist then
        if slot_inits.isinteger then
            slot_inits.conslist -> slot_inits;
        elseif slot_inits.isvector then
            slot_inits.destvector.conslist -> slot_inits;
        else
            mishap( 'NEEDED SLOT INITIALISER', [^slot_inits] )
        endif;
    endunless;
    lvars item = ();
    until slot_inits.null do
        lvars ( field, value, slot_inits ) = slot_inits.dest.dest;
        unless #| value -> field( item ) |# == 0 do
            mishap( 'EXTRA RESULTS FROM SLOT INITIALISER', [^field] )
        endunless;
    enduntil
enddefine;

sysprotect( "set_slots" );

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 12 1997
        Allowed more general form of slot_inits to conform to description
        in REF OBJECTCLASS.
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
