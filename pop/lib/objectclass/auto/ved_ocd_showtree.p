/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/ved_ocd_showtree.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass => ved_oca_showtree ved_ocd_showtree;

uses ved_oca;
uses showtree;

define ocx_showtree( reln ); lvars reln;

    define dlocal showtree_root( x ); lvars x;
        x.hd -> x;
        if x then x.class_name else ' ... ' endif
    enddefine;

    if vedargument = nullstring then
        vederror( 'NO ARGUMENT' )
    endif;
    lvars name = vedargument.consword <> "_key";
    if name.isdefined then
        lvars val = identof( name ).idval;
        if val.isclass then
            showtree( make_tree( val, reln ) );
        else
            vederror( 'CLASS NAME REQUIRED' )
        endif
    else
        vederror( 'NO SUCH CLASS' )
    endif
enddefine;

define global vars procedure ved_ocd_showtree();
    ocx_showtree( inferiors )
enddefine;

define global vars procedure ved_oca_showtree();
    ocx_showtree( supers_of_class )
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Fixed various typos and added: uses ved_oca
 */
