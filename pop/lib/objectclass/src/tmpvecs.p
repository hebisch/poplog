/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/tmpvecs.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Temporary Vectors, i.e. vectors of phantoms --------------------------

compile_mode :pop11 +strict;

section $-objectclass;

;;;
;;; We rely heavily on the fact that datalength works on tmpvecs!  So
;;; they have to be implemented as vectors.  In this case we implement them
;;; as a vector of phantoms.
;;;

constant tmpvec_key = conskey( "tmpvec", "ulong" );
constant nulltmpvec = class_init( tmpvec_key )( 0 );

define constmpvec( n ) -> v; lvars n, v;
    lconstant inittv = class_init( tmpvec_key );
    lconstant subscr = class_fast_subscr( tmpvec_key );
    lvars v = inittv( fi_check( n, 0, false ) );
    lvars i;
    fast_for i from n by -1 to 1 do
        newphantom() -> subscr( i, v )
    endfor;
enddefine;

define subscrtmpvec( n, v ); lvars n, v;
    lconstant subscr = class_subscr( tmpvec_key );
    subscr( n, v ).contphantom
enddefine;

subscrtmpvec -> class_apply( tmpvec_key );

procedure( tv ); lvars tv;
    appdata( '<tmpvec', cucharout );
    lvars i;
    for i from 1 to datalength( tv ) do
        cucharout( ` ` );
        subscrtmpvec( i, tv ).pr;
    endfor;
    cucharout( `>` );
endprocedure -> class_print( tmpvec_key );

procedure( tv1, tv2 ); lvars tv1, tv2;
    lconstant subscr = class_subscr( tmpvec_key );
    lvars k = datalength( tv1 );
    if k == datalength( tv2 ) then
        lvars n;
        fast_for n from 1 to k do
            returnunless(
                contphantom( subscr( n, tv1 ) ) =
                contphantom( subscr( n, tv2 ) )
            )( false )
        endfor;
        return( true );
    else
        false
    endif
endprocedure -> class_=( tmpvec_key );


constant procedure lock_table =
    newanyproperty(
        [], 64, 1, false,
        false, false, "tmparg",
        false, false
    );

define ismeltedtmpvec( v ); lvars v;
    lvars melted = false;
    appdata(
        v,
        procedure( p ); lvars p;
            if p.isundefphantom then
                true -> melted
            endif
        endprocedure
    );
    return( melted );
enddefine;

define explode_tmpvec( v ); lvars v;
    appdata( v, contphantom )
enddefine;

;;; Prevents the elements of a tmpvec from melting away.  Note that locking
;;; twice does NOT create a period of unlockedness!  (important)
;;; Returns false if any element of the vector has melted.
define locktmpvec( v ) -> ok; lvars v;
    lvars data = [];
    lvars ok = true;
    appdata(
        v,
        procedure( p ); lvars p, c;
            conspair( contphantom( p ) ->> c, data ) -> data;
            ok and c /== undefphantom -> ok;
        endprocedure
    );
    data -> lock_table( v );
enddefine;

;;; Allows the elements of a tmpvec to melt away.  I always deal gingerly
;;; with sys_grbg_list.  Especially here.
define unlocktmpvec( v ); lvars v;
    lock_table( v );                ;;; put on stack.
    false -> lock_table( v );
    lvars x = ();                   ;;; pop from stack.
    if x.islist then
        sys_grbg_list( x );
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 12 1995
        Added Steve Knight's definition of equality on tmpvecs
 */
