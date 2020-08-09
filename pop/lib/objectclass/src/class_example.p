/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/class_example.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass => class_example;

constant procedure class_example_table =
    newanyproperty(
        [], 8, 1, false,
        false, false, "tmparg",
        false,
        procedure( k, p ); lvars k, p;
            class_new( k )() ->> p( k )
        endprocedure
    );

sysunprotect( "class_example" );

define global class_example( k ); lvars k;
    class_example_table( k )
enddefine;

define updaterof class_example( v, k ); lvars v, k;
    if datakey( v ) == check_datakey( k, key_key ) then
        v -> class_example_table( k )
    else
        mishap( 'occe: INCONSISTENT ARGUMENTS', [^v ^k] )
    endif
enddefine;

sysprotect( "class_example" );

endsection;
