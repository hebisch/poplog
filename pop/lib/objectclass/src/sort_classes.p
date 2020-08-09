/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/sort_classes.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Comparing and Sorting Classes ----------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;

;;; -- Comparing Classes ----------------------------------------------------

define classes_differ( path1, path2 ); lvars path1, path2;
    lvars n1 = datalength( path1 );
    lvars n2 = datalength( path2 );
    lvars i;
    for i from 1 to min( n1, n2 ) do
        if path1( i ) /== path2( i ) then
            return( true )
        endif;
    endfor;
    return( n1 > n2 or n2 > n1 );
enddefine;

endsection;
