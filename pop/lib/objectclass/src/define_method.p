/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/define_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass => define_method;

sysunprotect( "define_method" );

define :define_form global method();
    method_form( "METHOD", "enddefine" )
enddefine;

sysprotect( "define_method" );

endsection;
