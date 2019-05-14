/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/nprintf.p
 >  Purpose:        PRINTF followed by a newline
 >  Author:         John Williams, Jul 10 1985
 */
compile_mode:pop11 +strict;

section;

define global nprintf();
    printf();
    cucharout(`\n`)
enddefine;

endsection;
