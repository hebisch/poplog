/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/pop_null_device.p
 > Purpose:         Standard "null" device
 > Author:          John Williams, Nov 13 1990
 > Documentation:   REF * SYSIO
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

define lconstant Zero(); erasenum(), 0 enddefine;

lconstant
    Read_null   =   Zero(% 4 %),
    Test_null   =   Zero(% 1 %),
    Clear_null  =   erase,
    Write_null  =   erasenum(% 4 %),
    Flush_null  =   erase,
    Seek_null   =   Zero(% 3 %),
    ;

define lconstant Close_null() with_nargs 1;
    ->; exitfrom(sysclose)
enddefine;

global constant pop_null_device
    = consdevice('null', false, undef, 0,
                {%  {% Read_null, Test_null, Clear_null %},
                    {% Write_null, Flush_null %},
                    Seek_null, Close_null,
                %});

endsection;
