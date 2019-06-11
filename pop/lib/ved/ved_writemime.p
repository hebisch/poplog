/* --- Copyright University of Birmingham 2001. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_writemime.p
 > Purpose:         Write the marked range and mimencode or mimedecode it
 > Author:          Aaron Sloman, Feb  1 2001
 > Documentation:
 > Related Files:   LIB * mimencode, mimedecode, ved_writemime
 */



section;

uses mimencode

define ved_writemime();
    ;;; ENTER writemime <filename>
    ;;; ENTER writemime -decode <filename>

    dlocal
        vedbreak = false,
        vedstatic = false,
        ;

    vednextline();

    if isstartstring('-decode ', vedargument) then
        allbutfirst(datalength('-decode '), vedargument) -> vedargument;
        mimedecode(vedrangerepeater(vvedmarklo, vvedmarkhi), vedargument)
    else
        mimencode(vedrangerepeater(vvedmarklo, vvedmarkhi), vedargument);
        ;;; delete the termin produced at the end
    endif;
enddefine;  

endsection;
