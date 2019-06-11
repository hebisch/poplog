/* --- Copyright University of Birmingham 2001. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_readmime.p
 > Purpose:         Read in a specified file and mimencode, or mimedecode it
 > Author:          Aaron Sloman, Feb  1 2001
 > Documentation:
 > Related Files:   LIB * mimencode, mimedecode, ved_writemime
 */

section;

uses mimencode

define ved_readmime();
    ;;; ENTER readmime <filename>
    ;;; ENTER readmime -decode <filename>

    dlocal
        vedbreak = false,
        vedstatic = false,
        vedediting = false;     ;;; don't show till finished

    vednextline();

    if isstartstring('-decode ', vedargument) then
        allbutfirst(datalength('-decode '), vedargument) -> vedargument;
        mimedecode(vedargument, vedcharinsert)
    else

        mimencode(vedargument, vedcharinsert);
        ;;; delete the termin produced at the end
    endif;
    chain(vedrefresh);
enddefine;  

endsection;
