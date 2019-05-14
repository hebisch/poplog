/* --- Copyright University of Birmingham 2001. All rights reserved. ------
 > File:            $poplocal/local/auto/mimedecode.p
 > Purpose:         Do the equivalent of mimencode -u to decode BASE64 file
 > Author:          Aaron Sloman, Jun  2 2000 (see revisions)
 > Documentation:   Below
 > Related Files:   LIB * mimencode, ved_readmime, ved_writemime, MAN mimencode
 */


/*
define mimedecode(input, output);

input is a string, representing a file name, or a character repeater.

output is a string, representing a file name, or a character consumer.

6 bit characters from input are read in four at a time, decoded, and then
output as three 8 bit characters. Newlines are ignored.

The final group of four characters may end with one or two = signs, and in that
case have to treated specially. If there is only one `=`, then use the first
three characters to generate two characters to be output. If there are two `=`
signs, then use only the first two characters to generate one character to be output.


See RFC 2045

*/

/*

For test examples, seee LIB * mimencode

;;; should produce a mishap
mimedecode('.login', charout);


*/


compile_mode :pop11 +strict;

section;

define mimedecode(input, output);
    ;;; read in characters in the mimencoded file, and spew out via
    ;;; output the mime-encoded version

    define lconstant decode(num) -> char;
        ;;; decode character in mime-encoded file. Could be made a lot more
        ;;; efficient.
        locchar(num, 1,
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/') -> char;
        if char then char fi_- 1 -> char
        else
            mishap('Unexpected character read in', [^num, ^input])
        endif;

    enddefine;

    lvars
        procedure infile =
            if isstring(input) then discin(input) else input endif,

        procedure out_char =
            if isstring(output) then discout(output) else output endif,

        field1, field2, field3, field4,
        finished = false,
        ;

    define lconstant outfile(char);
        if char == `\r` then
            if out_char == vedcharinsert then
                ;;; ignore carriage returns
            else
                out_char(char)
            endif;
        else
            out_char(char);
        endif;
    enddefine;

    repeat

        ;;; repeatedly read in a group of four characters and output a group of 3
        infile() -> field1;
        if field1 == `\n` then
            infile() -> field1;
        endif;

        quitif(field1 == termin);

        decode(field1) -> field1;

        ;;; Read in remaining 6 bit characters
        ;;; to be converted (with field1) to three 8 bit ones.
        infile() -> field2;
        infile() -> field3;
        infile() -> field4;

        if field4 == `=` then
            ;;; last group of 4
            outfile( (field1 << 2) fi_|| ((decode(field2) ->> field2) fi_>> 4));
            if field3 == `=` then
                ;;; use only field1 and field 2;
            else
                outfile( ((field2 fi_&& 2:1111) fi_<< 4) fi_|| ((decode(field3) ->>field3) >> 2));
            endif;
            quitloop();

        else

            outfile( (field1 << 2) fi_|| ((decode(field2) ->> field2) >> 4));
            outfile( ((field2 fi_&& 2:1111) fi_<< 4) fi_|| ((decode(field3) ->> field3) >> 2));
            outfile( ((field3 fi_&& 2:11) fi_<< 6) || decode(field4));
        endif;

    endrepeat;

    if outfile == charout then
        sysflush(popdevout);
    elseunless out_char == vedcharinsert then
        outfile(termin);
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Feb  1 2001
    Changed so that character `\r` is ignored if inserting into ved.
        (For text files from PC users).
    Also changed not to print term if writing into Ved.
 */
