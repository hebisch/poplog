/* --- Copyright University of Birmingham 2001. All rights reserved. ------
 > File:            $poplocal/local/auto/mimencode.p
 > Purpose:         Use mimencode (BASE64) algorithm to encode a file
 > Author:          Aaron Sloman, Jun  2 2000 (see revisions)
 > Documentation:   Below for now
 > Related Files:   LIB * mimedecode,  MAN mimencode
 */

/*
define mimeencode(input, output);

input is a string, representing a file name, or a character repeater.

output is a string, representing a file name, or a character consumer.

8 bit characters from input are read in three at a time, the 24 bits divided
into four groups of 6 bits, the 6 bits encoded using this algorithm:

    define mimecode(num);
        fast_subscrs(num+1,
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/')
    enddefine;

and four 6 bit printable characters are output. After every 72 characters a
newline is output.

If the last block of characters contains fewer than three, then padding with
zeros is used to output two or three six bit characters, as appropriate, and
one or two `=` characters output to complete the group of four.

See RFC 2045

*/


/*
;;; Test examples.
mimencode('.login', 'test.mime');
mimedecode('test.mime', 'mime.out');

mimedecode('test.mime', charout);

mimencode('.login', charout);

mimencode('.cshrc', 'test.mime');
mimedecode('test.mime', 'mime.out');

mimencode('.twmrc', 'test.mime');
mimedecode('test.mime', 'mime.out');

mimencode('.Xdefaults', 'test.mime');
mimedecode('test.mime', 'mime.out');

mimedecode('test.mime', cucharout);

mimencode('.Xdefaults', charout);

*/

section;

define mimencode(input, output);
    ;;; read in characters in the file, and spew out via
    ;;; outfile the mime-encoded version

    define lconstant mimecode(num);
        fast_subscrs(num fi_+ 1,
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/')
    enddefine;

    lvars
        procedure infile =
            if isstring(input) then discin(input) else input endif,

        procedure outfile =
            if isstring(output) then discout(output) else output endif,

        col = 0, field1, field2, field3, finished = false;


    repeat
        infile() -> field1;
        if field1 == termin then
            unless col == 0 then
                outfile(`\n`)
            endunless;
            quitloop();
        endif;
        infile() -> field2;
        if field2 == termin then
            false ->> field2 -> field3;
            true -> finished;
        else
            infile() -> field3;
            if field3 == termin then
                false -> field3;
                true -> finished;
            endif;
        endif;
        ;;; we have now read in one, two or three 8 bit characters.
        ;;; output four 6 bit characters.
        ;;; output first field

        if finished then
            outfile(mimecode(field1 >> 2));
            if field2 then
                outfile(mimecode( ((field1 fi_&& 2:11) fi_<< 4) fi_|| (field2>>4) ) );
                outfile(mimecode( (field2 fi_&& 2:1111) fi_<< 2 ) );
                outfile(`=`);
            else
                outfile(mimecode( (field1 && 2:11) fi_<< 4 ) );
                outfile(`=`);
                outfile(`=`);
            endif;

            outfile(`\n`);
            quitloop();
        else
            ;;; output four characters
            outfile(mimecode(field1 >> 2));
            outfile(mimecode( ((field1 fi_&& 2:11) fi_<< 4) fi_|| (field2 >> 4) ) );
            outfile(mimecode( ((field2 fi_&& 2:1111) fi_<< 2) fi_|| (field3 >> 6) ) );
            outfile(mimecode( field3 fi_&& 2:111111 ));
            col + 4 -> col;
            if col == 72 then
                outfile(`\n`);
                0 -> col
            endif;
        endif;

    endrepeat;

    if outfile == charout then
        sysflush(popdevout);
    elseunless outfile == vedcharinsert then
        outfile(termin);
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Feb 11 2001
    Fixed bug where a string had been used instead of a character
 */
