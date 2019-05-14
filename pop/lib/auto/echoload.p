/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/echoload.p
 >  Purpose:        echo file as it is loaded
 >  Author:         Steven Hardy, Sep 1979 (see revisions)
 >  Documentation:  HELP * ECHOLOAD
 >  Related Files:
 */

compile_mode :pop11 +strict;

section;

global vars procedure (pause = identfn, echoload_input);


define global vars macro echoload;
    dlocal echoload_input;
    lvars stack, last = `\n`, procedure outchar = cucharout;

    consvector(stacklength()) -> stack;

    procedure();
        dlocal popnewline = true;
        discin(sysfileok(rdstringto([; ^termin ^newline])))
    endprocedure() -> echoload_input;

    define dlocal pause();
        define dlocal interrupt();
            popready();
            exitfrom(pause);
        enddefine;
        until charin() == `\n` then enduntil
    enddefine;

    subsystem_compile(
        procedure() -> char;

            define dlocal interrupt();
                exitto(nonmac echoload)
            enddefine;

            echoload_input() -> char;
            unless char == termin then
                if last == `\n` then
                    appdata(popprompt, outchar)
                endif;
                outchar(char)
            endunless;
            char -> last;
        endprocedure, "pop11");

    /* interrupt comes out here */
    clearstack();
    explode(stack)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  3 1996
        Fixed BR johnw.1035
--- Aaron Sloman, Aug 17 1986
        Fixed so as not to need ";"
        Used lvars instead of sections
        Use echoload_input to hold character repeater
*/
