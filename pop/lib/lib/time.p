/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.all/lib/lib/time.p
 > Purpose:        time how long a procedure takes (CPU and garbage time)
 > Author:         Aaron Sloman, Dec 1982 (see revisions)
 > Documentation:  HELP * TIME
 > Related Files:  LIB * GCTIME
 */
compile_mode :pop11 +strict;

;;; This construct reads a line from the input, obeys it
;;; and prints out the time taken to process the command
;;;
;;; If the line starts with ' * <integer> ', then the rest of the line is the
;;; command and the command is repeated <integer> times, e.g.
;;;     time * 3000  erase(sqrt(99));

section;

define global vars syntax time;
    lvars n, procedure proc, input, cpu, newcpu, gc, time,
        inved=iscaller(ved_lmr);
    dlocal popnewline;
    if inved then
        true -> popnewline; ;;; to recognise end of line
        [%until (readitem() ->> input) == termin or input == newline do
            input
        enduntil%]
    else
        readline()
    endif -> input;
    ;;; look for a multiplier clause
    if listlength(input) > 2 and input(1) == "*" and isinteger(input(2)) then
        destpair(back(input)) -> input -> time;
        ;;; input is the embedded action
        unless time > 0 then vederror('TIME needs positive integer: ' >< time) endunless;
        [lvars n = ^time; until n==0 do n fi_- 1 -> n; ^^ input enduntil] -> input
    else
        false -> time;
    endif;
    pop11_compile([procedure; ^^ input endprocedure]) -> proc;
    repeat
        if inved then vedcheck() endif;
        popgctime -> gc;
        systime() -> cpu;
        if time then
            ;;; measure time for loop
            time -> n;
            until n == 0 do n fi_- 1 -> n enduntil;
            systime() fi_- cpu -> cpu;   ;;; time taken for the loop
            cpu fi_+ systime() -> cpu;    ;;; notional start time
        endif;
        proc();
        max(systime(),cpu) -> newcpu;
        (newcpu fi_- cpu)/100.0 -> cpu;
        printf(
            (popgctime fi_- gc)/100.0, cpu, 'CPU TIME: %p   GC TIME: %p \n');
    endrepeat;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  5 1992
        Removed references to popconstruct
--- John Gibson, Oct 23 1992
        Made syntax
--- Aaron Sloman, Oct  7 1988
    Made popnewline true locally so that it can cope with commands
    that don't end with ";"
--- Aaron Sloman, Jan 25 1986
    Fixed so that can be used with marked range in VED
    Used lvars instead of section
    Changed to give more accurate account of time.
    Prevented negative times if '* <integer>' used.
    Changed to make popconstruct true
--- Roger Evans, July 1983 - MODIFIED - call of MATCHES replaced to avoid
    section variable problems
 */
