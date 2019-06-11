/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_nextsame.p
 > Purpose:         Find next location when this file and other file match
 > Author:          Aaron Sloman, Jun  1 1991 (see revisions)
 > Documentation:   HELP * VED_NEXTSAME, HELP * DIFF
 > Related Files:   LIB * VED_DIFF
 */

section;

;;; Search down at most ved_nextsame_limit lines in each file, looking
;;; for line that's the same in both files. User assignable limit.
global vars

    ved_nextsame_limit = 60,

    ved_nextsame_dot = false;   ;;; if false ignore lines starting '.'


define global ved_nextsame;
    ;;; Search for a pair of lines that are identical. Use steadily
    ;;; increasing depth of search until ved_nextsame_limit is reached

    dlocal vedwarpcontext = false;  ;;; suppress exposures

    lvars
        buffer1, buffer2,           ;;; The two top VED buffers
        linestart1, linestart2,     ;;; Current line in each file
        line1, line2,               ;;; Line being tested in each file
        lim1, lim2,                 ;;; The search limits
        string1,                    ;;; The text line in first buffer
        lim,                        ;;; Current limit of search
        ;

    ;;; See whether number argument specified
    strnumber(vedargument) -> lim;
    if lim then lim -> ved_nextsame_limit endif;

    ;;; Set up data-structures and variables for the two files
    vedbuffer -> buffer1;
    vedline -> linestart1;
    vedtrimline();
    vvedbuffersize -> lim1;

    vedswapfiles();
    vedbuffer -> buffer2;
    vedline -> linestart2;
    vedtrimline();
    vvedbuffersize -> lim2;

    vedswapfiles();

    if linestart1 > lim1 then vederror('AT END') endif;
    if linestart2 > lim2 then vederror('AT END OF OTHER FILE') endif;

    ;;; Try pairs of lines in increasing depth from start points
    ;;; (Zig-zagging along diagonals)
    fast_for lim from 0 to ved_nextsame_limit do
        for
            linestart2 fi_+ lim -> line2;
            linestart1 -> line1;
        step
            line2 fi_- 1 -> line2;
            line1 fi_+ 1 -> line1;
        till
            line2 fi_< linestart2 or line1 fi_> lim1
        do

            unless line2 fi_> lim2 then

                fast_subscrv(line1, buffer1) -> string1;

                if string1 /= nullstring
                and (ved_nextsame_dot or fast_subscrs(1, string1) /== `.`)
                and string1 = fast_subscrv(line2, buffer2)
                then
                    ;;; Two identical lines have been found
                    ;;; Set cursor location in both files
                    vedjumpto(line1,1);
                    vedswapfiles();
                    vedjumpto(line2,1);
                    vedswapfiles();
                    vedputcommand('nextdiff');
                    return();
                endif
            endunless

        endfor
    endfast_for;

    vedputmessage('NO COMMON LINE FOUND WITHIN '
                    sys_>< ved_nextsame_limit sys_>< ' LINES');
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Oct 10 1991
        Made -ved_nextsame- global.
--- John Gibson, Jun 11 1991
        Replaced incorrect use of vedusewindows with vedwarpcontext
 */
