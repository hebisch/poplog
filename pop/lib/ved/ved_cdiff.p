/* --- Copyright University of Birmingham 1996. All rights reserved. ------
 > File:            C.all/lib/ved/ved_cdiff.p
 > Purpose:         Detecting differences in files, ignoring 'white' space
 > Author:          Aaron Sloman, Jan 22 1996 (see revisions)
 > Documentation:   HELP * CDIFF
 > Related Files:   LIB * VED_NEXTCSAME, LIB * VED_DIFF
 */

compile_mode :pop11 +strict;

;;; Ignore spaces tabs and newlines (or other characters in the string
;;; VED_CDIFF_IGNORE) while comparing two files.


section $-ved => ved_cdiff ved_cdiff_ignore ved_ignore_case;

global vars ved_cdiff_ignore;   ;;; characters to ignore

unless isstring(ved_cdiff_ignore) then
    '\s\t\n\r\St\Sp\Sn' -> ved_cdiff_ignore;
endunless;

global vars ved_ignore_case;
unless isboolean(ved_ignore_case) then
    false -> ved_ignore_case;
endunless;


define vars ved_cdiff();
    lvars char1,char2,
        buff1,l1,c1,s1,llim1,clim1,
        buff2,l2,c2,s2,llim2,clim2,result;
    dlocal vedstartwindow, vedwarpcontext = false;

    define lconstant procedure rep1()-> char1;
        lvars char1;
        ;;; character repeater for buffer 1
        repeat
            if l1 == llim1 then
                termin -> char1
            elseif c1 == clim1 then
                `\n` -> char1;
                1 -> c1; l1 fi_+ 1 -> l1;
                vedusedsize(fast_subscrv(l1 ,buff1)->>s1) fi_+ 1 -> clim1;
            else
                fast_subscrs(c1,s1) -> char1;
                c1 fi_+ 1 -> c1
            endif;
            if char1 == termin then return()
            elseunless locchar(char1, 1, ved_cdiff_ignore) then return()
            endif;
        endrepeat
    enddefine;

    define lconstant procedure rep2()-> char2;
        lvars char2;
        ;;; character repeater for buffer 2
        repeat
            if l2 == llim2 then
                termin -> char2;
            elseif c2 == clim2 then
                `\n` -> char2;
                1 -> c2; l2 fi_+ 1 -> l2;
                vedusedsize(fast_subscrv(l2 ,buff2)->>s2) fi_+ 1 -> clim2;
            else
                fast_subscrs(c2,s2) -> char2;
                c2 fi_+ 1 -> c2
            endif;
            if char2 == termin then return()
            elseunless locchar(char2, 1, ved_cdiff_ignore) then return()
            endif;
        endrepeat
    enddefine;

    if listlength(vedbufferlist) fi_< 2 then
        vederror('Only one file being edited');
    endif;
    if vedline > vvedbuffersize then vedenderror() endif;
    vedtrimline();
    ;;; set up variables for this buffer
    vedbuffer->buff1,vedline->l1,vedcolumn->c1,
        vedthisline()->s1,vvedbuffersize+1->llim1,vvedlinesize+1->clim1;
        min(c1,clim1) -> c1;

    if vedstartwindow == vedscreenlength then
        vedscreenlength >> 1 -> vedstartwindow;
    endif;

    vedswapfiles();

    if vedline > vvedbuffersize then vedenderror() endif;
    ;;; set up variables for other buffer
    vedbuffer->buff2,vedline->l2,vedcolumn->c2,
        vedthisline()->s2,vvedbuffersize+1->llim2,vvedlinesize+1->clim2;
        min(c2,clim2) -> c2;
    vedswapfiles();
    vedputmessage('SEARCHING');
    ;;; run each repeater ignoring spaces, newlines, tabs, etc
    ;;; until one or other is exhausted or they differ
    repeat
        rep1() -> char1;
        rep2() -> char2;
        if char1 == termin or char2 == termin then
            if char1 == char2 then
                vedputmessage('NO DIFFERENCE'); return()
            else quitloop();
            endif
        elseif char1 == char2 then   ;;; continue searching
        else
            ;;; difference found
            if ved_ignore_case then
                if (char2 fi_<= `Z` and `A` fi_<= char2
                    and char1 == (char2 || 2:0100000))
                or (char1 fi_<= `Z` and `A` fi_<= char1
                    and char2 == (char1 || 2:0100000))
                then nextloop()
                else quitloop()
                endif
            else quitloop()
            endif
        endif
    endrepeat;
    vedswapfiles();
    vedjumpto(l2,c2);
    vedcheck();
    vedswapfiles();
    vedjumpto(l1,c1);
    vedcheck();

    ;;; Now set up nextcsame command
    dlocal ved_on_status = true;
    lconstant nextcsame_command = 'nextcsame';
    vedcharup();
    if vedthisline() = nextcsame_command then
        vedlinedelete();
    endif;
    false -> ved_on_status;
    vedputcommand(nextcsame_command);

    vedputmessage('FOUND NON_MATCHING CHARACTERS');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 22 1996
        Installed at Sussex
--- Aaron Sloman, May 3 1993
        Extended ved_cdiff_ignore
        Fixed to work with ved_nextcsame
--- John Gibson, Jun  4 1991
        Uses vedwarpcontext
--- Aaron Sloman, Jul 10 1990
    Fixed not to keep reopening windows under pw*m
--- Aaron Sloman, Nov 18 1986 further efficiency improvements. Use locchar
    not strmember, re-order case test, and increase limits by 1 so that ==
    can be used instead of >
--- Aaron Sloman, Nov 17 1986 made much faster by major reorganisation.
*/
