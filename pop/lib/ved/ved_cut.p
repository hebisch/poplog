/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_cut.p
 >  Purpose:        Cut text between last two positions on vedpositionstack
                    and store in VVEDCUT_DUMP
 >  Author:         Aaron Sloman, Jan 1985 (see revisions)
 >  Documentation:  HELP * VEDCOMMS /ved_cut
 >  Related Files:  LIB * VED_SPLICE, LIB * VED_DS
 */
compile_mode :pop11 +strict;

;;; Text may be re-inserted at current location using VED_SPLICE

section;

vars vvedcut_dump = [];

define vars ved_cut();
    lvars p1,p2, linea cola flag lineb colb line col;
    dlocal vveddump, vvedmarkprops;
    unless listlength(vedpositionstack) > 1 then
        vederror('NOT ENOUGH STACKED POSITIONS FOR CUT')
    endunless;

    fast_destpair(fast_destpair(vedpositionstack)) -> vedpositionstack -> p2 -> p1;
    ;;; make p1 the earlier position
    if p1(1) > p2(1) or (p1(1) == p2(1) and p1(2) > p2(2)) then
        p1,p2 -> p1 -> p2
    endif;
    p1(1)-> linea; p1(2)-> cola;
    p2(1)-> lineb; p2(2)-> colb;
    vedpositionpush(); vedline -> line; vedcolumn -> col;
    vedjumpto(p2);
    if (lineb == linea fi_+ 1 and colb == 1
        and cola fi_> vedusedsize(vedbuffer(linea)))
    or (linea == lineb and cola == colb)
    then
            ;;; nothing to cut. don't alter vvedcut_dump
        return
    elseif line == lineb and line /== linea then
        ;;; messy if cursor to right of text
        if colb fi_> vvedlinesize then  ;;; cutting whole line
            min(col,vvedlinesize) -> col;
        elseif veddelspaces and col fi_> colb then
            ;;; adjust col to allow for spaces to be deleted when line breaks
            while vedcurrentchar() == `\s` do
                vedcharright(); col fi_- 1 -> col;
            endwhile;
        endif
    endif;
    if lineb == linea then
        min(vvedlinesize fi_+ 1, vedcolumn) -> vedcolumn;
        [%subvedstring(cola,vedcolumn fi_- cola,vedthisline())%] -> vvedcut_dump;
        until vedcolumn fi_<= cola do vedchardelete() enduntil;
        vedrefreshrange(vedline,vedline,undef);
    else
        ;;; p2 on subsequent line -- delete intervening stuff
        vedmarkpush();
        false -> vvedmarkprops;
        if vedcolumn fi_> vvedlinesize then
            ;;; whole of lineb to be deleted, so mark it.
        else
            ;;; break it
            unless vedcolumn == 1 then vedcharinsert(`\n`); endunless;
            vedcharup();
        endif;
        vedmarkhi();
        vedjumpto(linea,cola);
        if vedcolumn fi_> vvedlinesize then vednextline()
        elseunless vedcolumn == 1 then  vedcharinsert(`\n`);
        endif;
        vedmarklo();
        ved_d(); vveddump -> vvedcut_dump; vedchardown();
        vedmarkpop();
    endif;
    ;;; sort things out if cursor was on lines changed.
    if line fi_< linea or line fi_> lineb then    ;;; not in cut area
        vedpositionpop()
    else
        cola == 1 -> flag;      ;;; true if cut from start of linea
        if line == linea then   ;;; originally on same line as p1
            vedjumpto(line,col);
            if flag and lineb /== linea then 1 -> vedcolumn ;;; whole line gone
            elseif col >= cola then ;;; to right of cut point
                if linea /== lineb then vednextline();      ;;; was in cut area
                elseif col < colb then cola -> vedcolumn    ;;; ditto
                else col fi_- (colb fi_- cola) -> vedcolumn ;;; was right of cut
                endif;
            endif
        else  ;;; somewhere after linea, but not beyond lineb
            vedjumpto(if flag then linea else linea fi_+ 1 endif, 1);
            if line == lineb then
                ;;; originally on same line as p2
                if col fi_<= colb then 1 else col - colb fi_+ 1 endif -> vedcolumn
            endif
        endif;
    endif
enddefine;


endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Mar 27 1996
        Changed vedcharinsert(`\r`) to vedcharinsert(`\n`)
--- John Gibson, Sep  7 1995
        Changed to use subvedstring
--- John Gibson, Jan 18 1992
        Changed to use subdstring
--- Jonathan Meyer, Jun 17 1991 Made it compile under compile mode strict
--- Aaron Sloman, May 12 1989 - Avoid error if cutting to right end of line
--- Aaron Sloman, Jun 2 1986 - Avoid refreshing so much
--- Aaron Sloman, Jul 21 1985 - Yet more cases distinguished.
--- Aaron Sloman, Jun 19 1985 - modified to be far more sensitive to relations
    between cursor position and portion of text cut.
 */
