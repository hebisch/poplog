/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedblocks.p
 > Purpose:        Commands for manipulating blocks of text in the editor
 > Author:         Aaron Sloman, May 21 1987 (see revisions)
 > Documentation:  HELP * VEDBLOCKS
 > Related Files:  LIB * VEDCUTBLOCK, * VEDYANKBLOCK, * VEDFILLBLOCK,
 >              * VEDOVERLAYBLOCK * VEDREFRESHBLOCK
 */
compile_mode :pop11 +strict;


;;; LIB VEDBLOCKS                                    Aaron Sloman May 1987

section;

global vars
    vvedblockdump={},       ;;; always a vector of strings

    vedchattyblocks = false,
        ;;; if true check with user before using stacked positions.

    ;


/*  +++++++++++++++UTILITIES++++++++++++++++  */

uses vedrefreshblock;
uses vedcutblock;
uses vedyankblock;
uses vedfillblock;
;;; uses vedoverlayblock;   ;;; left to be autoloaded when used

define lconstant getblockargs() -> line1->col1->line2->col2;
    ;;; Called by VED ENTER commands
    ;;; If two numbers given as argument use them to define number
    ;;; of lines down and columns across.
    ;;; If four numbers are given, use them to define start line,
    ;;; start column, end line end column. If no arguments are given then
    ;;; use top two positions on vedpositionstack and if vedchattyblocks is
    ;;; true then check that the user intended this
    lvars line1,col1,line2,col2,p1,p2,args,len,
        oldstack=vedpositionstack;  ;;; saved just in case
    dlocal vedchattyblocks;
    if vedargument = nullstring then
        ;;; get arguments from vedpositionstack
        unless listlength(vedpositionstack) > 1 then
            vederror('NOT ENOUGH STACKED POSITIONS FOR BLOCK')
        endunless;
        fast_destpair(fast_destpair(vedpositionstack))
            -> vedpositionstack -> p2 -> p1;
        p1(1)-> line1; p1(2)-> col1;
        p2(1)-> line2; p2(2)-> col2;
    else
        false -> vedchattyblocks;   ;;; no checking needed
        ;;; replace commas with spaces
        while strmember(`,`,vedargument)->> col1 do
            `\s` -> fast_subscrs(col1,vedargument);
        endwhile;
        listlength(sysparse_string(vedargument) ->> args) -> len;
        unless len == 2 or len == 4 then
            vederror('TWO OR FOUR ARGUMENTS NEEDED')
        endunless;
        if len == 2 then
            ;;; get line and column relative to current location
            (vedline ->> line1) + args(1) -1 -> line2;
            (vedcolumn ->> col1)  + args(2) -1 -> col2;
        else
            ;;; len == 4
            dl(args) -> col2 -> line2 -> col1 -> line1
        endif
    endif;
    ;;; make line2 > line1 and col2 > col1
    if line1 > line2 then line1,line2 -> line1->line2 endif;
    if col1 > col2 then col1,col2 -> col1->col2 endif;
    if vedchattyblocks then
        cons_with consstring {%
             dest_characters(
                 {'Chunk from line:' ^line1 'col:' ^col1 'TO line:'
                     ^line2 'col:' ^col2 ' OK? y/n'})
             %} -> args;
        vedputmessage(args);
        unless strmember(vedinascii(), 'yY\r') then
            oldstack -> vedpositionstack;
            vederror('ABORTED')
        endunless;
        vedputmessage(nullstring);  ;;;clear the message
    endif
enddefine;



/*  +++++++++++++++COMMANDS++++++++++++++++  */

define vars ved_dtb();
    ;;; Delete Text Block and refresh screen
    lvars line1,col1,line2,col2;
    getblockargs() -> line1->col1->line2->col2;
    vedcutblock(line1,col1,line2,col2,not(vedstatic)) -> vvedblockdump;
    if vedstatic then
        vedfillblock(line1,col1,line2,col2,`\s`);   ;;; will refresh
    else
        vedrefreshblock(line1,col1,line2,col2,true);
        ;;; adjust vedcolumn if to right of text-block removed
        if vedline fi_>= line1 and  vedline fi_<= line2
        and vedcolumn fi_>= col1 then
            max(col1, vedcolumn fi_- (col2 fi_- col1 fi_+ 1)) -> vedcolumn
        endif
    endif
enddefine;


define vars ved_sdtb;
    ;;; delete text-block, overlaying with spaces
    ;;; copy a text-block into vvedblockdump and replace it with spaces
    dlocal vedstatic=true;
    ved_dtb();
enddefine;

define vars ved_stb;
    ;;; Save a text-block by copying into into vvedblockdump
    lvars line1,col1,line2,col2;
    getblockargs() -> line1->col1->line2->col2;
    vedcutblock(line1,col1,line2,col2,false) -> vvedblockdump;
    ;;; no need to refresh
enddefine;

define vars ved_ytb;
    ;;; Yank text-block. Starting at current location insert strings
    ;;; stored in vvedblockdump into current line and subsequent lines
    vedyankblock(vvedblockdump);
enddefine;

define vars ved_sytb;
    ;;; Static mode yank text block
    ;;; yank in text from vvedblockdump, over-writing text in the buffer
    dlocal vedstatic=true;
    vedyankblock(vvedblockdump);
enddefine;

define vars ved_yotb();
    ;;; like ved_sytb but don't over write where there are spaces in
    ;;; vvedblockdump
    valof("vedoverlayblock")(vvedblockdump)
enddefine;

define vars ved_mtb;
    ;;; move text-block
    ved_dtb();
    vedyankblock(vvedblockdump);
enddefine;

define vars ved_smtb();
    ;;; In static mode move text block
    dlocal vedstatic = true;
    ved_mtb()
enddefine;

define vars ved_ttb;
    ;;; copy a text-block to current location
    ved_stb();
    vedyankblock(vvedblockdump);
enddefine;

define vars ved_sttb;
    ;;; in static mode copy a text-block to current location
    dlocal vedstatic=true;
    ved_ttb();
enddefine;

define vars ved_itb;
    ;;; Insert spaces in a text-block. Shift text right unless vedstatic true
    lvars line1,col1,line2,col2;
    getblockargs() -> line1->col1->line2->col2;
    vedfillblock(line1,col1,line2,col2,`\s`);   ;;; will refresh
enddefine;

define vars ved_sitb;
    ;;; in Static mode, insert spaces in text block
    dlocal vedstatic=true;
    ved_itb()
enddefine;

constant vedblocks = true;      ;;; for use with "uses"

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec  8 1989
    added vedoverlayblock and ved_yotb
--- Aaron Sloman, Nov 28 1988
    Added variable vedblocks, for use with "uses"
 */
