/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedwyse.p
 >  Purpose:        FOR WYSE terminal emulating Televideo 925
 >  Author:         A.Sloman, 1984 (see revisions)
 >  Documentation:  HELP * VEDWYSE
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; This package is provided for user convenience. It is not supported,
;;; and it may change.

;;; Based largely on Aaron Sloman's TVI.p of June 82 for VT52 emulation.


;;; The following assumes 16 function keys, F1 to F16, which transmit
;;;     ESC char (char = A to P unshifted,
;;;     ! @ # $ % ^ & * ( ) _ + - = { }  shifted)

;;; Then there are 6 more keys (which don't seem to work!!!):
;;;
;;;         KEY             : FUNCTION
;;;         Char Insert     : Line Insert Above
;;;         Char Delete     : Delete Char
;;;         Line Insert     : Line Insert Below
;;;         Line Delete     : Line Delete
;;;         Line Erase      : Refresh Screen
;;;         Page Erase      : Enter Command

;;; The four Arrow function keys produce small moves in the appropriate
;;; direction. When preceeded by ESC they act as they would on the Visual 200
;;; without the ESC (i.e. large moves).

;;; FUNC key doesn't work


;;; To Enter commands use ESC RERUTN



;;; a few VED procedures need to be re-defined.

;;; suppress interrogation of terminal
false -> vedterminalselect;

define vedscreeninsertchar(_x);
    vedscreenescape(vvedscreeninsertmode);
    true -> vedinserting;
    vedscreenoutput(_x);
    false -> vedinserting;
enddefine;

define vedscreencharright();
;;; the original version puts in an ESC which cocks up the 925
    vedoutascii(vvedscreencharright);
    vedscreencolumn fi_+ 1 -> vedscreencolumn;
enddefine;

/*
define vedreadfunctionkey();
    ;;; when the FUNCTION button is depressed, any other key causes
    ;;; transmission of CTRL-A, then the normal code, then RETURN
    ;;; This procedure is invoked when CTRL-A is read in
    vars vedchar;
    vedinascii() -> vedchar;    ;;; Read the character
    erase(vedinascii());    ;;; read carriage return, but ignore it

    switchon vedchar ==
    case `7  then vedcharupleft()    ;;; Keypad + FUNCT still does
                                     ;;; diagonal moves
    case `8  then vedcharup()
    case `9  then vedcharupright()
    case `4  then vedcharleft()
    case `5  then vedcharmiddle()
    case `6  then vedcharright()
    case `1  then vedchardownleft()
    case `2  then vedchardown()
    case `3  then vedchardownright()
    case `0  then vedstatusswitch()
    case `,  then vedwordleft()
    case `.  then vedwordright()
    case `\^A` then vedscreenleft()     ;;; CTRL-A typed 3 times will do this!
    case `a then vedtopfile();
    case `z then vedendfile();
    case `g then vedredokey();      ;;; redo last command
    case `- then vedredokey();
    case `\^M then vedenterkey();          ;;; treat return key as ENTER
    case `@ then vedclearhead();        ;;; F1
    case `A then vedlinedelete();       ;;; F2
    case `B then vedcleartail();        ;;; F3
    case `C then vedwordleftdelete();   ;;; F4
    case `D then vedwordrightdelete();  ;;; F5
    case `E then vedsetstatic();        ;;; F6
    case `F then vedmarklo();           ;;; F7
    case `G then vedmarkhi();           ;;; F8
    case `H then ved_m();               ;;; F9
    case `I then vedpushkey();          ;;; F10
    case `J then vedpopkey();           ;;; F11
    else vedscreenbell()
    endswitchon
enddefine;
*/

define vedwyse();
    vednographics();    ;;; replaces graphic codes

    false ->> veddumbvdu ->> vednocharinsert
        ->vednolineinsert;
    true ->> vedscrollscreen -> vednokeypad;

    copy(vednormaltable) -> vednormaltable;
    copy(vedescapetable) ->> vedescapetable -> vednormaltable(`\^[`);

    ;;; characters to control VDU modes and actions
    identfn -> vvedscreenresetpad;
    identfn -> vvedscreensetpad;
    identfn -> vvedscreenalpha;
    identfn -> vvedscreengraphic;
    identfn -> vvedscreenovermode;

    `\^[` -> vedescape;

    `T` -> vvedscreencleartail;
    `W` -> vvedscreendeletechar;
    `R` -> vvedscreendeleteline;
    `E` -> vvedscreeninsertline;
    `Q` -> vvedscreeninsertmode;    ;;; works only for one character
    `=` -> vvedscreenpoint;

    `\^H` -> vvedscreencharleft;
    `\^L` -> vvedscreencharright;
    `\^K` -> vvedscreencharup;

    vedsetkey('\^[\^M',"vedenterkey");         ;;; ESC RETURN, go to command line

    vedsetkey('\^[A',veddotdelete);      ;;; F1 delete character under cursor
    vedsetkey('\^[!', vedrefresh);             ;;; shift F1 refresh screen

    vedsetkey('\^[B',vedclearhead);      ;;; F2 delete left of line
    vedsetkey('\^[@', ved_yankw);        ;;; SHIFT F2 Undo last part line delete
    vedsetkey('\^[C',vedlinedelete);     ;;; F3 delete whole line
    vedsetkey('\^[#', ved_yankl);        ;;; SHIFT F3 Undo last line delete
    vedsetkey('\^[D',vedcleartail);      ;;; F4 delete right of line
    vedsetkey('\^[E', vedwordleftdelete);    ;;; F5 Delete word left of cursor
    vedsetkey('\^[%', vedwordrightdelete);   ;;; SHIFT F5 Delete word right of cursor
    vedsetkey('\^[F',vedmarklo);         ;;; F6 mark start of range
    vedsetkey('\^[G',vedmarkhi);         ;;; F7 mark end of range
    vedsetkey('\^[H',ved_m);            ;;; F8 move marked range after here
    vedsetkey('\^[h',ved_t);            ;;;Shift F8 copy marked range after here
    vedsetkey('\^[I', vedpushkey);               ;;; F9 Push Current Position
    vedsetkey('\^[J', vedpopkey);                ;;; F10 Pop Current Position
    vedsetkey('\^[)', vedexchangeposition);  ;;; SHIFT F10 Swap Current Position
    vedsetkey('\^[K', vedlineabove);         ;;; F11 insert line above
    vedsetkey('\^[L', vedlinebelow);         ;;; F12 insert line below
    vedsetkey('\^[M', vedtopfile);           ;;; F13 Go to top of file
    vedsetkey('\^[N', vedendfile);           ;;; F14 Go to end of file
    vedsetkey('\^[O', vedwordleft);          ;;; F15 move word left
    vedsetkey('\^[P', vedwordright);         ;;; F16 move word right

    vedsetkey('\^[-', "vedredokey");       ;;; ESC - (hyphen) Redo last command
    vedsetkey('\^[,', vedstatusswitch);      ;;; ESC , (comma) switch status

    ;;; ESC followed by keypad number should do a big move

    vedsetkey('\^[7',vedcharupleftlots);
    vedsetkey('\^[8',vedcharuplots);
    vedsetkey('\^[9',vedcharuprightlots);
    vedsetkey('\^[4',vedcharleftlots);
    vedsetkey('\^[5',vedscreenmiddle);
    vedsetkey('\^[6',vedcharrightlots);
    vedsetkey('\^[1',vedchardownleftlots);
    vedsetkey('\^[2',vedchardownlots);
    vedsetkey('\^[3',vedchardownrightlots);


;;;    vedsetkey('\^[Q', vedlineabove);    ;;; char insert key
;;;    vedsetkey('\^[W', veddotdelete);    ;;; char delete key
;;;    vedsetkey('\^[E', vedlinebelow);    ;;; line insert key
;;;    vedsetkey('\^[R', vedlinedelete);   ;;; delete line key

;;;    vedsetkey('\^[T', vedrefresh);      ;;; Key above the 7 on keypad
;;;    vedsetkey('\^[Y', "vedenterkey");        ;;; Key above the 8 on keypad

;;;    vedsetkey('\^A',  vedreadfunctionkey); ;;; initiate function key sequence

    ;;; Large moves
    vedsetkey('\^[\^K',vedscreenup);    ;;; UP ARROW
    vedsetkey('\^[\^V',vedscreendown);  ;;; DOWN ARROW
    vedsetkey('\^[\^H',vedtextleft);    ;;; LEFT ARROW
    vedsetkey('\^[\^L',vedtextright);   ;;; RIGHT ARROW

    ;;; Small moves
    vedsetkey('\^K',vedcharup);
    vedsetkey('\^V',vedchardown);
    vedsetkey('\^H',vedcharleft);
    vedsetkey('\^L',vedcharright);

    ;;; PROBLEM - if you are on line 24, column 80 this terminal performs
    ;;;           a line feed automatically which screws up the display!
    ;;; HACK    - set line length to 79 so that this never happens!

;;;    79 -> vedscreenwidth;
;;;    77 -> vedlinemax;  ;;; avoids annoying horizontal scrolling for documents
enddefine;

vedwyse -> vedinit;
vedinit();
"wyse" -> vedterminalname;

/* --- Revision History ---------------------------------------------------
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
--- Aaron Sloman, Sep  5 1986 removed vednographic definition
*/
