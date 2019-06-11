/* --- Copyright University of Sussex 2000. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedlinuxkeys.p
 > Purpose:         Set up VED key bindings for linux type terminals
 > Author:          Andrew Sayers (Birmingham), 16 Feb 2000
                    Extended by Aaron Sloman, 30 Jul 2001
 > Documentation:   REF * VEDTERMINALS, HELP * LINUXTERM
 > Related Files:   LIB * VEDXTERMKEYS, LIB * VEDLINUXSCREEN, LIB * VEDLINUX
 */
compile_mode :pop11 +strict;

section;

uses vedxtermkeys;

define vedlinuxkeys();
    vedxtermkeys();
    ;;; F1
    vedsetkey('\^[[[A', "vedmarklo");
    vedsetkey('\^[\^[[[A', "ved_mbf");
    ;;; F2
    vedsetkey('\^[[[B', "vedmarkhi");
    vedsetkey('\^[\^[[[B', "ved_mef");
    ;;; F3
    vedsetkey('\^[[[C', "vedclearhead");
    vedsetkey('\^[\^[[[C', "ved_yankw");
    ;;; F4
    vedsetkey('\^[[[D', "vedlinedelete");
    vedsetkey('\^[\^[[[D', "ved_yankl");
    ;;; F5
    vedsetkey('\^[[[E', "vedcleartail");
    vedsetkey('\^[\^[[[E', "ved_yankw");
    ;;; F6
    vedsetkey('\^[[17~', "vedwordleftdelete");
    vedsetkey('\^[\^[[17~', "ved_yankw");
    ;;; F7
    vedsetkey('\^[[18~', "vedwordrightdelete");
    vedsetkey('\^[\^[[18~', "ved_yankw");
    ;;; F8
    vedsetkey('\^[[19~', "ved_m");
    vedsetkey('\^[\^[[19~', "ved_mi");
    ;;; F9
    vedsetkey('\^[[20~', "ved_t");
    vedsetkey('\^[\^[[20~', "ved_ti");
    ;;; F10
    vedsetkey('\^[[21~', "vedrefresh");
    vedsetkey('\^[\^[[21~', "ved_mo");
    ;;; F11
    vedsetkey('\^[[23~', "vedpushkey");
    vedsetkey('\^[\^[[23~', "vedpopkey");
    ;;; F12
    vedsetkey('\^[[24~', "vedexchangeposition");
    vedsetkey('\^[\^[[24~', "ved_crm");

    ;;; NEXT FOUR ALREADY SET
    ;;; vedsetkey('\^[Op', "vedwordleft");
    ;;; vedsetkey('\^[On', "vedwordright");
    vedsetkey('\^[OM', "vedenter");
    vedsetkey('\^[Ol', "vedstatusswitch");
    vedsetkey('\^[OS', "vedredocommand");

    ;;; strange Delete key contents, also Home, End
    vedsetkey('\^[[3~', "veddotdelete");
    vedsetkey('\^[[1~', "vedtopfile");
    vedsetkey('\^[[4~', "vedendfile");
enddefine;

endsection;
