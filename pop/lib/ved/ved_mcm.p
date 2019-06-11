/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_mcm.p
 > Purpose:        Mark "current" message, in a Unix mail file
 > Author:         Aaron Sloman, Nov  1 1986 (see revisions)
 > Documentation:  See comments below
 > Related Files:  LIB * VED_REPLY  * VED_MDIR  * VED_SEND
 */
compile_mode :pop11 +strict;

;;; Mark Current Message

section;

;;; The following is a separate procedure because it may one day be
;;; useful outside ved_mcm. Till then make it lconstant

define lconstant vedmailbounds(proc1,proc2);

    ;;; Execute proc1 at beginning of message, proc2 at end
    ;;; Not totally reliable at finding bounds!

    lvars line, proc1, proc2;

    ;;; Protect ved search state variables
    dlocal ved_search_state;

    dlocal vedpositionstack;

    vedpositionpush();

    ;;; find text above cursor
    if vvedlinesize == 0 then
        until vedline == 1 or vvedlinesize /== 0 do vedcharup() enduntil
    endif;

    if vedline == 1 then
        ;;; find where text starts in file
        while vvedlinesize == 0 do vedchardown() endwhile;
    endif;

    ;;; find beginning of message
    repeat
        unless vvedlinesize == 0 then
            vedthisline() -> line;
            if fast_subscrs(1,line) == `F` and isstartstring('From ', line) then
                quitloop()
            endif
        endunless;

        if vedline == 1 then
            vedpositionpop();
            vederror('NOT IN MAIL MESSAGE')
        endif;

        vedcharup()
    endrepeat;
    ;;; Found beginning of message
    proc1();
    vedline -> line;
    if ved_try_search('@aFrom ', []) and vedline > line then
        vedcharup();
    else vedendfile()
    endif;
    ;;; Found end of message
    proc2();
    vedpositionpop();
enddefine;

define vars ved_mcm;
    ;;; mark current message
    vedmailbounds(vedmarklo,vedmarkhi);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 29 1993
        Changed dlocal of vvedsr*ch vars to ved_search_state.
        ved*testsearch -> ved_try_search.
--- Aaron Sloman, Jun 20 1990
    Fixed behaviour on line before beginning of message. Changed to use
    isstartstring
--- Aaron Sloman, Aug 22 1988
    localised more variables in ved_mcm
 */
