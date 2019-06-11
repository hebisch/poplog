/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_followup.p
 > Purpose:         Follow up or reply to a news posting
 > Author:          Aaron Sloman, Mar 19 1989 (see revisions)
 > Documentation:   (None yet but see HELP * VED_NET, and see below)
 > Related Files:   LIB * VED_NET, * VED_GN, *VED_POSTNEWS
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; NB this software is not supported

/*
<ENTER> followup
    Makes a copy of message in a temporary non-writeable file,
    prepares a header, and then globally indents original message
    with '> '. Delete bits you don't want to quote.

<ENTER> followup reply
    Is similar but prepares an email heading to go to the sender
    using ved_send. To: line and Cc: line may need some editing.

*/

section;

global vars followup_general;
unless isboolean(followup_general) then
    false -> followup_general   ;;; transform 'general' to 'followup'
endunless;

global vars followup_announce;
unless isboolean(followup_general) then
    false -> followup_announce  ;;; transform 'announce' to 'followup'
endunless;

define lconstant extractline(string, truncate, header_limit) -> found;
    lvars string, line, header_limit, found=false, truncate;
    vedtopfile();
    if vedteststartsearch(string) and vedline < header_limit then
        vedthisline() -> found;
        vedlinedelete();
        if truncate then
            allbutfirst(datalength(string), found) -> found
        endif
    endif;
enddefine;

define get_header_line(string, header_limit) -> found;
    lvars string, line, found, header_limit;
    vedtopfile();
    extractline(string, false, header_limit) -> found;

    if found then found else string endif -> line;

    vedjumpto(1,1);
    vedlineabove();
    line -> vedthisline();
    vedtextright(); vedcharright();
enddefine;

define ved_followup;
    ;;; used to reply to or follow up a news article.
    lvars line, file=systmpfile(false,'followup',nullstring),
         message_id = nullstring, sender = nullstring, count,
         reply = (vedargument = 'reply'), header_limit, replyto = false;
    dlocal prmishap;
    procedure; true -> vedediting endprocedure <> prmishap <> vedrefresh
        -> prmishap;
    vedmarkpush();
    ;;; find end of header
    vedtopfile();
    repeat
    quitif(vvedlinesize == 0);
        vedchardown();
    endrepeat;
    vedline -> header_limit;
    false -> vvedmarkprops;
    nullstring -> vedargument;
    ved_mbf(); ved_mef(); ved_copy(); vedmarkpop();
    edit(file);
    false ->> vedediting ->> vedwriteable -> vedbreak;
    ;;; vedbreak must be true while headers are constructed
    ved_y();
    vedpositionpush();

    define lconstant header_end;
        vedpositionstack(1)(1) + header_limit
    enddefine;

    get_header_line('Subject: ', header_end()) ->;
    unless issubstring('Re:',vedthisline()) then
        9 -> vedcolumn; vedinsertstring(' Re:');
    endunless;

    ;;; get Message ID
    extractline('Message-ID: ', true, header_end()) -> message_id;
    ;;; get sender
    extractline('From: ', true, header_end()) -> sender;
    unless sender then
        true -> vedediting;
        vedrefresh();
        vederror('No "From:" line with sender')
    endunless;
    ;;; get References and message_id
    get_header_line('References: ', header_end()) ->;
    if message_id then vedinsertstring(message_id) endif;
    if reply then
        get_header_line('Path: ', header_end()) ->;
        'News-' sys_>< vedthisline() -> vedthisline();
        extractline('Reply-To: ', true, header_end()) -> line;
        vedtopfile(); vedlineabove();
        if line then
            'To: ' sys_>< line -> vedthisline();
            vedlinebelow();
            'Cc: ' sys_>< sender -> vedthisline();
            'USED  "Reply-To:" and "From:" LINES' -> replyto;
        else
            'To: ' sys_>< sender -> vedthisline()
        endif;
    else
        get_header_line('Summary: ', header_end()) ->;
        get_header_line('Keywords: ', header_end()) ->;
        get_header_line('Distribution: ', header_end()) ->;
        if extractline('Followup-To: ', true, header_end()) ->> line then
            vedtopfile(); vedlineabove();
            'Newsgroups: ' sys_>< line -> vedthisline();
        else
            get_header_line('Newsgroups: ', header_end()) ->;
        endif;

        0 -> count;
        unless followup_general then
            ;;; Turn .general to .followup in newsgroups
            if issubstring('.general', vedthisline()) then
                1 -> count;
                veddo('gsl/.general/.followup')
            endif
        endunless;
        unless followup_announce then
            ;;; Turn .announce to .followup in newsgroups
            if issubstring('.announce', vedthisline()) then
                count+1 -> count;
                veddo('gsl/.announce/.followup')
            endif
        endunless;
/*
;;; This is wrong. Approved doesn't mean moderated?
        if extractline('Approved: ', true, header_end()) ->> line then
            vedtopfile(); vedlineabove();
            'To: ' sys_>< line -> vedthisline();
        endif;
*/
    endif;
    extractline('Lines: ', false, header_end()) -> ;
    vedpositionpop();
    vednextline();
    vedmarklo();
    ved_mef();
    veddo('gsr/@a/> /');
    unless reply or sender = nullstring then
        vedmarkfind();
        vedlineabove();
        vedinsertstring(sender); vedinsertstring(' writes:\n');
    endunless;
    vedjumpto(1,1); vedcheck();
    true -> vedediting;
    true -> vedbreak;
    68 -> vedlinemax;
    vedrefresh();
    lblock;
        lvars message;
        if replyto then replyto else nullstring endif -> message;
        if count > 1 then 'CHECK DUPLICATE NEWS GROUPS ' sys_>< message -> message
        endif;
        vedputmessage(message);
    endlblock;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun 22 1991
    Fixed bug: if 'Approved' line exists, should not insert To: line.
    Also now takes note of "Followup-To:" line.
    (Both bugs reported by Leila Burrell-Davis)
--- Aaron Sloman, Mar 21 1990
    Transferred to public poplog library
--- Aaron Sloman, Aug 20 1989
    Made to insert followups to announce as well as general in .followup.
    Prevented hanging up in errors because of vedediting false.
--- Aaron Sloman, Apr 30 1989
    If there's a Reply-To: line, then the 'To:' line uses that, and
    the address in the 'From:' line goes to 'Cc:' line.
--- Aaron Sloman, Apr 26 1989
    Made default followup_general false, because sussex.followup now
    exists.
--- Aaron Sloman, Apr 25 1989
    Changed vedlinemax for followup or reply files.

    Code to change '.general' to '.followup' in Newsgroups: line
    put in but only works if followup_general is set false.
--- Aaron Sloman, Apr 22 1989
    Made vedbreak true in followup files.
    Cleaned up and generalized code, using -extractline-
--- Aaron Sloman, Mar 22 1989
    Made the followup and reply options behave a bit more like those
    in other systems.
 */
