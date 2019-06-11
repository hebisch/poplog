/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_postnews.p
 > Purpose:         Send news using inews on local or remote machine
 > Author:          Aaron Sloman, May 29 1988 (see revisions)
 > Documentation:   HELP * VED_POSTNEWS, * VED_NET
 > Related Files:   LIB * VED_NET, * VED_GN
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; NB this software is not supported

;;; NB NB NB NB -- this file may need to be edited on some machines
;;; See 'MAY NEED CHANGING' comments below

/*
To use this, first create header lines, e.g. using <ENTER> postnews

Then edit tnem

Then <ENTER> postnews will post the whole file.

If your .signature file has no more than 4 lines it will be added
automatically by "inews"

<ENTER> postnews
    Puts news header on current file if it does not have one
    Posts current file if there is a news header already

<ENTER> postnews <name>
    Posts current file using rsh or remsh on remote machine <name>

<ENTER> postnews new
    Stars new temporary file with news header

<ENTER> postnews cancel
    Sends cancel message for current news article that has been read
    in from news files. (It needs the Message-ID header line.)
    (See HELP * VED_NET for reading news)

These commands will use the default inews_remote_host to post news if the news
file (/usr/lib/news/inews) is not available on the current machine. This will
work only if the user has an account on remote host that accepts remote shell
commands. Change default host name below for your machine.


This program is modelled partly on lib ved_send
Assign FALSE to ved_send_wait make it post in the background
*/

section;

lvars
    inews_prog_file = '/usr/lib/news/inews',/* MAY NEED CHANGING */
    inews_arg = 'inews';
;

/*ON SOME MACHINES USE 'remsh' INSTEAD OF 'RSH'*/

lvars rsh_command = '/usr/ucb/rsh';         /* MAY NEED CHANGING */
unless sys_file_stat(rsh_command, {}) then
    '/usr/bin/remsh' -> rsh_command;
endunless;

lvars rsh_command_name = sys_fname_nam(rsh_command);


/* The next assignment can be over-ridden by an argument to ved_postnews*/
global vars inews_remote_host;

unless isstring(inews_remote_host) then
    'syma' -> inews_remote_host         /* WILL NEED CHANGING */
endunless;


;;; copied from lib ved_send
global vars ved_send_wait;
unless isboolean(ved_send_wait) then true -> ved_send_wait
endunless;


define lconstant extractline(string, header_limit) -> found;
    ;;; Get substring from header line starting with string
    lvars string, header_limit, found;
    vedendfile();
    if vedteststartsearch(string) and vedline < header_limit then
        allbutfirst(datalength(string), vedthisline())
    else false
    endif -> found
enddefine;

define constant cancelargs() -> string;
    ;;; Work out arguments for cancelling message to inews
    lvars id, groups, domain, string, header_limit;
    vedtopfile();
    ;;; find end of header
    until vedline > vvedbuffersize or vvedlinesize = 0 do
        vedchardown();
    enduntil;
    vedline -> header_limit;
    extractline('Message-ID: ', header_limit) -> id;
    extractline('Newsgroups: ', header_limit) -> groups;
    extractline('Distribution: ', header_limit) -> domain;
    unless id then
        vederror('No Message-ID: field')
    endunless;
    unless groups then
        vederror('No Newsgroups: field')
    endunless;

    if domain then ' -d ' sys_>< domain else nullstring endif -> domain;

    cons_with consstring
    {%
        explode('-c \'cancel '), explode(id), `'`,
        explode(' -n '), explode(groups),
        explode(domain) %}-> string
enddefine;

define lconstant sendnews(cancelling);
    ;;; If cancelling is false send the marked range as a news file
    ;;; If true then send cancelling message for current news message
    lvars
        din, dout, line, num = 1, limit, child, dev, cancelling,
        inews_args,
        inews_prog = false,
        inews_endarg = if cancelling then cancelargs() else '-h' endif
        ;

    dlocal popexit, inews_remote_host;

    max(num,vvedbuffersize) -> limit;
    until vedusedsize(vedbuffer(num)) /== 0 do
        num fi_+ 1 -> num;
        if num fi_> limit then vederror('NO MESSAGE') endif
    enduntil;

    ;;; set up pointer to remote machine running inews, if necessary
    if vedargument /= nullstring then
        vedargument -> inews_remote_host;
        false
    else
        inews_prog_file
    endif -> inews_prog;

    if inews_prog and (readable(inews_prog) ->> dev) then
        ;;; inews available on this machine
        sysclose(dev);
        if cancelling then
            ;;; send cancel message on this machine and return
            sysobey(
                inews_prog_file sys_>< space sys_>< inews_endarg
                    sys_>< ' < /dev/null', `$`
                    );
            return();
        else
            [^inews_arg ^inews_endarg]-> inews_args;
        endif
    else
        ;;; Do it remotely
        [^rsh_command_name ^inews_remote_host
                ^(inews_prog_file sys_>< space sys_>< inews_endarg)] -> inews_args;
        rsh_command -> inews_prog;
    endif;

    if sys_fork(true) ->> child then
        vedputmessage('Command being sent in background');
        sys_wait(child) -> (,)
    else
        ;;; child
        identfn -> popexit;
        [] -> vedbufferlist;
        false -> vedediting;
        ;;; do an extra fork to prevent a zombie
        if not(ved_send_wait) and sys_fork(false) then
            ;;; child just exits - waited for by parent
        else
            ;;; if ved_send_wait then child, else grandchild
            ;;; Make the pipe.
            syspipe(false) -> din -> dout;
            if sys_fork(false) ->> child then
                ;;; still grandchild - put characters into pipe (other end closed)
                sysclose(din);
                unless cancelling then
                    repeat
                        veddecodetabs(subscrv(num,vedbuffer)) -> line;
                        syswrite(dout,line,datalength(line));
                        syswrite(dout,'\n',1);
                    quitif(num == limit);
                        num fi_+ 1 -> num
                    endrepeat;
                endunless;
                sysclose(dout);
            else
                ;;; Previously done after 'else' below. Moved here for safety
                sysclose(dout);
                din -> popdevin;
                if ved_send_wait and sys_vfork(false) then
                    ;;; just exit so that offspring has no parent
                else
                    ;;; great-grandchild
                    sysexecute(inews_prog, inews_args, false)
                endif
            endif
        endif;
        fast_sysexit();
    endif
enddefine;

define do_newsheader();
    ;;; For preparing news file for posting. The default strings can
    ;;;  be changed
    vedtopfile();
    vedlineabove();
    applist([
        'Subject:\n'
        'Newsgroups:   (e.g. local.test, uk.ikbs)\n'
        'Distribution: (e.g. local, uk, eunet, world)\n'
        'Keywords:\n\n'
        '[N.B. leave blank line before news text]\n'
        ], vedinsertstring);
    vedtopfile();
    vedputmessage('EDIT HEADERLINES AS APPROPRIATE - SEND USING POSTNEWS')
enddefine;


define do_postnews(cancelling);
    ;;; If cancelling is true then cancel previously sent message, using
    ;;; Message-ID.
    ;;; Otherwise post. File should have all the headers, e.g.
    ;;;     Subject: Newsgroups: Distribution: Keywords:
    ;;; if not call ved_newsheader

    lvars cancelling;

    dlocal vedchanged, cucharout,
         vedautowrite=false, vedpositionstack,
         pop_file_versions=1;

    ;;; Prevent printout invoking vedrestorescreen
    charout -> cucharout;

    false -> vedautowrite;

    if cancelling then
        pr('\n** CANCELLING. PAUSE IN CASE OF ERROR MESSAGES FROM NEWS HOST.\n');
    else
        pr('\n** SENDING NEWS. PAUSE IN CASE OF ERROR MESSAGES FROM NEWS HOST.\n');
    endif;
    vedpositionpush();
    sendnews(cancelling);
    ;;; give inews time to print error message - 8 secs
    syssleep(800);
    vedrestorescreen();
    vedpositionpop();
    vedputmessage('Done');
enddefine;

define ved_postnews;
    lvars line;
    dlocal vedargument;
    if vedargument = 'new' then
        edit(systmpfile(false, 'postnews', nullstring));
        do_newsheader();
    elseif vedargument = 'cancel' then
        nullstring -> vedargument;
        do_postnews(true)
    else
        vedbuffer(1) -> line;
        if isstartstring('Subject: ',line)
        or isstartstring('Newsgroups: ',line)
        or isstartstring('References: ',line)
        or isstartstring('Distribution: ',line)
        then
            do_postnews(false)
        else
            do_newsheader()
        endif
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 21 1994
        Changed to use new sys_fork etc
--- Robert John Duncan, Aug 11 1992
        Removed system dependencies: now uses 'remsh' only if /usr/ucb/rsh
        doesn't exist
--- Aaron Sloman, Jun  5 1990
    Changed "cancel" to "cancelling" to prevent clash with lib cancel
--- Aaron Sloman, May 27 1990
    Tidied up, and added "cancel" option
--- Aaron Sloman, Mar 20 1990
    Transferred to Public library
    Also added 'remsh' option for non-berkeley unix.
--- Jonathan Meyer, Nov 12 1989
    Added "new" argument to ved_postnews that open tmp file.
--- Aaron Sloman, Apr 10 1989
    Changed to use fast_sysexit
--- Aaron Sloman, Mar 27 1989
    Made it set screen non-raw etc so that error messages are readable.
    There's an 8 second delay added before the PRESS RETURN message
--- Aaron Sloman, Mar 19 1989
    Changed to use SYMA as default remote host
    Changed to allow alternative start lines in the heading
 */
