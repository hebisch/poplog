/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 >  File:            C.unix/lib/ved/ved_net.p
 >  Purpose:        examining net news
 >  Author:         Aaron Sloman, Jan 1987 (see revisions)
 >  Documentation:  HELP * VED_NET   * VED_POSTNEWS
 >  Related Files:  /usr/spool/news/...
 >              LIB * VED_GN  * VED_POSTNEWS  *VED_FOLLOWUP
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; NB this software is not supported

section;

uses sysisdirectory;
uses sysfileinode;
uses sysstring;

lconstant sysstring2 = copy(sysstring);

;;; The next variables may have to be altered on a site-wide or per user
global vars
    vednewsdir,             ;;; pathname for news directory
    vedoldnewsdir,          ;;; ditto for old news, if saved
    vednewsrc,              ;;; user's file specifying news groups
    gn_show_sender,         ;;; show sender as well as subject in index files
    newsgroupdone = '.',    ;;; used to indicate that group is to be updated
    gn_bracket_index,       ;;; if true bracket subject and sender in indexes
    gn_hide_header,         ;;; if true then scrolls past header of news file
    gn_include_Re,          ;;; if true, include subject lines with "Re:"
;

;;; NOW SET DEFAULTS
unless isstring (vednewsdir) then
    '/usr/spool/news' -> vednewsdir
endunless;

unless isstring(vedoldnewsdir) then
    '/usr/spool/oldnews' -> vedoldnewsdir
endunless;

unless isstring(vednewsrc) then
    '$HOME/.newsrc' -> vednewsrc
endunless;

unless isboolean(gn_show_sender) then
    true -> gn_show_sender
endunless;

unless isboolean(gn_bracket_index) then
    true -> gn_bracket_index
endunless;

unless isboolean(gn_hide_header) then
    true -> gn_hide_header
endunless;

unless isboolean(gn_include_Re) then
    true -> gn_include_Re
endunless;

global constant vedsavenewsdir=vednewsdir;

define ved_oldnews;
    ;;; switch to reading old news
    vedoldnewsdir -> vednewsdir;
enddefine;

define ved_newnews;
    ;;; switch to reading new news
    vedsavenewsdir -> vednewsdir;
enddefine;

;;; String to go on second line of index files
lconstant header_string=
'--------------USE <ENTER> gn to get news file-----------------';

;;; this property records inodes of news files that have been read
lconstant procedure seenfile=newproperty([],30,false,true);

define lconstant procedure end_of_first_space(line) with_props 0;
    lvars line, loc;
    subscrv(line, vedbuffer) -> line;   ;;; now a string
    locchar(`\s`, 1, line) -> loc;
    if loc then loc fi_+ 1 else 1 endif;
enddefine;

define lconstant procedure markseenfile with_props 1;   ;;; 1 to save space
    ;;; To indicate file has been read mark current line of
    ;;; index file produced by vedgetsubjectline.
    dlocal vedstatic = true;
    end_of_first_space(vedline) -> vedcolumn;
    vedcharinsert(`*`)
enddefine;

define lconstant vedgetfilenumbers(startnum,group) ->numbers ->maxnum
    with_props 2;
    ;;; get sorted list of file numbers corresponding to news group,
    ;;; ignoring those with numbers earlier than startnum
    ;;; return highest number found and list
    lvars group,startnum,maxnum=startnum,nextnum,numbers, repeater;
    ;;; get existing file names and make a list of new numbers
    vedputmessage(group);
    lvars groupdir;
    unless group(datalength(group)) == `*` then
        group -> groupdir;
        group dir_>< '*' -> group
    else
        allbutlast(1, group) -> groupdir;
    endunless;
    sys_file_match(group,nullstring,false,true) -> repeater;
    vedputmessage('Getting message numbers - ' sys_>< group);
    [%until (repeater() ->> nextnum) == termin do
        if nextnum then
            if (strnumber(nextnum) ->> nextnum) and nextnum > startnum then
                /** to fix symlink problem on syma - IR 7/12/93 **/
                nextunless(readable(groupdir dir_>< nextnum));

                nextnum;
                max(nextnum,maxnum) -> maxnum;
            endif
        else
            -> nextnum; ;;; setting directory. Should happen only once
        endif;
    enduntil%] -> numbers;
    if numbers == [] then vedputmessage('NOTHING NEW'); return endif;
    syssort(numbers, nonop <) -> numbers;
enddefine;

define lconstant procedure getfilenamechars(pathname);
    ;;; put on stack the part of the pathname that comprises the file name
    lvars pathname, len = datalength(pathname), col;
    if locchar_back(`/`, len, pathname) ->> col then
        col fi_+ 1 -> col;
        fast_for col from col to len do fast_subscrs(col, pathname) endfast_for
    else
        explode (pathname)
    endif
enddefine;


define lconstant procedure vedgetsubjectline(file) with_props 3;
    ;;; find Subject line and From: line in file, and insert them
    ;;; into VED buffer after inote number of file
    ;;; If the file has already been seen this session mark it

    lvars file,dev,n,col,stacklen = stacklength(),
         subject=false,fromline=false, subject_length=0,from_length=0,
         inode=sysfileinode(file), string=sysstring;
    dlocal vedediting = false;

    sysopen(file,0,"line") -> dev;
    ;;; look for Subject line
    repeat
        fast_sysread(dev, 1, string, sysstringlen) -> n;
        if not(subject) and isstartstring('Subject:', string) then
            ;;; save subject line
            string -> subject; n fi_- 1 -> subject_length;
            unless fromline then sysstring2 -> string endunless;
        endif;
        if gn_show_sender and not(fromline)
        and isstartstring('From:', string) then
            string -> fromline; n fi_- 1 -> from_length;
            unless subject then sysstring2 -> string endunless;
        endif;
    quitif(n fi_< 3);   ;;; end of file header
    quitif(subject and (fromline or not(gn_show_sender)));
    endrepeat;
    sysclose(dev);

    lvars loc;
    returnif(
        not(gn_include_Re)
        and subject
        and (issubstring('Re:', subject) ->> loc)
        and (loc < subject_length));

    vednextline();
    ;;; Prepare the file description.
    ;;; File name (number)
    getfilenamechars(file); `\s`;
    if seenfile(inode) then `*` else `\s` endif;
    if gn_bracket_index then `[` endif;
    ;;; Get subject
    if subject then
        fast_for col from 10 to subject_length  do
            fast_subscrs(col,subject)
        endfor;
    else
        explode(' ??? no Subject line ??? ')
    endif;
    if gn_bracket_index then `]` endif;
    if fromline then
        if gn_bracket_index then `\s`, `[` else `\t`, `|`, `|`, `\s` endif;
        fast_for col from 7 to from_length  do
            fast_subscrs(col,fromline)
        endfor;
        if gn_bracket_index then `]` endif
    endif;
    consstring(stacklength() - stacklen);
    unless gn_bracket_index then vedencodetabs() endunless -> vedthisline();
enddefine;



define lconstant procedure setup_gn();
    unless vedargument = '1' or vedcommand = 'gn' then
        vedputcommand('gn');
        unless ved_on_status then
            procedure;
                dlocal ved_on_status = true;
                3 -> vedcolumn;
            endprocedure()
        endunless;
    endunless
enddefine;

define lconstant procedure makeindexfile(numbers,group_path,group)
    with_props 4;
    ;;; given a list of numbers and a news group directory
    ;;; make an index with the directory and then subject lines
    lvars number, numbers, group_path, group, line;
    dlocal vedmaxscrollhor = false;

    define dlocal interrupt;
        clearstack();
        goto READY
    enddefine;

    vededit(systmpfile('/tmp',group,nullstring), vedhelpdefaults);
    false -> vedbreak;
    vedinsertstring(group_path);
    vedlinebelow();
    8 -> vedcolumn; vedinsertstring(group);
    vedlinebelow();
    vedinsertstring(header_string);
    ;;; make sure buffer is big enough
    vedline -> line;
    line + listlength(numbers) -> vedline;
    vedbufferextend();
    line -> vedline;
    vedputmessage('getting subject lines');
    fast_for number in numbers do
        vedgetsubjectline(group_path dir_>< number);
        if number mod 10 == 0 then
            vedputmessage('Message number: ' sys_>< number)
        endif
    endfast_for;
    READY:*
    end_of_first_space(4) -> vedleftmargin;
    vedleftmargin fi_- 1 -> vedcolumnoffset;

    vedjumpto(3, vedleftmargin fi_+ 1);

    1 -> vedlineoffset;     ;;; show all but first line
    ved_save_file_globals();
    vedchardown();
    setup_gn();
    vedrefresh();
enddefine;

define lconstant procedure filedir(string) -> string;
    ;;; given a path name return the string corresponding to the directory
    lvars string, n;
    sysfileok(string) -> string;
    locchar_back(`/`, datalength(string),string) -> n;
    if n then
        substring(1, n fi_- 1, string) -> string
    else
        nullstring -> string
    endif
enddefine;

define lconstant procedure vedgetsubjects(path,group) with_props 5;
    ;;; create file with directory name at top, and file numbers
    ;;; followed by subjects on remaining lines
    lvars path, group, dir, number, filenumbers;
    if sysisdirectory(path) then
        path -> dir;
    else
        filedir(path) -> dir;
    endif;
    vedgetfilenumbers(0,path) -> filenumbers ->;
    if filenumbers == [] then return
    else
        makeindexfile(filenumbers,dir,group);
    endif;
enddefine;

define lconstant groupfilename(string) -> string with_props 6;
    lvars s string;
    ;;; replace "." or space in news group name with "/"
    if strmember(`.`,string) or strmember(`\s`,string) then
        copy(string) -> string;
        fast_for s from 1 to datalength(string) do
            if strmember(string(s),'\s.') then
                `/` -> string(s)
            endif
        endfast_for
    endif
enddefine;

define global ved_net;
    ;;; general purpose news browser. Gets directory listings
    ;;; or subject lines
    lvars s, group,
        ls_flags = '-C ',
        path = sysfileok(vednewsdir);

    dlocal vedediting, vvedgotoplace;

    if vedargument /= nullstring and vedargument(1) == `-` then
        ;;; there is either '-s' or ls_flags at beginning
        locchar(`\s`,1,vedargument) ->s;
        if s then
            ;;; split vedargument at space into flags and argument
            substring(1,s,vedargument) -> ls_flags; ;;; include space
            while vedargument(s fi_+ 1) == `\s` do s fi_+ 1 -> s endwhile;
            allbutfirst(s,vedargument) -> vedargument;
        else
            ;;; nothing but flags
            vedargument sys_>< space -> ls_flags;
            nullstring -> vedargument
        endif
    endif;

    vedargument -> group;
    groupfilename(vedargument) -> vedargument;

    ;;; build up path name
    path dir_>< vedargument -> path;
    if ls_flags = '-s ' then
        vedgetsubjects(path,group); return();
    elseif sysisdirectory(path) or locchar(`*`,1,path) then
        ls_flags sys_>< path -> vedargument;
        vedputmessage('ls ' sys_>< vedargument);            ;;; may take time
        valof("ved_ls")();
        vedlineabove(); vedinsertstring(path);
    else
        'Subject:' -> vvedgotoplace;
        vededit(sysfileok(path));
        vedline - 1 -> vedlineoffset;
    endif;
    false -> vedwriteable;
enddefine;

define global ved_oldnet;
    dlocal vednewsdir=vedoldnewsdir;
    ved_net();
enddefine;

define lconstant get_last_number  with_props 7;
    ;;; assume line ends with a number. Move cursor to left of it
    ;;; and return the number
    lvars line,c;
    vedtrimline();
    vedsetlinesize();
    vedthisline() -> line;
    fast_for c from vvedlinesize-1 by -1 to 1 do
        quitunless(isnumbercode(line(c)))
    endfast_for;
    c fi_+ 1 -> vedcolumn;
    ;;; get number of last file accessed in this group
    vednextitem()
enddefine;


define lconstant make_group_index with_props 8;
    ;;; read news subjects corresponding to news group on current line
    ;;; in .newsrc file
    lvars group,group_path,groupnum,maxnum,numbers,c,line;
    define dlocal interrupt;
        chainfrom(make_group_index,vedinterrupt);
    enddefine;
    repeat
        while strmember(`!`,vedthisline()) do
            vedchardown();
        endwhile;
        vedthisline() -> line;
        if vedatend() then vedputmessage('NO MORE ENTRIES'); return()
        else
            if vvedlinesize == 0 or subscrs(1,line) == `#` then
                vedchardown();
                nextloop()
            endif;
            strmember(`:`,line) -> c;
            if c == vvedlinesize then
                ;;; no number. Insert 0
                vedtextright(); vedcharright(); vedcharinsert(`0`);
            elseunless c and isnumbercode(line(vvedlinesize)) then
                vederror('.newsrc FILE FORMAT should be foo.baz.grum: 63')
            endif;
            ;;; get group and replace dots with "/"
            substring(1,c - 1,line) -> group;
            vednewsdir dir_>< groupfilename(group) -> group_path;
            ;;; get number at end of line (last number accessed).
            get_last_number() -> groupnum;
            ;;; get list of new file numbers
            vedgetfilenumbers(groupnum,group_path) -> numbers -> maxnum;
            if numbers == [] then
                vednextline();
                if vedargument = newsgroupdone then return
                elseif vedargument = '1' then
                    vedputmessage('NOTHING NEW IN: ' sys_>< group);
                    return;
                endif;  ;;; will go on searching
            else
                if vedargument = newsgroupdone then
                    ;;; update .newsrc entry
                    vedcleartail();
                    vedinsertstring('' >< maxnum);
                    vednextline();
                    setup_gn();
                else
                    ;;; get ready for next one
                    vednextline();
                    makeindexfile(numbers,group_path,group);
                    vedputmessage(group sys_>< ': message numbers from '
                        sys_>< hd(numbers) sys_>< ' to ' sys_>< last(numbers));
                endif;
                return()
            endif
        endif
    endrepeat
enddefine;

lvars last_news_file=false;

define global procedure ved_gn;
    ;;; Get News item, from index
    ;;; If used in a news file, go back to index file.
    ;;; If used with an argument (unless a number), chain to VED_NET
    ;;; If used in newsrc file call make_group_index for current line
    ;;; If not in a group index file, then get .newsrc
    ;;; If used in a news group index file without an argument go to the news
    ;;;     item on current line. If used with an argument which is a
    ;;;     number, go to the news item with that number.
    ;;;     assume first line of file has directory name
    ;;;     assume first item on current line is the file number
    ;;;     get path name and mark the line
    lvars   filename, inode, group_path, group, file, line, string, num,
            last_num, oldfile,
            newsrc=sysfileok(vednewsrc);
    dlocal  vedediting, vedfiletypes, ved_search_state, poplastchar;


    ;;; if given a non number argument, invoke ved_net
    if vedargument /= nullstring and not(strnumber(vedargument))
    and vedargument /= newsgroupdone
    then
        chain(valof("ved_net"))
        ;;; if in news file, quit
    elseif vedpathname = last_news_file then
        ved_q()
        ;;; if in newsrc make index for group on current line
    elseif vedpathname = newsrc or vedcurrent = newsrc then
        chain(make_group_index)
    else
        ;;; Check if in index file for a group. First line should start with
        ;;;   vednewsdir. 3rd line should be header_string.
        ;;;   Beginning of current line should be a number.
        ;;;   Get file number at beginning of line or, from vedargument
        if isvector(vedbuffer) then
            vedbuffer(1)
        else
            nullstring
        endif -> group_path;
        if vvedbuffersize > 3
        and vedbuffer(3) = header_string and issubstring(vednewsdir,1,group_path)
        then
            if strnumber(vedargument) then
                ;;; search for line with the number
                unless vedtestsearch('@a' sys_>< vedargument,false) then
                    vederror('NO NEWS ITEM: ' sys_>< vedargument)
                endunless
            elseif vedargument = newsgroupdone then
                vedendfile(); vedcharup();
            endif;
            ;;; get number at beginning of line
            1 -> vedcolumn;
            repeat
                vedmoveitem() -> filename;
            quitif(isinteger(filename));
                if filename == termin then ;;; at end of file
                    ved_q();
                    goto RESTART;
                    ;;; previously vederror('NO MORE FILES');
                endif;
            endrepeat;
            markseenfile();
            ;;; go to next line
            vedchardown();
            filename -> last_num;
            sysfileok(group_path dir_>< filename) -> filename;
            if sysfileinode(filename) ->> inode then
                true -> seenfile(inode)
            else vederror('FILE HAS DISAPPEARED')
            endif;
            ;;; update .newsrc file
            if vedpresent(newsrc) ->> file then
                vedbuffer(2) -> group;
                allbutfirst(skipchar(`\s`, 1, group) - 1, group)
                    sys_>< ":" -> group;
                ved_current_file -> oldfile;
                file -> ved_current_file;
                vedpositionpush();
                vedtrimline();
                for vedline to vvedbuffersize do
                    ;;; search for required group
                    subscrv(vedline,vedbuffer) -> string;
                    if isstartstring(group,string) then
                        ;;; update last number accessed
                        get_last_number() -> num;
                        if last_num > num then
                            vedfileisonscreen(ved_current_file) -> vedediting;
                            vedcleartail();
                            vedinsertstring(last_num sys_>< nullstring);
                            if vedediting then vedscr_flush_output() endif
                        endif;
                        quitloop;
                    endif
                endfor;
                vedpositionpop();
                oldfile -> ved_current_file;
                true -> vedediting;
            endif;
            if vedargument = newsgroupdone then
                ved_q();
                setup_gn();
                vedrefreshrange(vedline - 1,vedline - 1,undef)
            else
                if gn_hide_header then
                    '@a@z' -> vvedgotoplace;    ;;; find first blank line
                endif;
                [^^vedfiletypes ['' {vedindentstep 8}]] -> vedfiletypes;
                vededit(filename, vedhelpdefaults);
                vedpathname -> last_news_file;
                if vvedbuffersize = 0 then
                    vedinsertstring('---------------EMPTY---------------');
                    nullstring -> vedmessage;
                else
                    ;;; Display author
                    fast_for line to vedline do
                        fast_subscrv(line, vedbuffer) -> string;
                        if isstartstring('From: ', string) then
                            vedputmessage(allbutfirst(6, string));
                            quitloop
                        endif
                    endfor;
                endif;
                if gn_hide_header and vvedbuffersize /== 0 then
                    until vvedlinesize /== 0 or vvedlinesize > vvedbuffersize do
                        vedchardown();
                        vedscrollup();
                    enduntil;
                endif;
            endif
        else
            ;;; not an index file
            ;;; Start to read news - get .newsrc.
            ;;; Assume same format as 'rn' or 'readnews'
        RESTART:
            setup_gn();
            vededit(newsrc);
            vedputmessage('To get group for current line <ENTER> gn')
        endif
    endif;
    setup_gn();
enddefine;


define ved_gn1;
    dlocal vedargument, poplastchar;
    lvars newsrc= sysfileok(vednewsrc);
    if vedpathname = newsrc or vedcurrent = newsrc then
        '1' -> vedargument;
        make_group_index()
    else chain(ved_gn)
    endif
enddefine;

define ved_save;
    ;;; save current news file in specified file, appending if necessary
    ;;; Must copy original in case VED has altered tabs, trailing
    ;;; spaces, etc.
    sysobey('cat ' sys_>< vedpathname sys_>< ' >> ' sys_>< vedargument)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1994
        Changed to use vededit
--- Ian Rogers, Dec  7 1993
        Fixed vedgetfilenumbers to deal with symlinks to unmounted files
--- John Gibson, Nov  5 1993
        Made poplastchar dlocal to ved_gn, ved_gn1
--- Jonathan Meyer, Sep 29 1993
        Changed vvedsr*ch vars to ved_search_state.
--- John Gibson, Apr 19 1993
        Changed to use ved_current_file instead of old vedsave/setglobals
--- Aaron Sloman, Jul  3 1991
    Replaced that call of vedputcommand with setup_gn();
--- Aaron Sloman, Jun 29 1991
    Changed makeindexfile to do vedputcommand('gn')
    Added gn_include_Re control variable to allow follow ups to be
    ignored.
--- John Williams, Mar 19 1991
    Changed second arg to -sys_file_match- to -nullstring- instead of
    -false- (which John Gibson considers "deprecated").
--- Aaron Sloman, Mar 20 1990
    Transferred to public Poplog
--- Aaron Sloman, Dec 12 1989
    Fixed bug due to interaction between empty file and gn_hide_header
--- Aaron Sloman, May 28 1989
    Changed to include news group in temporary file name
--- Aaron Sloman, May 15 1989
    Changed to cope with vednewsdir starting with '$' or '~'
--- Aaron Sloman, May  7 1989
    Put ved_ls in quotes to postpone autoloading.
    Used a version of -filedir- that does not create garbage.
    Also will speed up compilation
    Called ved*editor instead of edit to reduce autoloading
--- James Goodlet, May  1 1989 New global variable -gn_hide_header-
        introduced to suppress new behaviour of scrolling past header, if
        not desired.  Defaults to true, which is the scrolling behaviour.
--- Aaron Sloman, Apr 30 1989 Slightly improved startup for each news file
--- Aaron Sloman, Apr 17 1989
        Now accepts blank lines in vednewsrc file and comment lines
        starting with #
--- Aaron Sloman, Apr  5 1989 Made newsgroup visible in index file
--- Aaron Sloman, Apr  4 1989 Slightly tidied up marking seen line.
--- Aaron Sloman, Mar 28 1989
    Added gn_bracket_index to control format of index file for different
    tastes.
    Prevented spurious insertions of 'gn' in command line buffer
    Made vedcolumnoffset and vedleftmargin sensitive to size of number
    at beginning of line in index files.
--- Aaron Sloman, Mar 20 1989
    Restored previous version of ved_save. Added comment explaining why
    it was necessary, and changed vedcurrent to vedpathname.
--- Aaron Sloman, Mar 20 1989
        Changed to use fast_sysread. I think it's safe
        Altered ved_save to use ved_wappr.
--- Aaron Sloman, Mar 13 1989 - further reduction of garbage collections,
        by using sysstring and sysstring2 for subject line and from line.
--- Aaron Sloman, Mar 13 1989 - made getsubjectlines much faster by not
        using sysfilename, substring, explode, etc. Added getfilenamechars
--- Aaron Sloman, Feb 27 1989 - handled interrupt better in makeindexfile
--- Aaron Sloman, Feb 25 1989 - made vedcolumn on status line sensible
--- Aaron Sloman, Feb 25 1989 - Fixed tabs in news files.
--- Aaron Sloman, Feb 23 1989
    Made it put group name on status line when in index file.
    Made <ENTER> gn. remove "." afterwards.
--- Aaron Sloman, Feb 12 1989
    Fixed so that if interrupted does not leave junk on stack

    Altered to show instruction to use <ENTER> gn in Subject Index file

    Altered so that <ENTER> gn1, or <ENTER> gn 1, will try only ONE
    news group
--- Aaron Sloman, Feb 10 1989
    Fixed to work with gn_show_sender set false
--- Aaron Sloman, Feb  2 1989
    Altered format to show subject and sender in one line, and no
    longer prints out lines while they are being read in, to save time.
--- Aaron Sloman, jan 11 1989
    Did 7 -> vedleftmargin in index file.
--- Aaron Sloman, Dec 18 1988
    Prevented the default directory names over-writing previously assigned
        strings
    Made ved_gn put subject line and sender line, in square brackets
        in the index file for a news group. Updated HELP file.
    Made it display index file without making numbers visible on left.
--- Aaron Sloman, Nov 23 1988 -- really made vedbreak false in index files
--- Aaron Sloman, Mar  9 1987 -- Following requests from users:
    1. ved_gn in a news file just goes back to index file, and doesn't
    get next news file. (You may not want it).
    2. ved_gn at the end of an index file quits it and goes back to .newsrc
    3. If subject line in an index file wraps around, ved_gn on the wrapped
    line no longer gives an error - it just goes to the next line starting
    with a number.
--- Aaron Sloman, Feb 23 1987
    1. made it mark only seen file number
    2. ved_gn in newsrc now skims till it finds a group with new news
    3. ved_gn in news file quits, returns to index and gets new news file
    4. ved_gn can be given a numerical argument if in index file
    generally tidied up
--- Aaron Sloman, Feb 19 1987
    various minor changes, including vedbreak false in index files.
--- Aaron Sloman, Jan 25 1987
    1. fixed to use 'ls -C' not 'ls -Ct' as default
    2. Lots of extra facilities
    3. HELP VED_NET re-written

*/
