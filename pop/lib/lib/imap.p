/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/lib/lib/imap.p
 > Purpose:         Pop-11 Interface to IMAP (Internet Mail Access Protocol)
 > Author:          John Gibson, Sep 15 1998
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses unix_sockets;
include cons_network_device.ph;
include itemise.ph;

    ;;; pophost("sitemailname") is used for this, unless it's already
    ;;; been set to a string
vars imap_default_host;

vars imap_trace_protocol;
if isundef(imap_trace_protocol) then false -> imap_trace_protocol endif;


lconstant macro (
    BUFLEN          = 1024,

    FUNC_READ       = 0,
    FUNC_SYNC       = 1,
    FUNC_APPEND     = 2,
    FUNC_CLOSE      = 3,
);

lconstant
    Status          = 'Status:',
    Status_RO       = 'Status: RO',
    Status_O        = 'Status: O',

    Recent          = '\\Recent',
    Seen            = '\\Seen',
;

lvars
    buffer          = false,
    select_socket,
    select_next_tag,
    curr_mbx_name,
    mbx_num_messages,
    mbx_num_recent,
    mbx_uidvalidity,
    mbx_uidnext,
    mbx_writeable,
    ignore_fetch_response = false,
    procedure next_uid_of_mbx,
;

define lconstant uid_of_mess =
    newproperty([], 128, false, "tmparg")
enddefine;

define lconstant write_n(n);
    lvars N = n;
    until n == 0 do
        () -> fast_subscrs(n,buffer);
        n fi_- 1 -> n
    enduntil;
    syswrite(select_socket, buffer, N);
    if imap_trace_protocol then syswrite(popdevout, buffer, N) endif
enddefine;

define lconstant send_command(s) -> tag;
    select_next_tag + 1 ->> tag -> select_next_tag;
    'A' sys_>< tag -> tag;
    write_n(#| explode(tag), `\s`, explode(s), `\r`, `\n` |#)
enddefine;

define lconstant get_literal(nbytes);
    lvars dev, rep, line;
    cons_network_device(select_socket, 0, false, true, erase, nullstring, false)
                                        -> dev;
    nbytes -> subscrv(NTD_REM_NBYTES, device_user_data(dev));
    vedfile_line_repeater(dev) -> rep;
    consvector(#|
        Status,             ;;; placeholder for later
        until (rep() ->> line) == termin do line enduntil;
        while dup() == nullstring or dup() = '\Nt' do
            ;;; strip trailing blank lines
            () ->
        endwhile
    |#)
enddefine;

define lconstant get_items(bcount) -> bcount;
    lvars c = sysread(select_socket, buffer, BUFLEN), n, s, len;
    dlvars bcount;

    define split_brackets(s);
        lvars len = datalength(s);
        if subscrs(1,s) == `(` then
            bcount + 1 -> bcount;
            '(';
            if len /== 1 then split_brackets(allbutfirst(1,s)) endif
        elseif subscrs(len,s) == `)` then
            if len /== 1 then split_brackets(allbutlast(1,s)) endif;
            ')';
            bcount - 1 -> bcount
        else
            s
        endif
    enddefine;

    if c /== 0 and fast_subscrs(c,buffer) == `\n` then
        c - 1 -> c;
        if c /== 0 and fast_subscrs(c,buffer) == `\r` then
            c - 1 -> c
        endif
    endif;

    #| sys_parse_string(substring(1,c,buffer), split_brackets) |# -> n;
    returnif(n == 0);

    () -> s;
    if (datalength(s) ->> len) >= 3 and s(1) == `{` and s(len) == `}`
    and (strnumber(substring(2,len-2,s)) ->> c) then
        get_literal(c)
    else
        s
    endif
enddefine;

define lconstant make_sublists(list, top);
    lvars list, top, pair, last = false, s;
    lconstant um_ms = 'UNMATCHED ")" IN RESPONSE FROM MAIL SERVER';
    fast_for pair on list do
        fast_front(pair) -> s;
        if s = '(' then
            make_sublists(fast_back(pair), false)
                                    -> (fast_front(pair), fast_back(pair))
        elseif s = ')' then
            if top then mishap(0, um_ms) endif;
            return( if last then
                        [] -> fast_back(last);
                        list
                    else
                        []
                    endif, fast_back(pair) )
        endif;
        pair -> last
    endfor;
    unless top then mishap(0, um_ms) endunless
enddefine;

define lconstant get_response(command_tag, NO_p);
    lvars resp, len, t, name, num, bcount, code;

    define check_OK_response(len, resp);
        lvars r3, r4, code;
        returnunless(len >= 3);
        subscrl(3,resp) -> r3;
        if isendstring(']', r3) then
            if r3 = '[READ-ONLY]' then
                false -> mbx_writeable
            elseif r3 = '[READ-WRITE]' then
                true -> mbx_writeable
            endif
        endif;
        if len >= 4 and isendstring(']', subscrl(4,resp) ->> r4) then
            allbutlast(1, r4) -> code;
            if r3 = '[UIDVALIDITY' then
                strnumber(code) << 32 -> mbx_uidvalidity
            elseif r3 = '[UIDNEXT' then
                strnumber(code) -> mbx_uidnext
            endif
        endif
    enddefine;

    repeat
        [% get_items(0) -> bcount %] -> resp;
        listlength(resp) -> len;
        if imap_trace_protocol then
            dlocal pop_=>_flag = '> ', pop_pr_level = 2;
            resp =>
        endif;
        if len < 2 then goto ERROR endif;
        subscrl(1,resp) -> t;
        subscrl(2,resp) -> name;
        if t = '*' then
            ;;; untagged
            if (strnumber(name) ->> num) and len >= 3 then
                subscrl(3,resp) -> name;
                if name = 'EXISTS' then
                    num -> mbx_num_messages;
                elseif name = 'RECENT' then
                    num -> mbx_num_recent;
                elseif name = 'EXPUNGE' then
                elseif name = 'FETCH' and len >= 4 and subscrl(4,resp) = '('
                then
                    nextif(ignore_fetch_response);
                    [%  dl(tl(tl(tl(tl(resp)))));
                        while bcount > 0 do
                            get_items(bcount) -> bcount
                        endwhile ->         ;;; erase final ')'
                    %];
                    make_sublists(dup(), true)
                else
                    goto ERROR
                endif
            elseif name = 'OK' then
                if command_tag then
                    check_OK_response(len, resp)
                else
                    ;;; just opened connection
                    unless lmember_=('IMAP4rev1', resp) then
                        mishap(0, 'MAIL SERVER CONNECTION IS NOT IMAP4rev1')
                    endunless
                endif
            elseif name = 'FLAGS' then
            elseif name = 'BYE' then
                if curr_mbx_name or command_tag then
                    mishap(0, 'MAIL SERVER HAS CLOSED OR REFUSED CONNECTION')
                else
                    ;;; autologout
                    sysclose(select_socket);
                    false -> select_socket;
                    return
                endif
            else
                goto ERROR
            endif;
            returnunless(command_tag)
        elseif t = '+' then
            returnif(command_tag == "+");
            goto ERROR
        else
            ;;; tagged
            if t = command_tag then
                if name = 'OK' then
                    check_OK_response(len, resp);
                    return
                endif;
                if lowertoupper(name) = 'NO' then NO_p() endif
            endif;
            ;;; else NO or BAD or wrong tag
            goto ERROR
        endif;
    endrepeat;

ERROR:
    mishap(resp, 1, 'INVALID OR UNEXPECTED RESPONSE FROM MAIL SERVER')
enddefine;

define lconstant get_responses(command);
    lvars NO_p = identfn;
    if isprocedure(command) then (), command -> (command, NO_p) endif;
    get_response(send_command(command), NO_p)
enddefine;

define lconstant append_message(vec);
    lvars   i, dev, line, dd, len = datalength(vec), command_tag,
            procedure lcons;
    cons_network_device(pop_null_device, 1, buffer, true, erase, nullstring,
                                false) -> dev;
    device_user_data(dev) -> dd;
    0 -> subscrv(NTD_REM_NBYTES, dd);
    vedfile_line_consumer(dev) -> lcons;
    for i from 2 to len do      ;;; ignore Status line
        lcons(fast_subscrv(i,vec))
    endfor;

    send_command(sprintf(
        subscrv(NTD_REM_NBYTES, dd),
        if strmember(`R`,subscrv(1,vec)) then ' (\\Seen)' else nullstring endif,
        curr_mbx_name,
            'APPEND %S%S {%P}')) -> command_tag;

    get_response("+", identfn);

    select_socket -> subscrv(NTD_IODEV, dd);
    for i from 2 to len do      ;;; ignore Status line
        lcons(fast_subscrv(i,vec))
    endfor;
    lcons(nullstring);

    get_response(command_tag, identfn)
enddefine;

define lconstant get_netrc_password(user, host) /* -> password */;
    lvars   c = false, tok, tok2, dev = sysopen('$HOME/.netrc', 0, false),
            machine = pop_undef, login = pop_undef;
    dlvars  procedure crep;
    dlocal 0 %  if dlocal_context == 1 then false -> dev endif,
                if dlocal_context fi_<= 2 and dev then sysclose(dev) endif
             %;

    returnunless(dev) (false);
    discin(dev) -> crep;

    define next_token();
        lvars n = 0;
        repeat
            crep() -> c;
            returnif(c == termin) (consstring(n), c);
            if c == `\s` or c == `\t` or c == `\n` then
                nextif(n == 0);
                return(consstring(n), c)
            else
                c;
                n fi_+ 1 -> n
            endif
        endrepeat
    enddefine;

    repeat
        returnif(c == termin or (next_token() -> (tok, c), tok = nullstring))
                                                (false);
        if tok = 'default' then
            false -> machine;
            nextloop
        endif;
        quitif(c == termin or (next_token() -> (tok2, c), tok2 = nullstring));
        if tok = 'machine' then
            tok2 -> machine
        elseif tok = 'login' then
            tok2 -> login
        elseif tok = 'password' then
            if (not(machine) or machine = host) and login = user then
                returnif(sysfilemode(dev) &&=_0 8:044 or user = 'anonymous')
                                            (tok2);
                sys_raise_exception(0, '$HOME/.netrc: FILE IS NOT READABLE BY OWNER ONLY', `W`);
                return(false)
            endif
        elseif tok = 'account' then
        elseif tok = 'macdef' then
            repeat
                returnif(c == termin) (false);
                if c == `\n` then
                    crep() -> c;
                    returnif(c == termin) (false);
                    quitif(c == `\n`)
                endif;
                crep() -> c
            endrepeat
        else
            ;;; corrupt?
            quitloop
        endif
    endrepeat;

    sys_raise_exception(0, '$HOME/.netrc: FILE IS CORRUPT', `W`);
    false
enddefine;

define vars imap_get_password(mess_string);
    mishap(0, 'CANNOT GET PASSWORD FOR MAIL SERVER LOGIN')
enddefine;

define lconstant select_apply(mbxname, buffs, func, nonexistent_p, p);
    lvars   sock, password, host, user, userhost, service, cvec,
            org_mbxname = mbxname, use_anon_password = false;
    dlocal  select_socket, select_next_tag, next_uid_of_mbx,
            mbx_num_messages, mbx_num_recent, mbx_uidvalidity, mbx_uidnext,
            mbx_writeable, curr_mbx_name = false;
    dlocal 0 %  if dlocal_context == 1 then false -> sock endif,
                if dlocal_context == 2 and sock then sysclose(sock) endif
             %;

    define connection = newmapping([], 2, false, false) enddefine;

    unless buffer then inits(BUFLEN) -> buffer endunless;

    if mbxname and isstartstring('imap://', mbxname) then
        ;;; IMAP URL
        sys_parse_url(mbxname) -> (, mbxname);
        dl(mbxname) -> (host, mbxname);
        dest(host) -> (user, host);
        if user then hd(user) -> user endif;
        dl(host) -> (host, service)
    else
        false ->> user -> service;
        nullstring -> host
    endif;

    unless user then
        if host /= nullstring then
            ;;; assume anonymous@host
            true -> use_anon_password;
            'anonymous'
        else
            popusername
        endif -> user
    endunless;
    unless host /= nullstring or isstring(imap_default_host ->> host) then
        pophost("sitemailname") ->> host -> imap_default_host
    endunless;
    user <> '@' <> host -> userhost;

    unless connection(userhost) ->> cvec then
        consvector(false, 0, false, newmapping([], 8, false, false), 4)
                                        ->> cvec -> connection(userhost)
    endunless;
    explode(cvec) -> (sock, select_next_tag, password, next_uid_of_mbx);
    if sock and isclosed(sock) then false -> sock endif;
    sock -> select_socket;

    ;;; in case connection has auto-logged out
    while select_socket and sys_input_waiting(select_socket) do
        get_response(false, identfn)
    endwhile;

    unless (select_socket ->> sock) or func == FUNC_CLOSE then
        ;;; (re)open connection
        0 ->> select_next_tag -> subscrv(2,cvec);
        unless isstring(password) then
            if get_netrc_password(user, host) ->> password then
                password
            elseif use_anon_password then
                ;;; use e-mail address of end user
                popusername <> '@' <> pophost("sitemailname")
            else
                imap_get_password('\{b}Mail server password for ' <> userhost
                                        <> ': ')
            endif -> password
        endunless;
        sys_socket_to_service([^host ^(service or 'imap')], "line")
                            ->> sock ->> select_socket -> subscrv(1,cvec);
        get_response(false, identfn);
        get_responses('LOGIN ' <> user <> '\s' <> password,
                            mishap(%userhost, 1, 'MAIL SERVER LOGIN FAILED'%) );
        ;;; cache password if login successful
        password -> subscrv(3,cvec)
    endunless;

    if mbxname = nullstring then false -> mbxname endif;
    mbxname or 'INBOX' -> curr_mbx_name;

    if func == FUNC_CLOSE then
        false -> next_uid_of_mbx(curr_mbx_name);
        returnif(datalength(next_uid_of_mbx) /== 0);
        chain(identfn)      ;;; closes socket if open
    endif;

    ;;; use this prop as a reference count of mailboxes using the device
    unless next_uid_of_mbx(curr_mbx_name) then
        0 -> next_uid_of_mbx(curr_mbx_name)
    endunless;
    false ->> mbx_num_messages ->> mbx_num_recent ->> mbx_uidvalidity
            -> mbx_uidnext;

    get_responses(if func == FUNC_READ then 'EXAMINE '
                  else 'SELECT '
                  endif <> curr_mbx_name, nonexistent_p);

    if curr_mbx_name and not(mbx_writeable) and func /== FUNC_READ then
        mishap(org_mbxname, 1, 'MAILBOX IS NOT WRITEABLE')
    endif;

    ;;; apply procedure
    p(mbxname, buffs);

    select_next_tag -> subscrv(2,cvec);
    returnunless(curr_mbx_name);        ;;; i.e. if select failed

    mbx_uidvalidity || mbx_uidnext -> next_uid_of_mbx(curr_mbx_name);
    get_responses('CLOSE');

    if func == FUNC_SYNC and mbxname and mbx_num_messages == 0 then
        ;;; delete the (ordinary) mailbox
        get_responses('DELETE ' <> curr_mbx_name)
    endif
enddefine;

define lconstant read_messages(range_str) -> buffs;
    lvars l, pair, name, val, save, vec = false, flags = [], uid = false;

    [% get_responses('FETCH ' <> range_str <> ' (FLAGS UID BODY.PEEK[])') %]
                    -> buffs;

    for pair on buffs do
        hd(pair) ->> l -> save;
        until l == [] do
            dest(dest(l)) -> (name, val, l);
            if name = 'FLAGS' then
                val -> flags
            elseif name = 'UID' then
                strnumber(val) -> uid
            elseif name = 'BODY[]' then
                val ->> vec -> hd(pair)
            endif
        enduntil;
        sys_grbg_list(save);
        if lmember_=(Recent, flags) then
            Status
        elseif lmember_=(Seen, flags) then
            Status_RO
        else
            Status_O
        endif.copy -> subscrv(1,vec);
        sys_grbg_list(flags);
        mbx_uidvalidity || uid -> uid_of_mess(vec)
    endfor
enddefine;

define lconstant ReadMailbox(_, _);
    returnunless(curr_mbx_name) ([]);
    if mbx_num_messages == 0 then
        []
    else
        read_messages('1:*')
    endif
enddefine;

define imap_read_mailbox(mbxname) /* -> buffs */;
    select_apply(mbxname, false, FUNC_READ,
                    procedure;
                        false -> curr_mbx_name;
                        exitto(select_apply)
                    endprocedure,
                ReadMailbox)
enddefine;

define lconstant SyncMailbox(mbxname, buffs) -> new_buffs;
    lvars   n, num_mess, mbx_buff, mbx_buffs, buff, deletions, range_str,
            nappend, clear_Recent, set_Seen, m_stat;
    dlvars next_uid;

    define mess_=(vec1, vec2);
        lvars i, len = datalength(vec1);
        returnunless(datalength(vec2) == len) (false);
        ;;; ignore flags when testing equality
        fast_for i from 2 to len do
            returnunless(fast_subscrv(i,vec1) = fast_subscrv(i,vec2)) (false)
        endfor;
        true
    enddefine;

    define lmember_mess_=(vec, list);
        until list == [] do
            returnif(mess_=(fast_front(list), vec)) (list);
            fast_back(list) -> list
        enduntil;
        false
    enddefine;

    define is_new_mess(vec);
        lvars uid = uid_of_mess(vec);
        not(uid) or uid >= next_uid
    enddefine;

    define make_range_string(numlist) -> string;
        lvars n, l, last_n = hd(numlist), last_m = last_n;
        #| for n in tl(numlist) do
                if n-1 == last_m then
                    n -> last_m
                else
                    `,`, dest_characters(last_n);
                    if last_m /== last_n then `:`, dest_characters(last_m) endif;
                    n ->> last_n -> last_m
                endif;
            endfor;
            `,`, dest_characters(last_n);
            if last_m /== last_n then `:`, dest_characters(last_m) endif;
            `\s`
        |#;
        consstring(()-1) -> string;
        -> ;        ;;; erase first comma
    enddefine;

    define do_store(numlist, flag_str);
        lvars range_str;
        returnif(numlist == []);
        fast_ncrev(numlist) -> numlist;
        make_range_string(numlist) -> range_str;
        sys_grbg_list(numlist);
        dlocal ignore_fetch_response = true;    ;;; .SILENT doesn't work
        get_responses('STORE ' <> range_str <> flag_str)
    enddefine;


    mbx_num_messages -> num_mess;
    next_uid_of_mbx(curr_mbx_name) -> next_uid;

    if num_mess == 0 then [] else read_messages('1:*') endif -> mbx_buffs;
    1 -> n;
    [] ->> deletions ->> clear_Recent -> set_Seen;

    until buffs == [] or mbx_buffs == [] do
        hd(mbx_buffs) -> mbx_buff;
        if mess_=(mbx_buff, hd(buffs)) then
            dest(buffs) -> (buff, buffs);
            sys_grbg_destpair(mbx_buffs) -> (_, mbx_buffs);
            ;;; ensure message flags set
            subscrv(1,mbx_buff) -> m_stat;
            if m_stat = Status then n :: clear_Recent -> clear_Recent endif;
            if not(strmember(`R`,m_stat))
            and strmember(`R`, subscrv(1,buff)) then
                n :: set_Seen -> set_Seen
            endif;
            n + 1 -> n
        elseif is_new_mess(mbx_buff) or lmember_mess_=(mbx_buff, tl(buffs))
        then
            quitloop
        else
            n :: deletions -> deletions;
            sys_grbg_destpair(mbx_buffs) -> (_, mbx_buffs);
            n + 1 -> n
        endif
    enduntil;

    do_store(clear_Recent, '-FLAGS.SILENT (\\Recent)');
    do_store(set_Seen, '+FLAGS.SILENT (\\Seen)');

    [%  until mbx_buffs == [] do
            sys_grbg_destpair(mbx_buffs) -> (mbx_buff, mbx_buffs);
            if is_new_mess(mbx_buff) and not(lmember_mess_=(mbx_buff, buffs))
            then
                if buffs /== [] then n :: deletions -> deletions endif;
                mbx_buff
            else
                n :: deletions -> deletions
            endif;
            n + 1 -> n
        enduntil
    %] -> new_buffs;

    if buffs /== [] then
        ;;; append all messages from buffs
        applist(buffs, append_message);
        applist(new_buffs, append_message);
        listlength(buffs) + listlength(new_buffs) -> nappend;
        num_mess + nappend -> num_mess;
        mbx_uidnext + nappend -> mbx_uidnext
    endif;

    num_mess - listlength(deletions) -> num_mess;
    do_store(deletions, '+FLAGS.SILENT (\\Deleted)');

    num_mess -> mbx_num_messages
enddefine;      /* SyncMailbox */

define lconstant recreate(buffs);
    lvars mbxname = curr_mbx_name;
    if buffs == [] then
        0 -> mbx_num_messages;
        false -> curr_mbx_name
    else
        ;;; try to create and then re-select
        get_responses('CREATE ' <> mbxname);
        get_responses('SELECT ' <> mbxname)
    endif;
    exitto(select_apply)
enddefine;

define imap_sync_mailbox(mbxname, buffs) /* -> new_buffs */;
    select_apply(mbxname, buffs, FUNC_SYNC, recreate(%buffs%),
                                                            SyncMailbox)
enddefine;

define lconstant AppendMailbox(mbxname, buffs);
    ;;; append all messages from buffs
    applist(buffs, append_message)
enddefine;

define imap_append_mailbox(mbxname, buffs);
    select_apply(mbxname, buffs, FUNC_APPEND, recreate(%buffs%), AppendMailbox)
enddefine;

define imap_close_mailbox(mbxname);
    select_apply(mbxname, false, FUNC_CLOSE, false, false)
enddefine;

constant imap = true;

endsection;
