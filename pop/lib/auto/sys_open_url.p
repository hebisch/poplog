/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/lib/auto/sys_open_url.p
 > Purpose:         Open an Internet URL
 > Author:          John Gibson, Dec  7 1995 (see revisions)
 > Documentation:   REF * SOCKETS
 */
compile_mode :pop11 +strict;

section;

uses unix_sockets;
include cons_network_device.ph;

constant procedure sys_open_url;

lvars
    open_url,
    open_mode,
    open_org,
    open_org_rest,
    open_errc,
    open_url_rest
;


define lconstant Cons_dev(iodev, mode, buffer, net_ascii, close_p);
    cons_network_device(
        iodev, mode, buffer, net_ascii, close_p,
        'URL\s' <> open_url,
        consstring(#| explode('<URL:'), explode(open_url), `>` |#)
    )
enddefine;

define lconstant Write_command(dev, cmnd, params);
    lvars dev, cmnd, params;
    consstring(#|
        explode(cmnd),
        if params /= nullstring then `\s`, explode(params) endif,
        `\r`, `\n`
    |#) -> cmnd;
    syswrite(dev, cmnd, datalength(cmnd))
enddefine;

define lconstant Get_status_code(s, len);
    lvars s, len, d1, d2, d3;

    define lconstant get_digit(c);
        lvars c;
        isnumbercode(c) and c fi_- `0`
    enddefine;

    len fi_>= 3
    and (get_digit(fast_subscrs(1,s)) ->> d1)
    and (get_digit(fast_subscrs(2,s)) ->> d2)
    and (get_digit(fast_subscrs(3,s)) ->> d3)
    and ((d1 fi_* 10 fi_+ d2) fi_* 10 fi_+ d3)
enddefine;

define lconstant Checkdl_rest(rest);
    lvars rest;
    if islist(rest) then
        dl(rest)
    else
        mishap(open_url, 1, 'INVALID URL (not Internet protocol format)')
    endif
enddefine;

define lconstant Open_ftp() -> dev;
    lvars   (hostspec, cdpath) = Checkdl_rest(open_url_rest),
            ftp_sock, dctrl_sock = false, d_sock = false, dev, dirlist,
            cwd, cwd_list, name, type, user, pass, resp, port, c, cmnd,
            mode = open_mode, errc = open_errc, localhost = sys_host_name();

    define lconstant close_devs();
        if ftp_sock then
            if d_sock then sysclose(d_sock) endif;
            if dctrl_sock then sysclose(dctrl_sock) endif;
            sysclose(ftp_sock)
        endif
    enddefine;

    dlocal 0 %  if dlocal_context == 1 then false -> ftp_sock endif,
                if dlocal_context == 2 then close_devs() endif
             %;

    define lconstant ftp_mishap(mess);
        lvars mess;
        mishap(open_url, 1, 'ERROR OPENING ftp: URL (' sys_>< mess sys_>< ')')
    enddefine;

    define lconstant Get_response(cmnd, params);
        lvars cmnd, params, nread, b = sysstring;
        if cmnd then Write_command(ftp_sock, cmnd, params) endif;
        while (sysread(ftp_sock, b, sysstringlen) ->> nread) >= 4
        and fast_subscrs(4,b) == `-` and Get_status_code(b, nread) do
            ;;; has continuation -- skip it
        endwhile;
        Get_status_code(b, nread)
    enddefine;


    ;;; parse CWDs
    [%  while (sys_split_url(`/`, cdpath, 2:010) -> cdpath) ->> cwd do
            cwd
        endwhile
    %] -> cwd_list;
    sys_split_url(`;`, cdpath, 2:110) -> (name, type);
    if type then
        lowertoupper(type) -> type;
        unless datalength(type) == 6 and isstartstring('TYPE=',type)
        and strmember(subscrs(6,type)->>type, 'DLAI')
        then
            ftp_mishap('Invalid representation type')
        endunless
    endif;
    if name = nullstring and type /== `L` then `D` -> type endif;
    type == `D` or type == `L` -> dirlist;
    if dirlist and mode == 1 then
        ftp_mishap('Directory listing invalid in create mode')
    endif;

    ;;; open connection
    sys_socket_to_service(tl(hostspec), "line") -> ftp_sock;
    unless Get_response(false, false) == 220 then
        ftp_mishap('Can\'t connect to ftp server')
    endunless;

    ;;; login
    hd(hostspec) -> user;
    if user then dl(user) else 'anonymous', false endif -> (user, pass);
    unless (Get_response('USER', user) ->> resp) == 230 then
        if resp /== 331 then
            ftp_mishap('Login failed')
        elseif pass or user = 'anonymous' or user = 'ftp' then
            unless pass then popusername <> '@' <> localhost -> pass endunless;
            unless Get_response('PASS', pass) == 230 then
                ftp_mishap('Invalid username or password')
            endunless
        else
            ftp_mishap('Password required')
        endif
    endunless;

    ;;; do cwds
    for cwd in cwd_list do
        unless (Get_response('CWD', cwd) ->> resp) == 250 then
            if resp == 550 then
                if errc == `A` or errc == `D` then chain(false, identfn) endif;
                ftp_mishap('Invalid directory')
            else
                ftp_mishap('Change directory failed')
            endif
        endunless
    endfor;

    ;;; set representation type
    if open_org == true and not(type) then `I` -> type endif;
    if type and not(dirlist) then
        unless Get_response('TYPE', consstring(type, 1)) == 200 then
            ftp_mishap('Can\'t set representation type')
        endunless
    endif;

    if mode == 1 and errc == `F` and Get_response('SIZE', name) == 213 then
        ;;; return false if exists
        chain(false, identfn)
    endif;

    ;;; create control port and listen on it
    sys_socket(`i`, `S`, false) -> dctrl_sock;
    localhost -> sys_socket_name(dctrl_sock, 1);
    dl(sys_socket_name(dctrl_sock)) -> (localhost, port);
    consstring(#|
        fast_for c in_string localhost do
            if c == `.` then `,` else c endif
        endfor,
        `,`,
        dest_characters(port >> 8), `,`, dest_characters(port && 16:FF)
    |#) -> port;
    unless Get_response('PORT', port) == 200 then
        ftp_mishap('Can\'t set data port')
    endunless;

    ;;; initiate transfer
    if type == `D` then
        'NLST'
    elseif type == `L` then
        'LIST'
    elseif mode == 0 then
        'RETR'
    else
        'STOR'
    endif -> cmnd;
    unless (Get_response(cmnd, name) ->> resp) == 150 then
        if resp = 550 then
            if errc /== `N` then chain(false, identfn) endif;
            'No such file'
        elseif resp == 553 then
            'Permission denied'
        else
            'Failed to open data connection'
        endif;
        ftp_mishap()
    endunless;

    sys_socket_accept(dctrl_sock, open_org) -> d_sock;
    sysclose(dctrl_sock);
    false -> dctrl_sock;

    define lconstant Close(dev);
        lvars dev, resp;
        if mode == 0 then
            until sysread(d_sock, sysstring, sysstringlen) == 0 do enduntil
        else
            sysflush(dev)
        endif;
        sysclose(d_sock);
        Get_response(false, false) -> resp;
        sysclose(ftp_sock);
        unless resp == 226 then ftp_mishap('Error in data transfer') endunless
    enddefine;

    Cons_dev(d_sock, mode, false, type/==`I`, Close) -> dev;
    if mode == 0 then
        [%  200, nullstring,
            'content-type:',if type == `I` then 'image/unknown'
                            elseif dirlist then 'text/dirlist'
                            else 'text/unknown'
                            endif
        %] -> subscrv(NTD_ATTR_LIST,device_user_data(dev))
    endif
enddefine;

define lconstant Open_http() -> dev;
    lvars   (hostspec, url_path) = Checkdl_rest(open_url_rest), sock,
            dev, dd, c, keywd, rest, resp, status, pos, content_length;

    define lconstant Close(dev);
        lvars dev;
        sysclose(sock)
    enddefine;

    define lconstant get_char(dev);
        lvars dev;
        lconstant b = writeable inits(1);
        if fast_sysread(dev, 1, b, 1) == 0 then
            termin
        else
            fast_subscrs(1,b)
        endif
    enddefine;

    define lconstant skip_white(dev) -> c;
        lvars dev, c;
        while (get_char(dev) ->> c) == `\s` or c == `\t` do endwhile
    enddefine;

    define lconstant get_header_line(c, dev, no_keywd) -> (c, keywd, rest);
        lvars dev, c, n = 0, rest, keywd = false, no_keywd;
        repeat
            unless c then get_char(dev) -> c endunless;
            if c == `\n` then
                returnunless(keywd or n /== 0) (false ->> rest -> c);
                quitunless((get_char(dev) ->> c) == `\s` or c == `\t`);
                skip_white(dev) -> c;
                `\s`; n fi_+ 1 -> n;
                nextloop
            endif;
            unless keywd or no_keywd then
                if c == `:` then
                    consstring(c, n fi_+ 1) -> keywd;
                    0 -> n;
                    skip_white(dev) -> c;
                    nextloop
                else
                    uppertolower(c) -> c
                endif
            endunless;
            unless c == `\r` then c; n fi_+ 1 -> n endunless;
            false -> c
        endrepeat;
        consstring(n) -> rest
    enddefine;


    sys_socket_to_service(tl(hostspec), true) -> sock;
    Write_command(sock, 'GET', '/' <> url_path <> '\sHTTP/1.0');
    Write_command(sock, 'From:', popusername <> '@' <> sys_host_name());
    for c in open_org_rest do
        if isstring(c) then Write_command(sock, c, nullstring) endif
    endfor;
    Write_command(sock, nullstring, nullstring);

    Cons_dev(sock, 0, inits(512), false, Close) -> dev;
    device_user_data(dev) -> dd;

    unless get_char(dev) == `H` and get_char(dev) == `T`
       and get_char(dev) == `T` and get_char(dev) == `P`
       and get_char(dev) == `/`
       and (;;; get status line
            repeat 3 times get_char(dev) -> endrepeat;
            get_header_line(skip_white(dev), dev, true) -> (c, , status);
            Get_status_code(status, datalength(status)) ->> resp
        )
    then
        ;;; old server -- assume text/html
        subscrv(NTD_BUF_POS,dd) -> pos;
        subscrv(NTD_BUF_NBYTES,dd) + subscrv(NTD_BUF_POS,dd) - 1
                    -> subscrv(NTD_BUF_NBYTES,dd);
        1 -> subscrv(NTD_BUF_POS,dd);
        [200 '' 'content-type:' 'text/html'] -> subscrv(NTD_ATTR_LIST,dd);
        return
    endunless;

    ;;; read header lines
    false -> content_length;
    [%  resp, allbutfirst(3, status);
        while (get_header_line(c, dev, false) -> (keywd, rest)) ->> c do
            if keywd = 'content-length:' then
                strnumber(rest) -> content_length
            elseif keywd = 'location:' then
                'uri:' -> keywd
            endif;
            keywd, rest
        endwhile
    %] -> subscrv(NTD_ATTR_LIST,dd);

    if resp == 200 or resp == 203 then
        ;;; success or partial info -- return device
        if isinteger(content_length) then
            content_length -> subscrv(NTD_REM_NBYTES,dd)
        endif
    else
        ;;; anything else -- just return attribute list
        sysclose(sock);
        subscrv(NTD_ATTR_LIST,dd) -> dev
    endif
enddefine;

define lconstant Open_file();
    lvars   (hostspec, url_path) = Checkdl_rest(open_url_rest),
            host = uppertolower(hd(tl(hostspec)));
    unless host = nullstring or host = 'localhost'
    or host = sys_host_name() then
        mishap(open_url, 1, 'INVALID file: URL (not local host)')
    endunless;
    unless isstartstring('$',url_path) or isstartstring('~',url_path) then
        '/' <> url_path -> url_path
    endunless;
    if open_mode == 0 then
        sysopen
    else
        syscreate
    endif (url_path, open_mode, open_org, open_errc)
enddefine;

define sys_open_url(url, mode, org);
    lvars url, mode, org, errc, scheme;

    if isinteger(org) then
        ;;; optional character arg specifying error action
        ((), url, mode, org) -> (url, mode, org, errc);
        unless errc == `N` or errc == `F`
        or (mode /== 1 and (errc == `D` or errc == `A`))
        then
            mishap(errc, 1, 'sys_open_url: INVALID ERROR CHARACTER CODE')
        endunless
    else
        if mode == 1 then `N` else `F` endif -> errc
    endif;

    check_string(url);
    checkinteger(mode, 0, 1);
    unless islist(org) then org :: [] -> org endunless;

    dlocal  open_url = url, open_mode = mode,
            (open_org, open_org_rest) = dest(org),
            open_errc = errc, open_url_rest;

    sys_parse_url(url) -> (scheme, open_url_rest);

    if scheme = 'ftp' then
        Open_ftp()

    elseif scheme = 'http' then
        Open_http()

    elseif scheme = 'file' then
        Open_file()

    else
        mishap(url, 1, 'INVALID URL (unknown or unsupported protocol)')
    endif;

enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  8 1998
        Split off sys_split_url and sys_parse_url as new library procedures.
--- John Gibson, Dec 23 1997
        Split off device cons procedure as new library procedure
        cons_network_device.
 */
