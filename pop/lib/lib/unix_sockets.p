/* --- Copyright University of Sussex 2004. All rights reserved. ----------
 > File:            C.unix/lib/lib/unix_sockets.p
 > Purpose:         Unix socket interface
 > Author:          John Gibson May  6 1994 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

include sysdefs.ph;

#_IF not(DEF BERKELEY or DEFV SYSTEM_V >= 4.0)
    #_TERMIN_IF DEF POPC_COMPILING
    mishap(0, 'SOCKETS NOT SUPPORTED IN THIS SYSTEM');
#_ENDIF


section;
exload_batch;

include unix_sockets.ph;
include unix_errno.ph;

l_typespec

    sockaddr_un
      { sun_family  :short,
        sun_path    :byte[].exacc_ntstring
      },

    sockaddr_in
      { sin_family  :short,
        sin_port    :ushort,
        sin_addr_b  :byte[0],   ;;; dummy for getting byte addr of sin_addr
        sin_addr    :uint,
        sin_zero    :byte[8]
      },

    hostent
      { h_name      !exptr,
        h_aliases   !exptr,
        h_addrtype  !int,
        h_length    !int,
        h_addr      !exptr.:exptr
      },

    servent
      { s_name      !exptr,
        s_aliases   !exptr,
        s_port      !int,
        s_proto     !exptr.exacc_ntstring.consword
      },

    protoent
      { p_name      !exptr,
        p_aliases   !exptr,
        p_proto     !int
      },

    netent
      { n_name      !exptr,
        n_aliases   !exptr,
        n_addrtype  !int,
        n_net       !uint
      }
;

exload unix_sockets
    #_IF DEFV SYSTEM_V >= 4.0 or DEF SCO
        ['-lsocket']
    #_ENDIF

lconstant
    U_close(fd) :int                                <- close,
    U_socket(af,type,protocol) :int                 <- socket,
    U_socketpair(af,type,protocol,sv) :int          <- socketpair,
    U_getsockname(s,name,namelenp) :int             <- getsockname,
    U_getpeername(s,name,namelenp) :int             <- getpeername,
    U_bind(s,name,namelen) :int                     <- bind,
    U_connect(s,name,namelen) :int                  <- connect,
    U_listen(s,qlen) :int                           <- listen,
    U_accept(s,name,namelenp) :int                  <- accept,
    U_shutdown(s,how) :int                          <- shutdown,
    U_getsockopt(s,level,option,valp,lenp) :int     <- getsockopt,
    U_setsockopt(s,level,option,valp,len) :int      <- setsockopt,
    U_gethostbyname(name) :exptr.:hostent           <- gethostbyname,
    U_getservbyname(name,proto) :exptr.:servent     <- getservbyname,
    U_getprotobyname(name) :exptr.:protoent         <- getprotobyname,
    U_getnetbyname(name) :exptr.:netent             <- getnetbyname,
    U_inet_ntoa(in) :exptr.exacc_ntstring           <- inet_ntoa,
    U_inet_addr(cp) :uint                           <- inet_addr,
    U_inet_makeaddr(net,lna) :uint                  <- inet_makeaddr,
    U_recv(s,buf,len,flags) :int                    <- recv,
    U_recvfrom(s,buf,len,flags,from,fromlenp) :int  <- recvfrom,
    U_send(s,buf,len,flags) :int                    <- send,
    U_sendto(s,buf,len,flags,to,tolen) :int         <- sendto,
    U_htons(val) :ushort                            <- htons,
    U_htonl(val) :uint                              <- ntohl,
    U_ntohs(val) :ushort                            <- ntohs,
    U_ntohl(val) :uint                              <- ntohl,
;;; This is no longer supported. See revision notes
;;; U_errno :int                                    <- errno,
    pop_call_in_addr_arg(inp,func) :exptr,
    pop_call_in_addr_res(net,lna,inp,func),
#_IF DEF OSF1
    _pop_sigmask(block) :void,
#_ENDIF
endexload;

lconstant macro (
    SOL_SOCKET      = 16:FFFF,  ;;; options for socket level
    ;;; see $popsrc/errors.p for definition of DO_ERRNO_VAL
    ERRNO           = [DO_ERRNO_VAL],
    NAMEBUF_SIZE    = 256,
);

lconstant
    intvec1 = writeable initintvec(1),
    intvec2 = writeable initintvec(2),
    ;

define :inline lconstant HTONS(val);
    (exacc U_htons(val) fi_&& 16:FFFF)
enddefine;

define :inline lconstant HTONL(val);
    exacc U_htonl(val)
enddefine;

define :inline lconstant NTOHS(val);
    (exacc U_ntohs(val) fi_&& 16:FFFF)
enddefine;

define :inline lconstant NTOHL(val);
    exacc U_ntohl(val)
enddefine;


;;; --- NAME <-> SOCKADDR MAPPING -----------------------------------------

;;; AF_UNIX

define lconstant name_to_sa_UNIX(name) -> (sockaddr_un, namesize);
    lvars name, sockaddr_un, namesize;
    sysfileok(name) -> name;
    SIZEOFTYPE(:short) + datalength(name) -> namesize;
    initexptr_mem(namesize+1) -> sockaddr_un;
    AF_UNIX -> exacc sockaddr_un.sun_family;
    name    -> exacc sockaddr_un.sun_path
enddefine;
;;;
define updaterof name_to_sa_UNIX(sockaddr_un, namesize) /* -> name */;
    lvars sockaddr_un, namesize;
    exacc sockaddr_un.sun_path
enddefine;

;;; AF_INET

define lconstant inet_name_to_sa(name) -> (sockaddr_in, namesize, proto);
    lvars   name, sockaddr_in, namesize, addr, hostent, servent,
            netent, lna, port = 0, org_name = name, proto = false;

    define inv_name(ms);
        lvars ms;
        mishap(org_name, 1, 'INVALID INTERNET SOCKET NAME (' <> ms <> ')')
    enddefine;

    define call_getbyname(func, name);
#_IF DEF OSF1
        ;;; OSF doesn't deal properly with EINTR inside gethostbyname
        dlocal 0 %exacc _pop_sigmask(1), exacc _pop_sigmask(0) %;
#_ENDIF
        exacc (1):exptr func(name)
    enddefine;

    SIZEOFTYPE(:sockaddr_in) -> namesize;
    initexptr_mem(namesize) -> sockaddr_in;

    0 -> port;
    if islist(name) then
        if listlength(name) == 2 then
            dl(name) -> (name, port);
            unless isinteger(port) then
                if isvector(port) then
                    if datalength(port) == 2 then
                        explode(port) -> (port, proto)
                    else
                        inv_name('invalid service vector')
                    endif
                endif;
                if isword(proto) then fast_word_string(proto) -> proto endif;
                if isstring(port) and (not(proto) or isstring(proto)) then
                    ;;; service spec
                    exacc U_getservbyname(port, proto) -> servent;
                    unless is_valid_external_ptr(servent) then
                        inv_name('unknown server')
                    endunless;
                    exacc servent.s_proto -> proto;
                    NTOHS(exacc servent.s_port) -> port
                elseif isstring(proto) then
                    consword(proto) -> proto
                endif
            endunless;
            unless isinteger(port) and port >= 0 then
                inv_name('invalid port')
            endunless
        else
            inv_name('invalid host/port list')
        endif
    endif;

    if name == "*" then INADDR_ANY -> name endif;

    if isvector(name) then
        ;;; net spec
        INADDR_ANY -> lna;
        if datalength(name) == 2 then
            explode(name) -> (name, lna)
        else
            inv_name('invalid net address vector')
        endif;
        if isstring(name) then
            call_getbyname(U_getnetbyname, name) -> netent;
            unless is_valid_external_ptr(netent)
            and exacc netent.n_addrtype == AF_INET then
                inv_name('unknown or invalid network name')
            endunless;
            exacc netent.n_net -> name
        endif;
        unless isinteger(name) and isinteger(lna) then
            inv_name('invalid net and/or local address part')
        endunless;
        exacc pop_call_in_addr_res(name, lna,
                        exacc[@,nc] sockaddr_in.sin_addr, U_inet_makeaddr)

    elseif isstring(name) then
        ;;; try numeric notation first
        if datalength(name) /== 0 and isnumbercode(name(1))
        and (exacc U_inet_addr(name) ->> addr) /= 16:FFFFFFFF then
            addr -> exacc sockaddr_in.sin_addr
        else
            ;;; try as hostname
            call_getbyname(U_gethostbyname, name) -> hostent;
            unless is_valid_external_ptr(hostent)
            and exacc hostent.h_addrtype == AF_INET then
                inv_name('unknown or invalid hostname')
            endunless;
            move_bytes( 1, exacc[nc] hostent.h_addr,
                        1, exacc[nc] sockaddr_in.sin_addr_b,
                        SIZEOFTYPE(:uint))
        endif
    elseif isintegral(name) and name >= 0 then
        HTONL(name) -> exacc sockaddr_in.sin_addr
    else
        inv_name('address not a string or (big)integer >= 0')
    endif;

    HTONS(port) -> exacc sockaddr_in.sin_port;
    set_bytes(0, 1, exacc[nc] sockaddr_in.sin_zero, 8);
    AF_INET -> exacc sockaddr_in.sin_family
enddefine;

define lconstant name_to_sa_INET = inet_name_to_sa <> erase enddefine;
;;;
define updaterof name_to_sa_INET(sockaddr_in, namesize) /* -> name */;
    lvars sockaddr_in, namesize, inaddr, port;
    NTOHS(exacc sockaddr_in.sin_port) -> port;
    returnif(port == 0) (false);
    NTOHL(exacc sockaddr_in.sin_addr) -> inaddr;
    if inaddr == INADDR_ANY then
        "*"
    else
        exacc_ntstring(exacc pop_call_in_addr_arg(
                        exacc[@,nc] sockaddr_in.sin_addr, U_inet_ntoa))
    endif -> inaddr;
    if port == 0 then
        inaddr
    else
        [^inaddr ^port]
    endif
enddefine;

define sys_socket_name_trans =
    newassoc([
            [^AF_UNIX   ^name_to_sa_UNIX]
            [^AF_INET   ^name_to_sa_INET]
        ])
enddefine;

lconstant
    noname_ms = 'NO NAME TRANSLATION AVAILABLE FOR SOCKET ADDRESS FAMILY';

define lconstant name_to_sockaddr(name, af) /* -> (namebuf, namesize) */;
    lvars name, af, trans_p;
    if sys_socket_name_trans(af) ->> trans_p then
        chain(name, trans_p)
    else
        mishap(af, 1, noname_ms)
    endif
enddefine;

define lconstant sockaddr_to_name(namebuf, namesize) /* -> name */;
    lvars namebuf, namesize, trans_p, af = exacc :short namebuf;
    if sys_socket_name_trans(af) ->> trans_p then
        chain(namebuf, namesize, updater(trans_p))
    elseif af == AF_UNSPEC then
        false
    else
        mishap(af, 1, noname_ms)
    endif
enddefine;


;;; --- SOCKET CREATION -------------------------------------------------

define lconstant is_socket =
    newproperty([], 8, false, "tmparg")
enddefine;

define lconstant check_sock(sock) /* -> af */;
    lvars sock, svec;
    if is_socket(sock) ->> svec then
        svec(1)         ;;; af
    else
        mishap(sock, 1, 'SOCKET NEEDED')
    endif
enddefine;

define lconstant open_socks(af, type, protocol, routine) -> res;
    lvars   af, type, protocol, res, routine, retry = 1,
            clex = (routine == U_socket);
    repeat
        exacc (4):int routine(af, type, protocol, intvec2) -> res;
        quitif((Sys_fd_open_check(res, clex, retry) ->> retry) < 0)
    endrepeat;
    if res < 0 then
        mishap(af, type, protocol, 3, '%CAN\'T CREATE SOCKET(S) (%M)')
    endif
enddefine;


define lconstant proto_number(proto) -> protonum;
    lvars proto, protonum, protoent;
    define lconstant cache = newassoc([]) enddefine;

    returnif(cache(proto) ->> protonum);
    exacc U_getprotobyname(fast_word_string(proto)) -> protoent;
    if is_valid_external_ptr(protoent) then
        exacc protoent.p_proto -> protonum
    elseunless (strnumber(proto) ->> protonum) and isinteger(protonum)
    and protonum >= 0 then
        mishap(proto, 1, 'UNKNOWN OR INVALID PROTOCOL NAME')
    endif;
    protonum -> cache(proto)
enddefine;

define lconstant create_socks(af, type, routine) /* -> (res,svec) */;
    lvars af, type, routine, protocol = 0, protoent;
    if isword(type) then
        ;;; optional protocol specified
        ((), af, type) -> (af, type, protocol);
        proto_number(protocol) -> protocol
    endif;

    checkinteger(af, 0, false);
    if af == `u` then
        AF_UNIX -> af
    elseif af == `i` then
        AF_INET -> af
    endif;
    checkinteger(type, 0, false);
    if type == `S` then
        SOCK_STREAM -> type
    elseif type == `D` then
        SOCK_DGRAM -> type
    endif;

    open_socks(af, type, protocol, routine),
    consvector(af, type, protocol, 3)
enddefine;

define lconstant make_sock_dev(fd, svec, org) -> sock;
    lvars   fd, svec, org,
            sock = Sys_cons_device('socket', false, 2, org, fd, true);
    svec -> is_socket(sock)
enddefine;

define sys_socket(/*af, type,*/ org) with_nargs 4;
    lvars org;
    make_sock_dev(create_socks((), U_socket), org)
enddefine;

define sys_socket_pair(/*af, type,*/ org) with_nargs 4;
    lvars org, s1, s2, (, svec) = create_socks((), U_socketpair);
    explode(intvec2) -> (s1, s2);
    make_sock_dev(s1, svec, org);
    make_sock_dev(s2, svec, org);
enddefine;


;;; --- SOCKET NAMES ---------------------------------------------------

define lconstant get_sock_name(sock, routine) -> name;
    lvars   sock, routine, af = check_sock(sock), res, name, retry = 1,
            namebuf = EXPTRINITSTR(:byte[NAMEBUF_SIZE]);
    repeat
        NAMEBUF_SIZE -> intvec1(1);
        exacc (3):int routine(device_os_channel(sock), namebuf, intvec1) -> res;
        quitif((Sys_fd_open_check(res, false, retry) ->> retry) < 0)
    endrepeat;

    if res >= 0 then
        sockaddr_to_name(namebuf, intvec1(1))
    elseif ERRNO == ENOTCONN then
        false
    else
        mishap(sock, 1, '%CAN\'T GET SOCKET NAME (%M)')
    endif -> name;

    sys_grbg_fixed(namebuf)
enddefine;

define sys_socket_name(sock) /* -> name */;
    lvars sock;
    get_sock_name(sock, U_getsockname)
enddefine;
;;;
define updaterof sys_socket_name(name, sock);
    lvars   name, sock, res, retry = 1, qlen = false, fd, namebuf, namesize;

    if isinteger(sock) then
        ;;; queue length specified -- start listening after bind
        ((), name, sock) -> (name, sock, qlen);
        checkinteger(qlen, 1, false)
    endif;

    name_to_sockaddr(name, check_sock(sock)) -> (namebuf, namesize);
    device_os_channel(sock) -> fd;

    repeat
        exacc U_bind(fd, namebuf, namesize) -> res;
        quitif((Sys_fd_open_check(res, false, retry) ->> retry) < 0)
    endrepeat;

    sys_grbg_fixed(namebuf);
    if res < 0 then
        mishap(name, sock, 2, '%CAN\'T ASSIGN SOCKET NAME (%M)')
    endif;

    if qlen and exacc U_listen(fd, qlen) < 0 then
        mishap(name, sock, qlen, 3, '%LISTEN FAILED ON SOCKET (%M)')
    endif
enddefine;

define lconstant do_connect(peername, sock, namebuf, namesize, changes_p,
                                                        conn_retries);
    lvars   peername, sock, res, retry, namebuf, namesize, conn_retries,
            changes_p, fd = device_os_channel(sock),
            sockname = sys_socket_name(sock), connect_started;

    repeat
        changes_p(sock);
        false -> connect_started;
        1 -> retry;

        repeat
            repeat
                exacc U_connect(fd, namebuf, namesize) -> res;
                quitif((Sys_fd_open_check(res, false, retry) ->> retry) < 0);

                ;;; An interrupt means the connection attempt may have
                ;;; actually started, so just wait after EALREADY, and
                ;;; take EISCONN to mean success
                if res < 0 and ERRNO == EINTR then
                    true -> connect_started
                endif
            endrepeat;

            quitif(res >= 0) (2);           ;;; success

            quitunless(connect_started);
            if ERRNO == EISCONN then
                ;;; take this to mean success
                0 -> res;
                quitloop(2)
            endif;
            ;;; HPUX (possibly others?) returns EADDRINUSE instead of EALREADY,
            ;;; so test for them both ...
            quitunless(ERRNO == EALREADY or ERRNO == EADDRINUSE);
            ;;; else wait a bit if already in progress
            syssleep(50)
        endrepeat;

        ;;; come here for (genuine) error
        quitunless(ERRNO == ECONNREFUSED);

        ;;; ECONNREFUSED -- fd of socket is now useless, so must close it
        ;;; and create another
        while Sys_fd_open_check(exacc U_close(fd), false, 0) == 0 do endwhile;
        open_socks(explode(is_socket(sock)), U_socket)
                                    ->> fd -> device_os_channel(sock);
        if sockname then sockname -> sys_socket_name(sock) endif;

        if conn_retries == 0 then
            mishap(peername, sock, 2, 'CAN\'T ASSIGN SOCKET PEERNAME (Connection refused)')
        endif;
        conn_retries-1 -> conn_retries;
        ;;; wait a sec and try again with the new socket
        syssleep(100)
    endrepeat;

    returnunless(namebuf);
    sys_grbg_fixed(namebuf);
    if res < 0 then
        mishap(peername, sock, 2, '%CAN\'T ASSIGN SOCKET PEERNAME (%M)')
    endif
enddefine;

define sys_socket_peername(sock) /* -> peername */;
    lvars sock;
    get_sock_name(sock, U_getpeername)
enddefine;
;;;
define updaterof sys_socket_peername(peername, sock);
    lvars   peername, sock, namebuf, namesize, af, conn_retries = 5, p = erase;

    if isinteger(sock) then
        ;;; optional connect retry count
        ((), peername, sock) -> (peername, sock, conn_retries);
        checkinteger(conn_retries, 1, false)
    endif;
    if isprocedure(sock) then
        ;;; optional procedure to apply to sock before connect attempt
        ((), peername, sock) -> (peername, sock, p)
    endif;

    check_sock(sock) -> af;
    if peername or is_socket(sock)(2) /== SOCK_DGRAM then
        name_to_sockaddr(peername, af)
    else
        ;;; false for SOCK_DGRAM
        false, 0
    endif -> (namebuf, namesize);

    do_connect(peername, sock, namebuf, namesize, p, conn_retries)
enddefine;


;;; --- OTHER ROUTINES ---------------------------------------------------

define sys_socket_to_service(peername, org) -> sock;
    lvars   peername, org, sock, type,
            (namebuf, namesize, proto) = inet_name_to_sa(peername);
    unless proto then
        mishap(peername, 1, 'INTERNET SERVICE NAME NEEDED')
    endunless;
    if proto = "tcp" then
        SOCK_STREAM
    elseif proto = "udp" then
        SOCK_DGRAM
    else
        mishap(peername, 1, 'SERVICE PROTOCOL NOT tcp OR udp')
    endif -> type;

    sys_socket(AF_INET, type, org) -> sock;
    do_connect(peername, sock, namebuf, namesize, erase, 5)
enddefine;


define sys_socket_accept(sock, org) /* -> conn_sock */;
    lvars sock, org;
    check_sock(sock) -> ;

    procedure();
        lvars fd = device_os_channel(sock), cfd, retry = 1;
        dlocal % sys_async_io(sock, 0) % = false;

        ;;; wait for connection in X if running ...
        sys_device_wait(sock, [], [], true) -> (,,);

        repeat
            exacc U_accept(fd, dup(null_external_ptr)) -> cfd;
            quitif((Sys_fd_open_check(cfd, false, retry) ->> retry) < 0)
        endrepeat;

        if cfd < 0 then
            mishap(sock, 1, '%ACCEPT FAILED ON SOCKET (%M)')
        else
            make_sock_dev(cfd, is_socket(sock), org)
        endif
    endprocedure()
enddefine;

define sys_socket_shutdown(sock, how);
    lvars sock, how;
    check_sock(sock) -> ;
    checkinteger(how, 0, 2);
    if exacc U_shutdown(device_os_channel(sock), how) < 0 then
        mishap(sock, how, 2, '%SHUTDOWN FAILED ON SOCKET (%M)')
    endif
enddefine;


;;; --- SEND AND RECEIVE -----------------------------------------------

define lconstant get_option_args(sock, option) -> (option, level, bool);
    lvars sock, option, level = SOL_SOCKET, bool = true;
    check_sock(sock) -> ;
    checkinteger(option, 0, false);

    if option &&/=_0 TCPBIT then
        option &&~~ TCPBIT -> option;
        proto_number("tcp") -> level
    endif;
    if option &&/=_0 INTBIT then
        option &&~~ INTBIT -> option;
        false -> bool
    endif
enddefine;

define sys_socket_option(sock, option) -> value;
    lvars   sock, option, value, res,
            (opt, level, bool) = get_option_args(sock, option);
    SIZEOFTYPE(:int[2]) -> intvec1(1);
    exacc U_getsockopt(device_os_channel(sock), level, opt, intvec2, intvec1)
                                        -> res;
    if res < 0 then
        mishap(sock, option, 2, '%ERROR GETTING SOCKET OPTION (%M)')
    endif;
    intvec2(1) -> value;
    if option == SO_LINGER then
        value /== 0 and intvec2(2) -> value
    elseif bool then
        value /== 0 -> value
    endif
enddefine;
;;;
define updaterof sys_socket_option(value, sock, option);
    lvars   value, sock, option, res, len = SIZEOFTYPE(:int),
            (opt, level, bool) = get_option_args(sock, option);
    if option == SO_LINGER then
        SIZEOFTYPE(:int[2]) -> len;
        if value then 1, value else 0, 0 endif -> intvec2(2);
    elseif bool then
        if value then 1 else 0 endif
    else
        value
    endif -> intvec2(1);
    exacc U_setsockopt(device_os_channel(sock), level, opt, intvec2, len)
                                        -> res;
    if res < 0 then
        mishap(sock, option, 2, '%ERROR SETTING SOCKET OPTION (%M)')
    endif
enddefine;

define sys_socket_recv(sock, buff, nbytes, flags, want_fromname);
    lvars sock, buff, nbytes, flags, want_fromname, fd, res, namebuf;
    check_sock(sock) -> ;
    check_string(buff);
    checkinteger(nbytes, 0, false);
    checkinteger(flags, 0, false);
    device_os_channel(sock) -> fd;

    if want_fromname then
        EXPTRINITSTR(:byte[NAMEBUF_SIZE]) -> namebuf
    endif;

    repeat
        if want_fromname then
            NAMEBUF_SIZE -> intvec1(1);
            exacc U_recvfrom(fd, buff, nbytes, flags, namebuf, intvec1)
        else
            exacc U_recv(fd, buff, nbytes, flags)
        endif -> res;
        quitif(res >= 0);
        nextif(ERRNO == EINTR);
        mishap(sock, 1, '%ERROR RECEIVING FROM SOCKET (%M)')
    endrepeat;

    if want_fromname then
        res, sockaddr_to_name(namebuf, intvec1(1));
        sys_grbg_fixed(namebuf)
    else
        res
    endif
enddefine;

define sys_socket_send(sock, buff, nbytes, flags, toname);
    lvars sock, buff, nbytes, flags, toname, fd, res, namebuf, namesize, af;
    check_sock(sock) -> af;
    check_string(buff);
    checkinteger(nbytes, 0, false);
    checkinteger(flags, 0, false);
    device_os_channel(sock) -> fd;

    if toname then
        name_to_sockaddr(toname, af) -> (namebuf, namesize)
    endif;

    repeat
        if toname then
            exacc U_sendto(fd, buff, nbytes, flags, namebuf, namesize)
        else
            exacc U_send(fd, buff, nbytes, flags)
        endif -> res;
        quitif(res >= 0);
        nextif(ERRNO == EINTR);
        mishap(sock, 1, '%ERROR SENDING TO SOCKET (%M)')
    endrepeat;

    if toname then sys_grbg_fixed(namebuf) endif
enddefine;

constant unix_sockets = true;

endexload_batch;
endsection;



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec 31 2004
        Redefined ERRNO to correspond to changes made to system sources
        in  $popexternlib/c_core.c, $popsrc/unixdefs.ph $popsrc/errors.p

--- John Gibson, May 13 1998
        __pop_call_in_addr_res/arg -> pop_call_in_addr_res/arg
--- John Gibson, Mar 12 1997
        In inet_name_to_sa, made get{host,net}byname be called with
        signals blocked in OSF1.
--- John Gibson, Sep 12 1996
        Removed use of sys*iomessage.
--- John Gibson, Dec  1 1995
        Also changed do_connect to treat EADDRINUSE as equivalent to EALREADY
--- John Gibson, Nov 20 1995
        Fixed do_connect to deal with EALREADY and EISCONN errors after
        an interrupted connect attempt.
--- John Gibson, Mar 25 1995
        Changed ulong types to uint
--- John Gibson, Dec 14 1994
        Fix to inet_name_to_sa
 */
