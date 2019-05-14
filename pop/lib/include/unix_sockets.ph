/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/include/unix_sockets.ph
 > Purpose:         Unix socket interface
 > Author:          John Gibson May  6 1994
 > Documentation:
 */

#_TERMIN_IF DEF UNIX_SOCKETS_INCLUDED

section;

include sysdefs.ph;

    ;;; Socket Types

#_IF DEFV SYSTEM_V >= 4.0

iconstant macro (
    SOCK_STREAM     = 2,        ;;; stream socket
    SOCK_DGRAM      = 1,        ;;; datagram socket
    SOCK_RAW        = 4,        ;;; raw-protocol interface
    SOCK_RDM        = 5,        ;;; reliably-delivered message
    SOCK_SEQPACKET  = 6,        ;;; sequenced packet stream
);

#_ELSE

iconstant macro (
    SOCK_STREAM     = 1,        ;;; stream socket
    SOCK_DGRAM      = 2,        ;;; datagram socket
    SOCK_RAW        = 3,        ;;; raw-protocol interface
    SOCK_RDM        = 4,        ;;; reliably-delivered message
    SOCK_SEQPACKET  = 5,        ;;; sequenced packet stream
);

#_ENDIF


    ;;; Address families

iconstant macro (
    AF_UNSPEC       = 0,        ;;; unspecified
    AF_UNIX         = 1,        ;;; local to host (pipes, portals)
    AF_INET         = 2,        ;;; internetwork: UDP, TCP, etc.
    AF_IMPLINK      = 3,        ;;; arpanet imp addresses
    AF_PUP          = 4,        ;;; pup protocols: e.g. BSP
    AF_CHAOS        = 5,        ;;; mit CHAOS protocols
    AF_NS           = 6,        ;;; XEROX NS protocols
    AF_NBS          = 7,        ;;; nbs protocols
    AF_ECMA         = 8,        ;;; european computer manufacturers
    AF_DATAKIT      = 9,        ;;; datakit protocols
    AF_CCITT        = 10,       ;;; CCITT protocols, X.25 etc
    AF_SNA          = 11,       ;;; IBM SNA
    AF_DECnet       = 12,       ;;; DECnet
    AF_DLI          = 13,       ;;; Direct data link interface
    AF_LAT          = 14,       ;;; LAT
    AF_HYLINK       = 15,       ;;; NSC Hyperchannel
    AF_APPLETALK    = 16,       ;;; Apple Talk
    AF_NIT          = 17,       ;;; Network Interface Tap
    AF_802          = 18,       ;;; IEEE 802.2, also ISO 8802
    AF_OSI          = 19,       ;;; umbrella for all families used
    AF_X25          = 20,       ;;; CCITT X.25 in particular
    AF_OSINET       = 21,       ;;; AFI = 47, IDI = 4
    AF_GOSIP        = 22,       ;;; U.S. Government OSI
    AF_MAX          = 22,
);

    ;;; IP Values
iconstant macro (
    INADDR_ANY          = 16:00000000,
    INADDR_LOOPBACK     = 16:7F000001,
    INADDR_BROADCAST    = 16:FFFFFFFF,
);


    ;;; sys_socket_option
iconstant macro (
    INTBIT          = 2:1e23,   ;;; integer value (as opposed to boolean)
    TCPBIT          = 2:1e24,   ;;; TCP option

    ;;; Socket-level options
    SO_DEBUG        = 2:1e0,    ;;; turn on debugging info recording
    SO_ACCEPTCONN   = 2:1e1,    ;;; socket has had listen()
    SO_REUSEADDR    = 2:1e2,    ;;; allow local address reuse
    SO_KEEPALIVE    = 2:1e3,    ;;; keep connections alive
    SO_DONTROUTE    = 2:1e4,    ;;; just use interface addresses
    SO_BROADCAST    = 2:1e5,    ;;; permit sending of broadcast msgs
    SO_USELOOPBACK  = 2:1e6,    ;;; bypass hardware when possible
    SO_LINGER       = 2:1e7,    ;;; linger on close if data present (SPECIAL)
    SO_OOBINLINE    = 2:1e8,    ;;; leave received OOB data in line

    SO_SNDBUF       = 16:1001 || INTBIT,    ;;; send buffer size
    SO_RCVBUF       = 16:1002 || INTBIT,    ;;; receive buffer size
    SO_SNDLOWAT     = 16:1003 || INTBIT,    ;;; send low-water mark
    SO_RCVLOWAT     = 16:1004 || INTBIT,    ;;; receive low-water mark
    SO_SNDTIMEO     = 16:1005 || INTBIT,    ;;; send timeout
    SO_RCVTIMEO     = 16:1006 || INTBIT,    ;;; receive timeout
    SO_ERROR        = 16:1007 || INTBIT,    ;;; get error status and clear
    SO_TYPE         = 16:1008 || INTBIT,    ;;; get socket type
    SO_PROTOTYPE    = 16:1009 || INTBIT,    ;;; get/set protocol type

    ;;; TCP Options
    TCP_NODELAY     = 1 || TCPBIT,              ;;; don't delay send to coalesce packets
    TCP_MAXSEG      = 2 || TCPBIT || INTBIT,    ;;; set max segment size
);

    ;;; Send/Receive message flags
iconstant macro (
    MSG_OOB         = 2:1e0,    ;;; process out-of-band data
    MSG_PEEK        = 2:1e1,    ;;; peek at incoming message
    MSG_DONTROUTE   = 2:1e2,    ;;; send without using routing tables
);

iconstant UNIX_SOCKETS_INCLUDED = true;

endsection;
