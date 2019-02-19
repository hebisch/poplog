/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/unix_select.ph
 > Purpose:
 > Author:          John Gibson Jun 13 1994 (see revisions)
 */

#_IF DEF LINUX
deftype fd_mask = long;
    ;;; Use the standard definition
lconstant macro FD_SETSIZE = 1024;
#_ELSE
deftype fd_mask = int;
    ;;; Currently determined by FD_SETSIZE defined in c_core.h,
    ;;; although that could be removed to use the standard definition.
lconstant macro FD_SETSIZE = 256;
#_ENDIF

#_IF DEF AIX
        ;;; Crummy AIX doesn't round this properly
lconstant macro FD_SET_NWORDS = _pint(##(fd_mask)[_:FD_SETSIZE | 1.t]) + 1;
#_ELSE
lconstant macro FD_SET_NWORDS = _pint(##(fd_mask)[_:FD_SETSIZE | 1.r]);
#_ENDIF

deftype fd_set = fd_mask[FD_SET_NWORDS];

lconstant macro (
    FD_SET_BMASK    = _pint(##(1)[_1|fd_mask] _sub _1),
    FD_SET_WSHIFT   = -integer_length(FD_SET_BMASK),
);

define :inline lconstant _FD_OP(_fd, _fdsetp, _expr);
    lblock
        lvars   _ptr = (_fdsetp)@(fd_mask)[ _shift(_fd, _:FD_SET_WSHIFT) ],
                _shft = _fd _bimask _:FD_SET_BMASK,
                _mask = _shift(_1, _shft);
        _expr
    endlblock
enddefine;

define :inline lconstant FD_SET(_fd, _fdsetp);
    _FD_OP(_fd, _fdsetp, _ptr!(fd_mask) _biset _mask -> _ptr!(fd_mask))
enddefine;

define :inline lconstant FD_CLR(_fd, _fdsetp);
    _FD_OP(_fd, _fdsetp, _ptr!(fd_mask) _biclear _mask -> _ptr!(fd_mask))
enddefine;

define :inline lconstant FD_ISSET(_fd, _fdsetp);
    _FD_OP(_fd, _fdsetp, _ptr!(fd_mask) _bitst _mask)
enddefine;

define :inline lconstant FD_ZERO(_fdsetp);
    _ifill(_0, @@(fd_set)++, _fdsetp)
enddefine;

    /*  Structures used by sys_device_wait and _extern XptIOWait
    */
deftype fdesc_set = struct
  { -short      FDS_NFDS,           ;;; max fd + 1 in set
                FDS_MINFD;          ;;; min fd in set
    fd_set      FDS_FDSET;          ;;; fd bitmasks
  };

deftype wait_fdesc_sets = struct
  { fdesc_set   WFDS_RD_SET,        ;;; read set
                WFDS_WR_SET,        ;;; write set
                WFDS_EX_SET;        ;;; exception set
  };


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  6 1995
        Changed fd_set element type to int, made FD_ZERO use _ifill
 */
