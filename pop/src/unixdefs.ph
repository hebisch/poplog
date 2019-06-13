/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.unix/src/unixdefs.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ------------------ UNIX SYMBOL DEFINITIONS ----------------------------


;;; --- ERROR NUMBERS ---------------------------------------------------

#_INCLUDE '../lib/include/unix_errno.ph'

;;; lconstant macro _ERRNO = [_extern errno:data!(int)];

;;; Added by A.Sloman on advice from Waldek Hebisch 29 Jun 2003

/*
;;; Invoke active variable defined in errors.p
define active DO_ERRNO_VAL();

define updaterof active DO_ERRNO_VAL(_x);

*/

;;; lconstant macro _ERRNO = [_extern errno:data!(int)];
lconstant macro _ERRNO = [DO_ERRNO_VAL];


;;; --- BASIC TYPES ------------------------------------------------------

/* From <sys/types.h> */

deftype
    off_t   = -long,
    blkcnt_t= -long;

#_IF DEFV SYSTEM_V >= 4.0

deftype
    dev_t   = long,
    ino_t   = long,
    mode_t  = long,
    nlink_t = long,
    time_t  = -long,
    uid_t   = -long,
    gid_t   = uid_t;

#_ELSEIF DEFV LINUX >= 2.0  /* must come before BERKELEY */

deftype
    dev_t   = double,
    ino_t   = long,
    mode_t  = int,
    nlink_t = long,
    time_t  = -long,
    uid_t   = int,
    gid_t   = uid_t;

#_ELSEIF DEF LINUX      /* must come before BERKELEY */

deftype
    dev_t   = short,
    ino_t   = long,
    mode_t  = short,
    nlink_t = short,
    time_t  = -long,
    uid_t   = short,
    gid_t   = uid_t;

#_ELSEIF DEF AIX            /* must come before BERKELEY */

deftype
    dev_t   = long,
    ino_t   = long,
    mode_t  = long,
    nlink_t = -short,
    time_t  = -long,
    uid_t   = long,
    gid_t   = uid_t;

#_ELSEIF DEF BERKELEY

deftype
    nlink_t = short,
    time_t  = int;

  #_IF DEF OSF1

    deftype
        dev_t   = int,
        ino_t   = int,
        mode_t  = int,
        uid_t   = int,
        gid_t   = int;

  #_ELSE

    #_IF DEF HPUX
    deftype dev_t = -long;
    #_ELSE
    deftype dev_t = -short;
    #_ENDIF

    deftype
        ino_t   = long,
        mode_t  = short,
        uid_t   = short,
        gid_t   = short;

  #_ENDIF

#_ELSE      ;;; SVR3

deftype
    dev_t   = short,
    ino_t   = short,
    time_t  = int;

#_ENDIF



;;; --- FILE STAT BUFFER -------------------------------------------------

/* From <sys/stat.h> */

#_IF DEFV SYSTEM_V >= 4.0

struct STATB
  { dev_t   ST_DEV;
    long    ST_PAD1[3]; /* reserved for network id */
    ino_t   ST_INO;
    mode_t  ST_MODE;
    nlink_t ST_NLINK;
    uid_t   ST_UID;
    gid_t   ST_GID;
    dev_t   ST_RDEV;
    long    ST_PAD2[2];
    off_t   ST_SIZE;
    long    ST_PAD3;    /* future off_t expansion */
    time_t  ST_ATIME;
    long    ST_ATIME_X;
    time_t  ST_MTIME;
    long    ST_MTIME_X;
    time_t  ST_CTIME;
    long    ST_CTIME_X;
    long    ST_BLKSIZE;
    blkcnt_t ST_BLOCKS;
    byte    ST_FSTYPE[16];
    long    ST_PAD4[8]; /* expansion area */
  };

#_ELSEIF DEF ALPHA_LINUX    /* must come before BERKELEY */

struct STATB
  { dev_t   ST_DEV;
    ino_t   ST_INO;
    mode_t  ST_MODE;
    nlink_t ST_NLINK;
    uid_t   ST_UID;
    gid_t   ST_GID;
    dev_t   ST_RDEV;
    off_t   ST_SIZE;
    time_t  ST_ATIME;
    time_t  ST_MTIME;
    time_t  ST_CTIME;
    int     ST_BLKSIZE;
    -int    ST_BLOCKS;
    int     ST_FLAGS;
    int     ST_GEN;
  };

#_ELSEIF DEF LINUX      /* must come before BERKELEY */

struct STATB
  { dev_t   ST_DEV;
#_IF DEF ARM_LINUX or (DEF X86_LINUX and WORD_BITS = 32)
    int     ST_PAD1;
#_ENDIF
    ino_t   ST_INO;
#_IF DEF ARM_LINUX or (DEF X86_LINUX and WORD_BITS = 32)
    mode_t  ST_MODE;
    nlink_t ST_NLINK;
#_ELSEIF DEF X86_LINUX
    nlink_t ST_NLINK;
    mode_t  ST_MODE;
#_ELSE
    need to define !
#_ENDIF
    uid_t   ST_UID;
    gid_t   ST_GID;
#_IF DEF X86_LINUX and WORD_BITS = 64
    int     ST_PAD2;
#_ENDIF
    dev_t   ST_RDEV;
#_IF DEF ARM_LINUX or (DEF X86_LINUX and WORD_BITS = 32)
    int     ST_PAD3;
#_ENDIF
    off_t   ST_SIZE;
    long    ST_BLKSIZE;
    blkcnt_t ST_BLOCKS;
    time_t  ST_ATIME;
    long    ST_ATIME_X;
    time_t  ST_MTIME;
    long    ST_MTIME_X;
    time_t  ST_CTIME;
    long    ST_CTIME_X;
    long    ST_PAD4[3]; /* expansion area */
  };

#_ELSEIF DEF BERKELEY and not(DEF IRIX)

struct STATB
  { dev_t   ST_DEV;
    ino_t   ST_INO;
    mode_t  ST_MODE;
    nlink_t ST_NLINK;
  #_IF DEF AIX
    short   ST_FLAG;
  #_ENDIF
    uid_t   ST_UID;
    gid_t   ST_GID;
    dev_t   ST_RDEV;
    off_t   ST_SIZE;
    time_t  ST_ATIME;
    int     ST_SPARE1;
    time_t  ST_MTIME;
    int     ST_SPARE2;
    time_t  ST_CTIME;
    int     ST_SPARE3;
  #_IF DEF AIX
    -long   ST_BLKSIZE;
    blkcnt_t ST_BLOCKS;
  #_ELSE
    int     ST_BLKSIZE;
    int     ST_BLOCKS;
  #_ENDIF

  #_IF DEF HPUX
    31      ST_PAD;
    1       ST_REMOTE;
    dev_t   ST_NETDEV;
    ino_t   ST_NETINO;
    long    ST_SPARE4[9];
  #_ELSEIF DEF OSF1
    int     ST_FLAGS;       ;;; user defined flags for file
    int     ST_GEN;         ;;; file generation number
  #_ELSEIF DEF AIX
    int     ST_VFSTYPE;
    long    ST_VFS;
    long    ST_TYPE;
    long    ST_GEN;
    long    ST_RESERVED[9];
  #_ELSE
    long    ST_SPARE4[2];
  #_ENDIF
  };

#_ELSE      ;;; SVR3 or IRIX

struct STATB
  {
#_IF DEF IRIX
    ino_t   ST_INO;
    dev_t   ST_DEV;
#_ELSE
    dev_t   ST_DEV;
    ino_t   ST_INO;
#_ENDIF
    short   ST_MODE,
            ST_NLINK,
            ST_UID,
            ST_GID,
            ST_RDEV;
    int     ST_SIZE;
    time_t  ST_ATIME,
            ST_MTIME,
            ST_CTIME;
  };

#_ENDIF

lconstant macro (
    _STM_IFMT   = _8:170000,    ;;; mask for file type
    _STM_IFREG  = _8:100000,
    _STM_IFDIR  = _8:040000,
    _STM_IFIFO  = _8:010000,
    _STM_IFLNK  = _8:120000,
    );

#_IF DEF LINUX_ELF or DEF NCR or DEF DGUX or DEF SOLARIS_X86
;;; In these systems -- all PC Unix variants -- calls to the *stat()
;;; functions are replaced by the C compiler with calls to *xstat()
;;; alternatives. In some versions of Linux, the stat() functions
;;; themselves don't exist at all; typically they do exist but implement
;;; an older (SVR3?) semantics. In either case, pop has to indirect
;;; through these wrapper functions to get to the true, inlined
;;; definitions.
lconstant macro (
    stat = "pop_stat",
    lstat = "pop_lstat",
    fstat = "pop_fstat",
);
#_ENDIF

    ;;; Setting file times
struct UTIMBUF
  { time_t  UTM_ACTIME,     ;;; access time
            UTM_MODTIME;    ;;; modification time
  };


;;; --- DIRECTORY ENTRY --------------------------------------------------

#_IF DEFV SYSTEM_V >= 4.0 or DEF LINUX

struct DIRECT
  {
#_IF DEF LINUX
    -long   DIR_INO;        /* "inode number" of entry */
#_ELSE
    ino_t   DIR_INO;        /* "inode number" of entry */
#_ENDIF
    off_t   DIR_OFF;        /* offset of disk directory entry */
    short   DIR_RECLEN;     /* length of this record */
#_IF DEFV LINUX >= 2.0
    byte    DIR_TYPE;
#_ENDIF
    byte    DIR_NAME[];     /* name of file */
  };


#_ELSEIF DEF BERKELEY

struct DIRECT
  {
  #_IF DEFV SUNOS >= 4.0 or DEF AIX
    long    DIR_OFF;
  #_ENDIF
    ino_t   DIR_INO;
    short   DIR_RECLEN,
            DIR_NAMLEN;
    byte    DIR_NAME[];
  };

#_ELSE      ;;; SVR3

struct DIRECT
  { ino_t   DIR_INO;
    byte    DIR_NAME[14],
            DIR_END_BYTE[0];
  };

#_ENDIF


;;; --- IOCTLS -----------------------------------------------------------
;;; see also unix_tty.ph

#_IF DEF ATT386
/* AT&T System V/386 doesn't use IOR/IOW at all. So all following functions
   just ignore the read/write flags and the structure size flags, and do
   the same thing.
*/

define lconstant _IO(x,y);
    lvars x, y;
    _int((x<<8) || y)
enddefine;

define lconstant _IOR(x,y,s);
    lvars x, y, s;
    _int((x<<8) || y)
enddefine;

define lconstant _IOW(x,y,s);
    lvars x, y, s;
    _int((x<<8) || y)
enddefine;

#_ELSE
lconstant macro (
    IOC_OUT     = 16:40000000,
    IOC_IN      = 16:80000000,
    );

  #_IF DEF HPUX
lconstant macro IOC_VOID = 16:80000000; ;;; apparently same as IOC_IN !?
  #_ELSEIF DEF LINUX
lconstant macro IOC_VOID = 0;
  #_ELSE
lconstant macro IOC_VOID = 16:20000000;
  #_ENDIF


define lconstant _IO(x,y);
    lvars x, y;
    _int(IOC_VOID || (x<<8) || y)
enddefine;

define lconstant _IOR(x,y,s);
    lvars x, y, s;
    _int(IOC_OUT || (s<<16) || (x<<8) || y)
enddefine;

define lconstant _IOW(x,y,s);
    lvars x, y, s;
    _int(IOC_IN || (s<<16) || (x<<8) || y)
enddefine;

lconstant macro (
    _FIOCLEX    = _IO(`f`, 1),
    _FIONREAD   = _IOR(`f`, 127, SIZEOF(int)),
    _FIOASYNC   = _IOW(`f`, 125, 4),
    _FIOSETOWN  = _IOW(`f`, 124, 4),
    );

#_ENDIF /* AT&T System V */


;;;--- FILE CONTROL -----------------------------------------------------

lconstant macro (
    _F_DUPFD    = _0,
    _F_GETFD    = _1,
    _F_SETFD    = _2,
    _F_GETFL    = _3,
    _F_SETFL    = _4,
    );

#_IF DEF ATT386
lconstant macro (
    _F_GETLK    = _5,
    _F_SETLK    = _6,
    _F_SETLKW   = _7,
    );
#_ELSEIF DEF ALPHA_LINUX
lconstant macro (
        _F_SETOWN       = _5,
        _F_GETOWN       = _6,
        _F_GETLK        = _7,
        _F_SETLK        = _8,
        _F_SETLKW       = _9,
        );
#_ELSEIF DEF LINUX
lconstant macro (
    _F_GETLK    = _5,
    _F_SETLK    = _6,
    _F_SETLKW   = _7,
    _F_SETOWN   = _8,
    _F_GETOWN   = _9,
    );
#_ELSE
lconstant macro (
    _F_GETOWN   = _5,
    _F_SETOWN   = _6,
    );
#_ENDIF

lconstant macro (
    O_RDONLY    = 0,
    O_WRONLY    = 1,
    O_RDWR      = 2,
    O_ACCESS    = 3,        ;;; mask for read/write
    );

#_IF DEF LINUX
lconstant macro (
    O_NDELAY    = 8:4000,
    O_APPEND    = 8:2000,
    );
#_ELSE
lconstant macro (
    O_NDELAY    = 16:04,
    O_APPEND    = 16:08,
    );

#_ENDIF

#_IF DEF SYSTEM_V or DEF HPUX or DEF IRIX or DEF AIX

lconstant macro (
    O_CREAT     = 16:100,
    O_TRUNC     = 16:200,
    O_EXCL      = 16:400,
);

#_ELSEIF DEF LINUX

lconstant macro (
    O_CREAT     = 8:0100,
    O_TRUNC     = 8:1000,
    O_EXCL      = 8:0200,
);

#_ELSE

lconstant macro (
    O_CREAT     = 16:200,
    O_TRUNC     = 16:400,
    O_EXCL      = 16:800,
);

#_ENDIF


;;; --- TIMES/TIMERS -------------------------------------------------------

struct TMS
  { long    TMS_UTIME,
        TMS_STIME,
        TMS_CUTIME,
        TMS_CSTIME;
  };

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0 or DEF ATT386

    ;;; structures for gettimeofday(2)

struct TIMEZONE
  { int TZN_MINUTESWEST, TZN_DSTTIME; };

struct ITIMERVAL
  { struct TIMEVAL ITIMR_INTERVAL, ITIMR_VALUE; };

lconstant macro (
    _ITIMER_REAL    =   _0,
    _ITIMER_VIRTUAL =   _1,
    _ITIMER_PROF    =   _2,
    );


#_ELSE

struct TIMEB
  { long    TIM_TIME;
    short   TIM_MILLITM,
            TIM_TIMEZONE,
            TIM_DSTFLAG;
  };

#_ENDIF


;;; --- VIRTUAL MEMORY --------------------------------------------------

#_IF DEF BSD_MMAP

lconstant macro (
    _MAP_SHARED     = _1,
    _MAP_PRIVATE    = _2,
    );

  #_IF DEF OSF1 or DEF AIX or DEF ALPHA_LINUX
  lconstant macro _MAP_FIXED = _16:100;
  #_ELSE
  lconstant macro _MAP_FIXED = _16:10;
  #_ENDIF

#_ENDIF


;;; -- MISCELLANEOUS ------------------------------------------------------

#_IF true /* DEF HPUX or DEF IRIX or DEF SYSTEM_V */

;;; strings returned by tgetstr() have terminfo-style escapes
lconstant macro USE_TERMINFO = true;

#_ENDIF

;;; argument to _extern waitpid
#_IF DEFV SYSTEM_V >= 4.0

lconstant macro (
    WEXITED     = 2:1e0,
    WTRAPPED    = 2:1e1,
    WUNTRACED   = 2:1e2,
    WCONTINUED  = 2:1e3,
    WNOHANG     = 2:1e6,
    WNOWAIT     = 2:1e7,
    );

#_ELSE

lconstant macro WNOHANG = 2:1e0;

#_ENDIF


;;; --- POP DEVICE UNIT STRUCTURES ----------------------------------------

struct DEVUNIT_N            ;;; (a string)
  { word    V_LENGTH;
    full    KEY;
>-> int     UNT_NDEVS,          ;;; number of devs pointing to this unit
            UNT_FLAGS;          ;;; flags
    dev_t   UNT_DEV_NUM;        ;;; Unix device number
    ino_t   UNT_INO_NUM;        ;;; Unix inode number
    mode_t  UNT_MODE;           ;;; Unix mode bits
  };

struct DEVUNIT_P            ;;; (a vector)
  { word    V_LENGTH;
    full    KEY,
>->         UNT_IO_TRAP[3],     ;;; trap procedure or false for RD/WR/EX_SET
            ;;; rest for terminals only
            UNT_CURR_TTPARAMS,  ;;; params string tty params are currently set for
            UNT_INIT_TTPARAMS,  ;;; params string for initial state of tty
            UNT_TTPARM_STATUS;  ;;; parameter status value
  };

    ;;; Flags in UNT_FLAGS
lconstant macro (
    M_UNT_IGNORE_WRITE_ERR  = 2:1e0,    ;;; ignore write error on device
);


;;; --- OTHER MACROS -------------------------------------------------------

;;; Macros for dealing with type "dev_t" being two words

define :inline lconstant DEV_T_TO_VARS(_ptr, FIELD=item, _dev, _dev2);
#_IF ##(w)[_1|dev_t] = _2  \n
    _ptr@FIELD!(w)[_0] -> _dev; _ptr@FIELD!(w)[_1] -> _dev2
#_ELSE
    _ptr!FIELD -> _dev
#_ENDIF
enddefine;

define :inline lconstant VARS_TO_DEV_T(_dev, _dev2, _ptr, FIELD=item);
#_IF ##(w)[_1|dev_t] = _2  \n
    _dev -> _ptr@FIELD!(w)[_0]; _dev2 -> _ptr@FIELD!(w)[_1]
#_ELSE
    _dev -> _ptr!FIELD
#_ENDIF
enddefine;

define :inline lconstant DEV_T_==_VARS(_ptr, FIELD=item, _dev, _dev2);
#_IF ##(w)[_1|dev_t] = _2  \n
    (_ptr@FIELD!(w)[_0] == _dev and _ptr@FIELD!(w)[_1] == _dev2)
#_ELSE
    _ptr!FIELD == _dev
#_ENDIF
enddefine;


;;; --- IDENTIFIER DECLARATIONS --------------------------------------------

constant
        procedure (Sys_cons_device, Sys$-Encode_sys)
    ;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  9 1999
        Replaced test ALPHA_LINUX or (X86_LINUX and LIN*UX_GLI*BC) with
        DEFV LINUX >= 2.0
--- Robert Duncan, Mar 24 1999
        More changes for X86 Linux with glibc
--- John Gibson, Mar 20 1999
        Changes to fix x86 Linux "dev_t" type problem
--- Robert Duncan, Mar  8 1999
        Added typedefs for X86 Linux with glibc
--- Robert Duncan, Aug 25 1998
        Likewise for Solaris x86.
--- Robert Duncan, Jun 05 1998
        Added DG/UX as another system without a real stat()
--- John Gibson, Mar 26 1998
        Added AIX stuff
--- John Gibson, Mar  8 1997
        Added struct UTIMBUF
--- Robert Duncan, Aug  9 1996
        Included NCR in previous Linux hack
--- Robert Duncan, Apr 25 1996
        Added Linux ELF hack to overcome GCC stupidity
--- Integral Solutions Ltd, May 12 1995 (Julian Clinton)
        Stopped Linux using TERMINFO
--- John Gibson, Mar  2 1995
        OSF1 changes
--- John Gibson, Feb 27 1995
        Added UNT_FLAGS flags field and M_UNT_IGNORE_WRITE_ERR flag
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Changes for Linux.
--- John Gibson, Jun 14 1994
        Moved error numbers to lib/include/unix_errno.ph
--- John Gibson, Jun 13 1994
        Moved Unix select stuff to unix_select.ph
--- John Gibson, May 23 1994
        Added _ERRNO macro
--- John Gibson, May  5 1994
        Added/corrected some error numbers
--- Robert John Duncan, Mar 23 1994
        Fixed O_CREAT etc. for IRIX
--- John Gibson, Jan 29 1994
        Added O_CREAT etc
--- Robert John Duncan, Jan 26 1994
        Removed Dynix code (now defunct)
--- John Gibson, Sep 30 1993
        Added new SVR4 values for WNOHANG etc.
--- Robert John Duncan, Nov 26 1992
        Enabled USE_TERMINFO for SG IRIX
--- Robert John Duncan, Jul 22 1992
        Added definitions for SVR4
--- Robert John Duncan, Jun  1 1992
        Moved in definition of USE_TERMINFO from syscomp/sysdefs.p
--- Robert John Duncan, Jun 24 1991
        Reorganised to allow definitions for SG IRIX (half BSD, half SYSV)
--- John Gibson, Feb 23 1991
        Added UNT_MODE and _STM_IFIFO
--- John Gibson, Jan  3 1991
        Moved SIZEOF macro to declare.ph. Removed R*EAD_EI etc.
--- John Gibson, Dec 12 1990
        Added DEVUNIT structs
--- John Gibson, Oct 26 1990
        Added some more Unix error numbers
--- John Gibson, Aug 24 1990
        Better definition of ITIMERVAL from systimer.p
--- Robert John Duncan, Jul 19 1990
        Replaced SUN_RELEASE with SUNOS (again)
--- Robert John Duncan, Jul 18 1990
        Set MAX_DEVS for Ultrix.
--- Roger Evans, Jul 11 1990
        Fixed bug in FD_SET_WORD
--- Jonathan Meyer, Jul 10 1990
        Added DEF ATT386 directives to define IOR/IOW/IO correctly for
        System V, and also pick up the correct TIME structs
        Defined _F_GETLK, _F_SETLK and _F_SETLKW for ATT386, and also
        set MAX_DEVS correctly
--- Roger Evans, Jun 25 1990
        Added ITIMERVAL for BERKELEY systems
--- Roger Evans, May 14 1990
        Added macros to support FD_SET manipulation
 */
