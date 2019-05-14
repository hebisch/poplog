/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/include/sysdefs.ph
 > Purpose:         Defining system attributes
 > Author:          Roger Evans, Sep 21 1988 (see revisions)
 > Documentation:   HELP *SYSDEFS
 > Related Files:   LIB POPHOST
 */

/*
    This library defines iconstants commonly used in #_IF DEF
    statements, in particular all the major distinctions needed by Poplog
    system sources, and hence (probably) most users.
*/

    ;;; Don't reload if already loaded, or running with POPC in either
    ;;; POPC_COMPILING mode or normal mode.

    ;;; (POPC_SYSDEFS_LOADED is not the same as POPC_COMPILING; the latter is
    ;;; only defined in the file which is actually the target of popc
    ;;; compilation, ie will not be defined for normal-compiled syntax,
    ;;; macros, etc. This is a potential problem for cross-compilation ...)

#_TERMIN_IF DEF SYSDEFS_INCLUDED or DEF POPC_SYSDEFS_LOADED

section;

lvars h, t;     ;;; working


/* -- Machine Type ------------------------------------------------------- */

dest(sys_machine_type) -> (h, t);

#_IF h == "vax"
    iconstant VAX = true;
#_ELSEIF h == "sun3" or h == "sun3x"
    iconstant SUN = true;
    iconstant SUN3 = true;
#_ELSEIF h == "sun4"
    iconstant SUN = true;
    iconstant SUN4 = true;
#_ELSEIF h == "sun386"
    iconstant SUN = true;
    iconstant SUN386 = true;
#_ELSEIF h == "symmetry"
    iconstant SYMMETRY = true;
#_ELSEIF h == "hp9000"
    iconstant HP9000 = true;
    #_IF not(null(t)) and hd(t) == 700
        iconstant HP9000_700 = true;
    #_ELSE
        iconstant HP9000_300 = true;
        iconstant BOBCAT = true;
    #_ENDIF
#_ELSEIF h == "decstation"
    iconstant DECSTATION = true;
#_ELSEIF h == "mips"
    iconstant MIPS = true;
#_ELSEIF h == "iris"
    iconstant IRIS = true;
#_ELSEIF h == "drs6000"
    iconstant DRS6000 = true;
#_ELSEIF h == "alpha"
    iconstant ALPHA = true;
#_ELSEIF h == "pc"
    iconstant PC = true;
#_ELSEIF h == "ncr3000"
    iconstant NCR = true;
    iconstant NCR3000 = true;
#_ELSEIF h == "power"
    iconstant POWER = true;
#_ELSEIF h == "aviion"
    iconstant AVIION = true;
#_ENDIF


/* -- Operating System Type ---------------------------------------------- */

dest(sys_os_type) -> (h, t);

#_IF h == "vms"
    iconstant VMS = hd(t);

#_ELSEIF h == "unix"
    iconstant UNIX = true;
    dest(t) -> (h, t);
    #_IF h == "svr4"
        iconstant SYSTEM_V = hd(t);
    #_ELSEIF h == "att"
        iconstant SYSTEM_V = hd(t);
        #_IF DEF PC
            iconstant ATT386 = hd(t);
        #_ENDIF
    #_ELSEIF h == "ncr"
        iconstant SYSTEM_V = 4.0;
    #_ELSEIF h == "dgux"
        iconstant DGUX = hd(t);
        iconstant SYSTEM_V = 4.0;
    #_ELSEIF h == "sunos"
        iconstant SUNOS = hd(t);
        #_IF SUNOS >= 5.0
            iconstant SYSTEM_V = 4.0;
        #_ELSE
            iconstant BERKELEY = 4.2;
        #_ENDIF
        #_IF DEF SUN386
            iconstant COFF = true;
        #_ENDIF
    #_ELSEIF h == "hpux"
        iconstant HPUX = hd(t);
        iconstant BERKELEY = 4.2;
    #_ELSEIF h == "ultrix"
        iconstant ULTRIX = hd(t);
        iconstant BERKELEY = 4.2;
        #_IF DEF DECSTATION
            iconstant COFF = true;
        #_ENDIF
    #_ELSEIF h == "irix"
        iconstant IRIX = hd(t);
        #_IF IRIX >= 5.0
            iconstant SYSTEM_V = 4.0;
        #_ELSE
            iconstant BERKELEY = 4.3;
            iconstant COFF = true;
        #_ENDIF
    #_ELSEIF h == "linux"
        iconstant LINUX = hd(t);
        iconstant BERKELEY = 4.3;
        #_IF lmember("elf", t)
            iconstant LINUX_ELF = true;
        #_ENDIF
    #_ELSEIF h == "osf1"
        iconstant OSF1 = hd(t);
        iconstant BERKELEY = 4.3;
        iconstant COFF = true;
    #_ELSEIF h == "bsd"
        iconstant BERKELEY = hd(t);
        #_IF DEF VAX
            iconstant VAXULTRIX = true;
        #_ENDIF
    #_ELSEIF h == "aix"
        iconstant AIX = hd(t);
        iconstant BERKELEY = 4.3;
        iconstant COFF = true;
    #_ENDIF

    #_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0
        iconstant BSD_SOCKETS = true;
        iconstant BSD_SYMLINKS = true;
    #_ENDIF

    #_IF DEFV SYSTEM_V >= 4.0 or DEFV HPUX >= 9.0 or DEF OSF1 or DEF LINUX_ELF or DEF AIX
        iconstant SHARED_LIBRARIES = true;
    #_ENDIF

    #_IF (lmember("posix", t) ->> t)
        lvars posix = hd(tl(t));    ;;; a vector
        iconstant POSIX1 = subscrv(1,posix);
        #_IF datalength(posix) >= 2 and subscrv(2,posix)
            iconstant POSIX2 = subscrv(2,posix);
        #_ENDIF
        #_IF datalength(posix) >= 3 and subscrv(3,posix)
            iconstant POSIX3 = subscrv(3,posix);
        #_ENDIF
        ;;; etc ...
    #_ENDIF

#_ELSEIF h == "windows"
    iconstant WINDOWS = last(t);
    iconstant WIN32 = true;
    #_IF hd(t) == "nt"
        iconstant WINDOWS_NT = last(t);
    #_ENDIF
#_ENDIF


iconstant SYSDEFS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 11 1998
        Added cases for Data General AViiON (DG/UX)
--- John Gibson, Apr 30 1998
        Added cases for POWER/AIX
--- Robert Duncan, Aug  9 1996
        Added cases for NCR SVR4
--- Robert Duncan, Apr 25 1996
        Extended Linux case for ELF
--- John Gibson, Mar  4 1995
        Added OSF1 case and POSIX stuff
--- Robert John Duncan, Mar  3 1995
        Removed definitions for RIS*COS and DY*NIX
--- Poplog System, Jan 18 1995
        Added cases for Linux and ATT386 (set if PC and os is "att").
--- John Gibson, Nov 19 1994
        Added case for Alpha
--- Robert John Duncan, Sep  5 1994
        Added case for Windows (NT).
--- Robert John Duncan, Jul 11 1994
        Added case for IRIX 5
--- Simon Nichols, Oct 26 1993
        Added SHARED_LIBRARIES
--- Robert John Duncan, Jun  7 1993
        Added cases for SVR4
--- Robert Duncan, May 11 1993
        Subdivided HP9000 into HP9000_300 and HP9000_700
--- John Gibson, Oct 22 1992
        Replaced outer #_IF with #_TERMIN_IF, changed P*OPC to
        POPC_SYSDEFS_LOADED and sysdefs to SYSDEFS_INCLUDED
--- Robert John Duncan, Aug 12 1992
        SunOS 5.0 now classified as System V Release 4
        HP-UX reclassified as a BSD system, to agree with its POPC definition
        Slight reorganisation
--- Robert John Duncan, Aug 20 1991
        Removed tricky assignment to -prog*list- which wouldn't work
        for POPC
--- Robert John Duncan, Jun 21 1991
        Added IRIS & IRIX; deleted cases for sun2 and orion
--- Jonathan Meyer, Apr 18 1991
        Turned into an include file
--- Simon Nichols, Jun  5 1990
        Added constant COFF
--- Simon Nichols, May 25 1990
        Added MIPS and RIS*COS
--- Rob Duncan, Apr 27 1990
        Added DECSTATION & ULTRIX
--- Ian Rogers, Feb  6 1990
        Added VAXULTRIX
--- John Gibson, Oct 23 1989
        Added test for sun3x
--- Rob Duncan, Apr  4 1989
        Revised to use new -sys_machine_type- and the extra information
        from -sys_os_type-
--- Roger Evans, Oct 12 1988
        sectioned and made declarations global, added variable -sysdefs-
 */
