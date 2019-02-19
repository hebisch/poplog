/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/dirs.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;------------------- DIRECTORY DEFINITIONS -------------------------------

#_IF DEF UNIX

lconstant macro (
    POPDISK         =   '$usepop/',
    POPLOCAL        =   '$poplocal/',
    POPLOCALAUTO    =   '$poplocalauto/',
    POPAUTOLIB      =   '$popautolib/',
    POPVEDLIB       =   '$popvedlib/',
    POPSYS          =   '$popsys/',
    POPSAVELIB      =   '$popsavelib/',
    POPLIB          =   '$poplib/',

    POPDATABASE     =   POPDISK <> 'pop/lib/database/',
    POPTURTLE       =   POPDISK <> 'pop/lib/turtle/',

    VEDTERMLIB      =   POPVEDLIB <> 'term/',

    VEDHELPDIR      =   POPDISK <> 'pop/help/',
    VEDTEACHDIR     =   POPDISK <> 'pop/teach/',
    VEDREFDIR       =   POPDISK <> 'pop/ref/',
    VEDDOCDIR       =   POPDISK <> 'pop/doc/',

    VEDLOCHELPDIR   =   POPLOCAL <> 'local/help/',
    VEDLOCTEACHDIR  =   POPLOCAL <> 'local/teach/',
    VEDLOCREFDIR    =   POPLOCAL <> 'local/ref/',
    VEDLOCDOCDIR    =   POPLOCAL <> 'local/doc/',
    );

#_ELSEIF DEF VMS

lconstant macro (
    POPDISK         =   'usepop:',
    POPLOCAL        =   'poplocal:',
    POPLOCALAUTO    =   'poplocalauto:',
    POPAUTOLIB      =   'popautolib:',
    POPVEDLIB       =   'popvedlib:',
    POPSYS          =   'popsys:',
    POPSAVELIB      =   'popsavelib:',
    POPLIB          =   'poplib:',

    POPDATABASE     =   POPDISK <> '[pop.lib.database]',
    POPTURTLE       =   POPDISK <> '[pop.lib.turtle]',

    VEDTERMLIB      =   POPVEDLIB <> '[term]',

    VEDHELPDIR      =   POPDISK <> '[pop.help]',
    VEDTEACHDIR     =   POPDISK <> '[pop.teach]',
    VEDREFDIR       =   POPDISK <> '[pop.ref]',
    VEDDOCDIR       =   POPDISK <> '[pop.doc]',

    VEDLOCHELPDIR   =   POPLOCAL <> '[local.help]',
    VEDLOCTEACHDIR  =   POPLOCAL <> '[local.teach]',
    VEDLOCREFDIR    =   POPLOCAL <> '[local.ref]',
    VEDLOCDOCDIR    =   POPLOCAL <> '[local.doc]',
    );


#_ELSEIF DEF WINDOWS

lconstant macro (
    POPDISK         =   '%usepop%\\',
    POPLOCAL        =   '%poplocal%\\',
    POPLOCALAUTO    =   '%poplocalauto%\\',
    POPAUTOLIB      =   '%popautolib%\\',
    POPVEDLIB       =   '%popvedlib%\\',
    POPSYS          =   '%popsys%\\',
    POPSAVELIB      =   '%popsavelib%\\',
    POPLIB          =   '%poplib%\\',

    POPDATABASE     =   POPDISK <> 'pop\\lib\\database\\',
    POPTURTLE       =   POPDISK <> 'pop\\lib\\turtle\\',

    VEDTERMLIB      =   POPVEDLIB <> 'term\\',

    VEDHELPDIR      =   POPDISK <> 'pop\\help\\',
    VEDTEACHDIR     =   POPDISK <> 'pop\\teach\\',
    VEDREFDIR       =   POPDISK <> 'pop\\ref\\',
    VEDDOCDIR       =   POPDISK <> 'pop\\doc\\',

    VEDLOCHELPDIR   =   POPLOCAL <> 'local\\help\\',
    VEDLOCTEACHDIR  =   POPLOCAL <> 'local\\teach\\',
    VEDLOCREFDIR    =   POPLOCAL <> 'local\\ref\\',
    VEDLOCDOCDIR    =   POPLOCAL <> 'local\\doc\\',
    );

#_ELSE_ERROR
#_ENDIF


#_IF DEF SUN
    lconstant macro POPHOSTLIB = '$popsunlib/';
#_ELSE
    lconstant macro POPHOSTLIB = [];
#_ENDIF


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 11 1994
        Added definitions for Windows
--- Rob Duncan, Oct 25 1989
        Added VEDTERMLIB as directory for VED terminal libraries
--- John Gibson, Aug  1 1989
        Changed Unix dirs so they all end in `/`
--- John Williams, Jun  1 1989
        Added POPHOSTLIB (for machine dependent library, eg $popsunlib)
--- John Gibson, Jan 28 1989
        Made macros lconstants
 */
