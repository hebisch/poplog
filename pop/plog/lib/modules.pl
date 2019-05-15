/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/plog/lib/modules.pl
 >  Purpose:        modules (implemented as POP sections)
 >  Author:         Chris Mellish, ??? (see revisions)
 >  Documentation:  HELP * MODULES
 */

:- format(";;; library(modules) is no longer needed~n").

/*  --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 10 1992
        Politer message.
--- Rob Duncan, Aug  9 1989
    Deleted everything, leaving just the warning message. The module system
    is now part of the standard Prolog image -- see C.all/plog/src/modules.p
--- Simon Nichols, Feb. 20 1987 - changed precedences of module, import,
    export and global to work correctly with new parser.
--- Jonathan Laventhol, Mark 26 1984 - updated so as to avoid getting pop
    variable declaration messages.
 */
