/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/lib/vedxvedmenubar.p
 > Purpose:         Comprehensive XVed menu structure
 > Author:          Jonathan Meyer, Jul 26 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include xved_constants;

vars vedxvedmenubar =

[
    ['File'
        {'Open...' 'openfile'}
        {'Save' 'w1'}
        {'Save As...' 'savefileas'}
        {'Write To...' 'writefileto'}
        {'Insert...' 'insertfile'}
        space
        {'Quit' 'q'}
    ]
    ['Edit'
        {'Cut' 'seln cut'}
        {'Copy' 'seln copy'}
        {'Paste' 'seln paste'}
        {'Undo' 'seln undo'}
        space
        {'Search...' 'searchfor'}
        {'Search and Replace...' 'replace'}
        {'Repeat last Search' 're_search'}
        {'Repeat last Backsearch' 're_backsearch'}
    ]
    ['View'
        {'Start of File' vedtopfile}
        {'End of File' vedendfile}
        {'Start of Range' vedmarkfind}
        {'End of Range' vedendrange}
        {'Start of Procedure' {vedmarkpush ved_mcp vedmarkfind vedmarkpop}}
        {'End of Procedure' {vedmarkpush ved_mcp vedendrange vedmarkpop}}
    ]
    ['Compile'
        {'Current Line' vedloadline}
        {'Current Procedure' 'lcp'}
        {'Current File' 'l1'}
        {'Range' 'lmr'}
        {'Selection' 'seln compile'}
        space
        {'Library...' 'loadlibrary'}
        {'File...' 'compilefile'}
    ]
    [^XVMB_HELP_LABEL
        {'For Current Item' vedgetsysfile}
        {'For Selected Item' 'seln help'}
        {'Next Cross Reference' vednexthelp}
        {'Previous Cross Reference' vedprevioushelp}
        space
        {'Other Help...' 'gethelp'}
        {'About Poplog...' 'aboutpoplog'}
    ]
];

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 18 1995
        Changes to menu item labels to be more compatible with those on the
        control panel menus.
--- John Gibson, Jun 18 1993
        Data now contained in vedxvedmenubar itself; moved to lib dir.
--- John Gibson, Jun  7 1993
        Made it assign directly to $-xved$-xveddefaultmenubar if
        not(pop_runtime)
--- John Gibson, Jul 24 1992
        Removed assignment true -> xved_value("defaultWindow", "menubarOn");
        (widget default is for it to be on)
--- Jon Meyer, Jul 17 1992
        Modified menu bar structure to reduce number of menu buttons.
--- John Gibson, Sep 22 1991
        Removed assignment of true to menubarOn (loading this file should
        not affect whether the menubar is on or not).
 */
