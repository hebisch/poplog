/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/lib/vedxvedmenu.p
 > Purpose:         Comprehensive XVed menu structure
 > Author:          Jonathan Meyer, Jul 26 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include xved_constants;

vars vedxvedmenu =

[
    ['File'
        fixable
        {'New' 'newfile'}
        {'Open...' 'openfile'}
        space
        {'Save Current' 'w1'}
        {'Save All' 'w'}
        {'Save As...' 'savefileas'}
        {'Write To...' 'writefileto'}
        space
        {'Insert File...' 'insertfile'}
        {'Quit Current' 'q'}
        {'Empty Current' 'clear'}
    ]
    ['View'
        fixable
        {'Start of File' vedtopfile}
        {'End of File' vedendfile}
        {'Start of Range' vedmarkfind}
        {'End of Range' vedendrange}
        {'Start of Procedure' {vedmarkpush ved_mcp vedmarkfind vedmarkpop}}
        {'End of Procedure' {vedmarkpush ved_mcp vedendrange vedmarkpop}}
        space
        {'Table of Contents' {vedtopfile 'g'}}
        {'Indexed Section' 'g'}
        space
        {'Save Position' vedpositionpush}
        {'Restore Position' vedpositionpop}
        {'Swap With Saved' vedexchangeposition}
    ]
    [^XVMB_GOTOBUFF_LABEL fixable] ;;; individual windows will be added here
    ['Edit'
        fixable
        ['Select'
            fixable
            {'Range' 'select range'}
            {'Procedure' 'select procedure'}
            space
            {'Word' 'select word'}
            {'Line' 'select line'}
            {'Sentence' 'select sentence'}
            {'Paragraph' 'select paragraph'}
            {'Window' 'select window'}
            {'To Top' 'select tostartfile'}
            {'To End' 'select toendfile'}
            space
            {'Select All' 'select file'}
        ]
        space
        {'Cut' 'seln cut'}
        {'Copy' 'seln copy'}
        {'Paste' 'seln paste'}
        {'Undo' 'seln undo'}
        space
        {'Get Help' 'seln help'}
        {'Compile' 'seln compile'}
    ]
    ['Range'
        fixable
        ['Mark'
            fixable
            {'Start Here' 'mark start'}
            {'End Here' 'mark end'}
            space
            {'Line' 'mark line'}
            {'Paragraph' 'mark paragraph'}
            {'Window' 'mark window'}
            {'Procedure' 'mcp'}
            {'To Top' 'mbf'}
            {'To End' 'mef'}
            {'Mark All' 'mbe'}
            {'Unmark' 'crm'}
        ]
        ['Align'
            fixable
            {'Justify Left' 'jp'}
            {'Justify Both' 'jjp'}
            {'Block Center' 'bc'}
            space
            {'Align Center'    'ac'}
            {'Align Left'      'al'}
            {'Align Right'     'ar'}
            space
            {'Shift Left 1'  'bl'}
            {'Shift Right 1'  'br'}
            space
            {'Gobble Whitespace' 'gobble'}
            {'Sort' 'smr'}
        ]
        space
        {'Delete' 'd'}
        {'Undelete' 'yank'}
        {'Copy' 't'}
        {'Move' 'm'}
        {'Compile' 'lmr'}
        space
        {'abc -> ABC' 'ucr'}
        {'ABC -> abc' 'lcr'}
        space
        {'Copy In' 'ti'}
        {'Move In' 'mi'}
        {'Write to...' 'writemr'}
    ]
    ['Compile'
        fixable
        {'Current Line' vedloadline}
        {'Current Procedure' 'lcp'}
        {'Current File' 'l1'}
        {'All Files' 'l'}
        {'Range' 'lmr'}
        {'Selection' 'seln compile'}
        space
        {'Library...' 'loadlibrary'}
        {'File...' 'compilefile'}
    ]
    ['Search'
        {'Search for...' 'searchfor'}
        {'Search and Replace...' 'replace'}
        {'Repeat last Search' 're_search'}
        {'Repeat last Backsearch' 're_backsearch'}
    ]
    ['Properties'
        {'Files...' 'properties Files'}
        {'Store Management...' 'properties Store Management'}
        space
        {'Ved Buffer...' 'properties Ved Buffer'}
        {'XVed...' 'properties XVed'}
        {'XVed Window...' 'properties XVed Window'}
    ]
    ['Extras'
        fixable
        {'Refresh' vedrefresh}
        space
        {'Insert Ruler' 'ruler'}
        {'Make Heading' 'heading'}
        {'Indexify'   'indexify'}
        space
        {'word -> Word'  'capword'}
        {'WORD -> word'  'lcw'}
        {'word -> WORD'  'ucw'}
        space
        {'Email: Send Range' 'sendmr'}
        {'Email: Send File' 'send'}
        {'Read Email' 'mail'}
    ]
    [^XVMB_HELP_LABEL
        fixable
        ['View Help on'
            {'Online Documentation' 'help documentation'}
            {'Marked Ranges' 'help mark'}
            {'Search' 'help vedsearch'}
            {'ENTER Commands' 'help vedcomms'}
            {'XVed' 'teach xved'}
            {'Key Bindings' ved_hkeys}
            {'Mouse Bindings' 'help xvedmouse'}
        ]
        {'For Current Item' vedgetsysfile}
        {'Next Cross Reference' vednexthelp}
        {'Previous Cross Reference' vedprevioushelp}
        space
        {'Other help...' 'gethelp'}
        {'About Poplog...' 'aboutpoplog'}
    ]
];

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 18 1993
        Data now in vedxvedmenu itself.
--- John Gibson, Jun  7 1993
        Made it assign directly to $-xved$-xvedpopupmenu if not(pop_runtime)
--- Jonathan Meyer, Sep 16 1991 Changed "pushpin" to "fixable"
--- Jonathan Meyer, Sep 12 1991 Added Read Email
--- Jonathan Meyer, Sep  3 1991 Changed to use new menu specification
--- Jonathan Meyer, Sep  2 1991 Added 'aboutpoplog'
--- Jonathan Meyer, Aug  2 1991
        Changed all enter commands to use strings, which get shown on the
        command line. Introduced the vector type for compound commands
--- Adrian Howard, Aug  2 1991 : Added -proc- to stop autoloading
--- Jonathan Meyer, Jul 26 1991
        Moved out from src xvedmenubar.p. Made a few options less nested.
        Fixed 'Email' options
--- Adrian Howard, Jul 15 1991 : Changed "item" in Extras menu to "word" since
        default font choice made "item -> Item" illegable
*/
