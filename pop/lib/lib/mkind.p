/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/lib/lib/mkind.p
 > Purpose:         Pop11 "script" for building index of procedure definitions
 > Author:          John Williams, Sep 16 1992 (see revisions)
                        (original by Chris Slymon, June 1983)
 > Documentation:   HELP * POPINDEX, HELP * SOURCEFILE
 > Related Files:   C.unix/com/mkind, C.vms/com/mkind.com
                        LIB * BUILDINDEX, LIB * POPINDEX
 */

#_TERMIN_IF not(pop_runtime)

section;

/* Set the lvar Preserve_env false if index entries should NOT include
    unexpanded environment variables (e.g. $usepop) in filenames
*/

lvars Preserve_env = true;


/* Set up list of directories to index */

global vars popindexlist;

uses popxlib;       ;;; Ensure X directories are added to the searchlists

define lconstant Convert_searchlist(list, patt);
    lvars item list patt;
    for item in flatten_searchlist(list, false) do
        item dir_>< patt
    endfor
enddefine;


unless islist(popindexlist) do

    [%

        /* Main source, library, and include directories */

        Convert_searchlist(vedsrclist, '*.p'),
        Convert_searchlist(vedsrclist, '*.ph'),
        Convert_searchlist(popuseslist, '*.p'),
        Convert_searchlist(popuseslist, '*.ph'),
        Convert_searchlist('$popsrc' :: popincludelist, '*.ph'),

        /* Other directories below $usepop/pop/lib */

        '$usepop/pop/lib/flavours/*.p',
        '$usepop/pop/lib/lr_parser/.../*.p',
        '$usepop/pop/lib/objectclass/.../*.p',

        ;;; should these be included?
        '$usepop/pop/lib/obsolete/.../*.p',
        '$usepop/pop/lib/obsolete/.../*.ph',
        ' $usepop/pop/lib/turtle/*.p',

        /* Subsystem library directories */

        '$usepop/pop/lisp/flib/*.p',
        '$usepop/pop/plog/auto/*.p',
        '$usepop/pop/plog/lib/*.p',

        /* files in proto directories ??? */
        '$usepop/pop/lib/proto/*.p',
        '$usepop/pop/lib/proto/go/*/*.p',
        /* X directories not covered by popxlib */

        '$usepop/pop/x/pop/lib/X11/*.p',
        '$usepop/pop/x/pop/lib/Xaw/*.p',
        '$usepop/pop/x/pop/lib/Xm/*.p',
        '$usepop/pop/x/pop/lib/Xol/*.p',
        '$usepop/pop/x/pop/lib/Xpw/*.p',
        '$usepop/pop/x/pop/lib/xlib/*.p',
        '$usepop/pop/x/ui/lib/*.p',

        /* Xved sources */

        '$usepop/pop/x/ved/.../*.p',
        '$usepop/pop/x/ved/.../*.ph',

        /* Package directories */


        /* leave these out? */
;;;     '$usepop/pop/packages/contrib/.../*.p',
;;;     '$usepop/pop/packages/contrib/.../*.ph',

        '$usepop/pop/packages/bhamlib/.../*.p',
        '$usepop/pop/packages/bhamlib/.../*.ph',
        '$usepop/pop/packages/brait/.../*.p',
        '$usepop/pop/packages/brait/.../*.ph',
        '$usepop/pop/packages/lib/.../*.p',
        '$usepop/pop/packages/lib/.../*.ph',
        '$usepop/pop/packages/lockfile/.../*.p',
        '$usepop/pop/packages/lockfile/.../*.ph',
        '$usepop/pop/packages/master/.../*.p',
        '$usepop/pop/packages/master/.../*.ph',
        '$usepop/pop/packages/neural/.../*.p',
        '$usepop/pop/packages/neural/.../*.ph',
        '$usepop/pop/packages/newkit/.../*.p',
        '$usepop/pop/packages/newkit/.../*.ph',
        '$usepop/pop/packages/popvision/.../*.p',
        '$usepop/pop/packages/popvision/.../*.ph',
        '$usepop/pop/packages/rclib/.../*.p',
        '$usepop/pop/packages/rclib/.../*.ph',
        '$usepop/pop/packages/rcmenu/.../*.p',
        '$usepop/pop/packages/rcmenu/.../*.ph',
        '$usepop/pop/packages/teaching/.../*.p',
        '$usepop/pop/packages/teaching/.../*.ph',
        '$usepop/pop/packages/ved_gn/.../*.p',
        '$usepop/pop/packages/ved_gn/.../*.ph',
        '$usepop/pop/packages/ved_latex/.../*.p',
        '$usepop/pop/packages/ved_latex/.../*.ph',
        '$usepop/pop/packages/vedmail/.../*.p',
        '$usepop/pop/packages/vedmail/.../*.ph',
        '$usepop/pop/packages/vedutils/.../*.p',
        '$usepop/pop/packages/vedutils/.../*.ph',


        /* Main local directories */
        ;;; auto and lib in popautolist and popuseslist

    %] -> popindexlist

endunless;


/* Now create the index */

#_IF sys_fname_nam(poparg1) = 'mkind'

    max(popmemlim, 2000000) ->> popmemlim -> popminmemlim;
    1 -> pop_file_versions;

    uses buildindex;        ;;; Defines buildindex
    uses popindex;          ;;; Defines popindex_filename

    sysgarbage();
    sys_lock_heap();

    popindexlist ==>
    popindex_filename ==>

    buildindex(popindexlist, popindex_filename, Preserve_env);

#_ENDIF



endsection;

nil -> proglist;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 16 2005
        Extended for new version with package directories
--- Robert John Duncan, Dec  6 1995
        Added various missing directories and removed plog/src (already
        included in vedsrclist)
--- John Gibson, Nov  9 1995
        Removed pw*m dir
--- John Williams, Apr 30 1993
        Now does sys_lock_heap explicitly, rather than inside the call to
        buildindex.
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF at top of file so does nothing if not(pop_runtime)
        (e.g. when compiled with popc)
 */
