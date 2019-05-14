/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/popincludelist.p
 > Purpose:         Makes -popincludelist- autoloadable
 > Author:          Aaron Sloman, Jun  3 1990 (see revisions)
 > Documentation:   HELP * INCLUDE
 > Related Files:   LIB * INCLUDE, LIB * USES
 */
compile_mode:pop11 +strict;

section;

    /* Declared as incremental in popc_declare.ph */
vars popincludelist = [%
    '$poplib/include/'          dir_>< '',
    '$poplocal/local/include/'  dir_>< '',
    '$usepop/pop/lib/include/'  dir_>< '',
%];

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 26 1992
        Moved in initialisation from include.p
 */
