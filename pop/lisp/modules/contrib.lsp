#| --- Copyright University of Sussex 1992. All rights reserved. ----------
 | File:            C.all/lisp/modules/contrib.lsp
 | Purpose:         Add $popcontrib/lisp/modules to *MODULE-DIRECTORY-LIST*
 | Author:          John Williams, Sep 28 1990 (see revisions)
 | Documentation:   HELP * CONTRIB
 | Related Files:
 |#

(cl:provide :contrib)

(cl:in-package :poplog)

(pushnew "$popcontrib/lisp/modules/" *module-directory-list* :test #'equal)


#| --- Revision History ---------------------------------------------------
--- John Williams, Jun 30 1992
        Now does (provide :contrib) and (in-package :lisp)
 |#
