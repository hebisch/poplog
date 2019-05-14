/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/objectclass_help.p
 > Purpose:         Objectclass: setup search lists
 > Author:          Robert John Duncan, Nov 20 1995
 > Documentation:   HELP * OBJECTCLASS
 > Related Files:   C.all/lib/objectclass
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF objectclass_help or DEF POPC_COMPILING

section $-objectclass;

#_IF not(DEF objectclass_dir)
constant objectclass_dir = '$usepop/pop/lib/objectclass/';
#_ENDIF

;;; Extend library searchlists
extend_searchlist(objectclass_dir dir_>< 'lib/', popuseslist) -> popuseslist;
extend_searchlist(objectclass_dir dir_>< 'auto/', popautolist) -> popautolist;
extend_searchlist(objectclass_dir dir_>< 'rt/', popsyslist) -> popsyslist;

;;; Extend Ved searchlists
#_IF DEF vedprocess

uses ved_src;   ;;; for vedsrclist

lconstant
    objclass_teach      = [% objectclass_dir dir_>< 'teach/' % teach],
    objclass_help       = [% objectclass_dir dir_>< 'help/' % help],
    objclass_ref        = [% objectclass_dir dir_>< 'ref/' % ref],
    objclass_src        = [% objectclass_dir dir_>< 'src/' % src],
    objclass_runtime    = [% objectclass_dir dir_>< 'rt/' % src],
    objclass_teachlist  = [^objclass_teach ^objclass_help ^objclass_ref],
    objclass_helplist   = [^objclass_help ^objclass_teach ^objclass_ref],
    objclass_reflist    = [^objclass_ref ^objclass_help ^objclass_teach],
    objclass_srclist    = [^objclass_src ^objclass_runtime],
;

extend_searchlist(ident objclass_helplist, vedhelplist) -> vedhelplist;
extend_searchlist(ident objclass_teachlist, vedteachlist) -> vedteachlist;
extend_searchlist(ident objclass_reflist, vedreflist) -> vedreflist;
extend_searchlist(ident objclass_srclist, vedsrclist) -> vedsrclist;

#_ENDIF

endsection;     /* $-objectclass */

;;; for uses
vars $-objectclass_help = true;
