/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/shadowkey.p
 > Purpose:         external records 'shadowed' by pop data
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:   REF *SHADOWCLASS
 > Related Files:   LIB *SHADOWCLASS
 */

uses consshadowkey, shadowclass_data;

constant $-shadowkey = true;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May  5 1993
        Split this file into 3 parts:
            LIB * SHADOWKEY_KEY     (run-time parts of a shadowkey)
            LIB * CONSSHADOWKEY     (construct a shadowkey)
            LIB * SHADOWCLASS_DATA  (access shadowkey fields)
--- John Gibson, May  3 1993
        Removed unwanted uses for typespec_utils, and added comment
        at top-of-file about it.
--- John Gibson, Mar 17 1993
        Changes to enable POPC to handle shadowkeys generated in a file:
            Changed Create_class_procs so that all the closures in
        shk_procs(key) are based on perm constant procedures (shkP_Cons,
        shkP_Dest, etc), rather than being lexical closures. Similarily with
        shadowclass_field and shkP_Field_swap.
            Also avoided problem of having to declare conshadowkey as vars
        and then redefine it, by using conskey directly to construct
        shadowkey_key. (And made shadowkey_key a constant, which is also
        required by POPC.)
--- Robert John Duncan, Jul  7 1992
        Added some missing -writeable- declarations for compile-time
        structures
--- Adrian Howard, Sep  9 1991 : Added -isshadow-
--- Roger Evans, Sep  9 1991 fixed bug in SUbscr causing vectors in uservecs
        to be interpreted incorrectly.
--- Adrian Howard, Aug 30 1991 : Added -shadow_=- and made it the -class_=- of
        -shadowrec- structures.
--- Adrian Howard, Aug 29 1991 : Fixed two bugs in import procedures
        o import now always takes a length argument for vector type
          shadowclasses
        o now only returns an existing shadowclass if it is of the same type
          and size
--- Adrian Howard, Aug 13 1991 : Exported -shadowclass_import-
--- John Gibson, Aug 10 1991 Added -writeable- before initv and init_fixed in
        -Cons-
--- Roger Evans, Jul 31 1991 added SHF_TYPESPEC and fldmode slot
--- Roger Evans, Jul 24 1991 added updater for Refresh_subscr
--- Roger Evans, Jul  2 1991 added import procedures
--- Roger Evans, Apr  4 1991 fixed bug jonm.7 causing init and refresh
        procs to break on variable length fields
--- Roger Evans, Feb 14 1991 stopped Do_Exacc from clobbering class props
--- Roger Evans, Feb 10 1991 fixed another bug in init procedures
--- Roger Evans, Feb  6 1991 consshadowkey can now take props directly
        from an attribute list
--- Roger Evans, Jan 26 1991 improved class proc access interface
--- Roger Evans, Jan  9 1991 changed class proc access procedures and
        fixed bug in init procedures.
--- Roger Evans, Dec 10 1990 made constant exptr writeable, and fixed
        bug in redeclaration of consshadowkey
--- Roger Evans, Nov 27 1990 added init and refresh functions
--- Roger Evans, Nov 19 1990 made shadow_construct a real property
--- Roger Evans, Nov 19 1990 added shadow_length
 */
