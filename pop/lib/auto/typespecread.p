/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/typespecread.p
 > Purpose:         Read <typespec> from proglist and return list of items
 > Author:          John Gibson, Sep 16 1991
 > Documentation:   REF *DEFSTRUCT
 */
compile_mode:pop11 +strict;

uses typespec_utils;

section $-typespec_utils => typespecread;

define global typespecread() -> spec_items;
    lvars spec_items;
    dlocal pop_autoload = false;
    proglist_read_by(false, false, read_typespec) -> spec_items -> (,,)
enddefine;

endsection;
