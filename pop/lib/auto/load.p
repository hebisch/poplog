/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/load.p
 > Purpose:         Syntax for loading files
 > Author:          John Gibson, Jan 11 1993 (see revisions)
 > Documentation:   REF *POPCOMPILE
 */
compile_mode :pop11 +strict;

include ved_declare.ph;

section;

define sysloadwarning(lib_name);
    lvars lib_name;
    printf(';;; LOADING %S\n', [^lib_name])
enddefine;

vars procedure loadwarning = sysloadwarning;

define vars loadcompiler() with_nargs 1;
    dlocal subsystem_compile_warn = erase;
    subsystem_compile((), false);
enddefine;

define syntax load;
    lvars name;
    dlocal popnewline = true;
    sysfileok(rdstringto([; ^termin ^newline])) -> name;
    false -> popnewline;
    if name = nullstring then
        unless popclosebracket == termin
        and testdef vedvedname
        and sys_fname_extn(weakref vedvedname ->> name) = '.p'
        and readable(name) do
            mishap(0, 'load: NO FILENAME SUPPLIED')
        endunless
    elseif popclosebracket == termin then
        name -> weakref vedvedname
    endif;
    sysPUSHQ(name);
    sysCALL("loadwarning");
    sysPUSHQ(name);
    sysCALL("loadcompiler");
    ";" :: proglist -> proglist
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 25 1993
        sysloadwarning now uses new %S option to printf
--- John Williams, Jan 22 1993
        sysloadwarning now sets pop_pr_quotes false.
 */
