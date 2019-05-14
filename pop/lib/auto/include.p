/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/include.p
 > Purpose:         tasty front-end for the #_INCLUDE mechanism
 > Author:          James Goodlet, Aug 25 1989 (see revisions)
 > Documentation:   HELP * INCLUDE
 > Related Files:   LIB * USES
 */
compile_mode:pop11 +strict;

section;

define sysprincludewarn with_nargs 1;
    pr(';;; INCLUDING '); npr();
enddefine;

#_IF not(DEF princludewarn) or DEF POPC_COMPILING
vars procedure princludewarn = erase;
#_ENDIF

define lconstant do_include(ldflag);
    lvars dir, name;
    dlvars ldflag;
    dlocal popnewline = true;

    define lconstant really_do_include(fname);
        lvars fname, dev = readable(fname);
        if dev then
            princludewarn(device_full_name(dev));
            if ldflag and pop_pas_mode /== "popc" then
                [; pop11_compile(^dev);]
            else
                [; #_INCLUDE ^dev]
            endif nc_<> proglist -> proglist;
            exitfrom(do_include);
        endif;
    enddefine;

    /* get the raw filename to be included, with environment vars expanded */
    sysfileok(rdstringto([; ^termin ^newline])) -> name;

    /* append the default extension, if none given - note can't use
     * pop_default_type to pass default extension to compile as it would
     * override the addition of 'h'.  So have to enforce the addition of a
     * default extension, if one has not been given.  Therefore, only way
     * to have no extension is to call this function in context where
     * pop_default_type is nullstring.
     */
    unless pop_default_type = nullstring then
        if sys_fname_extn(name) == nullstring then
            /* note: this cannot happen on vms systems, and therefore we don't
             * need to worry about appending the version number.
             */
            name sys_>< pop_default_type sys_>< 'h' -> name;
        endif;
    endunless;

    /* recall that sys_fnam caches its parse vector */
    if isstartstring('/', sys_fname(name, 3) ->> dir) then
        /* absolute filename - just try to compile */
        really_do_include(name);
    elseif isstartstring('./', dir) or isstartstring('../', dir) then
        /* forced relative to the current directory */
        really_do_include(name);
    else
        /* relative to each member of popincludelist which, if the list
         * includes nullstring, subsumes the forced relative case.
         */
        for dir in popincludelist do
            really_do_include(dir dir_>< name);
        endfor;
    endif;
    mishap(name, 1, 'INCLUDE FILE NOT FOUND');
enddefine;

lconstant names = [include loadinclude];
applist(names, sysunprotect);

define macro include;
    do_include(false);
enddefine;

define macro loadinclude;
    do_include(true);
enddefine;

applist(names, sysprotect);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 28 1996
        Changed really_do_include to give the readable device directly to
        pop11_compile or #_INCLUDE (rather than leaving millions of garbage
        devices around)
--- John Gibson, Jan 16 1994
        Protected include & loadinclude
--- John Gibson, Jun 15 1993
        Changed so that loadinclude behaves as include when compiling with
        POPC.
--- John Gibson, Nov 26 1992
        Moved initialisation of popincludelist to popincludelist.p
--- John Gibson, Oct 11 1992
        Changed to compile with POPC
--- James Goodlet, Feb 26 1991 - extended to respect path component in given
        filename.  Supports absolute filenames (dir starting with /) and
        "forced relative" ones (dir starting with ./ or ../).
--- Roger Evans, Nov 11 1990 - added loadinclude
--- Roger Evans, May 31 1990 - added system and local include directories to
        default include path
 */
