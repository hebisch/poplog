/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.unix/src/syscomp/os_comms.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Related Files:   C.vms/src/syscomp/os_comms.p
 */

/* -------------------------------------------------------------------------

            OPERATING SYSTEM COMMANDS FOR ASSEMBLING & LINKING
                                (UNIX)

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

#_IF DEF IRIX
lconstant macro (
    AS_CMD  = '/usr/bin/as',
    AR_CMD  = '/usr/bin/ar',
);
#_ELSEIF DEFV SYSTEM_V >= 4.0
lconstant macro (
    AS_CMD  = '/usr/ccs/bin/as',
    AR_CMD  = '/usr/ccs/bin/ar',
);
#_ELSEIF DEF LINUX
lconstant macro (
    ;;; AS_CMD  = '/sklad/kompi/poplog/pp4/asm',
    AS_CMD  = '/usr/bin/as',
    AR_CMD  = '/usr/bin/ar',
);
#_ELSE
lconstant macro (
    AS_CMD  = '/bin/as',
    AR_CMD  = '/bin/ar',
);
#_ENDIF

;;; unix_as_options:
;;;     command line options for the assembler: may be defined in
;;;     "asmout.p"

#_IF not(DEF unix_as_options)
lconstant unix_as_options = [];
#_ENDIF

;;; unix_ld_crt_objects:
;;;     location of crt*.o files for linking: may be defined in "asmout.p"

#_IF not(DEF cc_link_command_header or DEF unix_ld_crt_objects)
lconstant
    unix_ld_crt_objects =
      #_IF DEFV SYSTEM_V >= 4.0
        '/usr/ccs/lib/crt[1in].o'
      #_ELSE
        '/lib/crt0.o'
      #_ENDIF
    ;
#_ENDIF

define assemble_files(plist);
    lvars plist;

    define lconstant asm(fpair);
        lvars fpair, (a_file, o_file) = destpair(fpair);
        sysobey(systranslate('POP__as') or AS_CMD,
                    ['as' ^^unix_as_options '-o' ^o_file ^a_file]);
#_IF DEF MIPS
        ;;; patch the object file
        if pop_status == 0 then
            sysobey('$popsrc/patch', ['patch' ^^unix_as_options ^o_file ^a_file]);
        endif;
#_ENDIF
        if pop_status /== 0 then
            mishap(0, 'ERRORS IN ASSEMBLER FILE (see above)')
        else
            add_created_file(o_file)
        endif
    enddefine;

    if islist(plist) then applist(plist, asm) else asm(plist) endif
enddefine;

define gen_link_command(exlink, link_cmnd, image_name, wobj_files, link_flags,
                        link_other, extern_libdir, share, makebase,
                        cleanup_command);
    lvars   f, dir, image_name, wobj_files, exlink, link_cmnd, link_flags,
            link_other, extern_libdir, share, makebase, cleanup_command,
            crt_objects;
    dlocal  asmf_charout, pop_file_mode = 8:777;

    define lconstant nl_printf();
        asmf_printf((), if exlink then '%p\n' else '%p \\\n' endif)
    enddefine;

    define lconstant out_obj_files(files);
        lvars f, files;
        for f in files do
            ;;; libraries are inside refs
            if isref(f) then cont(f) -> f endif;
            if exlink then sysfileok(f) -> f endif;
#_IF DEF LINUX
            if isstartstring('-R', f) then
  #_IF DEF cc_link_command_header
                '-Wl,-rpath,' <> allbutfirst(2, f) -> f
  #_ELSE
                '-rpath ' <> allbutfirst(2, f) -> f
  #_ENDIF
            endif;
#_ENDIF
            nl_printf(f)
        endfor
    enddefine;

    discout(link_cmnd) -> asmf_charout;
    add_created_file(link_cmnd);

    if exlink then
                out_obj_files(wobj_files)
    else
        asmf_pr('#!/bin/sh\n');
        asmf_printf(image_name, 'IM=$1\nIM=${IM:-%p}\n');

#_IF DEF cc_link_command_header
        ;;; Using C compiler to link (uses $POP__cc as command)
        asmf_pr('POP__cc=${POP__cc:-cc}\n');
        asmf_pr(cc_link_command_header);
        for f in link_flags do asmf_printf(f, '-Wl,%p \\\n') endfor;
                out_obj_files(wobj_files);
#_ELSE
        ;;; link command header string (defined in asmout.p)
        asmf_pr(unix_ld_command_header);
        for f in link_flags do asmf_printf(f, '%p \\\n') endfor;
            systranslate('POP__crt0') or unix_ld_crt_objects -> crt_objects;
  #_IF not(DEF SCO)             ;;; SCO has a -e, probably shouldn't
        if issubstring('\s-e\s', unix_ld_command_header) then
            out_obj_files(wobj_files);
            nl_printf(crt_objects)
  #_ENDIF
        else
            ;;; no -e, so assume entry point is start of text seg, in which
            ;;; case crt_objects must come first in link
            nl_printf(crt_objects);
            out_obj_files(wobj_files)
        endif
#_ENDIF
    endif;

    ;;; directory for pop library
    asmf_pr(#_IF DEF HPUX '-L ' #_ELSE '-L' #_ENDIF);
    nl_printf(extern_libdir);

    ;;; pop library (must come before link_other since that will contain
    ;;; any X libraries)
    nl_printf('-lpop');

    out_obj_files(link_other);

    ;;; standard libraries
#_IF DEF OSF1
    nl_printf('-lexc');
#_ELSEIF DEF NCR or DEF DGUX
    ;;; this may be needed by X libs in link_other
    nl_printf('-lgen');
#_ELSEIF DEF HPUX
    ;;; allow for extra libraries
    if unix_ld_extra_libraries and not(exlink) then
        nl_printf(unix_ld_extra_libraries);
    endif;
#_ENDIF
    asmf_pr(if exlink then '-lm\n' else
#_IF DEF NCR
         '-lm -lmw -lc\n'
#_ELSE
         '-lm -lc\n'
#_ENDIF
        endif);

    unless exlink then
        asmf_pr('ST=$?\n');

        if cleanup_command then asmf_printf(cleanup_command, '%p\n') endif;
        if makebase then
            asmf_pr('if [ $ST = 0 ] ; then rm -f $IM.stb ; fi\n')
        endif;
        asmf_pr('exit $ST\n')
    endunless;

    asmf_charout(termin)
enddefine;

define assemble_and_link(link_cmnd, a_files, im_name, makebase);
    lvars pair, a_files, link_cmnd, im_name, makebase;
    assemble_files(a_files);
#_IF DEF SCO
    sysobey('/bin/csh', ['csh' %link_cmnd, im_name%]);
#_ELSE
    sysobey(link_cmnd, [%link_cmnd, im_name%]);
#_ENDIF
    if pop_status /== 0 then
        mishap(0, 'ERRORS IN LINKING (see above)')
    elseif makebase then
        Extern_make_base(im_name, new_fname_extn(im_name, '.stb'), true)
    endif
enddefine;

sysunprotect("pop_max_filename_len");

define os_library_command(option, o_lib, o_files);
    lvars option, create_links, o_files, o_lib;

    define lconstant trunc_name(fname);
        lvars n, q, fname, trunc;
        lconstant   MAXLEN = 14;

        procedure();
            dlocal pop_max_filename_len = MAXLEN;
            sysfileok()
        endprocedure(fname) -> trunc;
        returnif(trunc = fname) (fname);

        ;;; encode excess as 3 alphanumeric chars following `%`

        define lconstant alphanum(n);
            lvars n;
            if n fi_< 0 then
                n fi_+ 63 -> n
            endif;
            if n == 0 then
                `_`
            elseif n fi_< 11 then
                n fi_+ #_< `0`-1 >_#
            elseif n fi_< 37 then
                n fi_+ #_< `A`-11 >_#
            else
                n fi_+ #_< `a`-37 >_#
            endif
        enddefine;

        copy(sys_fname_name(fname)) -> trunc;
        0 -> q;
        fast_for n from #_<MAXLEN-3>_# to datalength(trunc) do
            fast_subscrs(n, trunc) fi_+ (q fi_* n) -> q
        endfor;
        `%` -> fast_subscrs(#_<MAXLEN-3>_#, trunc);
        q fi_// 63 -> (n, q);
        alphanum(n) -> fast_subscrs(#_<MAXLEN-2>_#, trunc);
        q fi_// 63 -> (n, q);
        alphanum(n) -> fast_subscrs(#_<MAXLEN-1>_#, trunc);
        alphanum(q fi_rem 63) -> fast_subscrs(MAXLEN, trunc);
        sys_fname_path(fname) dir_>< substring(1,MAXLEN,trunc) -> trunc;

        if create_links then
            if isstring(create_links) and create_links /= nullstring then
                create_links dir_>< sys_fname_name(trunc) -> trunc
            endif;
            unless fname(1) == `/` then
                current_directory dir_>< fname -> fname
            endunless;
            syssymlink(fname, trunc) -> ;
            sysunlink -> is_tmp_file(trunc);
            add_created_file(trunc)
        endif;
        trunc
    enddefine;

    if option == "d" then
        false
    else
        sys_fname_path(o_lib)
    endif -> create_links;

    ;;; run "ar"
    sysobey(systranslate('POP__ar') or AR_CMD,
        [% 'ar',
            if option == "c" then 'qc' else word_string(option) endif,
            o_lib,
            applist(o_files, trunc_name)    ;;; module names
        %]);

;;; these do "ranlib" automatically
#_IF not(DEF SYSTEM_V or DEF HPUX or DEF OSF1 or DEF IRIX)
    ;;; run "ranlib"
    if option /== "x" and pop_status == 0 then
        sysobey('/usr/bin/ranlib', ['ranlib' ^o_lib])
    endif;
#_ENDIF

    if pop_status /== 0 then
        mishap(0, 'ERRORS RUNNING AR/RANLIB (see above)')
    endif
enddefine;


    /*  Procedure to run POPLINK/POPLIBR from POPC
    */
define run_comp_util(arg_list, name);
    lvars arg_list, name;
    sysobey(sysfileok('$popsys/' dir_>< name), [^name ^^arg_list] );
    if pop_status /== 0 then
        mishap(0, sprintf(lowertoupper(name), 'ERRORS RUNNING %p (see above)'))
    endif
enddefine;


endsection;     /* $-Popas */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  9 1999
        Made out_obj_files convert -R<dirname> to appropriate -rpath args
        for Linux
--- Julian Clinton, Aug  7 1998
        Add '-lmw' for NCR.
--- Robert Duncan, Jul  2 1998
        Allowed for extra libraries in HP-UX link command
--- Robert Duncan, Jun 05 1998
        Added libgen to link command for DG/UX (used at least by libXm)
--- Robert Duncan, Aug 13 1996
        Added libgen to link command for NCR (used at least by libXm)
--- John Gibson, Dec  7 1995
        Now allows env vars POP__as, POP__ar and POP__crt0 (if set) to
        override AS_CMD, AR_CMD and unix_ld_crt_objects
--- John Gibson, Mar 10 1995
        OSF changes
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Set options for Linux and SCO Open Desktop 3.0.
--- Robert John Duncan, Mar 21 1994
        Changes for SG IRIX 5+
--- John Gibson, Nov 19 1993
        Changed gen_link_command to take exlink arg to say just produce
        file containing object/library args
--- John Gibson, Nov 13 1993
        -lpop now the only pop library in link command
--- John Gibson, Aug 30 1993
        Replaced run*_poplink with run_comp_util
--- John Gibson, Jul 10 1993
        Added makebase argument to gen_link_command and assemble_and_link
--- Robert John Duncan, Jun 11 1993
        Changed gen_link_command to write out explicit references to the pop
        external libraries using the '-L' and '-l' options to ld and made it
        take an additional argument -- extern_libdir -- which is the
        directory containing the libraries as determined by poplink. This is
        necessary when the libraries are shared, because they may not be in
        the same directory when the image is run.
        Also tidied up references to the crt*.o files -- their location can
        now be specified by defining unix_ld_crt_objects in "asmout.p".
--- Simon Nichols, Jun  2 1993
        Removed addition of -ldl/-ldld to link command -- now done with
        a dummy exload in src/unixextern.p.
--- Robert John Duncan, Jun  2 1993
        Changes for SVR4
--- John Gibson, May 23 1993
        Removed addition of -lt*ermcap to link command -- now done with
        a dummy exload in src/termcap.p.
--- John Gibson, May 14 1993
        Added cleanup_command arg to gen_link_command
--- John Gibson, May  6 1993
        Removed addition of c_c*ore.obj from gen_link_command -- this is
        now part of $popexternlib/libpop.olb.
--- Simon Nichols, Mar  2 1993
        Changed location of crt?.o in Solaris 2.1
--- Simon Nichols, Jan 29 1993
        Changed name of conditional compilation flag SU*NOS_DYNAMIC to
        SHARED_LIBRARIES, as it's more descriptive and not SunOS specific.
        Added support for shared libraries on HP-UX.
--- John Gibson, Oct 20 1992
        Changed os_library_command to hash-encode module names longer
        than 14 chars
--- Robert John Duncan, Jul 24 1992
        Changed location of commands for SunOS 5.0 and the list of
        libraries to be given to ld
--- Robert John Duncan, Jul 21 1992
        Added SYSTEM_V to list of systems which don't need ranlib
--- Simon Nichols, Mar  3 1992
        Changes to support SunOS dynamic linking (temporarily flagged by
        SU*NOS_DYNAMIC).
--- Robert John Duncan, Jun 24 1991
        Added code for SG IRIX
--- John Gibson, Jan 21 1991
        Added -share- arg to -gen_link_command-
--- John Gibson, Nov 22 1990
        Changed malloc.o to c_c*ore.obj
--- John Gibson, Nov 22 1990
--- John Gibson, Sep 27 1990
        Replaced calls of -interrupt- with mishaps
--- Rob Duncan, May 31 1990
        Added non-standard run-time library for DECSTATION
--- John Gibson, May 10 1990
        Made $popsrc/malloc.o be systranslated (so it gets current
        value of $popsrc).
--- Rob Duncan, May  8 1990
        Added patch for MIPS and funny runtime libraries for MIPS/RISCOS
--- Rob Duncan, Nov  7 1989
        Added t*ermcap library ('-lt*ermcap') to end of link command
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 24 1989
        -os_library_command- moved here from poplibr_main.p
--- John Gibson, Jul 17 1989
        Extracted from Unix versions of asmout.p
        Added -run*_poplink-
 */
