/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/saved_images.p
 > Purpose:         Common Lisp interface to Poplog saved images
 > Author:          John Williams, Jan 13 1993 (see revisions)
 > Documentation:   HELP * SAVELISP
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

/* Lisp interface to saved images */

define lconstant Get_psv_filename(file);
    dlocal % pt_type(default_pathname) % = 'psv';
    namestring(merge_pathnames(file, false, false))
enddefine;


define initlisp(banner);
    dlocal pop_nobanner;
    ;;; defaults
    unless pop_true(banner) do
        true -> pop_nobanner
    endunless;
    syssetup()
enddefine;


define savelisp(file, init, lock, share);
    Get_psv_filename(file) -> file;
    if pop_true(lock) then
        sys_lock_system(file, pop_true(share), file)
    else
        syssave(file)
    endif;
    if /* restored */ then
        if pop_true(init) then
            initlisp(true)
        endif;
        true
    else
        false
    endif
enddefine;


define restorelisp() with_nargs 1;
    sysrestore(Get_psv_filename())
enddefine;


global vars active (
    making_saved_image = boolean_variable(% ident pop_record_writeable %),
    );


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 29 1995
        Added boolean variable making_saved_image.
 */
