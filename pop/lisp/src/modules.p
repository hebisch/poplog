/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/modules.p
 > Purpose:         Common Lisp Module package
 > Author:          John Williams, Nov  6 1985 (see revisions)
 > Documentation:   CLtL, p188
 > Related Files:   C.all/lisp/src/compile.p
 */

lisp_compile_mode;

section $-lisp => modules;

global vars modules = [];


define active module_directory_list;
    lisp_modules_list
enddefine;

define updaterof active module_directory_list() with_nargs 1;
    checkr_filenames() -> lisp_modules_list
enddefine;


define lconstant Checkr_module_name(name);
    if issymbol(name) then
        sym_to_filename(name)
    else
        get_simple_string(name)
    endif
enddefine;


define provide(item) -> item;
    Checkr_module_name(item) -> item;
    unless lmember_=(item, modules) do
        conspair(item, modules) -> modules
    endunless
enddefine;


define require(module, files);
    lvars name, file;
    defaults files nil;

    define lconstant Delete_module_name() with_nargs 1;
        if (/* dlocal_context */) == 2 then
            fast_ncdelete(name, modules, sys_=) -> modules
        endif
    enddefine;

    dlocal
        subsystem_compile_warn = #_< lisploadwarn(% true %) >_#,
        0 % (), (Delete_module_name(dlocal_context)) %,
        ;

    Checkr_module_name(module) -> name;
    if lmember_=(name, modules) then
        nil
    elseif files == nil then
        unless apply_in_pop11_environment(
                        name, module_directory_list, subsystem_libcompile)
        do
            unless issymbol(module)
            and symbol_package(module) == pop11_package
            and provide(name)
            and apply_in_pop11_environment(
                        sym_to_word(module), popuseslist, syslibcompile)
            do
                lisp_error('Cannot locate ~S module', [^module])
            endunless
        endunless;
        true
    else
        for file in checkr_filenames(files) do
            apply_in_pop11_environment(file, false, subsystem_compile)
        endfor;
        true
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Gibson, Jan 15 1993
        Add extra false arg to subsystem_compile
--- John Williams, Jan 15 1993
        Revised for new subsystem implementation
--- John Williams, Mar  4 1992
        Now wraps calls of -apply_in_pop11_environment- around calls to
        compiler procedures
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
