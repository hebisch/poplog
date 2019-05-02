/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/propsheet.p
 > Purpose:         Lisp Property sheets for the PUI
 > Author:          John Williams, Nov 29 1995 (see revisions)
 > Documentation:
 > Related Files:
 */

section $-lisp;


/* General propsheet <-> Lisp conversion procedures */

define lconstant Bool_convert() with_nargs 1;
    ;;; sheet value -> Lisp value
    lisp_true()
enddefine;


define updaterof lconstant Bool_convert() with_nargs 1;
    ;;; Lisp value -> sheet value
    pop_true()
enddefine;


define lconstant Int_or_nil_convert(x);             ;;; sheet -> Lisp
    if x = 'nil' then
        nil
    elseif isinteger(strnumber(x) ->> x) then
        x
    else
        nil
    endif
enddefine;


define updaterof lconstant Int_or_nil_convert(x);   ;;; Lisp -> sheet
    if x = nil then
        'nil'
    elseif isinteger(x) then
        consstring(#| dest_characters(x) |#)
    else
        'nil'
    endif
enddefine;


/* Debugger variables */

define lconstant Upl_convert(x);                    ;;; sheet -> Lisp
    lvars y;
    if x = ':ignore' then
        @:IGNORE
    elseif isinteger(strnumber(x) ->> y) then
        y
    else
        nil
    endif
enddefine;


define updaterof lconstant Upl_convert(x);          ;;; Lisp -> sheet
    if x = '@:IGNORE' then
        ':ignore'
    elseif isinteger(x) then
        consstring(#| dest_characters(x) |#)
    else
        'nil'
    endif
enddefine;


lconstant Debugger_sheet =
    [['*break-on-errors*'       true    (converter = ^Bool_convert,
                                         ident = ^(ident break_on_errors))]
     ['*break-on-interrupts*'   true    (converter = ^Bool_convert,
                                         ident = ^(ident break_on_interrupts))]
     ['*break-on-warnings*'     true    (converter = ^Bool_convert,
                                         ident = ^(ident break_on_warnings))]
     ['*lisp-calling-limit*'    'nil'   (converter = ^Int_or_nil_convert,
                                         ident = ^(ident lisp_calling_limit))]
     ['*error-print-length*'   '5'      (converter = ^Upl_convert,
                                         ident = ^(ident error_print_length))]
     ['*error-print-level*'    '3'      (converter = ^Upl_convert,
                                         ident = ^(ident error_print_level))]
     ['*inspect-print-length*' '5'      (converter = ^Upl_convert,
                                         ident = ^(ident inspect_print_length))]
     ['*inspect-print-level*'  '3'      (converter = ^Upl_convert,
                                         ident = ^(ident inspect_print_level))]
     ['*trace-print-length*'   '5'      (converter = ^Upl_convert,
                                         ident = ^(ident trace_print_length))]
     ['*trace-print-level*'    '3'      (converter = ^Upl_convert,
                                         ident = ^(ident trace_print_level))]
     ];


/* Reader variables */

define lconstant Ff_convert(x);                     ;;; sheet -> Lisp
    sysintern(lowertoupper(x), lisp_package)
enddefine;


define updaterof lconstant Ff_convert() with_nargs 1;   ;;; Lisp -> sheet
    string_capitalize(0, nil)
enddefine;


lconstant Reader_sheet =
    [['*read-base*'         2-36    (default = 10,
                                      ident = ^(ident read_base))]
     ['*read-default-float-format*'
                            menuof  ['Single-Float' 'Double-Float']
                                    (converter = ^Ff_convert,
                                     ident = ^(ident read_default_float_format))]
     ['*read-eval*'         true    (converter = ^Bool_convert,
                                     ident = ^(ident read_eval))]
     ['*read-prompt*'       '== '   (ident = ^(ident popprompt))]
    ];


/* Printer variables */

define lconstant Pcase_convert(x);                  ;;; sheet -> Lisp
    if x = 'Nil' then
        nil
    else
        sysintern(lowertoupper(x), keyword_package)
    endif
enddefine;


define updaterof lconstant Pcase_convert(x);        ;;; Lisp -> sheet
    lconstant PCASES = [% @:UPCASE, @:DOWNCASE, @:CAPITALIZE, nil %];
    unless fast_lmember(x, PCASES) do
        print_case -> x
    endunless;
    string_capitalize(x, 0, nil)
enddefine;


lconstant Printer_sheet =
    [['*print-escape*'      true    (converter = ^Bool_convert,
                                     ident = ^(ident print_escape))]
     ['*print-readably*'    false   (converter = ^Bool_convert,
                                     ident = ^(ident print_readably))]
     ['*print-pretty*'      true    (converter = ^Bool_convert,
                                     ident = ^(ident print_pretty))]
     ['*print-right-margin*' '76'   (converter = ^Int_or_nil_convert,
                                     ident = ^(ident print_right_margin))]
     ['*print-lines*'       'nil'   (converter = ^Int_or_nil_convert,
                                     ident = ^(ident print_lines))]
     ['*print-miser-width*' 'nil'   (converter = ^Int_or_nil_convert,
                                     ident = ^(ident print_miser_width))]
     ['*print-circle*'      true    (converter = ^Bool_convert,
                                     ident = ^(ident print_circle))]
     ['*print-length*'      'nil'   (converter = ^Int_or_nil_convert,
                                     ident = ^(ident print_length))]
     ['*print-level*'       'nil'   (converter = ^Int_or_nil_convert,
                                     ident = ^(ident print_level))]
     ['*print-array*'       true    (converter = ^Bool_convert,
                                     ident = ^(ident print_array))]
     ['*print-gensym*'      true    (converter = ^Bool_convert,
                                     ident = ^(ident print_gensym))]
     ['*print-radix*'       false   (converter = ^Bool_convert,
                                     ident = ^(ident print_radix))]
     ['*print-case*'        menuof  ['Upcase' 'Downcase' 'Capitalize' 'Nil']
                                    (converter = ^Pcase_convert,
                                     ident = ^(ident print_case))]
     ['*print-base*'        2-36    (default = 10,
                                        ident = ^(ident print_base))]
    ];


/* Various other variables */

define lconstant Package_convert() with_nargs 1;
    ;;; sheet -> Lisp
    find_package(lowertoupper())
enddefine;


define updaterof lconstant Package_convert() with_nargs 1;
    ;;; Lisp -> sheet
    uppertolower(package_name())
enddefine;


define lconstant Pathname_convert() with_nargs 1;
    ;;; sheet -> Lisp
    pathname()
enddefine;


define updaterof Pathname_convert() with_nargs 1;
    ;;; Lisp -> sheet
    checkr_filename()
enddefine;


lconstant Various_sheet =
    [['*package*'           'common-lisp-user'
                                    (converter = ^Package_convert,
                                     ident = ^(ident package))]
     ['*default-pathname-defaults*'
                            ^(checkr_filename(default_pathname))
                                    (converter = ^Pathname_convert,
                                     ident = ^(ident default_pathname))]
     ['*time-zone*'         0-11    (default = ^pop_time_zone,
                                     ident = ^(ident pop_time_zone))]
     ['*load-print*'        true    (converter = ^Bool_convert,
                                     ident = ^(ident load_print))]
     ['*load-verbose*'      true    (converter = ^Bool_convert,
                                     ident = ^(ident load_verbose))]
     ['*load-lock*'         true    (converter = ^Bool_convert,
                                     ident = ^(ident load_lock))]
     ];


/* Saver procedure for Lisp property sheets */

#_IF DEF pop_ui_add_property

define vars lisp_propsheet_save(name, sheet);
    lvars i, s, v;
    dlocal package = user_package;
    fastprocs for;

    [%  [^@IN-PACKAGE 'COMMON-LISP-USER'],
        for i from 1 to propsheet_length(sheet) do
            string_to_sym(lowertoupper(propsheet_field_name(sheet, i))) -> s;
            propsheet_field_value(sheet, i) -> v;
            if s == @*PACKAGE* and ispackage(v)
            then
                ;;; package objects are not print-readable
                package_name(v) -> v
            elseif ispair(v)
            or (issymbol(v) and not(v == true or v == nil or keywordp(v)))
            then
                [^@QUOTE ^v] -> v
            endif;
            [^@SETQ ^s ^v]
        endfor
    %];

    procedure();
        dlocal pr = lisp_pr, print_pretty = nil, print_readably = true,
               print_case = @:DOWNCASE, standard_output
            ;
        make_stream(false, cucharout) -> standard_output;
        SET_CUCHAROUT;
        applist(npr)
    endprocedure()
enddefine;


define lconstant Save =
    $-poplog_ui$-proptool_save_to_file(% 'init.lsp', lisp_propsheet_save %)
enddefine;

#_ELSE

lconstant Save = identfn;

#_ENDIF


/* List of args to pop_ui_add_property, called by Lisp_xsetup in
    C.all/lisp/src/subsystem_procedures.p
*/

lconstant CL_OPTIONS = 'Common Lisp';

global vars lisp_property_sheets =
    [{ ^CL_OPTIONS              ^Various_sheet      ^false      ^Save}
     {[^CL_OPTIONS 'Debugger']  ^Debugger_sheet     ^false      ^Save}
     {[^CL_OPTIONS 'Printer']   ^Printer_sheet      ^false      ^Save}
     {[^CL_OPTIONS 'Reader']    ^Reader_sheet       ^false      ^Save}
    ];


endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul 17 1996
        Modified titles of lisp_property_sheets for new UI options structure
--- John Williams, May  9 1996
        Definition of lisp_propsheet_save guarded with
            #_IF DEF pop_ui_add_property
--- John Williams, Mar 15 1996
        Added saver procedure lisp_propsheet_save.
--- John Williams, Feb 13 1996
        Fixed bug in calls to dest_characters.
 */
