/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_propertytool.p
 > Purpose:         Poplog Properties Panel
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui =>  pop_ui_propertytool,
                        pop_ui_property_list,
                        pop_ui_add_property,
                        pop_ui_remove_property,
                        pop_ui_show_property,
                        pop_ui_save_properties;

exload_batch

include sysdefs; ;;; for O/S type

uses
    propsheet,
    xpt_cursorplane,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiFileutils,
    pop_ui_choose_file, ;;; for pop_ui_file_search_* parameters
;

defclass OptionBox [writeable] {
    box_name,
    box_widget,
    box_contents,
    box_current_sheet,
};

defclass OptionSheet [writeable] {
    sheet_name,
    sheet_widget,
    sheet_fields,
    sheet_save_proc,
    sheet_needs_saving,
    sheet_box,
};

lvars
    box_parent = false,
        ;;; preferred parent for option boxes
    option_box_names = [],
        ;;; option boxes currently available
    sheet_modified_count = 0,
        ;;; number of sheets which need saving
;

    ;;; table of available option boxes
define option_box =
    newmapping([], 16, false, true);
enddefine;

define new_option_sheet(name, fields, save_p, box);
    consOptionSheet(name, false, fields, save_p, false, box);
enddefine;

    ;;; the full title of a sheet, including the box name
define sheet_title(sheet);
    lvars bname = box_name(sheet_box(sheet));
    lvars sname = sheet_name(sheet);
    sname and [^bname ^sname] or bname;
enddefine;

define set_modified(flag, sheet);
    unless flag == sheet_needs_saving(sheet) then
        flag -> sheet_needs_saving(sheet);
        if flag then
            sheet_modified_count + 1
        else
            sheet_modified_count - 1
        endif -> sheet_modified_count;
    endunless;
enddefine;

define save_sheet(sheet);
    if sheet_needs_saving(sheet)
    and sheet_save_proc(sheet)
    and XptIsLiveType(sheet_widget(sheet), "Widget")
    then
        sheet_save_proc(sheet)(sheet_title(sheet), sheet_widget(sheet));
    endif;
    set_modified(false, sheet);
enddefine;

define show_sheet(sheet);
    lvars pw = box_widget(sheet_box(sheet)), fields;
    if pw then
        lvars w = sheet_widget(sheet);
        if w then
            propsheet_refresh(w);
        else
            if isprocedure(sheet_fields(sheet) ->> fields) then
                fields() -> fields
            endif;
            propsheet_new(false, pw, fields) ->> w -> sheet_widget(sheet);
        endif;
        propsheet_show(w);
    endif;
enddefine;

define hide_sheet(sheet);
    if sheet_widget(sheet) then
        propsheet_hide(sheet_widget(sheet));
    endif;
enddefine;

define appsheets(p);
    lvars box;
    for _, box in_property option_box do
        applist(box_contents(box), p);
    endfor;
enddefine;

define new_option_box(name);
    consOptionBox(name, false, [], false);
enddefine;

define box_sheet(name, box);
    lvars sheet;
    for sheet in box_contents(box) do
        returnif(sheet_name(sheet) = name)(sheet);
    endfor;
    false;
enddefine;

define make_current_sheet(sheet, box);
    returnif(sheet == box_current_sheet(box));
    lvars old = box_current_sheet(box);
    sheet -> box_current_sheet(box);
    if box_widget(box) then
        if old then hide_sheet(old) endif;
        sheet_name(sheet) -> propsheet_field_value(box_widget(box), 1, 1);
        show_sheet(sheet);
    endif;
enddefine;

    ;;; update the index in an option box to reflect the current
    ;;; contents and the selected sheet
define set_index(box);

    lconstant SHEET_DEFAULT_LABEL = 'General';

        ;;; converter procedure for index values
    define convert_p(label) -> name;
        label /== SHEET_DEFAULT_LABEL and label -> name;
    enddefine;
    ;;;
    define updaterof convert_p(name) -> label;
        name or SHEET_DEFAULT_LABEL -> label;
    enddefine;

        ;;; callback procedure for selection from index:
        ;;; make the selected sheet current
    define accept_p(_, _, label) -> label;
        lvars sheet = box_sheet(convert_p(label), box);
        if sheet then
            make_current_sheet(sheet, box);
        else
            propsheet_undef -> label;
        endif;
    enddefine;

    define sheet_label(sheet);
        sheet_name(sheet) -> convert_p();
    enddefine;

    lvars w = box_widget(box);
    returnunless(w);
    lvars index = w(1);
    lvars sheet_labels = maplist(box_contents(box), sheet_label);
    propsheet_field(index, [Index menuof ^sheet_labels (nodefault, nolabel,
        converter = ^convert_p, accepter = ^accept_p)]);
    if box_current_sheet(box) then
        sheet_name(box_current_sheet(box)) -> propsheet_field_value(index, 1);
    endif;
    ;;; display the index only if there's a real choice
    length(sheet_labels) > 1 -> propsheet_visible(index);
enddefine;

    ;;; create the widgets for an option box
define realize_box(box) -> w;

        ;;; callback procedure for dialog buttons: do the default action
        ;;; but mark sheet for saving after Apply
    define button_cb(w, button) -> popdown;
        propsheet_handle_standard_button(w, button);
        if button == "Apply" and box_current_sheet(box) then
            set_modified(true, box_current_sheet(box));
        endif;
        button == "Dismiss" -> popdown;
    enddefine;

    returnif(box_widget(box) ->> w);
    ;;; create the propsheet box and its index sheet
    lvars title = 'Poplog: ' <> (box_name(box) sys_>< ' Preferences');
    propsheet_new_box(title, box_parent, button_cb, false) ->> w
        -> box_widget(box);
    lvars index = propsheet_new(false, w, false);
    box -> propsheet_user_data(index);
    ;;; set the current sheet
    lvars sheet = box_current_sheet(box);
    unless sheet or box_contents(box) == [] then
        hd(box_contents(box)) ->> sheet -> box_current_sheet(box);
    endunless;
    if sheet then
        set_index(box);
        show_sheet(sheet);
    endif;
    ;;; enable cursor feedback: requires the box to be realized
    XtRealizeWidget(w);
    true ->> XptBusyCursorFeedback(w) -> XptGarbageCursorFeedback(w);
enddefine;

define show_box(box);
    lvars w = box_widget(box);
    if w then
        propsheet_refresh(w, true);
    else
        realize_box(box) -> w;
    endif;
    propsheet_show(w);
enddefine;

define hide_box(box);
    if box_widget(box) then
        propsheet_hide(box_widget(box));
    endif;
enddefine;

define add_sheet(name, fields, save_p, box) -> sheet;
    if box_sheet(name, box) ->> sheet then
        unless sheet_fields(sheet) == fields then
            fields -> sheet_fields(sheet);
            set_modified(false, sheet);
            lvars w = sheet_widget(sheet);
            if w then
                false -> sheet_widget(sheet);
                propsheet_destroy(w);
            endif;
            if sheet == box_current_sheet(box) then
                show_sheet(sheet);
            endif;
        endunless;
        save_p -> sheet_save_proc(sheet);
    else
        new_option_sheet(name, fields, save_p, box) -> sheet;
        box_contents(box) nc_<> writeable [^sheet] -> box_contents(box);
        set_index(box);
    endif;
enddefine;

define remove_sheet(name, box);
    lvars sheet = box_sheet(name, box);
    if sheet then
        lvars sheets = box_contents(box);
        ncdelete(sheet, sheets, nonop==) ->> sheets -> box_contents(box);
        if sheet == box_current_sheet(box) then
            hide_sheet(sheet);
            if sheets /== [] and hd(sheets) ->> box_current_sheet(box) then
                show_sheet(box_current_sheet(box));
            endif;
        endif;
        set_index(box);
    endif;
enddefine;

define add_box(name) -> box;
    new_option_box(name) ->> box -> option_box(name);
    [^^option_box_names ^name] -> option_box_names;
enddefine;

define remove_box(name);
    delete(name, option_box_names) -> option_box_names;
    false -> option_box(name);
enddefine;

define check_title(title);
    if isstring(title) then
        (title, false);
    elseunless islist(title) then
        mishap(title, 1, 'LIST NEEDED');
    elseunless length(title) == 2 then
        mishap(title, 1, 'LIST OF LENGTH 2 NEEDED');
    else
        dl(title);
    endif;
enddefine;

define proptool_names();
    option_box_names;
enddefine;

define proptool_is_created(name);
    lvars box = option_box(name);
    box and box_widget(box);
enddefine;

define proptool_create(name, parent);
    dlocal box_parent = parent;
    lvars box = option_box(name);
    box and realize_box(box);
enddefine;

define proptool_needs_saving();
    return(sheet_modified_count /== 0);
enddefine;


/* -----------------------------------------------------------------------
    Public Interface
   ----------------------------------------------------------------------- */

    ;;; return a list of all currently-available property sheets, in
    ;;; alphabetical order
define active pop_ui_property_list;
    syssort(
        [% appsheets(sheet_title) %],
        false,
        procedure(x, y);
            if islist(x) then
                islist(y) and (alphabefore(x(1), y(1))
                    or x(1) == y(1) and alphabefore(x(2), y(2)));
            else
                islist(y) or alphabefore(x, y);
            endif;
        endprocedure);
enddefine;

    ;;; Add a new sheet to the available set; optional save_p argument
    ;;; is a procedure for saving the sheet values. Make it visible if
    ;;; display is true.
define pop_ui_add_property(title, fields, display);
    lvars save_p = false;
    if isprocedure(display) then
        ((), title, fields, display) -> (title, fields, display, save_p);
    endif;
    lvars box, (bname, sname) = check_title(title);
    unless option_box(bname) ->> box then
        add_box(bname) -> box;
    endunless;
    lvars sheet = add_sheet(sname, fields, save_p, box);
    if display then
        make_current_sheet(sheet, box);
        show_box(box);
    endif;
enddefine;

    ;;; Remove a sheet from the available set; the display argument is
    ;;; ignored
define pop_ui_remove_property(title, display);
    lvars box, (bname, sname) = check_title(title);
    if option_box(bname) ->> box then
        remove_sheet(sname, box);
        if box_contents(box) == [] then
            hide_box(box);
            remove_box(bname);
        endif;
    endif;
enddefine;

    ;;; Display the option sheet identified by title; parent is the
    ;;; parent widget to use if the containing box is being displayed
    ;;; for the first time
define pop_ui_show_property(title, parent);
    dlocal box_parent = parent;
    lvars box, sheet, (bname, sname) = check_title(title);
    if option_box(bname) ->> box then
        dlocal XptBusyCursorOn = true;
        if sname and box_sheet(sname, box) ->> sheet then
            make_current_sheet(sheet, box);
        endif;
        show_box(box);
    else
        warning(bname, 1, 'No such property sheet');
    endif;
enddefine;

    ;;; Old-style interface: allows the initial sheet to be <false>
    ;;; Interpreted here as meaning show everything
define pop_ui_propertytool(initial_sheet, parent);
    if initial_sheet then
        pop_ui_show_property(initial_sheet, parent);
    else
        dlocal XptBusyCursorOn = true;
        lvars box;
        for _, box in_property option_box do
            show_box(box);
        endfor;
    endif;
enddefine;

    ;;; run save procedures for all sheets marked as needing it
define pop_ui_save_properties();
    appsheets(save_sheet);
enddefine;


/* -----------------------------------------------------------------------
    Saving Properties
   ----------------------------------------------------------------------- */

lconstant START_MSG =
';;; START OF %P OPTIONS SAVED BY pop_ui_save_properties -- DO NOT EDIT\n';
lconstant END_MSG =
';;; END OF %P OPTIONS SAVED BY pop_ui_save_properties\n';

    ;;; use save_p to save the values of a property sheet to a file,
    ;;; typically an init file (init.p, etc.); key is an item
    ;;; identifying the block of saved values: the original contents of
    ;;; the file are preserved except for anything previously saved
    ;;; under the same key
define proptool_save_to_file(key, sheet, file, procedure save_p);
    ;;; get target file name
    sysfileok(file) -> file;
    if sys_fname_path(file) = nullstring then
        ;;; assume it's an init file in $poplib
        (systranslate('poplib') or '~/') dir_>< file -> file;
    endif;
    ;;; generate key strings
    lvars (start_msg, end_msg) =
        procedure();
            dlocal pop_pr_quotes = false;
            (sprintf(START_MSG, [^key]), sprintf(END_MSG, [^key]));
        endprocedure();
    ;;; open a temporary file for saving the values
    lvars tmpfile;
    dlocal 0 % false -> tmpfile, tmpfile and sysdelete(tmpfile) -> %;
    systmpfile(false, 'pop_ui_save', sys_fname_extn(file)) -> tmpfile;
    lvars odev = syscreate(tmpfile, 1, "line", `N`);
    ;;; open any existing file with this name to copy its current
    ;;; contents: mishap if the directory path is invalid, or the file
    ;;; doesn't have read access, etc. but not if the file just doesn't
    ;;; exist -- we could be creating it for the first time
    lvars idev = sysopen(file, 0, "line", `F`);
    if idev then
        ;;; copy current contents to tmpfile, stripping anything
        ;;; previously saved under this key
        lvars n, len = 128, buffer = inits(len), stripping = false;
        until (sysread(idev, buffer, len) ->> n) == 0 do
            if stripping then
                if isstartstring(end_msg, buffer) then
                    ;;; found end of previous save -- discard stripped
                    ;;; lines
                    erasenum(stripping);
                    false -> stripping;
                else
                    ;;; strip this line out, but keep it on the stack
                    ;;; just in case
                    substring(1, n, buffer), stripping + 1 -> stripping;
                endif;
            elseif isstartstring(start_msg, buffer) then
                ;;; start of previously saved block: strip it out
                start_msg, 1 -> stripping;
            else
                syswrite(odev, buffer, n);
            endif;
        enduntil;
        sysclose(idev);
        if stripping then
            ;;; saw the start of a previous save but not the end --
            ;;; better preserve what was there
            lvars line, lines = conslist(stripping);
            for line in lines do
                syswrite(odev, line, length(line));
            endfor;
        endif;
    endif;
    ;;; write out new save block
    syswrite(odev, start_msg, length(start_msg));
    ;;; call supplied save procedure with cucharout redirected to
    ;;; tmpfile
    procedure(odev);
        dlocal cucharout = discout(odev);
        printf(';;; %S\n', [% sysdaytime() %]);
        save_p(key, sheet);
    endprocedure(odev);
    syswrite(odev, end_msg, length(end_msg));
    sysclose(odev);
    ;;; replace original file with new version, keeping at least one
    ;;; backup
    if isinteger(pop_file_versions) and pop_file_versions < 2
    or DEF UNIX and pop_file_versions == false
    then
        dlocal pop_file_versions = 2;
    endif;
    sys_file_move(tmpfile, file);
enddefine;

    ;;; useful local value of pr while saving to init files
define proptool_save_pr(item);
    lvars item;
    dlocal pop_pr_quotes = true, pop_pr_radix = 10;
    if isboolean(item) then
        syspr(item and "true" or "false");
    elseif isword(item) then
        cucharout(`"`), syspr(item), cucharout(`"`);
    else
        syspr(item);
    endif;
enddefine;


/* -----------------------------------------------------------------------
    Initial Sheets
   ----------------------------------------------------------------------- */

lconstant macro (
    POPFILESHEET_TITLE  = 'File',
    POPMEMSHEET_TITLE   = 'Store Management',
    POPVEDSHEET_TITLE   = 'Ved',
);

    ;;; map a sheet title to a sheet widget
define lconstant title_to_widget(title);
    lvars box, sheet, (bname, sname) = check_title(title);
    (option_box(bname) ->> box) and
    (box_sheet(sname, box) ->> sheet) and
    sheet_widget(sheet);
enddefine;

    ;;; converter for boolean <--> yes/no menu choice
define lconstant binary_choice(choice, yes, no);
    choice == yes;
enddefine;
;;;
define updaterof binary_choice(bool, yes, no);
    bool and yes or no;
enddefine;

    ;;; compute the default value for a propsheet field: the idea is to
    ;;; get the value once from the field ident when the sheet is
    ;;; created -- after all user initialisations, we hope -- and then
    ;;; use that same value from then on
define lconstant initial_value(sheet, name) -> default;
    lvars sheet, name, default = propsheet_undef;
    lvars id = propsheet_field_ident(sheet, name);
    if id then
        idval(id) ->> default -> propsheet_field_default(sheet, name);
    endif;
enddefine;


/* Files ... */

    ;;; initial values for the File Tool
define lconstant search_default(sheet, name);
    lvars sheet, name;
    lvars (dir, pattern, /*file*/) = file_search_defaults(false,false,false);
    if name == "pop_ui_file_search_directory" then
        dir
    elseif name == "pop_ui_file_search_pattern" then
        pattern
    else
        nullstring
    endif;
enddefine;

define lconstant search_applier(sheet, name, value);
    lvars sheet, name, value;
    lvars (dir, pattern, /*file*/) = file_search_defaults(false,false,false);
    if name == "pop_ui_file_search_directory" then
        unless value = dir then
            value -> pop_ui_file_search_directory;
        endunless;
    elseif name == "pop_ui_file_search_pattern" then
        unless value = pattern then
            value -> pop_ui_file_search_pattern;
        endunless;
    endif;
enddefine;

    ;;; link file versions and backups fields
define lconstant fileversions_accepter(sheet, name, value) -> value;
    lvars sheet, name, value;
    lvars field = propsheet_field_record(sheet, "backups");
    value -> propsheet_sensitive(field);
enddefine;

define lconstant fileversions_converter(value) -> value;
    lvars value, sheet;
    if value then
        returnunless(title_to_widget(POPFILESHEET_TITLE) ->> sheet)(0 -> value);
        lvars field = propsheet_field_record(sheet, "backups");
        propsheet_field_value(field) -> value;
    endif;
enddefine;
;;;
define updaterof fileversions_converter(value) -> value;
    lvars value, sheet;
    returnunless(title_to_widget(POPFILESHEET_TITLE) ->> sheet)
        (value and true -> value);
    lvars field = propsheet_field_record(sheet, "backups");
    if value ->> propsheet_sensitive(field) then
        value -> propsheet_field_value(field);
        true -> value;
    endif;
enddefine;

define lconstant backups_accepter(sheet, name, value) -> value;
    lvars sheet, name, value;
    if isinteger(value) then
        max(0, value) -> value;
    else
        propsheet_undef -> value;
    endif;
enddefine;

define lconstant backups_default(sheet, name);
    lvars sheet, name;
    ;;; like initial_value, but there's no ident for this field
    pop_file_versions or 2 ->> propsheet_field_default(sheet, name);
enddefine;

    ;;; pop_file_mode (Unix only)
define lconstant filemode_accepter(sheet, name, value) -> value;
    lvars sheet, name, value;
    lvars len = length(value);
    returnif(len == 0 or len > 9)(propsheet_undef -> value);
    uppertolower(value) -> value;
    lconstant all = 'rwxrwxrwx', none = '---------';
    lvars i;
    for i to len do
        lvars c = value(i);
        returnunless(c == all(i) or c == `-`)(propsheet_undef -> value);
    endfor;
    if len < 9 then
        value <> allbutfirst(len, none) -> value;
    endif;
enddefine;

define lconstant filemode_converter(string) -> mode;
    lvars string, mode = 0;
    lvars i, len = length(string);
    for i to 9 do
        mode << 1 -> mode;
        if i <= len and string(i) /== `-` then mode + 1 -> mode endif;
    endfor;
enddefine;
;;;
define updaterof filemode_converter(mode) -> string;
    lconstant modes = {'---' '--x' '-w-' '-wx' 'r--' 'r-x' 'rw-' 'rwx'};
    lvars mode, string =
        modes(((mode >> 6) && 7) + 1) <>
        modes(((mode >> 3) && 7) + 1) <>
        modes((mode && 7) + 1);
enddefine;

    ;;; save sheet values to "init.p" file
define lconstant filesheet_saver(title, sheet);
    lvars title, sheet;
    dlocal pr = proptool_save_pr;
    if isstring(pop_ui_file_search_directory) then
        printf('vars pop_ui_file_search_directory = %p;\n',
            [^pop_ui_file_search_directory]);
    endif;
    if isstring(pop_ui_file_search_pattern) then
        printf('vars pop_ui_file_search_pattern = %p;\n',
            [^pop_ui_file_search_pattern]);
    endif;
    printf('%p -> pop_file_versions;\n', [^pop_file_versions]);
#_IF DEF UNIX
    printf('#_IF DEF pop_file_mode\n');
    printf('8:'), radix_apply(pop_file_mode, sys_syspr, 8), printf('\n');
    printf('#_ENDIF\n');
#_ENDIF
enddefine;

pop_ui_add_property(POPFILESHEET_TITLE, [
    [pop_ui_file_search_directory ^nullstring
        (label = 'Initial search directory', columns = 20,
         default = ^search_default,
         applier = ^search_applier)]
    [pop_ui_file_search_pattern ^nullstring
        (label = 'Initial search pattern', columns = 20,
         default = ^search_default,
         applier = ^search_applier)]
% #_IF DEF UNIX
    [pop_file_mode ^nullstring
        (label = 'File creation mode', columns = 20,
         converter = ^filemode_converter,
         accepter = ^filemode_accepter,
         default = ^initial_value,
         ident = ^(ident pop_file_mode):timer)]
  #_ENDIF
%   [pop_file_versions ^false
        (label = 'File backups',
         converter = ^fileversions_converter,
         accepter = ^fileversions_accepter,
         default = ^initial_value,
         ident = ^(ident pop_file_versions):timer)]
    + [backups 0
        (nolabel, columns = 8, aligned = ^false,
         accepter = ^backups_accepter,
         default = ^backups_default)]
], false, proptool_save_to_file(% 'init.p', filesheet_saver %));

/* Store Management ... */

lconstant
    gc_copy_choices = [Copying 'Non-Copying'],
    gc_copy_converter = binary_choice(% gc_copy_choices.dl %),
;

    ;;; popmemlim and popminmemlim are displayed in Kbytes
lconstant WORDS_PER_KBYTE = 1024/SIZEOFTYPE(:word);
;;;
define lconstant memlim_converter(kb) /* -> words */;
    lvars kb;
    kb * WORDS_PER_KBYTE
enddefine;
;;;
define updaterof memlim_converter(words) /* -> kb */;
    lvars words;
    (words + #_<WORDS_PER_KBYTE/2>_#) div WORDS_PER_KBYTE;
enddefine;

    ;;; ensure popmemlim and popminmemlim can never go below 0
define lconstant memlim_accepter(ps, button, val) -> val;
    lvars ps, button, val, real_val;
    if isreal(val) then
        intof(val) -> val;
        if propsheet_acceptreason == "increment" then
            val + 99 -> val;
        elseif propsheet_acceptreason == "decrement" then
            val - 99 -> val;
        endif;
        ;;; verify new value
        memlim_converter(val) -> real_val;
        if button = "popminmemlim" then
            if real_val > popmemlim then
                popmemlim -> memlim_converter() -> val;
            elseif real_val < 0 then
                0 -> val;
            endif;
        else
            lvars limit = max(popminmemlim, min(popmemused+50000, popmemlim));
            if real_val < limit then
                limit -> memlim_converter() -> val;
            endif;
        endif;
    else
        ;;; illegal value
        propsheet_undef -> val;
    endif;
enddefine;

    ;;; check GC ratio in range 1-64
define lconstant gcratio_accepter(sheet, name, value) -> value;
    lvars sheet, name, value;
    returnunless(isnumber(value))(propsheet_undef -> value);
    intof(value) -> value;
    if propsheet_acceptreason == "increment" then
        value + 4 -> value;
    elseif propsheet_acceptreason == "decrement" then
        value - 4 -> value;
    endif;
    if value <= 0 then
        propsheet_undef -> value;
    elseif value > 64 then
        64 -> value;
    endif;
enddefine;

    ;;; save sheet values to "init.p" file
define lconstant memsheet_saver(title, sheet);
    lvars title, sheet;
    dlocal pr = proptool_save_pr;
    printf(';;; See REF * SYSTEM\n');
    printf('max(%p,popmemlim) -> popmemlim;\n', [^popmemlim]);
    printf('max(%p,popminmemlim) -> popminmemlim;\n', [^popminmemlim]);
    printf('%p -> pop_gc_copy;\n', [^pop_gc_copy]);
    printf('%p -> popgcratio;\n', [^popgcratio]);
    printf('%p -> popgctrace;\n', [^popgctrace]);
enddefine;

pop_ui_add_property(POPMEMSHEET_TITLE, [
    [popmemlim 0 'Kb'
          ( label = 'Maximum heap size', columns = 16,
            converter = ^memlim_converter,
            accepter = ^memlim_accepter,
            default = ^initial_value,
            ident = ^(ident popmemlim):timer,
          )
    ]
    [popminmemlim 0 'Kb'
          ( label = 'Minimum heap size', columns = 16,
            converter = ^memlim_converter,
            accepter = ^memlim_accepter,
            default = ^initial_value,
            ident = ^(ident popminmemlim):timer,
          )
    ]
    [pop_gc_copy menuof ^gc_copy_choices
          ( label = 'Garbage collection algorithm',
            converter = ^gc_copy_converter,
            default = ^initial_value,
            ident = ^(ident pop_gc_copy):timer,
          )
    ]
    [popgcratio 0
          ( label = 'Garbage collection ratio', columns = 8,
            accepter = ^gcratio_accepter,
            default = ^initial_value,
            ident = ^(ident popgcratio):timer,
          )
    ]
    [popgctrace ^false
          ( label = 'Garbage collection trace',
            default = ^initial_value,
            ident = ^(ident popgctrace):timer,
          )
    ]
], false, proptool_save_to_file(% 'init.p', memsheet_saver %));

/* Ved ... */

lconstant
    readin_choices = [Tabs 'Trailing spaces'],
    hardtabs_choices = [Hard Soft],
    hardtabs_converter = binary_choice(% hardtabs_choices.dl %),
    scrollscreen_choices = ['Scroll screen' Redraw],
    scrollscreen_converter = binary_choice(% scrollscreen_choices.dl %),
    statusshowcols_choices = ['Column number' 'Line number'],
    statusshowcols_converter = binary_choice(% statusshowcols_choices.dl %),
;

define lconstant statusbufferlimit_accepter(sheet, field_name, value) -> value;
    lvars sheet, field_name, value;
    unless isinteger(value) and value >= 0 then
        propsheet_undef -> value;
    elseif propsheet_acceptreason == "increment" then
        value + 9 -> value;
    elseif propsheet_acceptreason == "decrement" then
        max(0, value - 9) -> value;
    endunless;
enddefine;

define lconstant autowrite_accepter(sheet, field_name, value) -> value;
    lvars sheet, field_name, value;
    lvars field = propsheet_field_record(sheet, "autowriteNum");
    value -> propsheet_sensitive(field);
enddefine;

define lconstant autowrite_converter(autowrite) -> autowrite;
    lvars autowrite, sheet;
    if autowrite then
        returnunless(title_to_widget(POPVEDSHEET_TITLE) ->> sheet)
            (0 -> autowrite);
        lvars field = propsheet_field_record(sheet, "autowriteNum");
        propsheet_field_value(field) -> autowrite;
    endif;
enddefine;
;;;
define updaterof autowrite_converter(autowrite) -> autowrite;
    lvars autowrite, sheet;
    returnunless(title_to_widget(POPVEDSHEET_TITLE) ->> sheet)
        (autowrite and true -> autowrite);
    lvars field = propsheet_field_record(sheet, "autowriteNum");
    if autowrite ->> propsheet_sensitive(field) then
        autowrite -> propsheet_field_value(field);
        true -> autowrite;
    endif;
enddefine;

define lconstant autowriteNum_accepter(sheet, field_name, value) -> value;
    lvars sheet, field_name, value;
    unless isinteger(value) and value >= 0 then
        propsheet_undef -> value;
    elseif propsheet_acceptreason == "increment" then
        value + 99 -> value;
    elseif propsheet_acceptreason == "decrement" then
        max(0, value - 99) -> value;
    endunless;
enddefine;

define lconstant autowriteNum_default(sheet, name);
    lvars sheet, name;
    vedautowrite or 1500 ->> propsheet_field_default(sheet, name);
enddefine;

define lconstant readin_applier(sheet, name, value);
    lvars sheet, name, value;
    member(readin_choices(1), value) -> vedreadintabs;
    member(readin_choices(2), value) -> vedreadintrailspaces;
enddefine;

define lconstant readin_default(sheet, name) -> default;
    lvars sheet, name;
    lvars default = [%
        if vedreadintabs then readin_choices(1) endif,
        if vedreadintrailspaces then readin_choices(2) endif
    %];
    ;;; always use this default from now on
    default -> propsheet_field_default(sheet, name);
enddefine;

    ;;; save sheet values to "vedinit.p" file
define vedsheet_saver(title, sheet);
    lvars title, sheet;
    dlocal pr = proptool_save_pr;
    printf(';;; See REF * VEDVARS\n');
    printf('%p -> vedautowrite;\n', [^vedautowrite]);
    printf('%p -> vedreadintabs;\n', [^vedreadintabs]);
    printf('%p -> vedreadintrailspaces;\n', [^vedreadintrailspaces]);
    printf('%p -> vedhardtabs;\n', [^vedhardtabs]);
    printf('%p -> vedscrollscreen;\n', [^vedscrollscreen]);
    printf('%p -> vedstatusshowcols;\n', [^vedstatusshowcols]);
    printf('%p -> vedstatusbufferlimit;\n', [^vedstatusbufferlimit]);
enddefine;

pop_ui_add_property(POPVEDSHEET_TITLE, [
    [autowrite ^false
        (label = 'Auto write',
         converter = ^autowrite_converter,
         accepter = ^autowrite_accepter,
         default = ^initial_value,
         ident = ^(ident vedautowrite):timer)]
    + [autowriteNum 0 changes
        (label = 'after', columns = 8, aligned = ^false,
         accepter = ^autowriteNum_accepter,
         default = ^autowriteNum_default)]
    [vedreadin someof ^readin_choices
        (label = 'Preserve in input',
         default = ^readin_default,
         applier = ^readin_applier)]
    [vedhardtabs menuof ^hardtabs_choices
        (label = 'Tab style',
         converter = ^hardtabs_converter,
         default = ^initial_value,
         ident = ^(ident vedhardtabs):timer)]
    [vedscrollscreen menuof ^scrollscreen_choices
        (label = 'Page up/down',
         converter = ^scrollscreen_converter,
         default = ^initial_value,
         ident = ^(ident vedscrollscreen):timer)]
    [vedstatusshowcols menuof ^statusshowcols_choices
        (label = 'Status line display',
         converter = ^statusshowcols_converter,
         default = ^initial_value,
         ident = ^(ident vedstatusshowcols):timer)]
    [vedstatusbufferlimit 0
        (label = 'Command history', columns = 8,
         accepter = ^statusbufferlimit_accepter,
         default = ^initial_value,
         ident = ^(ident vedstatusbufferlimit):timer)]
], false, proptool_save_to_file(% 'vedinit.p', vedsheet_saver %));

/* Pop-11 Printing ... */

define lconstant pop_pr_places_convert(value) -> value;
    ;;; preserve high 16 bits of pop_pr_places
    (pop_pr_places &&~~ 16:FFFF) || value -> value;
enddefine;
;;;
define updaterof pop_pr_places_convert(value) -> value;
    value && 16:FFFF -> value;
enddefine;

define lconstant pop_pr_exponent_convert(value) -> value;
    ;;; preserve possible integer value of pop_pr_exponent
    if value and pop_pr_exponent then pop_pr_exponent -> value endif;
enddefine;
;;;
define updaterof pop_pr_exponent_convert(value) -> value;
    not(not(value)) -> value;
enddefine;

define lconstant pop11_printing_save(title, sheet);
    dlocal pr = proptool_save_pr;
    printf(';;; See REF * PRINT\n');
    printf('%p -> pop_pr_level;\n', [^pop_pr_level]);
    printf('%p -> pop_pr_radix;\n', [^pop_pr_radix]);
    printf('%p -> pop_pr_places;\n', [^pop_pr_places]);
    printf('%p -> pop_pr_exponent;\n', [^pop_pr_exponent]);
    printf('%p -> pop_pr_ratios;\n', [^pop_pr_ratios]);
    printf('%p -> pop_pr_quotes;\n', [^pop_pr_quotes]);
enddefine;

pop_ui_add_property(['Pop-11' 'Printing'], [
    [pop_pr_level 0
        (label = 'Maximum depth',
         default = ^initial_value,
         ident = ^(ident pop_pr_level)]
    [pop_pr_radix 2-36
        (label = 'Output radix',
         default = ^initial_value,
         ident = ^(ident pop_pr_radix)]
    [pop_pr_places 0
        (label = 'Fractional places',
         default = ^initial_value,
         converter = ^pop_pr_places_convert,
         ident = ^(ident pop_pr_places)]
    [pop_pr_exponent ^false
        (label = 'Exponential format',
         default = ^initial_value,
         converter = ^pop_pr_exponent_convert,
         ident = ^(ident pop_pr_exponent)]
    [pop_pr_ratios ^false
        (label = 'Ratio format',
         default = ^initial_value,
         ident = ^(ident pop_pr_ratios)]
    [pop_pr_quotes ^false
        (label = 'Quoted strings',
         default = ^initial_value,
         ident = ^(ident pop_pr_quotes)]
], false, proptool_save_to_file(% 'init.p', pop11_printing_save %));

/* Pop-11 Exceptions ... */

define lconstant popsyscall_convert(value) -> value;
    value and 1 -> value;
enddefine;
;;;
define updaterof popsyscall_convert(value) -> value;
    isinteger(value) -> value;
enddefine;

define lconstant pop_mishap_doing_lim_convert(value) -> value;
    strnumber(value) -> value;
enddefine;
;;;
define updaterof pop_mishap_doing_lim_convert(value) -> value;
    isinteger(value) and value sys_>< nullstring or nullstring -> value;
enddefine;

define lconstant pop11_exceptions_save(title, sheet);
    dlocal pr = proptool_save_pr;
    printf(';;; See REF * EXCEPTION\n');
    printf('%p -> pop_message_min_detail;\n', [^pop_message_min_detail]);
    printf('%p -> pop_mishap_doing_lim;\n', [^pop_mishap_doing_lim]);
    printf('%p -> popsyscall;\n', [^popsyscall]);
enddefine;

pop_ui_add_property(['Pop-11' 'Exceptions'], [
    [pop_message_min_detail 0-5
        (label = 'Minimum detail level',
         default = ^initial_value,
         ident = ^(ident pop_message_min_detail)]
    [pop_mishap_doing_lim ''
        (label = 'Maximum backtrace length',
         default = ^initial_value,
         converter = ^pop_mishap_doing_lim_convert,
         ident = ^(ident pop_mishap_doing_lim)]
    [popsyscall ^false
        (label = 'Show system procedures',
         default = ^initial_value,
         converter = ^popsyscall_convert,
         ident = ^(ident popsyscall)]
], false, proptool_save_to_file(% 'init.p', pop11_exceptions_save %));

/* Pop-11 Compile Mode ... */

include pop11_flags;

lconstant pop11_compile_mode_map = [
    [defcon ^POP11_DEFINE_CONSTANT]
    [defpdr ^POP11_DEFINE_PROCEDURE]
    [global ^POP11_PERM_GLOBAL]
    [varsch ^POP11_VARS_CHECK]
    [oldvar ^POP11_OLD_VARS]
    [lprops ^POP11_NO_LEX_PDPROPS]
];

define lvars active pop11_compile_mode_flags;
    lvars flag, flags = caller_valof(ident pop_pop11_flags, false);
    [%  for flag in pop11_compile_mode_map do
            lvars b = flags &&/=_0 flag(2);
            if flag(1) == "lprops" then not(b) -> b endif;
            if b then flag(1) endif;
        endfor;
    %];
enddefine;
;;;
define updaterof pop11_compile_mode_flags value;
    lvars flag, flags = caller_valof(ident pop_pop11_flags, false);
    for flag in pop11_compile_mode_map do
        lvars b = fast_lmember(flag(1), value);
        if flag(1) == "lprops" then not(b) -> b endif;
        if b then flags || flag(2) else flags &&~~ flag(2) endif -> flags;
    endfor;
    set_global_valof(flags, ident pop_pop11_flags);
enddefine;

define lconstant pop11_compile_mode_save(title, sheet);
    printf(';;; See HELP * COMPILE_MODE\n');
    printf('compile_mode global :pop11');
    lvars flag, value = pop11_compile_mode_flags;
    for flag in pop11_compile_mode_map do
        printf('\s%c%p', [% member(flag(1), value) and `+` or `-`, flag(1) %]);
    endfor;
    printf(';\n');
enddefine;

pop_ui_add_property(['Pop-11' 'Compile Mode'], [
    [pop_pop11_flags someof [defcon global varsch] [defpdr lprops oldvar]
        (label = 'Flags for pop11',
         default = ^initial_value,
         ident = ^(ident pop11_compile_mode_flags))]
], false, proptool_save_to_file(% 'init.p', pop11_compile_mode_save %));

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 20 1996
        Changed show_sheet to allow sheet_fields to be a procedure that
        returns the list of fields.
--- Robert Duncan, Jul 23 1996
        Made OptionBox and OptionSheet classes writeable by default and
        added other writeable declarations as needed...
--- Robert Duncan, Jul 17 1996
        Rewritten to allow for multiple propsheet boxes, to support an
        increasing number of options sheets. An individual sheet is now
        identified by a pair of strings ['box-title' 'sheet-title'].
        Added initial sheets for Pop-11 options.
--- Robert John Duncan, Jun  7 1995
        Fixed dlocal in proptool_save_to_file which deletes the temporary
        file on exit; added missing uses for pop_ui_choose_file which
        defines identifiers on the Files option sheet
--- John Gibson, May 15 1995
        Fixed memlim_converter to use general pop words <-> kbytes conversion
        constant.
--- Robert John Duncan, May  4 1995
        Added support for saving sheet changes to a file. Changed the layout
        of the Files and Store Management sheets and added a new Ved sheet.
--- Robert John Duncan, Apr  4 1995
        Thorough revision of the way the tool is organised
--- Robert John Duncan, Apr  3 1995
        Changed pop_ui_propertytool so that it doesn't redisplay the
        initial_sheet if it's already displayed as the current_property.
        Also now respects changes to current_property made before the tool
        is initialised.
--- Integral Solutions Ltd, Jan 10 1994
        Modified previous fix for VMS.
--- Julian Clinton, Jan  5 1994
        Fixed problem with file versions and 'Set Defaults'.
--- John Gibson, Nov  9 1993
        Corrected propsheet_f*ield_sensitive to propsheet_sensitive
--- Integral Solutions Ltd (Julian Clinton), Sep 22 1993
        Modified pop_file_versions display so it can handle false as well
            as integer values.
        Minor mods to string labels.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Aug 27 1992
        Added -writeable- before calls of syssort
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Added check that memlim_accepter is actually being passed a number.
    Added conditional compilation so the -pop_ui_propertytool- symbol is
    only defined if OPEN LOOK or Motif have been added.
Julian Clinton, 14/10/91
    Set busy cursor on during property tool creation.
Julian Clinton, 11/10/91
    Changed heap size strings to be macros.
--- Jonathan Meyer, Sep 16 1991 Rewrote memlim accepter/converter. Tidied.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
--- Jonathan Meyer, Sep  5 1991
        Fixed bug in create_sheet. 'Property' -> 'Settings'.
        'Property Tool' -> 'Properties'
--- Integral Solutions Ltd, Sep  3 1991 (Julian Clinton)
    Added check in -memlim_accepter- to make sure strings are converted
    to numbers.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 9/8/91
    Added converters and accepter functions for memory limits.
Julian Clinton, 2/8/91
    Added propsheet_refresh to make sure displayed values are correct.
Julian Clinton, 19/7/91
    Removed display arg form pop_ui_propertytool and restructured the
    system to allow properties to be added and removed from the tool
    before pop_ui_propertytool has been called.
Julian Clinton, 18/7/91
    Added display argument to pop_ui_propertytool which defines whether
    to display the fields initially.
 */
