/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.x/x/ved/src/xveddialogs.p
 > Purpose:         Basic XVed dialog boxes
 > Author:          Jonathan Meyer, Jun 27 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now pop_ui;

;;; make this false to replace Xved's 'really quit' dialogue box
;;; with plain text dialogue on status line
global vars vedquitpanel = true;

section $-xved =>
        ved_writemr,
        ved_loadlibrary,
        ved_gethelp,
        ved_searchfor,
        ved_replace,
        ved_properties,
        ved_get_reply,
        xveddialogs,
;

#_IF DEF POPC_COMPILING
#_INCLUDE '$usepop/pop/x/ui/lib/popc_declare.ph'
#_ENDIF

include vedsearchdefs;

uses
    pop_ui_propertytool,
    pop_ui_prompttool,
    pop_ui_vedcomms,
;

;;; LIST OF PAIRS. FRONT OF PAIR CONTAINING WIDGET WHICH IS TO BE
;;; SET INSENSITIVE WHEN EDITOR EXITS, BACK CONTAINS OLD
;;; SENSITIVITY VALUE WHICH IS RESTORED ON EDITOR ENTRY
lvars editor_sensitive_widgets = [];

define lconstant set_focus(sheet,field, value) -> value;
    lvars sheet field value;
    if propsheet_acceptreason == "activate" then
        propsheet_set_focus_on(sheet, 2);
    endif;
enddefine;

define lconstant activate(sheet,field, value) -> value;
    lvars sheet field value;
    ;;; simulate pushing the first button
    if propsheet_acceptreason == "activate" then
        propsheet_activate(sheet, 1);
    endif;
enddefine;

;;; SET THE FOREGROUND AND BACKGROUND COLOURS OF THE XVED
;;; DIALOG PROPBOX w
define lconstant set_xved_dialog_colours(w);
    lvars w;
    lconstant names = [DialogForeground DialogBackground];
    dlocal xveddialogslist = [^w];
    ;;; force setting for w only
    xved_value("application", names) -> xved_value("application", names);
enddefine;

;;; RETURNS A PROCEDURE WHICH SETS THE SENSITIVITY OF THE BUTTONS IN THE LIST
;;; -buttons- OF -box-. IF THE FIELD -field- OF THE PROPSHEET -sheet- EQUALS
;;; -value- THEN THEY ARE SET UNSENSITIVE, OTHERWISE THEY ARE SET SENSITIVE
;;; INTENDED TO BE USED AS A TIMER.
define lconstant make_buttons_timer(sheet, field, value, box, buttons);
    lvars sheet, field, value, box, buttons;
    lvars field_had_value = not(sheet(field) = value);

    procedure;
        lvars button, field_has_value = (sheet(field) = value);
        if field_had_value and not(field_has_value) then
            false -> field_had_value;
            for button in buttons do;
                true -> propsheet_sensitive(box, button);
            endfor;
        elseif not(field_had_value) and field_has_value then
            true -> field_had_value;
            for button in buttons do;
                false -> propsheet_sensitive(box, button);
            endfor;
        endif;
    endprocedure;

enddefine;

    ;;; Old synonyms
define ved_writemr;     ved_writerangeto() enddefine;
define ved_loadlibrary; ved_librarytool() enddefine;
define ved_gethelp;     ved_helptool() enddefine;


/* ==== Misc. Variables Used By Search (& Replace) Dialogs ========== */

lvars
    search_box = false,     ;;; PROPBOX FOR SEARCH DIALOG
    search_sheet = false,   ;;; PROPSHEET FOR SEARCH DIALOG
    search_button_timer;    ;;; TIMER TO SET SENSITIVITY OF -search_box- BUTTONS


lvars
    replace_box = false,    ;;; PROPBOX FOR S&R DIALOG
    replace_sheet = false,  ;;; PROPSHEET FOR S&R DIALOG
    replace_button_timer;   ;;; TIMER TO SET SENSITIVITY OF -replace_box- BUTTONS

lconstant macro (
    WHOLE_WORDS = 'Whole Words',
    ATTACHED_TO = 'Attached To',
);


define lconstant get_srch_string() -> string;
    lvars string = subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata);
    unless isstring(string) then false -> string endunless
enddefine;

define lconstant get_subs_string() -> string;
    lvars string = subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata);
    unless isstring(string) then false -> string endunless
enddefine;

;;; REMOVE @a AND @z CHARACTERS FROM A SEARCH STRING RETURNING NEW STRING AND
;;; LIST OF ATTACHMENTS SUITABLE FOR PROPSHEET FIELD
define lconstant get_search_params(string) -> (string, attachments);
    lvars string, attachments = [];
    if isstartstring('@a', string) then
        allbutfirst(2, string) -> string;
        "Left" :: attachments -> attachments;
    endif;
    if isendstring('@z', string) then
        allbutlast(2, string) -> string;
        "Right" :: attachments -> attachments;
    endif;
enddefine;


;;; THE LAST SEARCH STRING. CONTROLS THE "Search For",
;;; "Replace" AND "Attached To" FIELDS
lvars
    old_srch_string         = '',
    old_last_search_string  = '',
    search_string_changed   = false,
    ;
;;;
define lconstant active last_search_string;
    lvars string = get_srch_string(), attachments;
    if old_srch_string /== string and string then
        get_search_params(string ->> old_srch_string)
                        -> (old_last_search_string, attachments);
        if search_sheet then attachments -> search_sheet(ATTACHED_TO) endif;
        if replace_sheet then attachments -> replace_sheet(ATTACHED_TO) endif;
        true
    else
        false
    endif -> search_string_changed;
    old_last_search_string;
enddefine;
;;;
define updaterof active last_search_string(val);
    lvars val;
enddefine;


;;; THE LAST REPLACE STRING. CONTROLS THE "With" FIELD, THIS IS SET TO
;;; THE SEARCH STRING IF -last_search_string- CHANGES WHILE -last_replace_string-
;;; STAYS THE SAME (ie, the user has done a straight search.)
;;;
lvars
    old_subs_string         = '',
    old_last_replace_string = '',
    replace_string_changed  = false,
    ;
;;;
define lconstant active last_replace_string;
    lvars string = get_subs_string();
    if  search_string_changed
        and not(replace_string_changed)
        and string == old_subs_string
    then
        false -> search_string_changed;
        true -> replace_string_changed;
        last_search_string ->> old_last_replace_string;
    elseif string == old_subs_string or not(string) then
        old_last_replace_string;
        false -> replace_string_changed;
    else
        string
            ->> old_subs_string
            ->> old_last_replace_string
            ->> replace_string_changed;
    endif;
enddefine;
;;;
define updaterof active last_replace_string(val);
    lvars val;
enddefine;

;;; TRUE IF LAST SEARCH ON ITEM BOUNDARY
;;; CONTROLS THE "Whole Words" FIELDS OF S&R DIALOGS
define lconstant active search_whole_words;
    not(subscrv(VEDSRCH_ANYWHERE, vedsearchdata))
enddefine;
;;;
define updaterof active search_whole_words(val);
    lvars val;
enddefine;

define lconstant set_timer_field(id, sheet, arg);
    lvars id, sheet, arg;
    "timer" -> propsheet_ident_class(id);
    id -> propsheet_field_ident(sheet, arg);
enddefine;


/* ==== Search ========================================================= */

define lconstant search_cb(box, button) -> ok;
    lvars box button name vfc ok = false,
            whole_file, use_case, embedded, go_forwards, search_char, attached;

    search_sheet(1) -> name;

    if button /== "Dismiss" and search_sheet('Find Procedure') then
        ;;; finding a procedure
        vedinput(veddo(%'f ' sys_>< name, true%));
    endif;

    search_sheet('Case Sensitive') -> use_case;
    search_sheet(ATTACHED_TO) -> attached;
    not(search_sheet(WHOLE_WORDS)) -> embedded;
    search_sheet('Search Direction') == "Forwards" -> go_forwards;
    if button == "Search" then
        if use_case then
            if go_forwards then
                if embedded then `/` else `"` endif
            else
                if embedded then `\\`  else ``` endif
            endif -> search_char;
            consstring(#|search_char,
                    if attached and lmember("Left", attached) then
                        `@`, `a`
                    endif;
                    explode(name),
                    if attached and lmember("Right", attached) then
                        `@`, `z`
                    endif;
                    search_char|#) -> name;
        else
            ;;; caseless search - NB ignores search direction/attached
            if embedded then 'ss ' else 'ww ' endif sys_>< name -> name;
        endif;
        vedinput(veddo(%name,true%));
    elseif button == "Repeat" then
        if use_case then
            vedinput(
                if go_forwards then
                    ved_re_search
                else
                    ved_re_backsearch
                endif);
        else
            vedinput(valof("vedcllocate")(%nullstring, embedded%));
        endif;
    else
        true -> ok;
        false -> sys_timer(search_button_timer, 2:100);
        propsheet_hide(box);
    endif;
enddefine;

define vars ved_searchfor;
    lvars string;
    lconstant sdir = 'Search Direction';

    define lconstant case_accepter(sheet, field, value) -> value;
        lvars sheet field value;
        if value then
            ;;; Case Sensitive
            true -> propsheet_sensitive(sheet, sdir, "Backwards");
            true -> propsheet_sensitive(sheet, ATTACHED_TO);
            propsheet_reset(sheet, sdir);
        else
            ;;; ignore case
            propsheet_save(sheet, sdir);
            "Forwards" -> sheet(sdir);
            false -> propsheet_sensitive(sheet, sdir, "Backwards");
            false -> propsheet_sensitive(sheet, ATTACHED_TO);
        endif;
    enddefine;

    define lconstant proc_accepter(sheet, field, value) -> value;
        lvars sheet, field, value, i;
        if value then
            fast_for i from 3 to 6 do
                propsheet_save(sheet, i);
                false -> propsheet_sensitive(sheet, i);
            endfast_for;
        else
            fast_for i from 3 to 6 do
                true -> propsheet_sensitive(sheet, i);
                propsheet_reset(sheet, i);
            endfast_for;
        endif;
    enddefine;

    unless search_sheet then
        (   propsheet_new_box(
                'Ved:Search', xveddummyshell, search_cb, [Search Repeat Dismiss]
            ) ->> search_box
        ) :: xveddialogslist -> xveddialogslist;

        propsheet_new(false, search_box, [
            ['Search For' '' (accepter = ^activate)]
            ['Find Procedure' false (accepter = ^proc_accepter)]
            [^sdir [Forwards Backwards]]
            [^ATTACHED_TO {Left Right} (default = [])]
            ['Case Sensitive' true (accepter = ^case_accepter)]
            [^WHOLE_WORDS false]

        ]) -> search_sheet;
        propsheet_show(search_sheet);
        set_xved_dialog_colours(search_box);

        ;;; TIMER TO ENSURE SEARCH BUTTONS INSENSITIVE IF NO SEARCH STRING
        make_buttons_timer(search_sheet, 1, '', search_box, [Search Repeat])
            -> search_button_timer;

        ;;; EVERYTHING EXCEPT THE DISMISS BUTTON INSENSITIVE WHEN NOT EDITING
        conspair(search_sheet, false) ::
            (conspair(propsheet_subpart(search_box, "Search"), false) ::
            (conspair(propsheet_subpart(search_box, "Repeat"), false) ::
            editor_sensitive_widgets))
                -> editor_sensitive_widgets;

        ;;; MAKE SURE FIELDS ARE UPDATED BY NON-DIALOG SEARCHES
        set_timer_field(ident search_whole_words, search_sheet, WHOLE_WORDS);
        set_timer_field(ident last_search_string, search_sheet, 1);
    endunless;

    search_whole_words -> search_sheet(WHOLE_WORDS);
    if get_srch_string() ->> string then
        get_search_params(string)
                -> (search_sheet(1), search_sheet(ATTACHED_TO))
    endif;

    XptCenterWidgetOn(search_box, "screen");
    1e6 -> sys_timer(search_button_timer, 2:100);
    propsheet_show(search_box);
enddefine;

/* ==== Search And Replace =========================================== */

define lconstant replace_cb(box, button) -> ok;
    lvars box button name newname vfc ok = false,
            embedded, interactive, attached, using, search_char;


    define lconstant explode_string(string, char, attached);
        lvars string, c, char, attached;
        if attached and lmember("Left", attached) then `@`, `a`, endif;
        if locchar(char,1,string) then
            fast_for c in_string string do
                if c == char then `\\`, c else c endif;
            endfast_for;
        else
            explode(string)
        endif;
        if attached and lmember("Right", attached) then `@`, `z`, endif;
        char;
    enddefine;

    replace_sheet("Replace") -> name;
    replace_sheet("With") -> newname;
    replace_sheet(WHOLE_WORDS) -> embedded;
    replace_sheet('Ask First') -> interactive;
    replace_sheet("In") -> using;
    replace_sheet(ATTACHED_TO) -> attached;
    if button /== "Dismiss" then
        if embedded then `"` else `/` endif -> search_char;
        if interactive then
            's'
        elseif using == "Line" then
            'gsl'
        elseif using == "Range" then
            'gsr'
        elseif using == "Procedure" then
            'gsp'
        else
            'gs'
        endif -> using;
        consstring(#|explode(using), search_char,
                explode_string(name, search_char, attached),
                explode_string(newname, search_char, false) |#) -> name;
        vedinput(
            if interactive then
                vedsetonscreen(%ved_current_file, ''%) <> veddo(%name,true%)
            else
                veddo(%name, true%)
            endif
        );
    else
        false -> sys_timer(replace_button_timer, 2:100);
        true -> ok;
        propsheet_hide(box);
    endif;
enddefine;

define vars ved_replace;
    lvars string;

    define lconstant ask_accepter(sheet, field, value) -> value;
        lvars sheet field value;
        if value then
            propsheet_save(sheet, "In");
            "File" -> sheet("In");
        endif;
        not(value) ->> propsheet_sensitive(sheet, "In", "Line");
                ->> propsheet_sensitive(sheet, "In", "Range");
                -> propsheet_sensitive(sheet, "In", "Procedure");
        ;;; need to do this after making all the fields sensitive
        unless value then
            propsheet_reset(sheet, "In");
        endunless;
    enddefine;

    unless replace_sheet then
        (   propsheet_new_box(
                'Ved:Replace', xveddummyshell, replace_cb, ['Search and Replace' Dismiss]
            ) ->> replace_box
        ) :: xveddialogslist -> xveddialogslist;

        propsheet_new(false, replace_box, [
            [Replace '' (accepter = ^set_focus)]
            [With '' (accepter = ^activate)]
            [In [Line Range Procedure File]]
            [^ATTACHED_TO {Left Right} (default = [])]
            [^WHOLE_WORDS false]
            ['Ask First' false (accepter = ^ask_accepter)]
        ]) -> replace_sheet;

        propsheet_show(replace_sheet);
        set_xved_dialog_colours(replace_box);

        ;;; TIMER TO ENSURE SEARCH BUTTONS INSENSITIVE IF NO SEARCH STRING
        make_buttons_timer(replace_sheet, "Replace", '', replace_box, [1])
            -> replace_button_timer;

        ;;; EVERYTHING EXCEPT THE DISMISS BUTTON INSENSITIVE WHEN NOT EDITING
        conspair(replace_sheet, true) ::
            (conspair(propsheet_subpart(replace_box, 1), true) ::
                editor_sensitive_widgets
            ) -> editor_sensitive_widgets;

        ;;; MAKE SURE FIELDS ARE UPDATED BY NON-DIALOG SEARCHES
        set_timer_field(ident search_whole_words, replace_sheet, WHOLE_WORDS);
        set_timer_field(ident last_search_string, replace_sheet, "Replace");
        set_timer_field(ident last_replace_string, replace_sheet, "With");
    endunless;

    search_whole_words -> replace_sheet(WHOLE_WORDS);
    if get_srch_string() ->> string then
        get_search_params(string)
                -> (replace_sheet(1), replace_sheet(ATTACHED_TO))
    endif;
    if get_subs_string() ->> string then
        string -> replace_sheet("With");
    endif;

    XptCenterWidgetOn(replace_box, "screen");
    1e6 -> sys_timer(replace_button_timer, 2:100);
    propsheet_show(replace_box);
enddefine;

/* XVed redefinition of ved_get_reply to prompt the user using a dialog */

lvars procedure old_get_reply;

define lconstant get_reply(mesg_string, answers) -> char with_props ved_get_reply;
    lvars mesg_string answers char, n;
    dlocal pop_ui_promptsource;
    if xvedvanilla or not(vedquitpanel) then
        chain(mesg_string, answers, old_get_reply)
    endif;

    if vedusewindows == "x" and answers = 'ync' then
            dlocal XptBusyCursorOn = true;
            wvedwindow -> pop_ui_promptsource;
            if isstartstring('FILE CHANGED', mesg_string) then
                'File changed. \nDo you wish to save your edits' -> mesg_string;
            elseif isstartstring('FILES CHANGED', mesg_string) then
                'You have unsaved edits.\nDo you wish to save them?' -> mesg_string;
            endif;
            pop_ui_prompttool('Ved:Warning', "question",
                mesg_string,true, [Yes No Continue], 1) -> (char,);
            uppertolower(char(1)) -> char;
    else
        chain(mesg_string, answers, old_get_reply);
    endif;
enddefine;

/* ===== Property Sheets ============================================== */

lconstant id_refresh_list = [
    ;;; list of identifiers whose property fields need explicit
    ;;; refreshing (rather than using a timer)
];

lvars properties_set_up = false;
;;;
define xved_dialogs_init(id);
    lvars id;

        ;;; copied from "pop_ui_propertytool.p"
    define lconstant initial_value(sheet, name) -> default;
        lvars sheet, name, default = propsheet_undef;
        lvars id = propsheet_field_ident(sheet, name);
        if id then
            idval(id) ->> default -> propsheet_field_default(sheet, name);
        endif;
    enddefine;

    define lconstant timeout_accept(sheet, name, val) -> val;
        lvars sheet, name, val;
        if isnumber(val) then
            if propsheet_acceptreason == "increment" then
                val + 49 -> val;
            elseif propsheet_acceptreason == "decrement" then
                val - 49 -> val;
            endif;
            max(1, min(round(val), 1000)) -> val;
        else
            propsheet_undef -> val;
        endif;
    enddefine;

        ;;; get the initial value of a "defaultWindow" property
    define lconstant xvedwin_default(sheet, name) -> default;
        lvars sheet, name;
        lvars default = xved_value("defaultWindow", name);
        lvars current = propsheet_field_value(sheet, name);
        unless datakey(default) == datakey(current) then
            if isstring(current) then
                nullstring
            elseif isinteger(current) then
                0
            else
                current
            endif -> default;
        endunless;
        ;;; use same value as default from now on
        default -> propsheet_field_default(sheet, name);
    enddefine;

        ;;; set a property on the current/default window
    define lconstant xvedwin_applier(sheet, name, val);
        lvars sheet, name, val;
        if val = nullstring or isinteger(val) and val <= 0 then
            ;;; null selection -- unset any previous choice
            "undef" -> xved_value("defaultWindow", name);
        else
            if val = 'false' then false -> val endif;
            val -> xved_value("defaultWindow", name);
            if wved_is_live_window(wvedwindow) then
                val -> xved_value("currentWindow", name);
            endif;
        endif;
    enddefine;

        ;;; save XVed resource values to 'vedinit.p' file: type may be
        ;;; "application" or "defaultWindow"
    define lconstant xvedprop_saver(title, sheet, type);
        lvars title, sheet, type;
        dlocal pr = $-poplog_ui$-proptool_save_pr;
        printf(';;; See REF * XVED\n');
        ;;; xved_value is generally unsafe to use unless xvedsetup has
        ;;; been called
        printf('#_IF DEF xved and vedusewindows == "x"\n');
        lvars i, name;
        for i to propsheet_length(sheet) do
            if isword(propsheet_field_name(sheet,i) ->> name) then
                ;;; name is the resource name
                lvars val = xved_value(type, name);
                if isstring(val) and length(val) > 0
                or isinteger(val) and val > 0
                or isboolean(val)
                then
                    printf('%p -> xved_value(%p,%p);\n', [
                        ^val ^type ^name
                    ]);
                endif;
            endif;
        endfor;
        printf('#_ENDIF\n');
    enddefine;

    define XVed_fields();
        lconstant PAD = '\s\s\s\s\s'; ;;; only way to get horizontal space?
        [
        [AutoWindowPlacement ^false
            (label = 'Auto Window Placement',
             default = ^initial_value,
             ident = ^(ident xvedautoplace):timer)]
        [AlwaysRaiseWindow ^false
            (label = 'Always Raise Window',
             default = ^initial_value,
             ident = ^(ident wvedalwaysraise):timer)]
        [SetInputFocus ^false
            (label = 'Set Input Focus',
             default = ^initial_value,
             ident = ^(ident xvedsetfocus):timer)]
        +['space1' message ^PAD (nolabel)]
        +['label1' message 'MultiClick TimeOut (msecs)' (nolabel)]
        [WarpPointer ^false
            (label = 'Warp Pointer',
             default = ^initial_value,
             ident = ^(ident xvedwarpmouse):timer)]
        +['space2' message ^PAD (nolabel)]
        +[MultiClickTimeOut 0
            (nolabel, columns = 16,
             accepter = ^timeout_accept,
             default = ^initial_value,
             ident = ^(ident xvedclicktime):timer)]
        [ShowFileName ^false
            (label = 'Show File Name',
             default = ^initial_value,
             ident = ^(ident xvedshowfilename):timer)]
        +['space3' message ^PAD (nolabel)]
        +['label2' message 'Max Windows' (nolabel)]
        [SetLineBreakOnResize ^false
            (label = 'Set Line Break On Resize',
             default = ^initial_value,
             ident = ^(ident wvedbreaktofit):timer)]
        +['space4' message ^PAD (nolabel)]
        +[MaxWindows 0
            (nolabel, columns = 16,
             default = ^initial_value,
             ident = ^(ident xvedmaxwindows):timer)]
        [SearchDoesSelect ^false
            (label = 'Search Does Select',
             default = ^initial_value,
             ident = ^(ident vedsearchdoesselect):timer)]
        [AutoCut ^false
            (label = 'Auto Cut',
             default = ^initial_value,
             ident = ^(ident xvedautocut):timer)]
        ]
    enddefine;

    define XVedWindow_fields();
        [
        [numRows 0 (label = 'Number of Rows',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [numColumns 0 (label = 'Number of Columns',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [foreground ^nullstring (label = 'Foreground',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [background ^nullstring (label = 'Background',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [statusForeground ^nullstring (label = 'Status Foreground',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [statusBackground ^nullstring (label = 'Status Background',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [fontSet ^nullstring (label = 'Font Set',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [boldFontSet ^nullstring (label = 'Bold Font Set',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [altFontSet ^nullstring (label = 'Italic Font Set',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [boldAltFontSet ^nullstring (label = 'Bold Italic Font Set',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        [pointerShape ^nullstring (label = 'Pointer Shape',
                default = ^xvedwin_default,
                applier = ^xvedwin_applier)]
        ]
    enddefine;

    if id then
        ;;; called from xved_value when changing an identifier
        if properties_set_up and fast_lmember(id, id_refresh_list) then
            ;;; currently, these are all done with timers so don't need
            ;;; refreshing
            ;;; propsheet_refresh(id);
        endif;
        return
    endif;

    returnif(properties_set_up);

    ved_get_reply -> old_get_reply;
    get_reply -> ved_get_reply;

    ;;; XVed application resources
    pop_ui_add_property(
        ['Ved' 'XVed'], XVed_fields, false,
        $-poplog_ui$-proptool_save_to_file(% 'vedinit.p',
                                        xvedprop_saver(%"application"%) %));

    ;;; XVed current/default window resources
    pop_ui_add_property(
        ['Ved' 'XVed Window'], XVedWindow_fields, false,
        $-poplog_ui$-proptool_save_to_file(% 'vedinit.p',
                                        xvedprop_saver(%"defaultWindow"%) %));

    true -> properties_set_up;
enddefine;


lvars shell = false;
;;;
define vars ved_properties;
    lvars sheet;
    if vedargument = nullstring then
        ['Ved' 'XVed Window']
    elseif vedargument = 'XVed' or vedargument = 'XVed Window' then
        ;;; old-style names
        ['Ved' ^vedargument]
    else
        vedargument
    endif -> sheet;
    unless shell then
        XtAppCreateShell('poplog', 'Poplog',
                        xtApplicationShellWidget,
                        xveddisplay,
                        XptArgList([{mappedWhenManaged ^false}]) ) -> shell
    endunless;
    pop_ui_propertytool(sheet, shell);
    applist(id_refresh_list, propsheet_refresh);
enddefine;


/* ===== Misc Dialogs ================================================== */

;;; SET THE SENSITIVITY OF EDITOR SENSITIVE WIDGETS ON ENTRY/EXIT

procedure;
    lvars pair, widget;
    fast_for pair in editor_sensitive_widgets do;
        XtIsSensitive(fast_front(pair) ->> widget) -> fast_back(pair);
        XtSetSensitive(widget, false)
    endfor;
endprocedure
<> xved_before_editor_exit -> xved_before_editor_exit;

procedure;
    lvars widget_and_value;
    fast_for widget_and_value in editor_sensitive_widgets do;
        XtSetSensitive(fast_destpair(widget_and_value));
    endfast_for;
endprocedure
<> xved_after_editor_entry -> xved_after_editor_entry;

constant xveddialogs = true;    ;;; signature for dialogs loaded

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 26 Jul 2003
        Added vedquitpanel. Make it false to suppress 'really quit' dialogue box
        Instead use command line.
--- John Gibson, Apr 30 1997
        Changed font to fontSet etc in XVedWindow_fields.
--- John Gibson, Nov 20 1996
        Changed xved_dialogs_init to pass procedures to pop_ui_add_property
        that return the field lists (means the field lists don't get created
        until the pui is actually used).
--- Robert Duncan, Jul 17 1996
        Changes for revised organisation of UI options sheets
--- John Gibson, Jul 28 1995
        Old ved_ synonyms made into proper procedures (necessary for Popc)
--- Robert John Duncan, Jun  7 1995
        pop_ui_vedcomms now exported
--- Robert John Duncan, May  4 1995
        Changes to UI XVed and XVed Window options sheets: different
        selection of fields; changes can be saved to "vedinit.p";
        identifiers in the XVed sheet use timers for updates rather than
        explicit refresh; fields in the XVed Window sheet affect both
        current and default windows.
        Most Ved commands moved out to new "pop_ui_vedcomms.p" since they
        use UI facilities rather than XVed ones.
--- Robert John Duncan, Apr  6 1995
        Changed calls to pop_ui_add_property to have a display argument of
        <false> so that the new sheets aren't displayed until explicitly
        requested
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply, and removed all following
        Xpt*SetXtWakeups (no longer necessary)
--- Julian Clinton, Dec 10 1993
        Added ident xvedsetfocus to id_refresh_list and added 'Set Input
        Focus' toggle to XVed property sheet.
--- John Gibson, Nov  8 1993
        Changed to use new Ved search stuff
--- John Gibson, Jun  4 1993
        Got rid of compile-time "timer" assignments to propsheet_ident_class
        and made them be done at run-time instead. Similarily got rid of all
        uses of propsh*eet_id.
--- Adrian Howard, Feb 15 1993
        User data fixed for "x" option in XVed Window property.
--- Adrian Howard, Sep 23 1992
        o The replace string is set to the search string if the user does
        a straight search
        o Search and replace dialogs now stay up under Motif untill the Dismiss
        button is pressed
--- Adrian Howard, Sep 18 1992
        Search and replace dialog box fields are now updated by user
        searches on the status line
--- Adrian Howard, Sep 17 1992
        o -xved_before|after_editor_exit|entry- now alters the sensitivity of
        the entire search and replace dialogs, including the buttons.
        o Search and replace buttons now insensitive if there is no search
        string
--- Adrian Howard, Sep 16 1992
        The search and replace dialogs now restore there fields from
        previous search and replace calls in VED.
--- Adrian Howard, Aug 25 1992
        Made XVed specific dialog boxes initialize to DialogFore/Background
--- Adrian Howard, Aug 20 1992
        Added updates of -xvedialogs- (used in xvedresources.p)
--- Adrian Howard, Jul 28 1992
        Now uses -xveddisplay- instead of -XptDefaultDisplay-
--- John Gibson, Jul 24 1992
        Made ved_get_reply be redefined only if xved_dialogs_init is called
--- John Gibson, Mar 28 1992
        Renamed init procedure as xved_dialogs_init, called from xvedsetup
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Set default multi-click time to 200.
    Commented out scrollbar and menubar properties.
    Commented out current VED buffer variables.
    Removed explicit string null terminators.
--- Jonathan Meyer, Sep 26 1991
        win_applier now does nothing in wvedwindow is not a valid
        window.
--- Jonathan Meyer, Sep 26 1991
        Caseless search now works via <ENTER> ww and <ENTER> ss.
        It also sets 'Attached To' insensitive.
--- Jonathan Meyer, Sep 25 1991
        Properties now initialised by xvedsetup
--- Jonathan Meyer, Sep 16 1991 Added 'Attached To' and 'Find Procedure' to
        search dialogs. Added several new property sheets
--- Jonathan Meyer, Sep  5 1991 Now uses pop_ui_prompttool.
--- Jonathan Meyer, Sep  2 1991 Rewrote in terms of pui
--- Adrian Howard, Aug  2 1991 : Fixed 'Insert Range', 'Write Range' and
        'Write File'.
--- Jonathan Meyer, Jul 31 1991 Added editor entry/exit actions
--- Adrian Howard, Jul 29 1991 : Removed references to UserLevel resource
--- Jonathan Meyer, Jul 27 1991
        Added xved_before_editor_exit
--- Jonathan Meyer, Jul  8 1991
        Added set_focus
--- Jonathan Meyer, Jul  5 1991
        Made search and search/replace dialogs set sensitivity of unavailable
        fields
--- Jonathan Meyer, Jul  3 1991
        Changed to use the optional extra arg of veddo to place the command
        on the status line.
 */
