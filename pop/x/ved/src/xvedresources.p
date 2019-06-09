/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedresources.p
 > Purpose:         Customization of XVed via resources
 > Author:          Jonathan Meyer, Apr  6 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

include xpt_xtypes.ph;
include xpt_generaltypes.ph;
include xved_constants.ph;
include xveddefs.ph;

section $-xved;

weak constant $-xveddialogs;
weak constant procedure (xved_dialogs_init, get_xm_colours, xved_popup_menu);
weak vars $-vedxvedmenubar, $-vedxvedmenu, xvedpopupmenushell;


;;; THESE WILL HOLD THE SHADOW AND SELECT
;;; COLOURS FOR A MOTIF WIDGET WHEN ITS
;;; BACKGROUND HAS BEEN UPDATED
lvars top_shadow_color, bottom_shadow_color, select_color;

lconstant
    UNSET               = consundef("value"),
    WINDOW_BASE_LABEL   = 'Ved - ',
    NO_PIXEL            = 16:FFFFFFFF,      ;;; n.b. this is a bigint ...
;

lconstant procedure (
    applvaluetable  = newproperty([],  8, UNSET, "perm"),
    dfltvaluetable  = newproperty([], 16, UNSET, "perm"),
    nextvaluetable  = newproperty([], 16, UNSET, "perm"),
    currvaluetable  = newproperty([], 16, UNSET, "perm"),
    dfltvaluesubtable = newmapping([], 16, UNSET, false),
);

lconstant macro (
    t_int               = 1,
    t_short             = 2,
    t_exptr             = 3,
    t_XptBoolean        = 4,
    t_XptPixel          = 5,
    t_XptPixmap         = 6,
    t_XptCursor         = 7,
    t_XptTranslations   = 8,
);

lvars
    ;;; window set by xved_value for "currentWindow"
    curr_window         = false,
    ;;; window for which WIN_resource is currently storing update args
    curr_update_win     = false,
    ;;; set true when updating field of subpart (i.e. false when
    ;;; current window is an xvedwin)
    updating_subpart    = false,

    ;;; set dlocally by xved_value
    curr_res_name, curr_res_class, curr_res_type,
    ;


;;; ----------------------------------------------------------------------

define lconstant translations_string(list) -> string;
    define lconstant cache = newproperty([], 2, false, "tmparg") enddefine;
    returnif(cache(list) ->> string);
    consstring(#|
        unless list /== [] and isstartstring('#',hd(list)) then
            explode('#override\n')
        endunless;
        for string in list do explode(string), `\n` endfor ->
    |#) ->> string -> cache(list)
enddefine;

define lconstant flush_win_updates();
    if curr_update_win then
        XtVaSetValues(curr_update_win, xved_va_arg_list());
        false -> curr_update_win
    endif
enddefine;

define lconstant WIN_resource(typenum);
    lvars typenum, ptr = EXPTRINITSTR(:exptr);
    fast_XtVaGetValues(XptCheckWidget(curr_window),
                                    XtNLookup(curr_res_name, "N"), ptr, 2);
    go_on typenum to Type:
        Type t_int:
            exacc[fast] :int ptr;               goto Type end;
        Type t_short:
            exacc[fast] :short ptr;             goto Type end;
        Type t_exptr:
            exacc[fast] :exptr ptr;             goto Type end;
        Type t_XptBoolean:
            exacc[fast] :XptBoolean ptr;        goto Type end;
        Type t_XptPixel:
            exacc[fast] :XptPixel ptr;          goto Type end;
        Type t_XptPixmap:
            exacc[fast] :XptPixmap ptr;         goto Type end;
        Type t_XptCursor:
            exacc[fast] :XptCursor ptr;         goto Type end;
        Type t_XptTranslations:
            exacc[fast] :XptTranslations ptr;
    endgo_on;

    sys_grbg_fixed(ptr)
enddefine;
;;;
define updaterof WIN_resource(val, typenum);
    lvars val, typenum, trans;
    if typenum == t_XptTranslations and (isstring(val) or islist(val)) then
        if islist(val) then translations_string(val) -> val endif;
        unless isstartstring('#replace', val) then
            flush_win_updates();
            fast_XtParseTranslationTable(val) -> trans;
            if isstartstring('#augment', val) then
                fast_XtAugmentTranslations
            else
                ;;; override
                fast_XtOverrideTranslations
            endif(curr_window, trans);
            return
        endunless
    elseunless val.isstring or val.isintegral or val.isboolean
    or val.isexternal_ptr_class then
        returnif(val == "undef");
        mishap(val, curr_res_name, 2, 'INVALID VALUE FOR WINDOW RESOURCE')
    endif;

    xved_set_args(XtNLookup(curr_res_name,"N"), val,
                                if curr_window == curr_update_win then
                                    false
                                else
                                    flush_win_updates(), true
                                endif);

    ;;; IF WE'RE USING MOTIF WE NEED TO UPDATE THE SHADOW COLOURS
    ;;; IF WE CHANGE THE BACKGROUND
    if xved == "motif" and updating_subpart and curr_res_name == "background"
    then
        xved_set_args(XtN topShadowColor, top_shadow_color, false);
        xved_set_args(XtN bottomShadowColor, bottom_shadow_color, false);
        xved_set_args(XtN selectColor, select_color, false);
    endif;

    curr_window -> curr_update_win;

enddefine;

weak constant procedure (
    xved_scrollbar_children,
    xved_menubar_children,
);

define lconstant children_of_widget(widget);
    lvars widget;
    [%  if testdef xved_scrollbar_children then
            weakref xved_scrollbar_children(widget)
        endif;
        if testdef xved_menubar_children then
            weakref xved_menubar_children(widget)
        endif
    %] nc_<> XptChildren(widget)
enddefine;

;;; SET/RETURN A RESOURCE FOR ALL WIDGETS IN THE WIDGET TREE ROOTED
;;; AT curr_window. IF A PARENT DOES NOT HAVE A RESOURCE AND
;;; THE CHILD DOES, THE VALUE OF THE CHILD'S RESOURCE IS RETURNED
lconstant no_resource = consundef("undef");

define WIN_tree_resource(spec);
    dlocal curr_window;
    if XptResourceInfo(curr_window, curr_res_name) then
        WIN_resource(spec);
    elseif XptVal[fast] curr_window(XtN numChildren:int) fi_> 0 then
        fast_for curr_window in children_of_widget(curr_window) do;
            returnunless(dup(WIN_tree_resource(spec)==no_resource));
        endfast_for;
    else
        no_resource;
    endif;
enddefine;
;;;
define updaterof WIN_tree_resource(value, spec);
    lvars value, spec;
    dlocal updating_subpart = true;

    ;;; UPDATE THE RESOURCE IN ALL WIDGETS IN THE WIDGET
    ;;; TREE ROOTED AT curr_window
    define lconstant do_update(curr_window);
        dlocal curr_window;
        if XptResourceInfo(curr_window, curr_res_name) then
            value -> WIN_resource(spec);
            if xved == "openlook" then
                ;;; IF WE'RE USING OPENLOOK CHANGE THE FONTCOLOR IF WE CHANGE
                ;;; THE FOREGROUND
                if curr_res_name == "foreground"
                and XptResourceInfo(curr_window, XtN fontColor) then
                    procedure;
                        dlocal curr_res_name = "fontColor";
                        value -> WIN_resource(spec)
                    endprocedure()
                endif;
            endif
        endif;
        applist(children_of_widget(curr_window), do_update);
    enddefine;

    ;;; CAN THE WIDGET BE MAPPED/UNMAPPED
    define :inline lconstant mappable(WIDGET);
        (fast_XtIsRealized(WIDGET) and fast_XtIsWidget(WIDGET))
    enddefine;

    ;;; MAP/UNMAP WIDGET IF YOU CAN
    define lconstant map(w);
        lvars w;
        if mappable(w) then fast_XtMapWidget(w) endif;
    enddefine;
    ;;;
    define lconstant unmap(w);
        lvars w;
        if mappable(w) then fast_XtUnmapWidget(w) endif;
    enddefine;

    returnif(value == "undef");

    ;;; UNMAP WIDGETS YOU'RE UPDATING TO PREVENT FLICKER
    lvars unmap_widgets =
        if fast_XtIsShell(curr_window) then
            children_of_widget(curr_window);
        else
            [% if fast_XtIsComposite(curr_window) then curr_window endif %];
        endif;
    applist(unmap_widgets, unmap);

    ;;; IF WE'RE USING MOTIF, AND WE'VE CHANGED THE BACKGROUND COLOUR
    ;;; WE NEED TO UPDATE THE SHADOWS
    if xved == "motif" and curr_res_name == "background" then
        weakref get_xm_colours(value, curr_window)
            -> (/*fg*/, top_shadow_color, bottom_shadow_color, select_color)
    endif;

    do_update(curr_window);
    applist(unmap_widgets, map);

enddefine;

define lconstant Get_table(type) -> table;
    lvars win = false, vfc, l, rclass = "*", rtype = "*";
    "*" ->> curr_res_class -> curr_res_type;
    if type.isxvedwin then
        type -> win;
        "currentWindow" -> type
    elseif islist(type) then
        if (listlength(type) ->> l) fi_> 2 then
            mishap(type, 1, 'INVALID VALUE-TYPE LIST')
        elseif l == 2 then
            dl(type) -> (rclass, rtype)
        elseif l == 1 then
            hd(type) -> rclass
        endif;
        "defaultWindow" -> type
    elseunless isword(type) then
        unless isvector(vedpresent(type) ->> vfc) then
            mishap(type, 1, 'NO SUCH FILE')
        elseunless wved_is_live_window(wved_window_of_file(vfc) ->> win) then
            mishap(type, 1, 'FILE HAS NO WINDOW')
        endunless;
        "currentWindow" -> type
    endif;
    if isstring(rclass) then consword(rclass) else rclass endif
                                                -> curr_res_class;
    if isstring(rtype) then consword(rtype) else rtype endif
                                                -> curr_res_type;
    false -> curr_window;
    if type == "currentWindow" then
        unless (win or wvedwindow ->> curr_window) and vedusewindows == "x"
        then
            mishap(0, 'NOT RUNNING XVed');
        endunless;
        currvaluetable
    elseif type == "nextWindow" then
        nextvaluetable
    elseif type == "defaultWindow" then
        dfltvaluetable
    elseif type == "application" then
        applvaluetable
    else
        mishap(type, 1, 'UNKNOWN VALUE TYPE')
    endif -> table
enddefine;

define lconstant Get_entry(table, key, upd, upd_val) -> entry;
    lvars len, mentry, lkey, wclass, wtype;
    table(key) -> entry;
    if entry == UNSET then mishap(key, 1, 'UNKNOWN VALUE NAME') endif;
    key -> curr_res_name;
    curr_res_class -> wclass;
    curr_res_type -> wtype;
    returnif(wclass == "*" and wtype == "*");

    ;;; dfltvaluetable only
    conslist(key, wclass, wtype, 3) -> lkey;
    if upd then
        if (dfltvaluesubtable(lkey) ->> mentry) == UNSET then
            if isref(entry) then copy(entry) else entry endif
                    ->> mentry -> dfltvaluesubtable(lkey);
            [] -> lkey
        elseif upd_val == "undef" then
            ;;; remove the entry
            UNSET -> dfltvaluesubtable(lkey);
            false -> mentry
        endif
    else
        if (dfltvaluesubtable(lkey) ->> mentry) == UNSET
        and wclass /== "*" and wtype /== "*" then
            "*" -> fast_subscrl(3,lkey);
            if (dfltvaluesubtable(lkey) ->> mentry) == UNSET then
                "*" -> fast_subscrl(2,lkey);
                wtype -> fast_subscrl(3,lkey);
                dfltvaluesubtable(lkey) -> mentry
            endif
        endif
    endif;
    sys_grbg_list(lkey);
    if mentry /== UNSET then mentry -> entry endif
enddefine;

define xved_value(type, key);
    lvars type, key, entry, table, keylist = key;
    lconstant worklist = writeable [0];
    dlocal curr_window, curr_res_name, curr_res_class, curr_res_type;

    Get_table(type) -> table;   ;;; sets curr_ values
    unless islist(key) then
        key -> hd(worklist);
        worklist -> keylist
    endunless;

    for key in keylist do
        Get_entry(table, key, false, false) -> entry;
        if entry.isident then
            idval(entry);
            if isundef(dup()) then ->, "undef" endif
        elseif entry.isprocedure then
            entry()
        elseif entry.isref then
            cont(entry)
        else
            entry
        endif
    endfor
enddefine;
;;;
define updaterof xved_value(type, key);
    lvars type, key, vallist, table, keylist = key;
    lconstant worklist = writeable [0];

    define lconstant do_updates();
        lvars value, entry;
        for key, value in keylist, vallist do
            Get_entry(table, key, true, value) -> entry;
            unless isclosure(entry) and pdpart(entry) == WIN_resource then
                flush_win_updates()
            endunless;
            value ->
                if entry.isident then
                    idval(entry)
                elseif entry.isprocedure then
                    entry()
                elseif entry.isref then
                    cont(entry)
                else
                    ()
                endif;

            if testdef xveddialogs and entry then
                /* check if any properties need updating */
                weakref[xveddialogs] xved_dialogs_init(entry)
            endif
        endfor
    enddefine;

    dlocal  curr_window, curr_res_name, curr_res_class, curr_res_type,
            0 %, false -> curr_update_win %;
    if curr_update_win then flush_win_updates() endif;

    Get_table(type) -> table;       ;;; sets curr_ values
    unless islist(key) then
        key -> hd(worklist);
        worklist -> keylist
    endunless;

    conslist(listlength(keylist)) -> vallist;

    if curr_window then
        ;;; locally set off WM size hints and reset them again after
        ;;; updating values
        procedure;
            dlocal % xved_size_hints_set(curr_window) % = false;
            do_updates();
            flush_win_updates();
        endprocedure()
    else
        do_updates();
        flush_win_updates();
    endif;

    sys_grbg_list(vallist)
enddefine;


;;; --- CURRENT WINDOW RESOURCES ------------------------------------------

;;; --- Basic resources for each new window -------------------------------

define lconstant init_win_res(type, reslist) -> reslist;
    lvars res, reslist, type = WIN_resource(% type %);
    for res in reslist do type -> currvaluetable(res) endfor
enddefine;

lconstant text_resources =

    ;;; int
    init_win_res( t_int,
    [   numRows
        numColumns
    ])

nc_<>
    ;;; short
    init_win_res( t_short,
    [   borderWidth
        hMargin
        vMargin
        selectionColorNum
    ])

nc_<>
    ;;; exptr
    init_win_res( t_exptr,
    [   fontSet
        boldFontSet
        altFontSet
        boldAltFontSet
        ;;; for backward compatibility
        font
        boldFont
        altFont
        boldAltFont
    ])

nc_<>
    ;;; XptBoolean
    init_win_res( t_XptBoolean,
    [   drawGraphicChars
        noBlink
        varWidthMode
    ])

nc_<>
    ;;; XptPixel
    init_win_res( t_XptPixel,
    [   foreground
        background
        statusForeground
        statusBackground
        highlightForeground
        highlightBackground
        color2Foreground
        color2Background
        color3Foreground
        color3Background
        color4Foreground
        color4Background
        color5Foreground
        color5Background
        color6Foreground
        color6Background
        color7Foreground
        color7Background
        color0AForeground
        color0ABackground
        color1AForeground
        color1ABackground
        color2AForeground
        color2ABackground
        color3AForeground
        color3ABackground
        color4AForeground
        color4ABackground
        color5AForeground
        color5ABackground
        color6AForeground
        color6ABackground
        color7AForeground
        color7ABackground
        cursorColor
        borderColor
        pointerForeground
        pointerBackground
    ])

nc_<>
    ;;; XptPixmap
    init_win_res( t_XptPixmap,
    [   borderPixmap
        backgroundPixmap
    ])

nc_<>
    ;;; XptCursor
    init_win_res( t_XptCursor,
    [   pointerShape
        pointer2Shape
    ])

nc_<>
    ;;; XptTranslations
    init_win_res( t_XptTranslations,
    [   translations
    ])

nc_<>
    ;;; set to a procedure below
    [   scrollbarForeground
        scrollbarBackground
        menubarForeground
        menubarBackground
        menubarOn
        scrollbarOn
        hscrollbarOn
        autoGeometry
    ]
;

define lconstant subpartResource(curr_res_name, rep, subpart);
    dlocal curr_res_name, curr_window;
    if wved_is_live_window(curr_window)
    and (xved_get_subpart(curr_window, subpart, false) ->> curr_window)
    then
        WIN_tree_resource(rep)
    else
        false
    endif
enddefine;
;;;
define updaterof subpartResource(value, curr_res_name, rep, subpart);
    lvars xvwin = curr_window;
    dlocal curr_res_name, curr_window;
    returnunless(wved_is_live_window(xvwin));
    repeat
        if xved_get_subpart(xvwin, subpart, false) ->> curr_window then
            value -> WIN_tree_resource(rep)
        endif;
        quitunless(subpart == xvedwin_scrollbar);
        ;;; do the horiz scrollbar as well
        xvedwin_hscrollbar -> subpart
    endrepeat
enddefine;
;;;
subpartResource(%"foreground",t_XptPixel,xvedwin_scrollbar%)
                            -> currvaluetable("scrollbarForeground");
subpartResource(%"background",t_XptPixel,xvedwin_scrollbar%)
                            -> currvaluetable("scrollbarBackground");
subpartResource(%"foreground",t_XptPixel,xvedwin_menubar%)
                            -> currvaluetable("menubarForeground");
;;;
;;; When we update the Menubar background, we also need to update the
;;; bg of the parent since
;;;     i)  There is a "gap" between the XpwScrollText and the menubar
;;;         under OW
;;;     ii) The bg of the scrollbar is the bg of the Form under OW3
define lconstant menubarBg();
    subpartResource("background",t_XptPixel,xvedwin_menubar);
enddefine;
;;;
define updaterof menubarBg(value);
    lvars value;
    dlocal curr_window;
    if wved_is_live_window(curr_window) then
        value -> subpartResource("background",t_XptPixel,xvedwin_menubar);
        if xved == "openlook" then
            fast_XtParent(wvedwindow) -> curr_window;
            dlocal curr_res_name = "background";
            value -> WIN_resource(t_XptPixel);
        endif
    endif;
enddefine;
;;;
menubarBg -> currvaluetable("menubarBackground");

define lconstant res_field_p();
    lvars name = curr_res_name;
    if name == "scrollbarOn" then
        xvedwin_scrollbar
    elseif name == "hscrollbarOn" then
        xvedwin_hscrollbar
    else
        xvedwin_menubar
    endif
enddefine;

define lconstant subpartOn();
    curr_window.wved_is_live_window
        and xved_get_subpart(curr_window, res_field_p(), true)
enddefine;
;;;
define updaterof subpartOn(value);
    lvars value, win;
    if curr_window.wved_is_live_window then
        returnunless(xved_get_subpart(curr_window, res_field_p(),
                            value and 'init') ->> win);
        if value then
            fast_XtManageChild(win);
        else
            fast_XtUnmanageChild(win);
        endif
    endif
enddefine;
;;;
subpartOn   ->> currvaluetable("scrollbarOn")
            ->> currvaluetable("hscrollbarOn")
            ->  currvaluetable("menubarOn");


;;; --- Other, and pseudo, current window resources -------------------------

;;; "x" and "y"
define lconstant position();
    lvars (x, y, _, _) = XptWMShellCoords(curr_window);
    if curr_res_name == "x" then x else y endif
enddefine;
;;;
define updaterof position(val);
    lvars val;
    if curr_res_name == "x" then
        val, false
    else
        false, val
    endif, false, false -> XptWMShellCoords(curr_window)
enddefine;
position ->> currvaluetable("x") -> currvaluetable("y");

define lconstant geometry(type);
    lvars x, y;

    define lconstant getval(name);
        if dup(xved_value(type, name)) == "undef" then ->, false endif
    enddefine;

    if type then
        unless curr_res_class == "*" and curr_res_type == "*" then
            [^curr_res_class ^curr_res_type] -> type
        endunless;
        (getval("x") ->> x) and getval("y") -> y
    else
        XptWMShellCoords(curr_window) -> (x, y, , );
        curr_window -> type
    endif;

    (x, y, getval("numColumns"), getval("numRows") -> XptParseGeometry())
    or "undef"
enddefine;
;;;
define updaterof geometry(val, type);
    lvars x, y, cols, rows;

    define lconstant putval(val, name);
        if val then val -> xved_value(type, name) endif
    enddefine;

    if val == "undef" then nullstring -> val endif;
    XptParseGeometry(val) -> (x, y, cols, rows);

    if type then
        unless curr_res_class == "*" and curr_res_type == "*" then
            [^curr_res_class ^curr_res_type] -> type
        endunless;
        putval(x, "x");
        putval(y, "y");
        putval(cols, "numColumns");
        putval(rows, "numRows")
    else
        ;;; current win
        if cols or rows then
            cols, rows -> XptVal[fast] curr_window(
                            XtN numColumns:int<OPT>, XtN numRows:int<OPT>)
        endif;
        if x or y then
            x, y, false, false -> XptWMShellCoords(curr_window)
        endif
    endif
enddefine;

geometry(%false%) -> currvaluetable("geometry");

;;; "iconic"
define lconstant iconic();
    not(wved_is_open_window(curr_window));
enddefine;
;;;
define updaterof iconic(val);
    not(val) -> wved_is_open_window(curr_window);
enddefine;
;;;
iconic -> currvaluetable("iconic");

define lconstant get_currwin(); curr_window enddefine;

;;; "iconPixmap"
get_currwin <> xved_icon_window_pixmap  -> currvaluetable("iconPixmap");

;;; "iconName"
get_currwin <> xved_icon_name           -> currvaluetable("iconName");

;;; "title"
get_currwin <> xved_window_label        -> currvaluetable("title");

;;; "raised"
get_currwin <> xved_is_raised_window    -> currvaluetable("raised");

define lconstant menubar();
    lvars mb = curr_window.xvedwin_menubar;
    if isvector(mb) then copylist(subscrv(2, mb)) else [] endif
enddefine;
;;;
weak constant procedure xved_init_menubar;
weak vars xvednewmenubar;
;;;
define updaterof menubar(new);
    lvars new = new or [], mb;
    returnunless(testdef xvedmenubar);
    dlocal weakref[xvedmenubar] xvednewmenubar = new;

    if isvector(curr_window.xvedwin_menubar) then
        ;;; replace or destroy existing menubar
        weakref[xvedmenubar] xved_init_menubar(curr_window)
    elseif new /== [] then
        ;;; init a new one
        dlocal curr_res_name = "menubarOn";
        true -> subpartOn()
    endif
enddefine;
;;;
menubar -> currvaluetable("menubar");

;;; "eventTable"
define lconstant event_table() -> table;
    curr_window.xvedwin_eventvec -> table;
    ;;; create a new table
    unless table then
        newproperty([],5,false,"perm") ->> curr_window.xvedwin_eventvec
                                        -> table;
    endunless;
enddefine;
;;;
define updaterof event_table(val);
    val -> curr_window.xvedwin_eventvec;
enddefine;
;;;
event_table -> currvaluetable("eventTable");

;;; "keyMapTable"
define lconstant keymap_table() -> table;
    curr_window.xvedwin_keyseqvec -> table;
    unless table then
        ;;; create a new table
        newproperty([],5,false,"perm") ->> curr_window.xvedwin_keyseqvec
                                            -> table;
    endunless;
enddefine;
;;;
define updaterof keymap_table(val);
    val -> curr_window.xvedwin_keyseqvec;
enddefine;
;;;
keymap_table -> currvaluetable("keyMapTable");

;;; "type" and "class"
define lconstant win_name();
    XtName( if curr_res_name == "type" then
                curr_window
            else
                XtParent(curr_window)
            endif)
enddefine;
;;;
define updaterof win_name(val);
    mishap(val, curr_res_name, 2, 'CAN\'T CHANGE WINDOW type OR class')
enddefine;
;;;
win_name ->> currvaluetable("type") -> currvaluetable("class");


;;; --- NEXT WINDOW AND DEFAULT WINDOW RESOURCES ------------------------

;;; N.B. Default things that are refs are saved by xved_save_defaults

    /*
     *  N.B. MUSTN'T USE xved_value AT EXECUTE-LEVEL
     *  (doesn't work with POPC)
     */

lconstant
    shell_resources = [iconX iconY iconic iconWindow iconName title
                        shellBorderWidth];

lvars res;

for res in [
    x
    y
    iconPixmap
    class
    type
    menubar
    raised
] nc_<> (shell_resources <> text_resources) do
    consref("undef") -> dfltvaluetable(res);
    consref("undef") -> nextvaluetable(res);
endfor;

geometry(%"nextWindow"%)        -> nextvaluetable("geometry");
geometry(%"defaultWindow"%)     -> dfltvaluetable("geometry");

define lconstant next_table() -> val;
    lvars table;
    if curr_res_name == "eventTable" then
        ident xvednexteventvec
    else
        ident xvednextkeyseqvec
    endif -> table;
    unless fast_idval(table)->>val then
        newproperty([],5,false,"perm") ->> fast_idval(table) -> val;
    endunless;
enddefine;
;;;
define updaterof next_table(val);
    if curr_res_name == "eventTable" then
        val.isproperty and val -> xvednexteventvec;
    else
        val.isproperty and val -> xvednextkeyseqvec;
    endif;
enddefine;

next_table                      -> nextvaluetable("eventTable");
next_table                      -> nextvaluetable("keyMapTable");

consref(xvedeventtable)         -> dfltvaluetable("eventTable");
consref(xvedkeyseqtable)        -> dfltvaluetable("keyMapTable");


;;; create type name for window buffers
define lconstant make_window_type();
    lvars fp = vedfileprops;
    if isword(fp) then
        allbutfirst(strmember(`.`, fp) or 0, fp sys_>< nullstring)
    elseif subsystem /== "pop11" or vedcompileable then
        subsystem sys_>< nullstring
    else
        'ved'
    endif
enddefine;
;;;
consref(make_window_type)       -> dfltvaluetable("type");

;;; create class name for window buffers
define lconstant make_window_class();
    lvars fp = vedfileprops, n;
    if isword(fp) then
        returnif(strmember(`.`, fp) ->> n)          (substring(1, n-1, fp));
        returnif(fp == "src")                               ('Sourcefile');
        returnif(fp == "lib" or fp == "include")            ('Library');
        returnif(issubstring('mail', 1, fp))                ('Mail');
        returnif(fast_lmember(fp, [help ref teach doc man]))('Documentation');
    endif;
    if vedcompileable then 'Program' else 'Text' endif
enddefine;
;;;
consref(make_window_class)      -> dfltvaluetable("class");

;;; create string for window labels
define lconstant make_window_title();
    lvars ns = vednamestring;
    WINDOW_BASE_LABEL <> allbutfirst(locchar(`\s`,1,ns), ns);
    if vedchanged and vedwriteable then () <> ' (needs writing)' endif
enddefine;
;;;
consref(make_window_title)      -> dfltvaluetable("title");

;;; create string for icon
define lconstant make_icon_name();
    lvars   curr = vedcurrent, len = datalength(curr),
            n = locchar_back(`\s`, len, curr), name;
    if n then
        sys_fname_namev(allbutfirst(n,curr)) -> name;
        if name = nullstring then n-1 -> n endif;
        allbutlast(len-n, curr) <> name
    else
        sys_fname_namev(curr)
    endif
enddefine;
;;;
consref(make_icon_name)         -> dfltvaluetable("iconName");


;;; Standard Ved Icons
lvars
    usexvedicons    = true,
    bitmap_suffix   = false,
;

define lconstant make_icon_pixmap();
    lvars fprops;
    lconstant bitmaps_available = [ved help ref teach doc lib src tmp];

    returnunless(usexvedicons) ("undef");

    vedfileprops -> fprops;
    unless fast_lmember(fprops, bitmaps_available) then
        if vedcompileable then "src"
        elseunless vedwriteable then "tmp"
        else "ved"
        endif -> fprops
    endunless;
    unless bitmap_suffix then
        xved_get_icon_filename(xveddummyshell, nullstring) -> bitmap_suffix
    endunless;
    XVEDBITMAPS dir_>< (fprops sys_>< bitmap_suffix)
enddefine;
;;;
consref(make_icon_pixmap)       -> dfltvaluetable("iconPixmap");

define lconstant make_icon_window();
    lvars pixmap = xved_default_window_value("iconPixmap");
    if pixmap /== "undef" then
        ;;; creates a new icon window
        xved_create_icon_window(pixmap, xved_default_window_value("iconName"))
    else
        "undef"
    endif
enddefine;
;;;
consref(make_icon_window)       -> dfltvaluetable("iconWindow");

consref(true)                   -> dfltvaluetable("drawGraphicChars");

lvars dflt_mbar;
;;;
define lconstant default_menubar;
    if testdef vedxvedmenubar then weakref vedxvedmenubar else dflt_mbar endif
enddefine;
;;;
define updaterof default_menubar(val);
    lvars val;
    if curr_res_class == "*" and curr_res_type == "*" then
        val ->  if testdef vedxvedmenubar then weakref vedxvedmenubar
                else dflt_mbar
                endif
    else
        consref(val)
            -> dfltvaluesubtable([menubar ^curr_res_class ^curr_res_type])
    endif
enddefine;
;;;
default_menubar                 -> dfltvaluetable("menubar");


define xved_default_window_value(name) -> val;
    lvars name, val, type;
    if islist(name) then
        chain(name, xved_default_window_value, applist)
    endif;

    if xvedisincreate ->> type then
        ;;; xvedisincreate is a list [class type]
        if (xved_value("nextWindow", name) ->> val) /== "undef" then
            ;;; clear the slot
            "undef" -> xved_value("nextWindow", name);
            return
        endif
    else
        "defaultWindow" -> type
    endif;
    if (xved_value(type, name) ->> val) /== "undef" then
        if val.isprocedure and not(val.isproperty) then
            fast_apply(val) -> val
        endif
    endif
enddefine;

define xved_make_create_args(is_for_text);
    lvars   is_for_text, name, val, def, is_for_text, res, server,
            use_iconPixmap = false;
    if is_for_text then
        text_resources
    else
        xved_server_vendor_string() -> server;
        ;;; NuTCRACKER does not handle iconWindows correctly, so just use
        ;;; iconPixmap instead
        issubstring('DECWINDOWS', server) and issubstring('NuTCRACKER', server)
                -> use_iconPixmap;
        shell_resources
    endif -> res;
    fast_for name in res do
        if name == "iconWindow" and use_iconPixmap then
            "iconPixmap" -> name
        endif;
        xved_default_window_value(name) -> def;
        if def /== "undef" then
            ;;; there is a value
            if name = "shellBorderWidth" then
                "borderWidth" -> name
            elseif name == "translations" and islist(def) then
                translations_string(def) -> def
            endif;
            XtNLookup(name,"N"), def
        endif;
    endfor;
enddefine;


;;; --- APPLICATION RESOURCES ---------------------------------------------

;;; N.B. Things that are idents are saved by xved_save_defaults

lvars
    oldbuttonbindings   = false,
    pwmbuttonbindings   = false,
    editkeyson          = true,
    bellvolume          = 100,
    windowmanager       = false,
    statusattop         = true,
    ;;; leave the rest as <undef>
    resourcefile,
    menuforeground,
    menubackground,
    dialogforeground,
    dialogbackground,
;

ident oldbuttonbindings     -> applvaluetable("UseOldMouseBindings");
ident pwmbuttonbindings     -> applvaluetable("UsePwmMouseBindings");
ident editkeyson            -> applvaluetable("EditKeysEnabled");
ident bellvolume            -> applvaluetable("BellVolume");
ident windowmanager         -> applvaluetable("WindowManager");
ident resourcefile          -> applvaluetable("ResourceFile");
ident usexvedicons          -> applvaluetable("UseXVedIcons");

ident xvedwarpmouse         -> applvaluetable("WarpPointer");
ident vedwarpcontext        -> applvaluetable("WarpContexts");
ident xvedsetfocus          -> applvaluetable("SetInputFocus");
ident xvedpopfocusonly      -> applvaluetable("PopFocusOnly");
ident xvedkeypadon          -> applvaluetable("KeypadKeysEnabled");
ident wvedalwaysraise       -> applvaluetable("AlwaysRaiseWindow");
ident wvedbreaktofit        -> applvaluetable("SetLineBreakOnResize");
ident xvedautoplace         -> applvaluetable("AutoWindowPlacement");
ident xvedautocut           -> applvaluetable("AutoCut");
ident xvedvscrollstep       -> applvaluetable("VScrollStep");
ident xvedhscrollstep       -> applvaluetable("HScrollStep");
ident xvedclicktime         -> applvaluetable("MultiClickTimeOut");
ident xvedshowfilename      -> applvaluetable("ShowFileName");
ident vedsearchdoesselect   -> applvaluetable("SearchDoesSelect");
ident statusattop           -> applvaluetable("StatusAtTop");
ident xvedmaxwindows        -> applvaluetable("MaxWindows");
ident xvedvanilla           -> applvaluetable("Vanilla");

lvars app_menu = false;
;;;
define lconstant menu();
    if testdef vedxvedmenu then weakref vedxvedmenu else app_menu endif
enddefine;
;;;
define updaterof menu(new);
    lvars new;
    if new == [] then false -> new endif;
    new -> if testdef vedxvedmenu then weakref vedxvedmenu else app_menu endif;
    if testdef xvedmenubar and vedsetupdone then
        ;;; (re)initialise it
        weakref[xvedmenubar] xved_popup_menu(false)
    endif
enddefine;
;;;
menu -> applvaluetable("Menu");

define lconstant color_resource(_, res_id, isdialog);
    lvars res_id, isdialog;
    fast_idval(res_id)
enddefine;
;;;
define updaterof color_resource(value, curr_res_name, res_id, isdialog);
    lvars value, res_id, isdialog;
    dlocal curr_res_name, curr_window;

    returnif(value == -1 or value = NO_PIXEL);
    if isdialog then
        ;;; xveddialogslist
        fast_for curr_window in xveddialogslist do
            value -> WIN_tree_resource(t_XptPixel);
        endfor
    else
        ;;; menu
        if testdef xvedmenubar
        and (weakref[xvedmenubar] xvedpopupmenushell ->> curr_window)
        then
            value -> WIN_tree_resource(t_XptPixel);
        endif
    endif;
    value -> fast_idval(res_id)
enddefine;

define lconstant active dialogForeground =
    color_resource(% "foreground", ident dialogforeground, true %)
enddefine;
;;;
ident dialogForeground -> applvaluetable("DialogForeground");

define lconstant active dialogBackground =
    color_resource(% "background", ident dialogbackground, true %)
enddefine;
;;;
ident dialogBackground -> applvaluetable("DialogBackground");

define lconstant active menuForeground =
    color_resource(% "foreground", ident menuforeground, false %)
enddefine;
;;;
ident menuForeground -> applvaluetable("MenuForeground");

define lconstant active menuBackground =
    color_resource(% "background", ident menubackground, false %)
enddefine;
;;;
ident menuBackground -> applvaluetable("MenuBackground");


l_typespec AppData {
    WarpContexts        :XptString,
    BellVolume          :ushort,
    WarpPointer         :XptBoolean,
    SetInputFocus       :XptBoolean,
    PopFocusOnly        :XptBoolean,
    KeypadKeysEnabled   :XptBoolean,
    AlwaysRaiseWindow   :XptBoolean,
    SetLineBreakOnResize:XptBoolean,
    AutoWindowPlacement :XptBoolean,
    EditKeysEnabled     :XptBoolean,
    AutoCut             :XptBoolean,
    ResourceFile        :XptString,
    UseOldMouseBindings :byte,
    UsePwmMouseBindings :XptBoolean,
    WindowManager       :XptString,
    VScrollStep         :XptShort,
    HScrollStep         :XptShort,
    ShowFileName        :XptBoolean,
    Vanilla             :XptBoolean,
    MenuForeground      :XptPixel,
    MenuBackground      :XptPixel,
    DialogForeground    :XptPixel,
    DialogBackground    :XptPixel,
    SearchDoesSelect    :XptBoolean,
    StatusAtTop         :XptBoolean,
    UseXVedIcons        :XptBoolean,
    MaxWindows          :XptInt
};

define :inline lconstant OFFSET(field=item);
    FIELDOFFSET(:AppData,field)
enddefine;

lconstant macro SIZEOF = nonmac SIZEOFTYPE;

lconstant
    appl_resources = nonwriteable {% #|
        (XtN BellVolume, XtC BellVolume, XtR Short, SIZEOF(:XptShort),
            OFFSET(BellVolume), XtR Immediate, 100),
        (XtN WarpPointer, XtC WarpPointer, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(WarpPointer), XtR Immediate, 0),
        (XtN WarpContexts, XtC WarpContexts, XtR String, SIZEOF(:XptString),
            OFFSET(WarpContexts), XtR Immediate, 'all'),
        (XtN ResourceFile, XtC ResourceFile, XtR String, SIZEOF(:XptString),
            OFFSET(ResourceFile), XtR Immediate, '$HOME/.Xdefaults'),
        (XtN SetInputFocus, XtC SetInputFocus, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(SetInputFocus), XtR Immediate, 1),
        (XtN PopFocusOnly , XtC PopFocusOnly, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(PopFocusOnly), XtR Immediate, 0),
        (XtN KeypadKeysEnabled, XtC KeypadEnabled, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(KeypadKeysEnabled), XtR Immediate, 1),
        (XtN EditKeysEnabled, XtC EditKeysEnabled, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(EditKeysEnabled), XtR Immediate, 0),
        (XtN AlwaysRaiseWindow, XtC AlwaysRaiseWindow, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(AlwaysRaiseWindow), XtR Immediate, 0),
        (XtN SetLineBreakOnResize, XtC SetLineBreakOnResize, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(SetLineBreakOnResize), XtR Immediate, 0),
        (XtN AutoWindowPlacement, XtC AutoWindowPlacement, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(AutoWindowPlacement), XtR Immediate, 0),
        (XtN AutoCut, XtC AutoCut, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(AutoCut), XtR Immediate, 1),
        (XtN UseOldMouseBindings, XtC UseOldMouseBindings, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(UseOldMouseBindings), XtR Immediate, 16:FF),
        (XtN UsePwmMouseBindings, XtC UsePwmMouseBindings, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(UsePwmMouseBindings), XtR Immediate, 0),
        (XtN WindowManager, XtC WindowManager, XtR String, SIZEOF(:XptString),
            OFFSET(WindowManager), XtR Immediate, 0),
        (XtN VScrollStep, XtC ScrollStep, XtR Short, SIZEOF(:XptShort),
            OFFSET(VScrollStep), XtR Immediate, 1),
        (XtN HScrollStep, XtC ScrollStep, XtR Short, SIZEOF(:XptShort),
            OFFSET(HScrollStep), XtR Immediate, 1),
        (XtN ShowFileName, XtC ShowFileName, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(ShowFileName), XtR Immediate, 1),
        (XtN Vanilla, XtC Vanilla, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(Vanilla), XtR Immediate, 0),
        (XtN MenuForeground, XtC MenuForeground, XtR Pixel, SIZEOF(:XptPixel),
            OFFSET(MenuForeground), XtR Immediate, NO_PIXEL),
        (XtN MenuBackground, XtC MenuBackground, XtR Pixel, SIZEOF(:XptPixel),
            OFFSET(MenuBackground), XtR Immediate, NO_PIXEL),
        (XtN DialogForeground, XtC DialogForeground, XtR Pixel, SIZEOF(:XptPixel),
            OFFSET(DialogForeground), XtR Immediate, NO_PIXEL),
        (XtN DialogBackground, XtC DialogBackground, XtR Pixel, SIZEOF(:XptPixel),
            OFFSET(DialogBackground), XtR Immediate, NO_PIXEL),
        (XtN SearchDoesSelect, XtC SearchDoesSelect, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(SearchDoesSelect), XtR Immediate, 0),
        (XtN StatusAtTop, XtC StatusAtTop, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(StatusAtTop), XtR Immediate, 1),
        (XtN UseXVedIcons, XtC UseXVedIcons, XtR Boolean, SIZEOF(:XptBoolean),
            OFFSET(UseXVedIcons), XtR Immediate, 1),
        (XtN MaxWindows, XtC MaxWindows, XtR Int, SIZEOF(:XptInt),
            OFFSET(MaxWindows), XtR Immediate, 1e6),
    |# div 7 %},

    AppData = EXPTRINITSTR(:AppData),
;

#_IF DEF POPC_COMPILING
section;
shadowclass-declare XptResourceList :XptResource[];
endsection;
#_ENDIF

shadowclass lconstant XptResourceList [nc, prefix nc_];


    /*  Called from xvedsetup
    */
define xved_get_app_resources();
    lvars widget, nres;

    XtVaGetApplicationResources(xveddummyshell, AppData,
            nc_consXptResourceList(explode(appl_resources)->>nres), nres, 0);

    define :inline lconstant GET_RES(name=item, import);
        import(exacc AppData.name) -> xved_value("application", "name");
    enddefine;

    define lconstant get_warp_contexts(str);
        lvars str, item;
        dlocal proglist_state = proglist_new_state(stringin(str));
        listread() -> item;
        if item == "false" then false else item endif
    enddefine;

    define lconstant get_bell_volume(v) -> v;
        lvars v;
        unless v.isinteger and v >= 0 and v <= 100 then
            warning(v,1,'Invalid value for bell volume, using -100- instead');
            100 -> v;
        endunless;
    enddefine;

    define lconstant get_old_mouse_bindings(v);
        lvars v;
        if v == 16:FF then xved == "openlook" else v /== 0 endif
    enddefine;

    GET_RES(ResourceFile,);
    GET_RES(BellVolume, get_bell_volume);
    GET_RES(WarpContexts, get_warp_contexts);
    GET_RES(WarpPointer,);
    GET_RES(SetInputFocus,);
    GET_RES(PopFocusOnly,);
    GET_RES(KeypadKeysEnabled,);
    GET_RES(EditKeysEnabled,);
    GET_RES(AlwaysRaiseWindow,);
    GET_RES(SetLineBreakOnResize,);
    GET_RES(AutoWindowPlacement,);
    GET_RES(AutoCut,);
    GET_RES(UseOldMouseBindings,get_old_mouse_bindings);
    GET_RES(UsePwmMouseBindings,);
    GET_RES(VScrollStep,);
    GET_RES(HScrollStep,);
    GET_RES(ShowFileName,);
    GET_RES(MenuForeground,);
    GET_RES(MenuBackground,);
    GET_RES(DialogForeground,);
    GET_RES(DialogBackground,);
    GET_RES(Vanilla,);
    GET_RES(SearchDoesSelect,);
    GET_RES(StatusAtTop,);
    GET_RES(UseXVedIcons,);
    GET_RES(MaxWindows,);

    if testdef xveddialogs then
        weakref[xveddialogs] xved_dialogs_init(false)
    endif
enddefine;


define xved_save_defaults();
    lvars   name, val, fname, str, saveout, class, type,
            resfile = sysfileok(xved_value("application", "ResourceFile"));

    lconstant geometry_parts = [x y numColumns numRows];

    define out_res(name, val);
        lvars string;
        explode(name);
        `:`, `\s`;
        if islist(val) then
            ;;; translations
            unless val /== [] and isstartstring('#',hd(val)) then
                explode('#override\\n\\\n')
            endunless;
            for string in val do
                `\t`;
                appdata(string, procedure(c);
                                    if c == `\e` then `\\`, `\\`, `[`
                                    else c
                                    endif
                                endprocedure);
                `\\`, `n`, `\\`, `\n`
            endfor -> (_,_,_,_)
        else
            dest_characters(if val == true then 'true'
                            elseif val == false then 'false'
                            else val
                            endif)
        endif;
        `\n`
    enddefine;

    consstring(#|
        `\n`,
        for name, val in_property applvaluetable do
            nextunless(isident(val));
            xved_value("application", name) -> val;
            nextif(isintegral(val) and (issubstring('Foreground',name)
                                        or issubstring('Background',name)));
            explode(XV_CLASS_NAME), `.`;
            out_res(name, val)
        endfor;

        define use_dres(class_type, name, val) -> (useit, val);
            not(lmember(name, geometry_parts))
            and (name == "geometry" or isref(val)) -> useit;
            if useit then
                xved_value(class_type, name) -> val;
                unless val.isstring or val.isintegral or val.isboolean
                or (name == "translations" and val.islist) then
                    false -> useit
                endunless
            endif
        enddefine;

        for name, val in_property dfltvaluetable do
            nextunless(use_dres([], name, val) -> val);
            explode(XV_CLASS_NAME);
            if lmember(name,shell_resources) then
                if name == "shellBorderWidth" then
                    "borderWidth" -> name
                endif;
                `.`
            else
                explode('*XpwScrollText.')
            endif;
            out_res(name, val)
        endfor;

        lvars geometry_done = [];

        for name, val in_property dfltvaluesubtable do
            dest(name) -> (name, class);
            nextif(lmember(name,shell_resources));
            if lmember(name, geometry_parts)
            and not(lmember_=(class, geometry_done)) then
                class :: geometry_done -> geometry_done;
                "geometry" -> name
            endif;
            nextunless(use_dres(class, name, val) -> val);
            explode(XV_CLASS_NAME);
            dl(class) -> (class, type);
            if type == "*" then 'XpwScrollText' -> type endif;
            if class == "*" then `*` else `.`, explode(class), `.` endif;
            explode(type), `.`;
            out_res(name, val)
        endfor;

    |#) -> str;

    discout(systmpfile(false, 'xvedsaveres', '') ->> fname) -> saveout;
    appdata(str, saveout);
    saveout(termin);
    /* Calls sysobey to run xrdb */
    sysobey('xrdb -merge ' sys_>< fname
                sys_>< ';xrdb -backup \'-\' -edit ' sys_>< resfile,`$`);
    sysdelete(saveout) ->
enddefine;


endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 18 1997
        Added varWidthMode resource
--- John Gibson, Apr 30 1997
        Added fontSet resources.
--- John Gibson, Nov 21 1996
        # Corrected types of text_resources.
        # Added translations resource for text, and shellBorderWidth as
          a pseudonym for shell borderWidth.
        # Made xved_get_app_resources always call xved_dialogs_init instead of
          being done by the Vanilla resource only when false.
--- John Gibson, Nov 12 1996
        Changed xved_value to allow a list [class type] etc instead of
        "defaultWindow" to constrain resources to particular classes
        and type. Rewrote xved_save_defaults.
--- John Gibson, Nov  5 1996
        Made xved_make_create_args set the iconPixmap resource instead of
        iconWindow for the DECWINDOWS NuTCRACKER server.
--- John Gibson, Apr 18 1996
        make_window_type/class now recognise a leading Class. in vedfileprops
        as being the window class.
--- John Gibson, Apr 16 1996
        Added autoGeometry text resource
--- John Gibson, Feb 28 1996
        Added UseOldMouseBindings application resource
--- John Gibson, Dec  6 1995
        Changed make_icon_name to do something sensible with pseudo-file
        names.
--- John Gibson, Oct 21 1995
        Added new window resources
--- John Gibson, Jan 23 1995
        # Fixed bug in xved_value where it was testing for <undef> to
          return "undef" instead, but not erasing the <undef>.
        # Made updaters of WIN_resource and WIN_tree_resource do nothing if
          given "undef" for value.
--- John Gibson, Feb 22 1994
        Added UseXVedIcons application resource
--- John Gibson, Feb 21 1994
        Reorganised text_resources
--- John Gibson, Feb 15 1994
        Added hscrollbar stuff
--- John Gibson, Jan 10 1994
        Added StatusAtTop application resource
--- John Gibson, Dec 17 1993
        Added MenuForeground/Background application resources
--- John Gibson, Dec 14 1993
        Added PopFocusOnly application resource
--- Julian Clinton, Dec 10 1993
        Modified default backup resource value for WarpPointer from 1 to 0
--- John Gibson, Nov 29 1993
        Added MaxWindows application resource
--- Jonathan Meyer, Sep 28 1993
        Added SearchDoesSelect.
--- John Gibson, Jun 12 1993
        Changed get_warp_contexts not to use Pop-11 compiler
--- John Gibson, Jun  7 1993
        Changed for POPC
--- John Gibson, Jun  3 1993
        Changes for POPC
--- Adrian Howard, Mar 22 1993
        Under OLIT changing the foreground of menus/scrollbars also changes the
        fontColor
--- John Gibson, Dec 19 1992
        Got rid of multiple initialisations of xved_ch*ildren_of_widget
        and replaced with lconstant children_of_widget calling scroll/menu
        procedures if present
--- Simon Nichols, Nov  3 1992
        Changed =< to <= in -get_bell_volume-.
--- Adrian Howard, Sep 24 1992
        o Resource setting of shadow/select colours now works properly with
        Motif gadgets.
        o Code for fetching shadow/select colours in Motif now only called
        once per widget-tree, rather than once per widget
--- Adrian Howard, Sep 23 1992
        The selectColor resource of Motif widgets is now updated if the
        background colour of a widget is changed
--- John Gibson, Sep  8 1992
        Got rid of all uses of T*YPESPEC
--- John Gibson, Aug 26 1992
        Sorted out XptBoolean fields in AppData structure
--- Adrian Howard, Aug 25 1992
        o Added "DialogForeground" and "DialogBackground" application resources
        o Added dynamic updating of dialog foreground and background colours
--- John Gibson, Aug 17 1992
        Made updater of xved_value locally set xved_size_hints_set to false
        when updating any values for current window
--- John Gibson, Aug 15 1992
        Added updating_subpart variable to make updater of WIN_resource
        only fiddle with Motif shadows if changing menubar/scrollbar
--- Adrian Howard, Aug 14 1992
        o Made bg of Form widget reflect the bg of the menubar
        o Altered -WIN_tree_resource- to use -children_of_widget- so menupane
        resources are updated correctly
        o Unmapped widgets during resource updates in -WIN_tree_resource- to prevent
        nasty flashing
--- Adrian Howard, Aug 13 1992
        Made sure shadow colours are updated in Motif
--- Adrian Howard, Aug 12 1992
        Added -WIN_tree_resource- so sub-buttons of menubars are affected
        by altering menubarForeground|Background
--- John Gibson, Jul 28 1992
        Added current window access for scroll/menubar/Fore/Background
--- John Gibson, Jul 28 1992
        Removed call of xved_reset_form from subpartOn -- not necessary for
        either OLIT or MOTIF, and screws up OLIT
--- John Gibson, Jul 24 1992
        Added "Vanilla" application resource
--- John Gibson, Jul 24 1992
        Removed assignments to "defaultWindow" for scrollbarOn and menubarOn
        (these are on in the widget by default)
--- John Gibson, Jun  5 1992
        Added ShowFileName application resource for controlling whether
        vednamestring is displayed on statusline by default
--- John Gibson, Feb 13 1992
        Added selectionColorNum to text resources
--- John Gibson, Jan 21 1992
        Changed xved_value so that it can take a list of key names instead
        of a single key, and made the updater update a contiguous sequence
        of current window WIN_resource fields with a single XtVaSetValues
--- John Gibson, Jan 15 1992
        Added markForeground/Background text resources
--- Jonathan Meyer, Jan 13 1992
        Added BellVolume application resource
--- John Gibson, Jan 13 1992
        Added altFont etc to text resources
--- John Gibson, Dec 28 1991
        Added drawGraphicChars text resource, removed UseOwnAppContext
        application resource and tidied up some others.
--- John Gibson, Dec 18 1991
        Added boldFont to text_resources
--- John Gibson, Nov  4 1991
        Added current window values for "type" and "class"
--- John Gibson, Oct 28 1991
        Added VScrollStep and HScrollStep application resources
--- John Gibson, Oct  3 1991
        Added proper "geometry" resource and other changes
--- John Gibson, Sep 29 1991
        xved_get_subpart takes 3rd arg to say managed only
--- John Gibson, Sep 27 1991
        Added "position" resource to existing "x" and "y", and changed
        default value procedure to be xved_make_window_position.
--- Jonathan Meyer, Sep 25 1991
        Rewrote subpartOn to use xved_get_subpart, and to reset the form
        when the subpartOn value is updated
--- John Gibson, Sep 23 1991
        false -> default menubarOn
--- Robert John Duncan, Sep 19 1991
        Temporarily undid last change, because XVed can't cope with it.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
        Changed default WarpPointer resource to 0.
--- Jonathan Meyer, Sep  3 1991
        Changed DEF MOTIF to isdefined("MOTIF") in xved_get_appl_resources
        so that it works correctly if MOTIF is loaded after XVED.
--- John Gibson, Sep  3 1991
        Changed OFFSET to use new FIELDOFFSET
--- John Gibson, Aug  2 1991
        Added pointerForeground/Background and put basic resources in
        a single list instead of having them in 3 different places
--- Jonathan Meyer, Jul 30 1991
        fixed "iconic" type - made it use wved_is_open_window
--- Jonathan Meyer, Jul 29 1991
        Allowed xved_value to take a vfc or wvedwindow as a type argument
--- Adrian Howard, Jul 29 1991 : Changed PwmButtonBindings to UsePwmMouseBindings
--- Jonathan Meyer, Jul 27 1991 offset -> OFFSET, sizeof -> SIZEOF
--- Jonathan Meyer, Jul 27 1991
        Added WindowManager. Haven't added code
        to set application resources based on WindowManager yet.
--- Adrian Howard, Jul 25 1991
        - removed references to UserLevel resource
        - added PwmButtonBindings resource
--- Jonathan Meyer, Jul  7 1991
        Made default warp contexts "all"
--- Jonathan Meyer, Jul  7 1991
        Removed usage of fast_idval (doesn't work with active vars)
--- Jonathan Meyer, Jul  5 1991
        Removed numRows/numColumns from the xved_get_app_resources proc
--- Jonathan Meyer, Jul  5 1991
        Removed x/y from the xved_make_create_args procedure (now done
        after the window creation)
--- Jonathan Meyer, Jul  3 1991
        Added scrollbarOn and Menu types
--- Jonathan Meyer, Jul  3 1991
        Corrected name of keyMapTable
--- Jonathan Meyer, Jul  2 1991
        Added borderPixmap
--- Adrian Howard, Jun 26 1991 : Default EditKeysEnabled is now -false-
--- Jonathan Meyer, Jun 25 1991
        Added EditKeysEnabled
--- Jonathan Meyer, Jun 24 1991
        Added MultiClickTimeOut
--- Jonathan Meyer, Jun 17 1991
        Added xved_save_defaults
 */
