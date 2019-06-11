/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_draw.p
 > Purpose:         Simple interactive drawing program for drawing boxes
 > Author:          Jonathan Meyer, Oct  8 1993 (see revisions)
 > Documentation:   REF *VED_DRAW
 > Related Files:
 */
compile_mode :pop11 +strict;
section;

/* ==== VED BASED TABLE DRAWING FACILITY ================================== */

uses veddrawline;

include vm_flags.ph;

lconstant
    draw_string = 'DRAWING ON (<DEL> to delete, <RETURN> to stop)',
    delete_string = 'DELETING ON (<DEL> to draw, <RETURN> to stop)'
;

lvars
    drawing     = false,
    old_static  = false,
;

define lconstant put_drawing_message();
    vedputmessage(if drawing == true then draw_string else delete_string endif)
enddefine;

define lconstant set_cursor();
    if drawing == true then `\[4]^` else `\[2]^` endif -> vedscreenstaticcursor
enddefine;

define lconstant set_locals(val);
    lvars val, id;
    lconstant id_list = [%
        ident drawing,
        ident old_static,
        ident vedscreenstaticcursor,
        ident ved_apply_action
    %];
    ;;; for protected ved_apply_action
    dlocal pop_vm_flags = pop_vm_flags || VM_NOPROT_PVARS;

    unless val then old_static -> vedstatic endunless;
    fast_for id in id_list do
        val -> is_vedfile_local(id, ved_current_file)
    endfor;
    if val then vedstatic -> old_static endif
enddefine;

define lconstant Draw(p);
    lvars procedure p;
    dlocal rubout = (drawing == "delete");

    ;;; veddrawline doesn't preserve vedline/vedcolumn so we use a wrapper
    define lconstant DrawLine with_nargs 4;
        lvars (sc, sl) = (vedcolumn, vedline);
        veddrawline((), sc, sl);
        vedjumpto(sl, sc)
    enddefine;

    if ved_on_status then
        p()
    else
        DrawLine(vedcolumn, vedline, p());
        put_drawing_message();
    endif;
enddefine;

define lconstant drawcharleft = Draw(%vedcharleft%) enddefine;
define lconstant drawcharright = Draw(%vedcharright%) enddefine;
define lconstant drawcharup = Draw(%vedcharup%) enddefine;
define lconstant drawchardown = Draw(%vedchardown%) enddefine;

;;; this is like vedinsertvedchar, but for drawing characters.
define lconstant insertdrawchar();
    lvars c = ved_last_char;
    if ved_on_status then
        vedinsertvedchar();
    else
        if     c == `h` then drawcharleft();
        elseif c == `j` then drawchardown();
        elseif c == `k` then drawcharup();
        elseif c == `l` then drawcharright();
        elseif c == `\s` then
            vedcharinsert(`\s);
            put_drawing_message();
        elseif c == `\\` then
            vedcharinsert(vedinascii());
            put_drawing_message();
        else
            vedscreenbell();
            put_drawing_message();
        endif;
    endif;
enddefine;

;;; toggle deletion of characters
define lconstant toggle_delete();
    if drawing == true then "delete" else true endif -> drawing;
    set_cursor();
    put_drawing_message();
enddefine;

define lconstant simple_action = newassoc([
        [^vedchardelete     ^toggle_delete]
        [^veddotdelete      ^toggle_delete]
        [^vedcharup         ^drawcharup]
        [^vedchardown       ^drawchardown]
        [^vedcharleft       ^drawcharleft]
        [^vedcharright      ^drawcharright]
        [^vedinsertvedchar  ^insertdrawchar]
    ])
enddefine;

define lconstant apply_action(p, old_apply, old_cursor);
    lvars p, old_apply, old_cursor, my_p, file;
    if simple_action(p) ->> my_p then
        my_p -> p
    elseif p == veddocr then        ;;; this one is a vars!
        if ved_on_status then
            set_cursor();
            ved_current_file -> file;
            old_apply(p);
            if ved_current_file == file then set_cursor() endif
        else
            set_locals(false)       ;;; finished
        endif;
        return
    elseif p == vedenter then
        old_cursor -> vedscreenstaticcursor
    elseif p == vedstatusswitch then
        if ved_on_status then
            set_cursor()
        else
            old_cursor -> vedscreenstaticcursor
        endif
    elseif p == vedsetstatic and not(ved_on_status) then
        put_drawing_message();
        return
    endif;
    chain(p, old_apply)
enddefine;

define vars ved_draw;
    if is_vedfile_local(ident drawing, ved_current_file) then
        ;;; drawing is on -- turn it off
        set_locals(false);
        vedputmessage('DRAWING OFF')
    else
        ;;; turn on drawing
        set_locals(true);
        true -> vedstatic;
        lblock
        compile_mode :vm -prmprt;
            apply_action(% ved_apply_action, vedscreenstaticcursor %)
                                                -> ved_apply_action;
        endlblock;
        true -> drawing;
        set_cursor();
        ;;; make sure redo works
        vedputcommand('draw');
        put_drawing_message()
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 23 1994
        Various fixes
--- John Gibson, Jan 15 1994
        Reimplemented, and made it change the cursor.
 */
