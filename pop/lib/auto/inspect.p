/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.all/lib/auto/inspect.p
 > Purpose:        A simple structure browser
 > Author:         John Williams, Apr 29 1987 (see revisions)
 > Documentation:  HELP * INSPECT
 > Related Files:  C.all/lisp/src/lispinspect.p
 */

compile_mode :pop11 +strict;

section;

include subsystem.ph;
include ved_declare.ph;


/* Global declarations */

global vars
    inspect_idents      =   false,
    inspect_more_label  =   '*** MORE ***',
    inspect_pairs       =   false,
    inspect_prompt      =   'inspect> ',
    ;


global vars procedure (
    inspect,
    inspect1,
    inspect_display,
    inspect_read_var,
    inspect_read_slotval,
    inspect_special_access   = (procedure; ->; false endprocedure),
    inspect_special_describe = (procedure; ->; false endprocedure),
    inspect_special_display  = (procedure; ->; false endprocedure),
    );


global vars inspect_help_list =
    [
    '-----------------------------------------------------------'
    'a <var>   Save the current object in <var>'
    'b         Print a backtrace of the history list'
    'e         Switch to evaluate mode'
    'g         Redisplay current object from specified slot number'
    'h or ?    Help, prints this'
    'i         Toggle display of identifiers'
    'k         Inspect key of current object'
    'm         Display more slots (if available)'
    'p         Toggle displaying of lists as pairs'
    'q         Quit, leave the inspector'
    's         Set a slot value to an evaluated expresion'
    'x         Expand dynamic list (by one element)'
    '.         Redisplay current object from the top'
    '-         Move out a level of inspection'
    '+         Move in a level of inspection'
    '0,1,2 ..  Inspect value of numbered slot'
    '-----------------------------------------------------------'
    ];

endsection;


section $-inspect;

/* Utilities */

define lconstant Ierror();
    nprintf();
    sys_clear_input(pop_charin_device);
enddefine;


define Ivalof(item);
    lvars id;
    if (isdeclared(item) ->> id) then
        idval(id)
    else
        pop_undef
    endif
enddefine;

updater(valof) -> updater(Ivalof);


define lconstant Iexpand_dynamic_list(list);
    lvars n = 0;
    while ispair(list) do
        if fast_front(list) == true
        and isprocedure(fast_back(list)) then
            hd(list) ->;
            return(n)
        endif;
        n fi_+ 1 -> n;
        fast_back(list) -> list
    endwhile;
    false
enddefine;


/* Reading */

define lconstant Iread_command(commandset, popprompt) -> item;
    lvars char;
    dlocal popprompt, poplastchar = 0;

    repeat
        cucharin() -> char;
        nextif(char == `:`);        ;;; for consistency with Lisp debugger
        if char == termin then
            cucharout(`\n`);
            `Q`
        else
            lowertoupper(char)
        endif -> char;
        quitif(char fi_> `\s`)
    endrepeat;
    if isnumbercode(char) then
        char fi_- `0` -> item;
        while isnumbercode(cucharin() ->> char) do
            (item * 10) + (char fi_- `0`) -> item
        endwhile
    elseif strmember(char, commandset) then
        consword(char, 1) -> item
    else
        Ierror(char, 'Invalid input:  %c  (type H for help, Q to quit)');
        false -> item
    endif
enddefine;


define lconstant Iread_slotnum();
    Iread_command('', 'Slot number? ')
enddefine;


define vars inspect_read_var() -> var;
    dlocal popprompt = 'Variable name? ';
    unless isword(incharitem(cucharin)() ->> var) do
        false -> var
    endunless
enddefine;


define vars inspect_read_slotval();
    lvars sl;
    dlocal pop_readline_prompt = 'New slot value? ';
    stacklength() -> sl;
    while stacklength() == sl do
        pop11_compile(readline())
    endwhile;
    setstacklength(sl fi_+ 1)
enddefine;


/* Access/update */

define lconstant Iget_vector_accessor(vec, n);
    if n fi_< datalength(vec) then
        n fi_+ 1, vec;
        class_fast_subscr(datakey(vec));
        true
    else
        false
    endif
enddefine;


define lconstant Iget_prologterm_accessor(term, n);
    if n == 0 then
        term;
        fast_prolog_functor;
        'Functor';
    elseif n fi_< datalength(term) then
        n, term;
        class_fast_subscr(prologterm_key);
        true;
    else
        false;
    endif;
enddefine;


define lconstant Iget_record_accessor(rec, n);
    lvars key, pdr;
    datakey(rec) -> key;
    if n fi_< datalength(key) then
        rec;
        class_access(n fi_+ 1, key) ->> pdr;
        if isword(pdprops(pdr)) then
            pdprops(pdr)
        else
            true
        endif
    else
        false
    endif
enddefine;


define lconstant Iget_number_accessor(num, n);

    define lconstant Iaccess_float(f, n) -> n;
        float_decode(f, true);
        subscr_stack(3 fi_- n) -> n;
        erasenum(3)
    enddefine;

    if isdecimal(num) then
        if n fi_< 3 then
            return(num, n, Iaccess_float,
                   fast_subscrv(n fi_+ 1, {'Mantissa' 'Exponent' '    Sign'}))
        endif
    elseif isratio(num) then
        if n == 0 then
            return(num, numerator, '  Numerator')
        elseif n == 1 then
            return(num, denominator, 'Denominator')
        endif
    elseif iscomplex(num) then
        if n == 0 then
            return(num, realpart, 'Realpart')
        elseif n == 1 then
            return(num, imagpart, 'Imagpart')
        endif
    endif;
    false
enddefine;


define lconstant Iget_list_accessor(list, n);
    lvars next;
    until n == 0 do
        fast_back(list) -> next;
        if atom(next) then
            if n == 1 then
                return(list, fast_back, 'End')
            else
                return(false)
            endif
        endif;
        n fi_- 1 -> n;
        next -> list
    enduntil;
    list, fast_front, true
enddefine;


define lconstant Iget_word_accessor(word, n);
    if n == 0 then
        if inspect_idents then
            word, identof, 'Ident'
        else
            word, Ivalof, 'Value'
        endif
    else
        false
    endif
enddefine;


define lconstant Iget_ident_accessor(id, n);
    if n fi_< 3 then
        id;
        explode(fast_subscrv(n fi_+ 1,
                    #_< {{% Ivalof,     ' Value' %}
                         {% isident,    'Status' %}
                         {% identprops, ' Props' %}} >_#))
    else
        false
    endif
enddefine;


lvars Ipropdata = [];

define lconstant Iget_propent_accessor(propent, n);
    if n == 0 then
        propent, fast_prop_entry_arg, ' Item'
    elseif n == 1 then
        propent, fast_prop_entry_value,  'Value'
    else
        false
    endif
enddefine;


define lconstant Iget_property_accessor(prop, n);
    lvars vec, state, entry;
    if n == 0 then
        prop, property_default, 'Default    '
    elseif n == 1 then
        prop, property_active,  'Active     '
    else
        n fi_- 1 -> n;
        if (fast_lmember(prop, Ipropdata) ->> vec) then
            fast_front(fast_back(vec)) -> vec
        else
            Prop_entry_state_init(prop) -> state;
            {% while Prop_entry_state_next(state) ->> entry do
                entry
            endwhile %} -> vec;
            conspair(prop, conspair(vec, Ipropdata)) -> Ipropdata
        endif;
        if n fi_<= datalength(vec) then
            fast_subscrv(n, vec), identfn, true
        else
            false
        endif
    endif
enddefine;


constant procedure Iget_access_pdr;

define lconstant Iget_array_accessor(array, n);
    lvars lo, hi;
    arrayvector_bounds(array) -> lo -> hi;
    if n fi_<= (hi fi_- lo) then
        Iget_access_pdr(arrayvector(array), lo fi_+ n fi_- 1)
    else
        false
    endif
enddefine;


define lconstant Iget_procedure_accessor(pdr, n);
    lvars bool;
    if n == 0 then
        pdr, pdprops, 'Props   '
    elseif n == 1 then
        pdr, updater, 'Updater '
    elseif (isclosure(pdr) ->> bool) then
        if n == 2 then
            if bool == 1 then
                pdpart(pdr), identfn,
            else
                pdr, pdpart,
            endif;
            'Pdpart  '
        elseif (n fi_- 2) fi_<= datalength(pdr) then
            if bool == 1 then
                fast_frozval(n fi_- 2, pdr), identfn, true
            else
                n fi_- 2, pdr, fast_frozval, true
            endif
        else
            false
        endif
    else
        false
    endif
enddefine;


define lconstant Iget_key_accessor(key, n);
    lvars spec;
    class_field_spec(key) -> spec;
    n fi_+ 1 -> n;
    if n fi_< (if islist(spec) then 10 elseif spec then 12 else 8 endif) then
        key;
        explode(fast_subscrv(n,
                #_< {{% class_dataword,   '  Dataword' %}      ;;; 1
                     {% class_field_spec, 'Field Spec' %}      ;;; 2
                     {% class_recognise,  'Recogniser' %}      ;;; 3
                     {% class_=,          '   Class_=' %}      ;;; 4
                     {% class_apply,      '   Applier' %}      ;;; 5
                     {% class_print,      '   Printer' %}      ;;; 6
                     {% class_hash,       '    Hasher' %}      ;;; 7
                     {% class_cons,       '  Cons pdr' %}      ;;; 8
                     {% class_dest,       '  Dest pdr' %}      ;;; 9
                     {% class_init,       '  Init pdr' %}      ;;; 10
                     {% class_subscr,     'Subscr pdr' %}}     ;;; 11
                >_#))
    elseif islist(spec) and (n fi_- 9 ->> n) fi_<= datalength(key) then
        n, key, class_access, true
    else
        false
    endif
enddefine;


define lconstant Iget_section_accessor(sect, n);
    if n == 0 then
        sect, section_name, '  Name'
    elseif n == 1 then
        sect, section_supersect, 'Parent'
    else
        n fi_- 1 -> n;
        section_subsect(sect) -> sect;
        if listlength(sect) fi_>= n then
            subscrl(n, sect), identfn, ' Child'
        else
            false
        endif
    endif
enddefine;


define Iget_access_pdr(item, n);
    lvars pdr;
    unless (inspect_special_access(item) ->> pdr) do
        if isvectorclass(item) then
            if isprologterm(item) then
                Iget_prologterm_accessor
            else
                Iget_vector_accessor
            endif
        elseif ispair(item) and not(inspect_pairs) then
            Iget_list_accessor
        elseif isrecordclass(item) then
            Iget_record_accessor
        elseif isnumber(item) then
            Iget_number_accessor
        elseif isword(item) then
            Iget_word_accessor
        elseif isident(item) then
            Iget_ident_accessor
        elseif isarray(item) then
            Iget_array_accessor
        elseif isproperty(item) then
            Iget_property_accessor
        elseif isprocedure(item) then
            Iget_procedure_accessor
        elseif iskey(item) then
            Iget_key_accessor
        elseif issection(item) then
            Iget_section_accessor
        elseif isprop_entry(item) then
            Iget_propent_accessor
        else
            return(false)
        endif -> pdr
    endunless;
    pdr(item, n)
enddefine;


define lconstant Iaccess(item, n) -> lab;
    if (Iget_access_pdr(item, n) ->> lab) then
        apply()
    endif
enddefine;


define lconstant Iupdate(valpdr, item, n) -> pdr;
    lvars sl;
    false -> pdr;
    stacklength() -> sl;

    define dlocal prmishap();
        setstacklength(sl);
        Ierror('*** Error updating slot ***');
        exitfrom(true, Iupdate)
    enddefine;

    popstackmark;
    if Iget_access_pdr(item, n) and (updater() ->> pdr) then
        valpdr() -> subscr_stack(stacklength() fi_- sl fi_- 1);
        pdr()
    else
        setstacklength(sl)
    endif
enddefine;


/* Display */

define lconstant active Iwindowsize();
    if testdef vedprocess then
        if weakref[vedprocess] vedediting then
            fi_max(weakref[vedprocess] vedwindowlength fi_- 5, 2)
        else
            fi_max(weakref[vedprocess] vedscreenlength fi_- 4, 2)
        endif
    else
        20
    endif
enddefine;


define lconstant Idisplay_slot(item, num, label, upd);
    lvars bra, ket;
    dlocal pop_pr_quotes = true;

    if upd then `[`, `]` else `{`, `}` endif -> ket -> bra;
    if isboolean(label) then
        nprintf(item, ket, num, bra, '  %c%i%c  %p')
    else
        nprintf(item, label, ket, num, bra, '  %c%i%c  %s:  %p')
    endif
enddefine;


define lconstant Idisplay_list(list, offs);
    lvars i, len, maxlen;
    list -> i;
    0 -> len;
    repeat
        if len == offs then
            i -> list
        endif;
        quitif(atom(i));
        fast_back(i) -> i;
        len fi_+ 1 -> len
    endrepeat;
    if i == nil then
        nprintf(len, 'A list of length %i')
    else
        nprintf(len, 'A dotted list of length %i')
    endif;
    returnif(offs fi_> len) (false, len);
    offs fi_+ Iwindowsize -> maxlen;
    fast_for i from offs to maxlen do
        if ispair(list) then
            Idisplay_slot(fast_destpair(list) -> list, i, false, true)
        else
            Idisplay_slot(list, i, 'End', true);
            return(false, i)
        endif
    endfast_for;
    nprintf(inspect_more_label);
    true, maxlen
enddefine;


define lconstant Idisplay_other(item, offs);
    lvars maxlen, i, lab, pdr;
    offs + Iwindowsize -> maxlen;
    fast_for i from offs to maxlen do
        if (Iget_access_pdr(item, i) ->> lab) then
            -> pdr;
            Idisplay_slot(pdr(), i, lab, updater(pdr))
        else
            return(false, i fi_- 1)
        endif
    endfast_for;
    nprintf(inspect_more_label);
    true, maxlen
enddefine;


define lconstant Idescribe(item);
    lvars bool, dword;
    dataword(item) -> dword;
    if inspect_special_describe(item) then
        nprintf()
    elseif isprocedure(item) then
        if isarray(item) then
            nprintf(boundslist(item), 'An array, bounds %p')
        elseif isproperty(item) then
            nprintf(property_size(item), datalength(item),
                    'A property with %i entries, table size %i')
        else
            if (isclosure(item) ->> bool) then
                if bool == 1 then 'protected closure' else "closure" endif
            else
                "procedure"
            endif -> dword;
            nprintf(pdnargs(item), dword, 'A %s of %i arguments')
        endif
    elseif isprologterm(item) then
        nprintf(datalength(item) fi_- 1, 'A prologterm with %i arguments');
    elseif isvectorclass(item) then
        nprintf(datalength(item), dword, 'A %s of length %i')
    elseif isrecordclass(item) or isident(item)
    or isratio(item) or iscomplex(item)
    or iskey(item) or issection(item) then
        nprintf(dword, 'A %s')
    elseif isprop_entry(item) then
        nprintf(dword, class_attribute(datakey(item), "prop_entry"), 'A %s %s')
    else
        nprintf(item, dword, 'A %s, %p')
    endif
enddefine;


define vars inspect_display(item, offs);
    lvars pdr;
    if (inspect_special_display(item) ->> pdr) then
        pdr(item, offs)
    elseif ispair(item) and not(inspect_pairs) then
        Idisplay_list(item, offs)
    else
        Idescribe(item);
        Idisplay_other(item, offs)
    endif
enddefine;


/* Inspect itself */

define vars inspect(object);
    lvars command, len, more, num, sl, temp, top;
    dlocal
        cucharin = charin,
        cucharout = charout,
        subsystem,
        Ipropdata,
        ;

    lconstant macro ( OBJ  = 1, OFFS = 2, LAST = 3, NEXT = 4 ) ;

    define lconstant syntax 1 !;
        lvars n;
        pop_expr_inst(pop_expr_item);
        sysPUSHQ(itemread());
        sysSWAP(1, 2);
        sysCALLQ -> pop_expr_inst;
        subscrv -> pop_expr_item;
    enddefine;

    define lconstant Ichecknum(lo, num, hi);
        num fi_>= lo and num fi_<= hi
    enddefine;

    define dlocal interrupt();
        setstacklength(sl);
        cucharout(`\n`);
        goto NEXTLOOP
    enddefine;

    define dlocal keyboard_interrupt();
        Ierror('\n*** Interrupt ***');
        interrupt()
    enddefine;

    stacklength() -> sl;
    {% object, 0, false, false %} ->> object -> top;
    inspect_display(object!OBJ, object!OFFS) -> len -> more;
    repeat
    NEXTLOOP:
        Iread_command('ASH?BMG.-+QKIPEX', inspect_prompt) -> command;
        quitif(command == "Q");
        if isinteger(command) then
            if Ichecknum(object!OFFS, command, len)
            and Iaccess(object!OBJ, command) then
            PUSH:
                -> temp;
                {% temp, 0, object, false %} ->> object!NEXT -> object;
                inspect_display(object!OBJ, object!OFFS) -> len -> more
            else
                Ierror(command, 'Cannot access slot %i')
            endif
        elseif command == "-" then
            if (object!LAST ->> temp) then
                temp -> object;
                inspect_display(object!OBJ, object!OFFS) -> len -> more
            else
                Ierror('At top of inspect stack')
            endif
        elseif command == "+" then
            if (object!NEXT ->> temp) then
                temp -> object;
                inspect_display(object!OBJ, object!OFFS) -> len -> more
            else
                Ierror('At bottom of inspect stack')
            endif
        elseif command == "A" then
            if (inspect_read_var() ->> temp) then
                object!OBJ -> valof(temp);
                nprintf(temp, 'Saved current object in %p')
            else
                Ierror('Invalid variable specified')
            endif
        elseif command == "S" then
            if (Iread_slotnum() ->> num) then
                if Ichecknum(object!OFFS, num, len)
                and Iupdate(inspect_read_slotval, object!OBJ, num) then
                    inspect_display(object!OBJ, object!OFFS) -> len -> more
                else
                    Ierror(num, 'Cannot update slot %i')
                endif
            endif
        elseif command == "G" then
            if (Iread_slotnum() ->> num) then
                num -> object!OFFS;
                inspect_display(object!OBJ, object!OFFS) -> len -> more
            endif
        elseif command == "M" then
            if more then
                len fi_+ 1 -> object!OFFS;
                inspect_display(object!OBJ, object!OFFS) -> len -> more
            else
                Ierror('No more to display')
            endif
        elseif command == "." then
        REDRAW:
            0 -> object!OFFS;
            inspect_display(object!OBJ, object!OFFS) -> len -> more
        elseif command == "B" then
            top -> temp;
            repeat
                if temp == object then
                    nprintf(temp!OBJ, '. %p')
                else
                    nprintf(temp!OBJ, '  %p')
                endif;
                quitunless(temp!NEXT ->> temp)
            endrepeat
        elseif command == "K" then
            datakey(object!OBJ);
            goto PUSH
        elseif command == "I" then
            not(inspect_idents) -> inspect_idents;
            goto REDRAW
        elseif command == "P" then
            not(inspect_pairs) -> inspect_pairs;
            goto REDRAW
        elseif command == "E" then
            subscr_subsystem(SS_COMPILER,subsystem)(cucharin);
            nprintf('');
            inspect_display(object!OBJ, object!OFFS) -> len -> more
        elseif command == "X" then
            if (Iexpand_dynamic_list(object!OBJ) ->> temp) then
                inspect_display(object!OBJ, temp) -> len -> more;
            endif
        elseif command == "H" or command == "?" then
            applist(inspect_help_list, nprintf)
        endif
    endrepeat
enddefine;


define vars inspect1(item);
    lvars len = 0;
    dlocal inspect_more_label;
    '*** Press <RETURN> to see more ***' -> inspect_more_label;
    while (inspect_display(item, len) -> len) do
        quitunless(systrmdev(poprawdevin));
        quitif(rawcharin() /== `\r`);
        len fi_+ 1 -> len
    endwhile
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 16 1996
        Replaced local interrupt in inspect with separate local interrupt
        and keyboard_interrupt.
--- John Williams, Jun 12 1995
        updater(Ivalof) now == updater(valof).
--- John Williams, Jun  1 1995
        Ved vars are now weakref'd.
--- John Williams, May 12 1993
        Fixed bugs in Idisplay_list (introduced by Andreas' fix below)
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem etc
--- John Gibson, Oct 10 1992
        Added strict
--- Andreas Schoter, Sep  9 1991
        Added check to -Idisplay_list- for case when "g'ing" to the end of
        a list. (Previously this got the slot numbering wrong)
--- John Williams, Jan 16 1991
        Now uses -class_field_spec- and -class_attribute-
--- John Williams, Jul 12 1990
        Made 'propent' local to -Iget_propent_accessor-
--- John Williams, May 11 1990
        Minor improvements to display of properties
--- John Williams, Jun  7 1989
        Now uses -isprop_entry-, no longer changes prop_entry -class_print-
--- John Williams, Jun  2 1989
        Now uses -fast_prop_entry_arg- and -fast_prop_entry_value-
--- John Gibson, May 24 1989
        Replaced use of -property_vector- with -Prop_entry_state_init/next-
--- John Williams, Apr  5 1989
        Now recognises protected closures
--- John Williams, Apr  4 1989
        -Iget_property_accessor- now uses -property_vector-
--- John Williams, Mar  8 1989
        -Ivalof- now uses -isdeclared- instead of -identprops-
--- John Williams, Feb  9 1989
        -Iread_command- now ignores colons (to soothe Lisp debugger users)
--- John Williams, Oct  6 1988
        -Ierror- now clears -pop_charin_device-
--- John Williams, Jun 22 1988
        Uses -fast_lmember- instead of -Lisp_list_assoc_val-
--- John Williams, Feb 12 1988
        Added -inspect1-
--- John Williams, Sep 14 1987
        Made -subsystem- a local of -inspect- (cf BR johnw.82)
--- Simon Nichols, Jul 15 1987
        Modified to inspect prologterms correctly.
        This involved a new procedure, -Iget_prologterm_accessor-,
        and small additions to -Idescribe- and -Iget_access_pdr-.
 */
