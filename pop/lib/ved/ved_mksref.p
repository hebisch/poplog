/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mksref.p
 > Purpose:         Make a hotlink to a doc file section
 > Author:          John Gibson, Dec 11 1996
 > Documentation:   REF * VEDCOMMS
 > Related Files:
 */
compile_mode :pop11 +strict;

global constant $-vedselectioncoords;

section $-ved => ved_mksref;

uses ved_g;

define ved_mksref;
    lvars args, chat_arg, line, col;
    dlvars other = false, s_string, comm;
    dlocal vedargument;

    define get_index();
        lvars dtype, vname, name, n, s, text;

        define stack_chars(s, attr);
            lvars c;
            if isword(s) then fast_word_string(s) -> s endif;
            fast_for c in_string s do c fi_|| attr endfor
        enddefine;

        ved_copy();
        unless listlength(vveddump) == 1
        and is_new_indexline(hd(vveddump)) ->> s_string then
            vederror('MARKED RANGE IS NOT AN INDEX LINE')
        endunless;
        s_string -> text;
        if isstartstring('...', s_string) then
            ;;; qualify with super-heading
            fast_for n from vvedmarklo-1 by -1 to 1 do
                is_new_indexline(fast_subscrv(n,vedbuffer)) -> s;
                if s and not(isstartstring('...', s)) then
                    s <> ':\s' <> allbutfirst(5,s_string) -> text;
                    quitloop
                endif
            endfor
        endif;
        allbutfirst(skipchar(`\s`,locchar(`\s`,1,text),text) - 1, text)
                                -> text;
        stack_chars(text, `\[bi2A]`);
        returnunless(other);

        vedpositionpush();
        vedtopfile();
        vedmoveitem() -> dtype;
        unless vedgetsysfilepdr(dtype) ->> vname then
            vederror('OTHER FILE IS NOT A DOCUMENTATION FILE')
        endunless;
        vednextitem() -> name;
        vedpositionpop();
        allbutfirst(4, vname) sys_>< '\s' sys_>< name -> comm;

        explode('\sin\s'),
        stack_chars(dtype, `\[i2A]`), explode('\{2A}\Ss*\Ss'),
        stack_chars(name, `\[i2A]`)
    enddefine;

    sysparse_string(vedargument) -> args;

    #|  if args /== [] and hd(args) = 'o' then
            ;;; reference to other file
            tl(args) -> args;
            true -> other;
            unless listlength(vedbufferlist) >= 2 then
                vederror('NO OTHER FILE')
            endunless;
            ved_file_change_apply(get_index, vedbufferlist(2))
        else
            get_index()
        endif
    |#;

    if args == [] then
        ;;; insert text at current position
        vedline, vedcolumn -> (line, col);
        vedinsertstring();
        vedcharinsert(`\s`);
        vedpositionpush();
        vedjumpto(line, col);
        ;;; search forward for the active segment
        while vedcurrentdchar() &&=_0 `\[A]` do vedwordright() endwhile
    else
        ;;; use the range specified by the ved_chat arg as the text
        erasenum();
        hd(args) -> chat_arg;
        chat_arg <> '\s+2A' -> vedargument;
        ved_chat();
        vedpositionpush();
        if chat_arg = 's' then
            explode(vedselectioncoords) -> (_, _, line, col);
            vedjumpto(line, col)
        endif;
        ;;; search back for the active segment
        while (vedcharleft(), vedcurrentdchar() &&=_0 `\[A]`) do endwhile
    endif;


    ;;; insert command with ved_tact
    if other then
        sprintf('=%S /@a%S;;;%S /...', [^comm ^s_string ^comm])
    else
        '=(N)/@a' <> s_string
    endif -> vedargument;
    ved_tact();

    vedpositionpop()
enddefine;


endsection;
