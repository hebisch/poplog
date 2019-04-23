/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/ved/src/vdtidy.p
 > Purpose:         Tidying up POP-11 text
 > Author:          Jon Cunningham and Aaron Sloman (see revisions)
 */

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedrepeater, vedmarkfind, vedmarkpush, vedmarkpop),
     ;

global vars
        procedure (ved_mcp), vedhardtabs,
     ;

section $-Sys$-Ved;

constant
        procedure (Span_insert, Span_delete, Refresh_screenline_tail,
        )
        string_of_spaces, string_of_tabs
     ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Ved =>   vedopeners, vedclosers, vedbackers, vedforwarders,
                        vedvarskeywords,
                        ved_tidy_skip_pairs, ved_tidy_skip_some,

                        ved_tidy, ved_jcp
                    ;


;;; globals for ved_tidy
vars
     ;;; opening brackets
     vedopeners      =
        [ ( define fast_for for foreach forevery if lblock procedure
        repeat fast_repeat switchon unless until while #| ],

     ;;; closing brackets
     vedclosers      =
        [ ) enddefine endfast_for endfor endforeach endforevery
         endif endlblock endprocedure endrepeat endswitchon
         endunless enduntil endwhile
         endfast_repeat end close |#],

     ;;; words to be de-indented if they occur at the beginning of a line
     vedbackers      =
        [ then times do else elseif elseunless step till forever quitif
          quitunless nextif nextunless returnif returnunless and or
          case ],

     ;;; words to be indented if they start a line
     vedforwarders   = [ -> ->> --> ==> => ],

     ;;; words starting declarations
     vedvarskeywords =
        [ lconstant lvars dlvars global constant vars dlocal section],

     ;;; words specifying unaltered indentation (i.e. from the occurrence
     ;;; of the word to an occurrence of the closing bracket mentioned).
     ved_tidy_skip_pairs     = [vedset [endvedset] lib [^newline ;]],

     ;;; words specifying that N additional items are to be read in and
     ;;; regarded as quoted, and therefore ignored. N is in a list.
     ved_tidy_skip_some      = [" [2] nonmac [1] nonop [1] nonsyntax [1]
         uses [1]]
     ;

lconstant ved_tidy_globals =
    [ vedopeners vedclosers vedbackers vedforwarders
        vedvarskeywords ved_tidy_skip_pairs ved_tidy_skip_some]
     ;

;;; First line of marked range assumed to be indented OK

define lconstant Change_indent(indent);
    ;;; Called to alter indentation at beginning of current line,
    ;;; by inserting or deleting tabs or spaces as necessary
    lvars indent, string, _size, _tabstop;
    dlocal vedcolumn, vedhardtabs;
    unless indent fi_< 0 or vvedlinesize == 0 then
        vedtextleft();
        indent fi_+ 1 -> indent;
        if indent fi_< vedcolumn then
            false -> vedhardtabs;
            Check_not_in_tab(indent);
            Span_delete(indent, vedcolumn, false);
        elseif indent fi_> vedcolumn  then
            if vednotabs then string_of_spaces else string_of_tabs endif
                -> string;
            datalength(string) -> _size;
            indent fi_- (indent fi_- 1) rem vedindentstep -> _tabstop;
            while vedcolumn fi_< _tabstop do
                Span_insert(string,
                    fi_min(_tabstop fi_- vedcolumn, _size), 0, vedcolumn);
            endwhile;
            if vedcolumn fi_< indent then
                Span_insert(string_of_spaces, indent fi_- vedcolumn,
                                                0, vedcolumn);
            endif;
        endif;
        Refresh_screenline_tail();
    endunless;
enddefine;

lvars
    _thisindent = 0,    ;;; used by ved_tidy_repeater and ved_tidy
    instring = false, linestart_was_instring = false;

define lconstant unindent_this_line();
    _thisindent fi_- vedindentstep -> _thisindent
enddefine;

define lconstant ved_tidy_repeater() -> _char;
    ;;; like vedrepeater, but keeps a record of whether it is in a string
    lvars _char;

    if vedline fi_> vvedbuffersize then
        termin -> _char
    elseif vedcolumn fi_> vvedlinesize then
        `\n` -> _char;
        if instring then
            Change_indent(_thisindent);
            -1 -> _thisindent;  ;;; prevent further changes till out of string
        endif;
        vedline fi_+ 1 -> vedline;
        vedusedsize(vedthisline()) -> vvedlinesize;
        1 -> vedcolumn;
        instring -> linestart_was_instring;
    else
        fast_subscrs(vedcolumn, vedthisline()) -> _char;
        vedcolumn fi_+ 1 -> vedcolumn;
        if _char == `'` then
            if instring then
                unless poplastchar == `\\` then
                    false -> instring
                endunless
            else
                unless poplastchar == ``` then
                    true -> instring
                endunless
            endif
        endif
    endif;
    _char -> poplastchar
enddefine;

define vars ved_tidy();
    lvars
        item
        lastitem,
        _count = 0, _partapply = 0,
        _indent,            ;;; current general indentation level
        _firstindent,
        _char
        labelstack = [],
        pcentstack = [[]],  ;;; record indentation level before "%"
        invars = false, indefine = false, skipend = false,
        listdepth = 0,

        procedure items = incharitem(ved_tidy_repeater),
        ;

    dlocal vedbreak = false,
        vedstatic = false, popnewline = true, instring,
        linestart_was_instring,
        _thisindent,        ;;; indentation level for current line
        ;

    ;;; Check globals are all lists, so that fast_lmember can be used

    for item in ved_tidy_globals do
        unless islist(valof(item)) then
            vederror('\{b}list needed for: ' <> (item sys_>< nullstring))
        endunless
    endfor;

    vedpositionpush();

    vedmarkfind();

    ;;; Find initial indentation level
    vedtextleft();
    vedcolumn fi_- 1  ->> _indent -> _firstindent;
    if vedcolumn == 1 then -1 else _indent endif -> _thisindent;

    false -> item;  ;;; for initial value of lastitem

    repeat
        item -> lastitem;

        ;;; read new item (using the repeater). Will skip comments (bug)
        items() -> item;
        _count fi_+ 1 -> _count;

        if linestart_was_instring then
            -1 -> _thisindent
        endif;

        if item == termin then
            quitloop    ;;; end of buffer
        elseif item == newline then
            ;;; If end of line reached. Go back and fix indentation
            vedcharup();
            unless vedline == vvedmarklo then
                Change_indent(_thisindent);
            endunless;
            false -> linestart_was_instring;
            if vvedlinesize fi_> 0
            and fast_subscrs(vvedlinesize,vedthisline()) == `\\` then
                -1 -> _thisindent;
            else
                _indent -> _thisindent;     ;;; default for next line
                0 -> _count
            endif;
            vednextline();
            if vedline fi_> vvedmarkhi then quitloop()
            elseif skipend then
                if islist(skipend) and fast_lmember(item, skipend) then
                    false -> skipend
                endif;
            endif;
            nextloop()
        endif;

        ;;; Settle indentation for current line, depending on first item
        if _count == 1  then
            if vedline == vvedmarklo then   ;;; beginning of range
                if fast_lmember(item, vedbackers)
                or fast_lmember(item, vedclosers)
                or fast_lmember(item, vedforwarders)
                then
                    vedindentstep fi_+ _indent -> _indent
                endif;
            elseif _thisindent fi_< 0 then
                ;;; Joined to previous line - do nothing
            elseif listdepth fi_> 0 then
                _indent -> _thisindent
            elseif invars or indefine or skipend
            or fast_lmember(item, vedforwarders)
            then
                _indent fi_+ vedindentstep -> _thisindent;
            elseif fast_lmember(item, vedbackers) or fast_lmember(item, vedclosers) then
                _indent fi_- vedindentstep -> _thisindent
            endif;
            if _thisindent fi_< 0 then
                0 -> _thisindent
            endif;
        elseif _count == 2 and item == ":" and listdepth == 0
        and not(indefine) and not(skipend)
        then
            ;;; Assume it's a label, and de-indent maximally
            _firstindent -> _thisindent
        endif;

        if skipend then
            /* When skipping n items, don't count newlines */
            if isinteger(skipend) then
                skipend fi_- 1 -> skipend;
                if skipend == 0 then
                    false -> skipend;
                endif
            elseif islist(skipend) and fast_lmember(item, skipend) then
                ;;; newline handled above
                false -> skipend;
            endif;
        elseif item == "[" or item == "{" then
            listdepth fi_+ 1 -> listdepth;
            vedindentstep fi_+ _indent -> _indent
        elseif listdepth fi_> 0 then
            if item == "]" or item == "}" then
                listdepth fi_- 1 -> listdepth;
                if _count == 1 then
                    unindent_this_line();
                endif;
                _indent fi_- vedindentstep -> _indent
            elseif item == "%" then
/*
                if _count == 1 then
                    unindent_this_line();
                endif;
                */
                [^ _indent ^ listdepth ^^ pcentstack] -> pcentstack;
                0 -> listdepth
            endif
        elseif invars then
            if item == ";" then
                false -> invars
            endif
        elseif indefine then
            if item == ";" then
                false -> indefine;
            endif;
        elseif (fast_lmember(item, ved_tidy_skip_some) ->> skipend) then
            skipend.back.front.front -> skipend;    ;;; an integer
        elseif (fast_lmember(item, ved_tidy_skip_pairs) ->> skipend) then
            skipend.back.front -> skipend;      ;;; a list of closers
        elseif item == "%" then
            if lastitem == "(" then
                ;;; partial application
                _partapply + 1 -> _partapply;
            elseif _partapply /== 0 then
                _partapply -1 -> _partapply;
                if _count == 1 then
                    unindent_this_line();
                endif
            elseif fast_front(pcentstack) == _indent then
                fast_destpair(fast_back(pcentstack))
                    -> pcentstack -> listdepth;

                nextchar(items) ->> _char -> nextchar(items);
                if _count == 1 and _char == `]` then
                    unindent_this_line();
                endif
            endif;
        elseif fast_lmember(item, vedvarskeywords) then
            true -> invars
        elseif fast_lmember(item, vedopeners) then
            vedindentstep fi_+ _indent -> _indent;
            if item == "define" or item == "procedure" then
                true -> indefine;
                conspair(_firstindent, labelstack) -> labelstack;
                _indent fi_- vedindentstep -> _firstindent;
            endif;
        elseif fast_lmember(item, vedclosers) then
            _indent fi_- vedindentstep -> _indent;
            if item == "enddefine" or item == "endprocedure"
            or item == "end"
            then
                destpair(labelstack) -> labelstack -> _firstindent
            endif;
        endif;
        if _indent fi_< 0 then 0 -> _indent endif;
    endrepeat;

    vedpositionpop();
enddefine;


define vars ved_jcp();
    ;;; justify current procedure
    vedmarkpush();
    false -> vvedmarkprops;
    ved_mcp();
    ved_tidy();
    vedmarkpop();
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1992
        Sectionised.
--- John Gibson, Jan  3 1992
        Changes to cope with dstrings
--- Aaron Sloman, Oct 27 1990
        Put "#|" into vedopeners, "|#" into vedclosers
--- Aaron Sloman, Jan 19 1990
        Handling of "%" in lists and partial application sorted out.
--- Aaron Sloman, Nov 12 1989
        Added "==>" and "=>" to vedforwarders
        Changed to use fast_lmember, with a test to ensure lists are provided
        Re-organised ved_tidy completely
        Removed "return" from vedbackers
        Fixed bug caused by localizing vedline,and vvedlinesize
        Stopped ved_tidy indenting last line of long strings. Required
        provision of special repeater.
--- Jason Handby, Jul 16 1989
        Added the list "ved_tidy_skip_some" for things like "uses"
        quotes that require a number of items to be skipped over. Put -nonop-
        -nonmac- and -nonsyntax- into -ved_tidy_skip_some- (previously buried in
        the code).
--- Jason Handby, Jul 13 1989
        Added a new category, "ved_tidy_skip_pairs", to enable chunks of stuff
        to be ignored (eg. between "vedset" and "endvedset", "uses" and newline),
        Fixed indentation after vedvarskeywords to be -vedindentstep- instead
        of 5
        Changed so that close list/vector brackets line up with open
        list/vector brackets
--- Aaron Sloman, Apr 15 1989
        Added "fast_repeat" and "endfast_repeat" to -vedopeners- and
        -vedclosers-
--- John Williams, Mar 31 1989
        Removed "lambda" and "function" from -vedopeners-
        Removed "enddo" and "exit" from -vedclosers-
--- John Gibson, Nov 24 1988
        Added "lblock" to -vedopeners-, "endlblock" to -vedclosers-.
--- Aaron Sloman, Jul  6 1988
        Added "section" to -vedvarskeywords-
        Replaced -vedfspecial- with -define_headers-
        Made -ved_tidy- treat procedure headers like define headers
        Made -indefine- remain true till semicolon reached.
        Additional minor fixes, e.g. dealing with "nonsyntax"
*/
