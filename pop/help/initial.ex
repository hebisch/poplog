HELP INITIAL.EX                                         Aaron Sloman July 1983

What follows is a collection of examples of things POPLOG users do to tailor
the system to suit themselves. I am grateful to colleagues and students for
permission to use their INIT.P and VEDINIT.P files, etc.


-- UNIX COMMAND FILES --------------------------------------------------------

An example UNIX command file to create a new saved image containing your own
goodies:

Creates a saved image called MYPOP.PSV containing all the stuff if the file
MYPOP.P. The image can be run with the UNIX command:

    pop11 -mypop

for which you can define a suitable abbreviation in your .login file:

    alias pop pop11 -mypop

Instead of MYPOP, use whatever you like. The source file and saved image do
not need to use the same name.

It is possible to save the image in such a way that it looks to see
whether it has been invoked with a file name, e.g.

    pop foo

in which case it will start up by running the editor and reading in the file
FOO.

If the POP system is changed (or re-linked) you'll have to build a new
saved image.

Here's a possible command file:

#
rm mypop.psv
pop11 << \\\\
load mypop.p        ;;; load your source files
if syssave("mypop") then    ;;; syssave produces TRUE  when image is restored
    ;;; so this branch runs when image is restored
    pr(popversion);         ;;; Prints out header, with version of POP
    popready -> interrupt;  ;;; optional, see help POPREADY
    true -> pop_first_setpop;   ;;; pretend no setpop yet

    /*
    The next instruction allows you to invoke this image by typing
            pop11 -mympop foo
    in which case POP will go straight into the editor with the file FOO
    */
    unless poparglist == [] then
        pr(newline);
        popval([ved ^^poparglist ;])
    endunless;
    setpop()
else
    sysexit()       ;;; this branch runs when image has been created
endif;
\\\\



============================================================================

/*----------INIT.P and VEDINIT.P combined - by Chris Slymon-----------*/

true -> popsyscall;     ;;; fuller error messages

define macro newp;
    ;;; create a new saved image, using current state, and delete old one.
    sysobey('delete poplib:initdone.psv.*');
    syssave('poplib:initdone.psv')=>
enddefine;

/* Add my poplib: to list of directories searched */
[% 'poplib:' %] <> popuseslist -> popuseslist;  /* for explicit lib commands */
[% 'poplib:' %] <> popautolist -> popautolist;    /* for autoloading */

800 -> vedautowrite;
false -> vedterminalselect;         ;;; use V200 suppress enquiry about terminals
false -> vedscrollscreen;           ;;; refresh, don't scroll, for screen down

/* invoke various local library extensions to ved */
uses autoindent;
uses vedkey5;
uses ved_ff;
uses ved_incsearch;

/* define facilities for abbreviation */

section $-library => vedsetmacro ved_killmac ved_lmac
                     ved_shmac ved_domac ved_nomac ved_setmac
                     ved_macro_expanders;

vars vedmacprops;

define vedlastword() -> vedtemp;
    vars vedline vedcolumn _string;
    vedsetlinesize();
    if vedcolumn == 1 or vedcolumn > vvedlinesize + 1
            or (vedthisline() -> _string; _string(vedcolumn - 1) = ` `) then
        false
    else
        vedcolumn -> vedtemp;
        vedcharleft();
        until vedatitemstart(vedcolumn,_string,vvedlinesize + 1) do
            vedcolumn - 1 -> vedcolumn;
        enduntil;
        consword(substring(vedcolumn,vedtemp-vedcolumn,_string))
    endif; -> vedtemp;
enddefine;

define vedwordbreak();
    vars vedtemp last_word;
    if vedonstatus then return endif;
    if (vedmacprops(vedlastword() ->> last_word) ->> vedtemp) then
        if vedtemp.isprocedure then
            vedtemp()
        else
            repeat length(last_word) times
                vedchardelete();
            endrepeat;
            vedinsertstring(vedtemp)
        endif
    endif;
enddefine;

define global vedsetmacro( word);
    if isproperty( word) then
        word -> vedmacprops;
    else
        -> vedmacprops( word);
    endif;
enddefine;

vars ved_pop_macros;

newproperty([
    [d 'define']
    [dg 'define global']
    [dm 'define macro']
    [c 'constant']
    [rt 'return']
    [t 'then']
    [v 'vars']
    [sc 'section']
    [sw 'switchon']
    [fc 'foreach']
    [fv 'forevery']
    [ul 'unless']
    [ut 'until']
    [w 'while']
    [p 'procedure']
    [l 'else']
    [li 'elseif']
    [lu 'elseunless']
    [qi 'quitif']
    [ql 'quitloop']
    [qu 'quitunless']
    [rp 'repeat']
    [ei 'endif']
    [ed 'enddefine']
    [er 'endrepeat']
    [esc 'endsection']
    [esw 'endswitchon']
    [ef 'endfor']
    [efc 'endforeach']
    [efv 'endforevery']
    [eul 'endunless']
    [eut 'enduntil']
    [ew 'endwhile']
    [ep 'endprocedure']
],
        30,false,true) -> ved_pop_macros;

vedsetmacro( ved_pop_macros);

define vedmatchendstartingat( column) -> _item;
    vars vedline vedcolumn vvedlinesize;
    until vedline = 1 do
        vedcharup();
        unless vvedlinesize = 0 then
            vedtextleft();
            if vedcolumn = column
                    and member(vednextitem() ->> _item,vedopeners) then
                return
            endif;
        endunless;
    enduntil;
    false  -> _item;
enddefine;

define vedemacro();
    vars ending;
    vedmatchendstartingat(vedcolumn - 1) -> ending;
    if ending then
        vedinsertstring('nd' >< ending);
    endif;
enddefine;

vedsetmacro(vedemacro,"e");

define vedebmacro();
    vars ending;
    vedmatchendstartingat(max(vedcolumn - 2 - vedindentstep, 1)) -> ending;
    if ending then
        vedchardelete();
        vedinsertstring('nd' >< ending >< ';');
    endif;
enddefine;

vedsetmacro(vedebmacro,"eb");

define global veddowordbreak( proc);
    vedwordbreak();
    proc();
enddefine;

global vars ved_macro_expanders;
[% ' ', ',', ';', '\n', '\r', '\t' %] -> ved_macro_expanders;

define vedmackey( string);
    vars temp;
    unless pdpart( vedgetproctable(string) ->> temp) == veddowordbreak then
        vedsetkey(string ,veddowordbreak(% temp %));
    endunless;
enddefine;

define global ved_domac();
    applist( ved_macro_expanders, vedmackey);
enddefine;

define vedresetmackey( string);
    vars temp;
    if isclosure( vedgetproctable(string) ->> temp) then
        vedsetkey(string , frozval( 1, temp));
    endif;
enddefine;

define global ved_nomac();
    applist( ved_macro_expanders, vedresetmackey);
enddefine;

define global ved_setmac();
    vars _space;
    if vedargument = '' then
        vederror('Usage: setmac <string> <substitution string>');
    elseif locchar( ` `, 1, vedargument) ->> _space then
        substring(_space + 1, length( vedargument) - _space, vedargument)
            -> vedmacprops(consword(substring(1,_space - 1,vedargument)));
    elseif vedmacprops( consword( vedargument)) then
        vedputmessage( vedargument >< ' cleared');
        false -> vedmacprops(consword(vedargument));
    else
        vederror('No macro defined for ' >< vedargument);
    endif;
enddefine;

define global ved_killmac();
    vedputmessage('Clearing macros');
    appproperty( vedmacprops,
        procedure item value;
            false -> vedmacprops( item);
        endprocedure);
enddefine;

define global ved_shmac();
    vars temp;
    if vedargument = '' then
        appproperty( vedmacprops,
            procedure item value;
                pr( newline);
                pr( item); pr(tab); pr(value);
            endprocedure);
    elseif vedmacprops(consword(vedargument)) ->> temp then
        vedputmessage( '' >< temp); /* could be a procedure */
    else
        vedputmessage('No macro defined for ' >< vedargument);
    endif;
enddefine;

define global ved_lmac();
    if vedargument = '' then
        vederror('Argument needed');
    endif;
    vedargument >< ' ' >< vedlastword() -> vedargument;
    ved_setmac();
enddefine;

endsection;

vedsetmacro( 'define global ved', "dgv");
vedsetmacro( '->', "--");
vedsetmacro( '->>', "-^");

vars ved_sfv;
ved_sourcefile -> ved_sfv;

define ved_vved();
    'ved_' >< vedargument >< '.p' -> vedargument;
    ved_ved();
enddefine;

;;; displays the current time

define ved_date;
    vedputmessage(sysdaytime())
enddefine;

define ved_mine();
    vedtopfile();
    vedlineabove();
    vedinsertstring('Chris Slymon, June 1983\n');
enddefine;

define ved_wswap();
    if vedstartwindow < 24 then '24' else '12' endif; -> vedargument;
    ved_ws();
enddefine;

define vedinitfile;
    unless issubstring(".",1,vedvedname) then
        70
    else
        78
    endunless; -> vedlinemax;

    if vedcurrent = 'output' then
        false -> vedautowrite;
    endif;
    if sysisprogfile(vedcurrent) then
        vedpopnewlines();
        ved_domac();
    else
        vedvednewlines();
        ved_nomac();
    endif;
enddefine;

define ved_cr();
    vars vedargument;
    '/\^M//' -> vedargument;
    ved_gs();
    '/\^L//' -> vedargument;
    ved_gs();
enddefine;

define ved_rqved();
    false -> vedchanged;
    ved_qved();
enddefine;

define ved_rwqved();
    vars vedwriteallfiles;
    true  -> vedwriteallfiles;
    false -> vedchanged;
    ved_wqved();
enddefine;

define vedfindnextword;
    '"' >< vednextitem() -> vedargument;
    ved_search();
enddefine;

define vedfindlastword;
    '"' >< vednextitem() -> vedargument;
    ved_backsearch();
enddefine;

define ved_nextword;
    vednextitem() -> vvedworddump;
enddefine;

vars locate_char_time; 100 -> locate_char_time;

define vedlocatechar();
    vars char;
    if charin_timeout( locate_char_time) ->> char then
        vedlocate(consstring(char,1))
    else
        vederror('too slow');
    endif;
enddefine;

define vedbacklocatechar();
    vars char;
    if charin_timeout( locate_char_time) ->> char then
        vedbacklocate(consstring(char,1))
    else
        vederror('too slow');
    endif;
enddefine;

define ved_ccl;
    vedscreenleft();
    until vedcolumn > vvedlinesize do
        vedchangecase();
    enduntil;
enddefine;

define ved_ao;
    false -> vedautowrite;
enddefine;

define vedlmax( num);
    num -> vedargument;
    ved_linemax();
enddefine;

define ved_section();
    vedlinebelow();
    vedinsertstring('section ' >< vedargument >< ' => ');
    vedpositionpush();
    vedinsertstring(';\n');
    vedendfile();
    vedinsertstring('\nendsection;');
    vedpositionpop();
enddefine;

;;; KEYS - <ESC> + function key on v200

[%
     `P`, ved_y,
     `Q`, ved_yankw,
     `R`, ved_yankl,
     ` `, ved_yankw,
     `!`, ved_swl,
     `"`, ved_swr,
     `#`, nonmac dcl,
     `$`, vedmarkfind,
     `%`, ved_jp,
     `&`, ved_t,
     ```, vedexchangeposition,
     `(`, ved_cps,
     `)`, undef,
     `*`, undef,
     `D`, ved_margin,
     `C`, ved_break,
     `A`, ved_wswap,
     `H`, ved_mef,
     `t`, ved_mbf

     %] -> vedescapetable( `\^[`);

vedsetkey('\^A', ved_xup);
vedsetkey('\^F', ved_sw);
vedsetkey('\^T', ved_tr);
vedsetkey('\^Z', ved_xdn);
vedsetkey('\^[a', ved_sla);
vedsetkey('\^[b', ved_slb);
vedsetkey('\^[d', ved_d);
vedsetkey('\^[g', ved_nextword);
vedsetkey('\^[s', ved_incsearch);
vedsetkey('\^[C', vedendline);              ;;; ->
vedsetkey('\^[0', vedlmax(% '70'%));
vedsetkey('\^[8', vedlmax(% '78'%));
vedsetkey('\^[]', ved_ucw);                 ;;; CONVERT keypad .
vedsetkey('\^[^', ved_lcw);                 ;;; CONVERT keypad ,
vedsetkey('\^[\^[b', vedbacklocatechar);
vedsetkey('\^[\^[e', ved_rrq);
vedsetkey('\^[\^[f', vedlocatechar);
vedsetkey('\^[\^[]', vedfindnextword);      ;;; <ESC> CONVERT keypad .
vedsetkey('\^[\^[^', vedfindlastword);      ;;; <ESC> CONVERT keypad ,

vedsetkey('\^[.',
    procedure;
        vars vedstatic;
        true -> vedstatic;
        vedcharinsert(`.`);
        vedcharright();
        vedchangecase();
    endprocedure);

vedsetkey('\^[,',
    procedure;
        vars vedstatic;
        true -> vedstatic;
        vedcharinsert(`,`);
        vedcharright();
        vedchangecase();
    endprocedure);


===========================================================================


;;; Jonathan Laventhol.  most of this really should be a vedinit.p

1 -> popsyscall;
1 -> popgctrace;

;;; compile init.p in current directory, unless that's this file
;;;
unless sysdirectory() = '[JCL]' then trycompile('init.p') endunless;

;;; add library to front of directory lists
;;;
'disk$2c:[jcl.lib]' :: delete('disk$2c:[jcl.lib]', popautolist) -> popautolist;
'disk$2c:[jcl.lib]' :: delete('disk$2c:[jcl.lib]', popuseslist) -> popuseslist;

uses newpr;
newpr(isproperty, procedure(p);
                    '<property>\n'.pr;
                    appproperty(p, printf(% '%p\t%p\n' %))
                  endprocedure);

;;; fix up ved.  swap screenright and textright keys to be like ded
;;;
false -> vedautowrite;          ;;; no autowrite
false -> vedversions;           ;;; no autopurging
false -> vedscrollscreen;       ;;; refresh, not scroll
4 -> vedindentstep;             ;;; small tabs
true ->> vedlmr_print_in_file   ;;; doit in file
-> vedlmr_errs_in_file;         ;;; ditto for errors
vedsetkey('\^[C', vedscreenright);      ;;; like in the ...
vedsetkey('\^[]', vedtextright);        ;;;     ... old country

vedsetkey('\^[b', procedure;
                   vedputmessage(if vedchanged then vedchanged else 'no' endif
                                    >< ' changes')
                  endprocedure);

uses vednkother;

========================================================================



12 -> item_chartype(`\\`);      /* make backslash an alphabeticiser */


true -> vedmidscreen;
1500 -> vedautowrite;

false -> vednotabs;

define ved_-();
    ;;; ENTER -
    ;;; ads two hyphens before, and a whole row of them after, stuff on line
    1 -> vedcolumn;
    vedinsertstring('-- ');
    vedtextright();
    vedcharinsert(` `);
    until vedcolumn == vedlinemax do vedcharinsert(`-`) enduntil;
enddefine;

define ved_^();
    ;;; ENTER ^
    ;;; search for another occurrence of item to right of cursor
    vedlocate(vednextitem())
enddefine;

;;; make <ESC> CTRL-N invoke it
vedsetkey('\^[\^N', ved_^);

;;; define ENTER ; as meaning put comment at beginning of line
;;; cannot define a procedure with name "ved_;" - so create name using
;;; consword, declare it, and assign suitable procedure to it.

popval([ vars ^(consword('ved_' >< ";")) ;]);

procedure;
    vedpositionpush();
    1 -> vedcolumn; vedinsertstring(';;;');
    vedpositionpop();
endprocedure -> valof(consword('ved_' >< ";"));

uses vedkey5;

uses ved_go;

define ved_delafter;
    ;;; ENTER DELAFTER <string>
    ;;; find every occurrence of string and delete to end of line
    vars vedautowrite;
    false -> vedautowrite;
    repeat vedlocate(vedargument); vedcleartail() endrepeat
enddefine;

define ved_tlo();
    ;;; transcribe current line out to other file
    vedmarkhi(); vedmarklo(); ved_to();
enddefine;

define ved_tli();
    ;;; transcribe line from other file to this one
    vedswapfiles();
    ved_tlo();
    vedswapfiles();
enddefine;


define ved_pg;
    ;;; put in '.pg' on line above
    vedlineabove(); 1 -> vedcolumn; vedinsertstring('.tp 5')
enddefine;


define vedinitfile;
    ;;; run each time file put on screen. Some of this should go in next proc
    vars _x;
    if sysisprogfile(vedcurrent) then
        false -> vednotabs
    elseif issubstring('.ref',1,vedcurrent)
    or issubstring('.help',1,vedcurrent)
    or issubstring('.teach',1,vedcurrent)
    or (sysdirectory() -> _x; issubstring('.REF',1,_x))
    or issubstring('.HELP',1,_x)
    or issubstring('.TEACH',1,_x)
    or issubstring('primer',1,_x)
    then true -> vednotabs
    endif;
    if (issubstring('news',1,vedcurrent) ->> _x)
        and _x + 3 == datalength( vedcurrent)
    then
        4 -> vedleftmargin
    endif;
    if vedcurrent = 'output' then false -> vedwriteable endif;
    if issubstring('primer',1,vedcurrent) then 72 -> vedlinemax endif;
enddefine;

define vedveddefaults();
    ;;; run once when file is read in, or new one started,
    ;;; before it's put on screen
    vars p;
    if locchar(`]`,1,vedcurrent) ->> p then p+1 else 1 endif -> p;
    if issubstring('.pma',p,vedcurrent)
    or issubstring('.mar',p,vedcurrent)
    or issubstring('.ps',p,vedcurrent)
    or issubstring('.s',p,vedcurrent)
    then 8
    else 4
    endif -> vedindentstep;
    vedinitfile();
enddefine;

;;; tell ved not to do auto line break for certain files
[ '.mar' '.pma' '.s' '.ps'] <> vednonbreakfiles -> vednonbreakfiles;


define ved_-();
    ;;; used to insert '-- ' before and lots of '-------' after text
    ;;; on current line, to make headings
    1 -> vedcolumn;
    vedinsertstring('-- ');
    vedtextright();
    vedcharinsert(` `);
    until vedcolumn == vedlinemax do vedcharinsert(`-`) enduntil;
enddefine;

======================================================================
Jon Cunningham

The following is taken from a file used by Jon Cunningham to build a saved
image which he uses for most of his work. It includes a lot of abbreviations
to save typing. Also a lot of alterations to the editor.

;;; define a collection of abbreviations
vars macro def; "define" -> nonmac def;
vars macro fed; "enddefine" -> nonmac fed;
vars macro --; "->" -> nonmac --;
vars macro ^; "=>" -> nonmac ^;
vars macro ^^; "==>" -> nonmac ^^;
vars macro ef; "elseif" -> nonmac ef;
vars macro fi; "endif" -> nonmac fi;
vars macro el; "else" -> nonmac el;
vars macro rep; "repeat" -> nonmac rep;
vars macro per; "endrepeat" -> nonmac per;
vars macro rof; "endfor" -> nonmac rof;
vars macro les; "unless" -> nonmac les;
vars macro sel; "endunless" -> nonmac sel;
vars macro til; "until" -> nonmac til;
vars macro lit; "enduntil" -> nonmac lit;

uses bye;       ;;; Make sure LIB BYE is loaded.

['[]' '[jonc.lib]' '[jonc.auto]'] ncjoin popuseslist -> popuseslist;
delete('USE$POP:[pop.lib.turtle]','[jonc.auto]' :: popautolist) -> popautolist;

false -> popwarnings;
false -> popmishaps;

vedsetup();
lib vedkey5;

define macro ;         ;;; CTRL V to invoke VED
    vedscreenraw();
    if vedvedname = vednullstring then 'temp.p' -> vedvedname endif;
    chain(vedveddefaults, vedvedname,vededitor);
enddefine;

vars oldvederror;
vederror -> oldvederror;

define vederror(string);
    ;;; Change VED's error handling so that if it doesn't recognise
    ;;; a command it tries to interpret it as a POP command
    if string = 'UNKNOWN COMMAND' then
        ':'><vedcommand -> vedargument;
        ved_pop11()
    else
        oldvederror(string)
    endif
enddefine;

define vedlastword() -> vedtemp;
vars vedpositionstack;
    if vedcolumn == 1 then false -> vedtemp; return endif;
    vedpositionpush();
    vedsetlinesize();
    if vedcolumn > vvedlinesize then
        vedtextright()
    elseif vedcolumn-1 > vedleftmargin and vedthisline()(vedcolumn-1) == ` ` then
        vedwordleft()
    endif;
    vedcolumn -> vedtemp;
    vedwordleft();
    if vedcurrentchar() == ` ` then vedwordright() endif;
    if vedtemp > vedcolumn then
        consword(substring(vedcolumn,vedtemp-vedcolumn,vedthisline()))
            -> vedtemp
    else
        false -> vedtemp
    endif;
    vedpositionpop()
enddefine;

define vednextword();
    vars vedpositionstack;
    vedpositionpush();
    vedwordright();
    vedlastword();
    vedpositionpop()
enddefine;

define vedfirstwordisopener();
    vars vedtemp;
    vedtextleft();
    vednextword() -> vedtemp;
    member(vedtemp,vedopeners) or member(vedtemp,vedbackers)
enddefine;

define veddeletetab();
;;;
repeat vedindentstep times
        vedchardelete()
    endrepeat
enddefine;

define myreturn();
;;; This procedure is assigned to the return key, to do indentation
vars dtab vedpositionstack;
    vedpositionpush();
    if sysfiletype(vedcurrent) = 'p' then
        if (vedcolumn /== 1)
                and (vedcharleft(); vedcurrentchar() /== `;`)
                and vedcurrentchar() /== `>`
        then
            if vedfirstwordisopener() then
                1
            else
                true
            endif
        else
            false
        endif -> dtab;
        vedtextleft();
        vedexchangeposition();
        vedcharinsert(`\r`);
        front(vedpositionstack)(2) -> vedpositionstack;
        repeat vedpositionstack - 1 times
            vedcharinsert(32)
        endrepeat;
        if dtab then
            if dtab == 1 then
                vedcharinsert(9)
            elseif vedcolumn >= vedindentstep-1 then
                veddeletetab()
            endif
        endif
    else
        vedtextleft();
        unless vvedlinesize == 0 then
            vedcolumn - 1 -> vedleftmargin
        endunless;
        vedpositionpop();
        vedcharinsert(`\r`)
    endif;
    repeat
        vedsetcursor();
    quitunless(dup(rawcharin()) == 127);
        veddeletetab(erase())
    endrepeat;
    vedinput()
enddefine;

define vedreturn();
    if vedonstatus then
        veddocr()
    else
        myreturn()
    endif
enddefine;

define vedoldabove();
vars vedtemp;
    vedtextleft();
    vedcolumn -> vedtemp;
    vedlineabove();
    vedtemp -> vedcolumn
enddefine;

define vedbelow();
    vedtextright();
    vedreturn();
enddefine;

define vedabove();
    if vedline == 1 then vedoldabove()
    else vedcharup();
        vedbelow()
    endif
enddefine;

false -> vedscrollscreen;

define autovars();
;;; for inserting variable declarations in current file
;;; insert declaration for last word typed in
vars vedpositionstack word;
    vedpositionpush();
    vedlastword() -> word;
    repeat
        vedbacklocate('define');
        quitunless(issubstring('enddefine',1,vedthisline()))
    endrepeat;
    repeat
        vednextline();
        quitunless(length(vedthisline()) > 3 and substring(1,4,vedthisline()) = ';;; ')
    endrepeat;
    if vednextword() == "vars" then
        vedlocate(';');
        vedinsertstring(' '><word)
    else
        vedlineabove();
        vedinsertstring('vars '><word><';')
    endif;
    vedpositionpop()
enddefine;

;;; Now a package to enable VED automatically to expand abbreviations

;;; set up table of abbreviations used by next procedure
vars vedmacprops;
newproperty([   [def 'define']
    [fed 'enddefine']
    [-- '->']
    [%"^"% '=>']
    [%"^^"% '==>']
    [ef 'elseif']
    [fi 'endif']
    [el 'else']
    [rep 'repeat']
    [per 'endrepeat']
    [rof 'endfor']
    [les 'unless']
    [sel 'endunless']
    [til 'until']
    [lit 'enduntil']
    [whi 'while']
    [ihw 'endwhile']
    [pro 'procedure']
    [orp 'endprocedure']
],
        30,false,true) -> vedmacprops;

define vedwordbreak(vedaction);
;;; expands abbreviations whenever a space is typed
vars vedtemp;
    if (vedmacprops(vedlastword()) ->> vedtemp) then
        if vedtemp.isprocedure then
            vedtemp()
        else
            vedcharinsert(` `);
            vedwordleft();
            vedwordleft();
            if vedcurrentchar() == ` ` then
                vedwordright()
            endif;
            vedwordrightdelete();
            vedinsertstring(vedtemp)
        endif
    endif;
    vedaction()
enddefine;

define vedendmac();
;;; expands end into appropriate thing - endif, enduntil, enddefine, etc.
;;; does this by searching up for matching opener. Assume indentation is OK
vars vedpositionstack vedtemp;
    vedcharinsert(` `);
    vedwordleft();
    vedwordleft();
    if vedcurrentchar() == ` ` then
        vedwordright()
    endif;
    vedcolumn -> vedtemp;
    vedpositionpush();
    repeat forever
        if vedline == 1 then
            '' -> vedtemp;
            quitloop
        endif;
        vedcharup();
        vedtextleft();
        if vedcolumn == vedtemp then
            vedwordright();
            if member(.vedlastword.dup,vedopeners) then
                -> vedtemp;
                quitloop
            else
                .erase
            endif
        endif
    endrepeat;
    vedpositionpop();
    vedwordrightdelete();
    vedinsertstring('end'><vedtemp)
enddefine;

vedendmac -> vedmacprops("end");

define vedbigswap();
;;; swaps files, but goes to larger window
    vedswapfiles();
    vedsetwindow()        
enddefine;

define ved_auto();
    vedsetkey(' ',vedwordbreak(% vedcharinsert(%` `%)%));
    vedsetkey(';',vedwordbreak(% vedcharinsert(%`;`%)%));
    vedsetkey(',',vedwordbreak(% vedcharinsert(%`,`%)%));
    vedsetkey('\r',vedwordbreak(% vedreturn %));
    vedsetkey('\n',vedwordbreak(% vedchardown pdcomp vedtextleft %))
enddefine;

define ved_unauto();
;;; resets auto macro stuff
    vedsetkey(' ',vedinsertvedchar);
    vedsetkey(';',vedinsertvedchar);
    vedsetkey('\r',vedreturn);
    vedsetkey('\n',vedchardown pdcomp vedtextleft);
    vedsetkey(',',vedinsertvedchar)
enddefine;

define vedinitfile();
    ;;; run whenever a file is set (or reset) on screen
    if sysfiletype(vedcurrent) = 'p' then
        ved_auto()
    else
        ved_unauto()
    endif;
enddefine;

ved_auto();

uses ved_index;

define vedveddefaults();
vars type;
    if vedcurrent = 'output'
        or (sysfiletype(vedcurrent) ->> type) = 'lis'
        or type = 'log'
    then
        false -> vedwriteable
    endif;
    if vedwriteable then
        vedinput('\^[?Mensureindex\^[?p')
    endif
enddefine;

erase(ved_h);
'temp.p' -> vedvedname;

;;; Tell VED abount files which should not have automatic line break
'.lis' :: ('.t' :: vednonbreakfiles) -> vednonbreakfiles;

false -> vedterminalselect;

uses ved_go;
60 -> ved_go_delay;
800 -> vedautowrite;

define vedendline();
    if vedcolumn = vvedlinesize + 1  then
        vedchardown();
    endif;
    vvedlinesize + 1 -> vedcolumn;
enddefine;

define vedbeginline();
vars vedtemp;
    vedcolumn -> vedtemp;
    vedtextleft();
    if vedcolumn >= vedtemp then
        if vedtemp > vedleftmargin + 1 then
            vedleftmargin + 1 -> vedcolumn
        elseif vedtemp > 1 then
            1 -> vedcolumn
        else
            vedcharup();
            vedtextleft()
        endif
    endif
enddefine;


;;; Map appropriate procedures onto key sequences.
;;; See HELP * ASCII and * STRINGS

vedsetkey('\^[)', vedabove);
vedsetkey('\^[*', vedbelow);
vedsetkey('\^[\^I', veddeletetab);
vedsetkey('\^[#',vedswapfiles);
vedsetkey('\^[x',vedbigswap);
vedsetkey('\^[s',vedsetstatic);
vedsetkey('\r', vedreturn);

vedsetkey('\^A',vedbeginline);
vedsetkey('\^Z',vedendline);
vedsetkey('\^P',ved_pop);
vedsetkey('\^V','\^[?Mved ');
vedsetkey('\^[u',autovars);
============================================================================
PLOGINIT.P Prolog initialisation files

/* ROGER EVANS : initialisation for Prolog - PLOGINIT.P
    compiled as POP-11 when Prolog starts up
*/


'[rogere.prolib]' :: popautolist -> popautolist;
'[rogere.prolib]' :: popuseslist -> popuseslist;

/* Set funtion keys to switch languages:
   assume they transmit ESC then a character.
   So define ESC as a macro which reads next character then
   switches subsystem.
*/
section $-escape(switch_subsystem_to,prolog_macro);

vars POP11code,TOPcode,PROLOGcode;

80 -> POP11code;  /* F0 */
81 -> TOPcode;    /* F1 */
82 -> PROLOGcode; /* F2 */

define vars macro   ;  ;;; ESC - doesn't print
    vars c;
    rawcharin() -> c;
    pr('\n\^G');
    if c == POP11code then
        switch_subsystem_to("pop11");
    elseif c == TOPcode then
        switch_subsystem_to("top");
    elseif c == PROLOGcode then
        switch_subsystem_to("prolog");
    else
        pr('Edit keys don\'t work outside VED\n');
        pr('(at least, most of them don\'t)\n');
        interrupt();
    endif;
enddefine;

nonmac  -> prolog_macro("");      ;;; make it work in Prolog too

section_export("");

section_cancel(current_section);

endsection;


================================================================
;;; PLOGINIT.P --- file compiled as POP11 on Prolog startup
;;;
;;; Jonathan laventhol, 15 July 1983.
;;; ------------------------------------------------------------------------

;;; if there is a PLOGINIT.PL in the login directory then reconsult it
;;;
if   readable('poplib:ploginit.pl')
then prolog_compile('poplib:ploginit.pl')
endif;

;;; how to make a private library accessable via library/1 and showlib
;;;
'disk$2c:[jcl.prolog]' :: prologliblist -> prologliblist;

/* ---- stuff in prolog: invoke prolog compiler for rest of file -----*/

:- prolog_language(prolog).

/* this could be in PLOGINIT.PL */

/* no silly messages */
    :- prolog_gctrace(on).                  /* "off" is the default */

/* member, append, and friends */
    :- library(useful).

/* autoload-or-fail action on unknown predicate */
    :- library(unknown), unknown(_, autoload).      /* "fail" is default" */

/* simple interface to pop11 */
    :- library(simplepop).

/* string printing stuff */
    :- library(strings).

/* make strings print as characters -- uses library(string) stuff */
    portray(S) :-
        string(S), !, writes(S).
===========================================================================
;;; LIB AUTOV55                                   Tom Khabaza Oct 1984
/*
 *  AUTOV55.P - detect v55-ness in an otherwise v200-like terminal
 *  should be loaded in vedinit.p if a v55 might be used.
 *  Uses answerback to detect V55 and loads LIB V55 if necessar.
 *
 *  adapted from Aaron Sloman's vedinit.p by Tom Khabaza, 12th October 1984
 *
 *  Note that this uses the v55 "answerback" facility - this means
 *  that it will not work on a new v55 until the answerback has been
 *  set up right - to do this (permanently) to a new terminal, use the
 *  following key sequence:
 *      <set up> <shift A> / <ESC> [ 5 5 V / <shift S>
 *  this will leave you in normal (non-setup) mode, and has changed
 *  the NON-VOLATILE memory to know the new answerback sequence.
 */

;;; For a more complex case, where saved images are to be invoked, see
;;; next three files, below

define vedvt52select();
    ;;; Run if standard VT52 answer has been received by VED
    ;;; test for Visual 55. Assumes usual VT52 terminal ID.
    ;;; also assumes V55 'answer back' has been set to '<ESC>[55V'
    ;;; (Press SET UP. Press SHIFT-A. type '/<ESC>[55V/')
    vars vvedscreensendidseq vedterminalselect;
    if vedterminalname == "v55" then return() endif;    ;;; LIB V55 loaded
    procedure(); rawcharout(`\^E`) endprocedure -> vvedscreensendidseq;
    [['[55V' %loadlib(%"v55"%)%] [^false ^identfn]] -> vedterminalselect;
    vedtermsetup();
enddefine;

===========================================================================
#MKMYPOP                                               A. Sloman Feb 1985
#A UNIX 4.2 CSHELL command file to create a 'layered' saved image with VED
#utilities compiled in. The image is called 'mypop.psv'

# I sometimes use a Visual 55, and have a saved image built on top of
# 'mypop.psv' with lib v55 included. It is built by the file MKV55, below.

#The second image is called 'v55.psv' and contains lib V55 to run
#a Visual 55 VDU, in place of the default V200 or VT100
#
#N.B. The same general technique works in VMS with DCL command files
#
rm mypop.psv        #delete old version
pop11 << \\\\       #compile to next occurrence of "\\\\"
compile('~/vedutil.p');
vars vedimage;
true -> vedimage;
1 -> popsyscall;
if sys_lock_system('~/mypop.psv',true,'MYPOP')  then
    printf(pop_system_version,'%p\n');          ;;; print date, version, etc
    true -> pop_first_setpop; setpop -> interrupt;
    if poparglist == [] then
    else
        popval([ved ^^poparglist])              ;;; go straight into ved
    endif;
else sysexit()
endif;
\\\\

echo "mypop ready"

===========================================================================
#MKV55                                                 A. Sloman Feb 1985
#A Unix CSHELL file which can be run after the previous one to build a
#second saved image.
rm ~aarons/v55.psv

#run pop-11 with previous saved image (see MKMYPOP above), then
#build new one on top of it.
pop11 -~/mypop.psv  << \\\\

vedsetup();     ;;; runs vedinit.p

lib v55;        ;;; load stuff for Visual 55 VDU.

if sys_lock_system('~aarons/v55.psv',true,'V55' ) then

    printf(pop_system_version,'%p\n');
    true -> pop_first_setpop; setpop -> interrupt;
    if poparglist == [] then
    else
        popval([ved ^^poparglist])
    endif;
else sysexit()
endif;
\\\\

echo "v55 ready"
===========================================================================
;;; VEDINIT.P                                        A. Sloman - Feb 1985

;;; This file is used in conjunction with the two saved images mypop.psv
;;; and v55.psv created by the CSHELL command files above.

;;; Set VEDFILETYPES and VEDNONBREAKFILES, which control values of
;;; VED variables when a new file is created or read in.

[
    ;;; first the default case
    ['' {vedindentstep 4} {vednotabs true} {vedbreak true}
        {popcompiler compile}]
;;; ['.p' {popcompiler compile}]    ;;; redundant-see default above
    ['.lsp' {popcompiler lisp_compile}]
    ['.pl' {popcompiler prolog_compile}]
    [['.p' '.pl' '.lsp'] {vedcompileable true }]
    [['.p' '.p-' '.c' '.ph' '.pl' '.pl-' '.com' '.s'] {vednotabs false}]
    [['.s' '.com' '.out' '.ps' 'veddir'] {vedindentstep 8}]
    ['news' {vedleftmargin 4}]  ;;; for editing news files
    [vednonbreakfiles {vedbreak false}]
    [['output' 'interact'] {vedwriteable false}] ;;; don't save 'output'
]-> vedfiletypes;

[ '.ph'  '.s' '.ps'] <> vednonbreakfiles -> vednonbreakfiles;

;;; When VED (or HELP, OR SHOWLIB, etc) is invoked it compiles vedinit.p,
;;; i.e. this file. Unless LIB V55 has already been compiled it checks
;;; to find out what terminal you are using.
;;; (Since we use a terminal exchange, this cannot be done using the
;;; computer port number.)

;;; Using the mechanism described in HELP * VEDTERMINALSELECT
;;; it looks to see if you are using a VT100 or V200. If the latter, it
;;; tries to decide whether it is a V55 (which emulates a V200, but has
;;; more facilities, including an answerback facility which can be
;;; programmed to answer <ESC> [ 55 V

;;; If there is no answer after 3 seconds (alas, a time built in to the
;;; system) it assumes its a V200, then restores 'mypop.pwv' unless
;;; it is already running that ('vedimage' is TRUE).
;;; If there is an answer saying its a VISUAL 55, then if the 'v55.psv'
;;; saved image exists, it is restored, otherwise LIB V55 is compiled.

;;; Bug - if you invoke VED after doing some work, e.g. loading files,
;;; the work will be lost if an image is restored.

vars vedimage;  ;;; set true in MKMYPOP, above


;;; Don't recompile lib v55 if already compiled.
;;; Use 'nil -> proglist' to terminate compilation
unless identprops("ved_v55") = undef then nil -> proglist endunless;


define v55;
    ;;; run when check for v55 has been successful, to decide what to do.
    ;;; 'v55.psv' can only be restored relative to 'mypop.psv'
    if isboolean(vedimage) and vedimage and readable('~aarons/v55.psv')
    then
        printf('restoring V55 image\n');
        sysrestore('~aarons/v55.psv');
    elseif vedediting then
        lib v55;
        vedscreenescape(vvedscreensetpad);
    else
        ;;; use sysexecute to restore relative to another image
        ;;; and hand over poparglist
        sysexecute('$usepop/pop/pop/pop11',
                ['pop11' '-~aarons/mypop' '-~aarons/v55'
                    %if null(poparglist) then
                        if vedcurrent and vedcurrent /= undef then vedcurrent endif
                     else dl(poparglist)
                     endif%],
                false);

    endif;
enddefine;


define vedvt52select();
    ;;; test for Visual 55. Assumes usual VT52 terminal ID.
    ;;; also assumes V55 'answer back' has been set to '<ESC>[55V'
    ;;; (To do this on V55, press SET UP. Press SHIFT-A. type '/<ESC>[55V/')
    vars vvedscreensendidseq vedterminalselect;
    lvars c;
    if vedterminalname == "v55" then return() endif;    ;;; already done
    ;;; V55 answer back is triggered by CTRL-E
    procedure(); rawcharout(`\^E`) endprocedure -> vvedscreensendidseq;
    [['[55V' ^v55] [^false ^identfn]] -> vedterminalselect;
    ;;; clear input buffer
    while sys_inputon_terminal(popdevraw) do
    erase(rawcharin());
    endwhile;
    pr('CHECKING TERMINAL. DON\'T TYPE AHEAD\n');
    sysflush(popdevraw);
    vedtermsetup();     ;;; uses vedterminalselect
enddefine;
===========================================================================


--- C.unix/help/initial.ex
--- Copyright University of Sussex 1986. All rights reserved. ----------
