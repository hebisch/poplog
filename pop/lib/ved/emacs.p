/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/emacs.p
 > Purpose:        A first attempt to make VED compatible with EMACS
 > Author:         Aaron Sloman, July 1984 (see revisions)
 > Documentation:  HELP * EMACS
 > Related Files:   $usepop/pop/packages/emacs
 >                  The emacs package allows emacs proper to be used
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; Based on a file by D. Corner and L. Hardman at Edinburgh University
;;; (e.g. for those who have dumb terminals)
;;; This file is provisional and may be replaced

section;

vars emacsnormaltable emacsescapetable emacsextendtable;

copy(vednormaltable)    -> emacsnormaltable;
copy(vedescapetable)    -> emacsescapetable;
{% repeat 127 times vedscreenbell endrepeat %}  -> emacsextendtable;

define vedgotoline (line);
    vedtrimline();
    line    -> vedline;
    vedsetlinesize()
enddefine;

ved_files   -> emacsextendtable(`\^B`);
ved_q   -> emacsextendtable(`\^C`);
emacsescapetable(`w`)   ->> emacsextendtable(`1`)
                ->> emacsextendtable(`z`)
                        ->  emacsextendtable(`Z`);
vedswapfiles            ->> emacsextendtable(`n`)
                        ->> emacsextendtable(`N`)
                        ->> emacsextendtable(`p`)
                        ->  emacsextendtable(`P`);
ved_l1              ->  emacsextendtable(`\^M`);
ved_wq              ->  emacsextendtable(`\^F`);

ved_q   -> emacsescapetable(`\^C`);
"vedredokey"    -> emacsescapetable(`\^M`);
ved_xdn -> emacsescapetable(`\^V`);

;;; We associate 8 directions of movement with <ESC>
;;; followed by one of the keypad numbers 1 2 3 4 6 7 8 9,
;;; and make <ESC> 5 switch between small moves and largeish moves.

vars ved_large_moves;
false -> ved_large_moves;

define ved_largeorsmall(l,s);
    if ved_large_moves then l() else s() endif
enddefine;

ved_largeorsmall(%vedchardownleftlots, vedchardownleft%)
    -> emacsescapetable(`1`);
ved_largeorsmall(%vedchardownlots, vedchardown%)
    -> emacsescapetable(`2`);
ved_largeorsmall(%vedchardownrightlots, vedchardownright%)
    -> emacsescapetable(`3`);
ved_largeorsmall(%vedcharleftlots, vedcharleft%)
    -> emacsescapetable(`4`);

procedure;
    not(ved_large_moves) -> ved_large_moves
endprocedure -> emacsescapetable(`5`);

ved_largeorsmall(%vedcharrightlots, vedcharright%)
    -> emacsescapetable(`6`);
ved_largeorsmall(%vedcharupleftlots, vedcharupleft%)
    -> emacsescapetable(`7`);
ved_largeorsmall(%vedcharuplots , vedcharup%)
    -> emacsescapetable(`8`);
ved_largeorsmall(%vedcharuprightlots , vedcharupright%)
-> emacsescapetable(`9`);


vedscreenup -> emacsescapetable(`,`);
vedscreendown   -> emacsescapetable(`.`);
vedtopfile  -> emacsescapetable(`<`);
vedendfile  -> emacsescapetable(`>`);
vedwordleft -> emacsescapetable(`b`);
vedwordrightdelete  -> emacsescapetable(`d`);
vedwordright    -> emacsescapetable(`f`);
vedwordleftdelete   -> emacsescapetable(`h`);
procedure  ;
    if  vedline == 1
    then  vederror('\{b}top of file')
    else  vedgotoline(max(1,vedline - (vedwindowlength-2)))
    endif
endprocedure        -> emacsescapetable(`v`);
"vedenterkey"   -> emacsescapetable(`x`);

ved_helpfor -> emacsescapetable(`?`);

vedstatusswitch -> emacsescapetable(`E`);
vedswapfiles    -> emacsescapetable(`X`);

;;; ^A is vedscreenleft
vedcharleft -> emacsnormaltable(`\^B`);

veddotdelete    -> emacsnormaltable(`\^D`);
vedtextright    -> emacsnormaltable(`\^E`);
procedure;
    if  vedcolumn > vvedlinesize
    then  vednextline()
    else  vedcharright()
    endif
endprocedure            -> emacsnormaltable(`\^F`);
vedscreenbell   -> emacsnormaltable(`\^G`);
vedchardelete   -> emacsnormaltable(`\^H`);
;;; TAB as standard    vedinsertvedchar
procedure ;
vars  newcolumn vedleftmargin;
    vedcolumn   -> newcolumn;
    vedtextleft();
    vedcolumn-1, newcolumn  -> vedcolumn    -> vedleftmargin;
    vedcharinsert(`\n`)
endprocedure        -> emacsnormaltable(`\^J`);
procedure;
    if  vedcolumn > vvedlinesize
    then  vednextline();
          vedchardelete()
    else  vedcleartail()
    endif
endprocedure            -> emacsnormaltable(`\^K`);
vedrefresh      -> emacsnormaltable(`\^L`);
;;; CR as before  veddocr
vedchardown     -> emacsnormaltable(`\^N`);
procedure; vars newcolumn;
    vedcolumn   -> newcolumn;
    vedcharinsert(`\n`);
    vedcharup();
    newcolumn   -> vedcolumn;
endprocedure            -> emacsnormaltable(`\^O`);
vedcharup   -> emacsnormaltable(`\^P`);
;;;  nothing yet for ^Q  vedscreenbell
vedscreenbell   -> emacsnormaltable(`\^R`); ;;; nothing for ^R
;;;  nothing yet for ^S  vedscreenbell
procedure ;
;;; transpose two characters
vars vedcolumn;
    if vedcolumn <= 2
    then  vedscreenbell()
    else
        vedcharleft(); vedcurrentchar();
        vedcharleft(); vedcurrentchar();
        vedcharright(); -> vedcurrentchar();
        vedcharleft(); -> vedcurrentchar();
    endif
endprocedure        -> emacsnormaltable(`\^T`);  ;;; transpose to be added
vedscreenbell   -> emacsnormaltable(`\^U`);  ;;; repeat to be added
procedure ;
    if  vedline >= vvedbuffersize
    then  vederror('\{b}end of file')
    else  vedgotoline(min(vvedbuffersize,vedline +(vedwindowlength -2)))
    endif
endprocedure    -> emacsnormaltable(`\^V`);
vedscreenbell   -> emacsnormaltable(`\^W`);  ;;; write to be added
emacsextendtable    -> emacsnormaltable(`\^X`);
vedscreenbell   -> emacsnormaltable(`\^Y`);  ;;; yank to be added
vedscrollup -> emacsnormaltable(`\^Z`);
emacsescapetable    -> emacsnormaltable(`\^[`);
vedscreenbell   -> emacsnormaltable(`\^]`);

emacsnormaltable    -> vednormaltable;

global vars emacs = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1992
        Added declaration of "emacs" for -uses-
--- Ben Rubinstein, Oct 12 1986
        vedenter, vedredo indirected through ..key
--- Poplog System, Nov 29 1988
        Changed "vedhelpfor" to "ved_helpfor" (cf ALPHA 8)
 */
