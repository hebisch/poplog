/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/readtable.p
 > Purpose:         Set up Common Lisp standard readtable
 > Author:          John Williams, Feb 25 1986 (see revisions)
 > Documentation:   Common Lisp Manual p267-270, 280-281
 */

lisp_compile_mode;

section $-lisp;


define lconstant Illegal();
 #_IF sys_os_type starts_with "vms"
    cucharout(`\n`);
 #_ENDIF
    vedscreenbell();
    advise('Edit keys don\'t work outside VED');
    setpop()
enddefine;


define lconstant Escape();
 #_IF VED_LOADED
    handlepopescape();
 #_ENDIF
    chainfrom(lispreaditem, lispreaditem)
enddefine;


/* Create standard readtable */

global constant lisp_default_readtable;


consreadtable(
    {%

;;;  0                   1                   2                   3
;;;  Ctrl-@              Ctrl-A              Ctrl-B              Ctrl-C
     Illegal,            Illegal,            Illegal,            Illegal,

;;;  4                   5                   6                   7
;;;  Ctrl-D              Ctrl-E              Ctrl-F              Ctrl-G
     Illegal,            Illegal,            Illegal,            Illegal,

;;;  8                   9                   10                  11
;;;  <backspace>         <tab>               <linefeed>          Ctrl-K
     constituent,        white,              white,              Illegal,

;;;  12                  13                  14                  15
;;;  <page>              <return>            Ctrl-N              Ctrl-O
     white,              white,              Illegal,            Illegal,

;;;  16                  17                  18                  19
;;;  Ctrl-P              Ctrl-Q              Ctrl-R              Ctrl-S
     Illegal,            Illegal,            Illegal,            Illegal,

;;;  20                  21                  22                  23
;;;  Ctrl-T              Ctrl-U              Ctrl-V              Ctrl-W
     Illegal,            Illegal,            Illegal,            Illegal,

;;;  24                  25                  26                  27
;;;  Ctrl-X              Ctrl-Y              Ctrl-Z              <escape>
     Illegal,            Illegal,            Illegal,            Escape,

;;;  28                  29                  30                  31
;;;  Ctrl-\              Ctrl-]              Ctrl-^              Ctrl-_
     Illegal,            Illegal,            Illegal,            Illegal,

;;;  32                  33                  34                  35
;;;  <space>             !                   "                   #
     white,              constituent,        tmac(%rdstring%),   constituent,

;;;  36                  37                  38                  39
;;;  $                   %                   &                   '
     constituent,        constituent,        constituent,        tmac(%quote%),

;;;  40                  41                  42                  43
;;;  (                   )                   *                   +
     tmac(%rdlist%),     tmac(%closer%),     constituent,        numsign,

;;;  44                  45                  46                  47
;;;  ,                   -                   .                   /
     tmac(%comma%),      numsign,            dot,                slash,

;;;  48                  49                  50                  51
;;;  0                   1                   2                   3
     digit,              digit,              digit,              digit,

;;;  52                  53                  54                  55
;;;  4                   5                   6                   7
     digit,              digit,              digit,              digit,

;;;  56                  57                  58                  59
;;;  8                   9                   :                   ;
     digit,              digit,              colon,              tmac(%scolon%),

;;;  60                  61                  62                  63
;;;  <                   =                   >                   ?
     constituent,        constituent,        constituent,        constituent,

;;;  64                  65                  66                  67
;;;  @                   A                   B                   C
     constituent,        alpha,              alpha,              alpha,

;;;  68                  69                  70                  71
;;;  D                   E                   F                   G
     expt,               expt,               expt,               alpha,

;;;  72                  73                  74                  75
;;;  H                   I                   J                   K
     alpha,              alpha,              alpha,              alpha,

;;;  76                  77                  78                  79
;;;  L                   M                   N                   O
     expt,               alpha,              alpha,              alpha,

;;;  80                  81                  82                  83
;;;  P                   Q                   R                   S
     alpha,              alpha,              alpha,              expt,

;;;  84                  85                  86                  87
;;;  T                   U                   V                   W
     alpha,              alpha,              alpha,              alpha,

;;;  88                  89                  90                  91
;;;  X                   Y                   Z                   [
     alpha,              alpha,              alpha,              constituent,

;;;  92                  93                  94                  95
;;;  \                   ]                   ^                   _
     single_escape,      constituent,        constituent,        constituent,

;;;  96                  97                  98                  99
;;;  `                   a                   b                   c
     tmac(%backquote%),  alpha,              alpha,              alpha,

;;;  100                 101                 102                 103
;;;  d                   e                   f                   g
     expt,               expt,               expt,               alpha,

;;;  104                 105                 106                 107
;;;  h                   i                   j                   k
     alpha,              alpha,              alpha,              alpha,

;;;  108                 109                 110                 111
;;;  l                   m                   n                   o
     expt,               alpha,              alpha,              alpha,

;;;  112                 113                 114                 115
;;;  p                   q                   r                   s
     alpha,              alpha,              alpha,              expt,

;;;  116                 117                 118                 119
;;;  t                   u                   v                   w
     alpha,              alpha,              alpha,              alpha,

;;;  120                 121                 122                 123
;;;  x                   y                   z                   {
     alpha,              alpha,              alpha,              constituent,

;;;  124                 125                 126                 127
;;;  |                   }                   ~                   <rubout>
     multiple_escape,    constituent,        constituent,        constituent,

;;;  128 - 255
;;;
     fast_repeat 128 times Illegal endfast_repeat
    %},
    0,
    @:UPCASE)
    -> lisp_default_readtable;


/* Copying readtables */

vars next_readtable_id = 1;             /* System readtable is 0 */


define lconstant GET_READTABLE_ID();
    next_readtable_id;
    dup() fi_+ 1 -> next_readtable_id
enddefine;


define lconstant Copy_rt_item(item) -> item;
    if isclosure(item) and datalength(item) /== 0 then
        copy(item) -> item;
        Copy_rt_item(fast_frozval(1, item)) -> fast_frozval(1, item)
    elseif isvector(item) then
        copy(item) -> item
    endif
enddefine;


define copy_readtable(r1, r2) -> r2;
    lvars vec, case;
    if r1 == nil then
        lisp_default_readtable -> r1
    else
        defaults r1 readtable;
    endif;
    defaults r2 nil;
    destreadtable(r1) -> (vec, /* id */, case);
    appdata(vec, Copy_rt_item);
    if r2 == nil then
        consvector(fast_vector_length(vec)) -> vec;
        consreadtable(vec, GET_READTABLE_ID(), case) -> r2
    else
        fill(readtable_vector(r2)) ->;
        case -> readtable_case_sym(r2)
    endif
enddefine;


/* Accessing readtable procedures */

define readtable_pdr(char, r);
    fast_subscrv0(char_code(char), readtable_vector(r))
enddefine;


define updaterof readtable_pdr(pdr, char, r);
    pdr -> fast_subscrv0(char_code(char), readtable_vector(r))
enddefine;


define set_syntax_from_char(to_ch, from_ch, to_rt, from_rt);
    lvars pdr;
    ;;; defaults
    unless pop_true(to_rt) do
        readtable -> to_rt
    endunless;
    unless pop_true(from_rt) do
        lisp_default_readtable -> from_rt
    endunless;
    readtable_pdr(from_ch, from_rt) -> pdr;
    unless is_char_mac(pdr)
    or pdr == white or pdr == multiple_escape or pdr == single_escape do
        constituent -> pdr
    endunless;
    pdr -> readtable_pdr(to_ch, to_rt);
    true
enddefine;


/* Set up standard initial readtable */

copy_readtable(nil, nil) -> readtable;

;;; Install @ macro into initial readtable, but *not* standard
ntmac(% do_pop11 %) -> readtable_pdr(CHARACTER `@`, readtable);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  1 1995
        Ved identifiers now guarded with #_IF VED_LOADED.
--- John Williams, Apr  3 1995
        Changes for readtable case.
--- John Williams, Jan 25 1995
        copy_readtable now copies closures and their frozvals (so that
        dispatch macros are handled properly).
 */
