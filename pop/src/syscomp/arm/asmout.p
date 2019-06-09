
#_INCLUDE 'common.ph'

section $-Popas;

constant macro CLOSURE_HDR_LEN = 7;

define global popint(n); lvars n; (n << 2) || 3; enddefine;

define global mcint(n); lvars n; n >> 2; enddefine;


;;; --- ASSEMBLER OUTPUT -----------------------------------------------

;;; Assemblers vary according to whether expressions are bracketed with
;;; [...] or (...)

;;; gas seems to prefer (...)
lconstant ASM_LPAR = '(', ASM_RPAR = ')';

;;; This can be used in a .s file wherever bracketing is needed
define macro Syspop$- ASM_EXPR;
    lconstant LPAR = consword(ASM_LPAR), RPAR = consword(ASM_RPAR);
    lvars item;
    dlocal pop_autoload = false;
    pop11_need_nextreaditem("(") -> ;
    LPAR;
    until pop11_try_nextitem([) ^termin]) ->> item do readitem() enduntil;
    RPAR;
    item :: proglist -> proglist;
    pop11_need_nextreaditem(")") -> ;
enddefine;

;;; Local labels

vars nextlabel;

define nextlab();
    dlocal pop_pr_radix = 36;
    '_L' >< nextlabel;
    nextlabel fi_+ 1 -> nextlabel;
enddefine;

;;; Global labels constructed from word plus prefix char

define lconstant asm_label(word, prefix_char);
    lconstant SEP_CHAR = `_`;
    lvars n, len, string, q, r, prefix_char, word;
    if isstring(word) then word else fast_word_string(word) endif -> string;
    datalength(string) -> len;
    cons_with consstring {%
        prefix_char, SEP_CHAR;
        fast_for n to len do
            go_on dup(fast_subscrs(n, string)) to
            ;;; 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
                c c c c c c c c c c c c c c c c     ;;; Ctrl-A -- Ctrl-P
                c c c c c c c c c c c c c c c c     ;;; Ctrl-Q -- SPACE
                s s s $ s s s s s s s s s s s d     ;;;   !    --   0
                d d d d d d d d d s s s s s s s     ;;;   1    --   @
                A A A A A A A A A A A A A A A A     ;;;   A    --   P
                A A A A A A A A A A s s s s u s     ;;;   Q    --   `
                a a a a a a a a a a a a a a a a     ;;;   a    --   p
                a a a a a a a a a a s s s s c c     ;;;   q    -- \(128)
            else c;
                        u:          ;;; underscore
                              `_` ;   ;;;      
            A: a: d:        ;;; alpha, digit, underscore
                nextloop;

            $ :
                if n /== len and fast_subscrs(n fi_+ 1, string) == `-`
                and (n /== 1 or len fi_> 2)
                then
                    ;;; replace section separator $- with SEP_CHAR
                    -> ;            ;;; erase $
                    unless n == 1 then `_`, `S`  endunless;
                    n fi_+ 1 -> n;  ;;; skip -
                    nextloop
                ;;; else drop thru to "s"
                endif;

            s: c:               ;;; other sign, control etc
                ;;; represent as 3 decimal digits
                () fi_// 100 -> q -> r;
                                `_` ;
                q fi_+ `0`;
                r fi_// 10 -> q -> r;
                q fi_+ `0`, r fi_+ `0`;

        endfor
    %};
enddefine;

constant procedure (
    asm_symlabel    = asm_label(%`c`%),
    asm_identlabel  = asm_label(%`i`%),
    asm_wordlabel   = asm_label(%`w`%),
    asm_wordidlabel = asm_label(%`z`%),
    asm_pdpropslabel= asm_label(%`p`%),
    asm_testdeflabel= asm_label(%`t`%),
);

;;; Lists of pop symbol prefixes and loader symbols for use by
;;; -Extern_make_base- in "unixextern.p"

global constant macro (

    $- POP_SYMBOL_PREFIXES = [[
            'c.' 'xc.' 'uc.' 'xuc.' 'i.' 'w.' 'z.' 'p.' 't.' '_end\^@'
        ]],

);

;;; Assembler directive strings

global constant macro (
    $- ASM_TEXT_STR = '\t.text',
    $- ASM_DATA_STR = '\t.data',
    $- ASM_BYTE_STR = '\t.byte\t',
    $- ASM_SHORT_STR= '\t.short\t',
    $- ASM_INT_STR  = '\t.word\t',
    $- ASM_WORD_STR = ASM_INT_STR,
);

;;; Starting and ending an assembly code file

;;; asm_startfile:
;;;     the Unix assembler likes a file name at the head of the file
;;;     (but of limited length)

lconstant ASM_MAX_FILENAME_LEN = 14;

sysunprotect("pop_max_filename_len");
define asm_startfile(name);
    lvars name;
    dlocal pop_max_filename_len = ASM_MAX_FILENAME_LEN;
    asmf_printf(sysfileok(name), '\t.file\t"%p"\n');
    asmf_printf('\t.arch armv5\n');
enddefine;
sysprotect("pop_max_filename_len");

constant procedure asm_endfile = identfn;


constant procedure (
    asm_startcode   = outcode(% '\n' <> ASM_TEXT_STR %),
    asm_startdata   = outcode(% '\n' <> ASM_DATA_STR %),
);

global constant procedure (
    asm_align_word =
            outcode(% '.align\t2' %),
    asm_align_double = 
            outcode(% '.align\t3' %)
    ;;; asm_align_file -- end-of-file alignment
    ;;; (don't define if not needed)
    ;;; asm_align_file = identfn,
);

;;; Planting labels and constants

define asm_outlab(lab);
    lvars lab;
    asmf_charout(lab), asmf_charout(`:`), asmf_charout(`\n`);
enddefine;

define asm_outlabset(lab, val);
    lvars lab, val;
    asmf_printf(val, lab, '\t.set\t%p, %p\n')
enddefine;

define asm_outglab(lab);
    lvars lab;
    asmf_charout('.globl\s'), asmf_charout(lab), asmf_charout(`\n`);
    asm_outlab(lab);
enddefine;

define asm_outglabset(lab, val);
    lvars lab, val;
    asmf_charout('.globl\s'), asmf_charout(lab), asmf_charout(`\n`);
    asm_outlabset(lab, val);
enddefine;

constant procedure asm_uselab = identfn;

define lconstant outdatum(n, type);
    lvars i, n, type;
    if n == 0 then return endif;
    asmf_charout(type);
    fast_for i from n by -1 to 2 do
        asmf_pr(subscr_stack(i)), asmf_charout(',\s');
    endfor;
    asmf_pr();
    erasenum(n-1);
    asmf_charout(`\n`);
enddefine;

global constant procedure (
    asm_outbyte     = outdatum(% ASM_BYTE_STR %),
    asm_outshort    = outdatum(% ASM_SHORT_STR %),
    asm_outint      = outdatum(% ASM_INT_STR %),
    asm_outword     = outdatum(% ASM_WORD_STR %),
);

define asm_out_dfloat(hipart, lopart);
    lvars hipart, lopart;
    asm_outint(lopart, hipart, 2);
enddefine;

    /*  -asm_addbits- is used to accumulate a sequence of bitfields into
        a (big)integer which -asm_outbits- is then called to output when the
        total number of bits accumulated occupies an exact number of bytes.
            -val- is the currently accumulated (big)integer, containing
        -nbits- bits so far; -new_val- is the field to be added, containing
        -new_nbits- bits.
            The value returned will be the -val- input for the next field
        (with -nbits- incremented by -new_nbits-), and so on. Other than
        initialising -val- to 0 at the start, nothing is assumed about how
        the fields are added to it, so it is the responsibility of this
        procedure and -asm_outbits- to output the bitfields correctly.
    */
    ;;; 386 version same as VAX ('little endian'), adds the fields at the
    ;;; top and outputs the bytes from the bottom up
define asm_addbits(new_val, new_nbits, val, nbits);
    lvars new_val, new_nbits, val, nbits;
    val || (new_val << nbits)   ;;; add the new field at the top
enddefine;

    /*  In this, -nbits- is a multiple of BYTE_BITS, and -val- has been
        produced by repeated calls of -asm_addbits-
    */
define asm_outbits(val, nbits);
    lvars val, nbits, count = 0;
    lconstant BYTE_MASK = (1<<BYTE_BITS)-1;
    until nbits == 0 do
        val && BYTE_MASK;           ;;; output bytes from the bottom up
        val >> BYTE_BITS -> val;
        nbits-BYTE_BITS -> nbits;
        ;;; this just saves putting each byte on a separate line
        if (count+1 ->> count) == 12 then asm_outbyte(count), 0 -> count endif;
    enduntil;
    if count /== 0 then asm_outbyte(count) endif
enddefine;

;;; asm_expr:
;;;     constructs an assembler expression of the form "opd1 op opd2" where
;;;     the operands may be integers, symbols or other expressions (both
;;;     the latter being represented as strings), and the operator may be
;;;     any word (assumed to be an infix operator). Used in "genproc.p".

define global asm_expr(opd1, op, opd2);
    lvars op, opd1, opd2;
    if op == "+" or op == "-" then
        if isintegral(opd1) and isintegral(opd2) then
            return(valof(op)(opd1, opd2));
        elseif opd1 == 0 then
            return(op == "+" and opd2 or ASM_LPAR <> '-' <> opd2 <> ASM_RPAR);
        elseif opd2 == 0 then
            return(opd1);
        endif;
    endif;
    consstring(#|
        explode(ASM_LPAR),
        dest_characters(opd1), ` `, explode(op), ` `, dest_characters(opd2),
        explode(ASM_RPAR),
    |#);
enddefine;

;;; asm_pdr_len:
;;;     construct an expression which will evaluate to the length of a
;;;     procedure (in long words) given its header size, execute address
;;;     and end address

define asm_pdr_len(hdr_size, exec_addr, end_addr);
    lvars hdr_size, exec_addr, end_addr;
    consstring(#|
        explode(ASM_LPAR), explode(ASM_LPAR),
        dest_characters(end_addr), explode(' - '), dest_characters(exec_addr),
        explode(ASM_RPAR),
        explode(' >> 2'),
        explode(ASM_RPAR), explode(' + '),
        dest_characters(hdr_size)
    |#);
enddefine;

;;; asm_gen_poplink_code:
;;;     code for undefined procedures generated by poplink

define asm_gen_poplink_code(outlabs, nfroz, jmplab);
    lvars outlabs, nfroz, jmplab, l, offs;
    ;;; plant exec labels
    outlabs();
    ;;; Push nfroz frozvals (last nfroz longwords planted)
    ;;; 'r10' is the user stack pointer
    asm_outlab(nextlab() ->> l);
    fast_for offs from nfroz*4 by -4 to 4 do
        asmf_printf(asm_expr(l, "-", offs), '\tldr r0, %p\n');
        asmf_pr('\tstr r0, [r10, #-4]!\n');
    endfast_for;
    ;;; Jump to -jmplab-
    nextlab() -> l;
    asmf_printf(l, '\tldr r0, %p\n');
    asmf_pr('\tbx r0\n');
    asm_outlab(l);
    asm_outword(jmplab, 1);
    ;;; Return the number of longwords planted
    asm_align_word();
    (nfroz*2+3);
enddefine;


/*
 *  Exfunc closure code -- jump to subroutine _exfunc_clos_action (aextern.s)
 *  with exfunc_closure record address in a reg, etc. This procedure is
 *  passed the label of _exfunc_clos_action. The code generated must be
 *  padded to exactly 4 words.  It must preserve argument registers
 *  and call-preserved registers.  On ARM that means we can only use
 *  r12...
 */

define asm_gen_exfunc_clos_code(action_lab);
    lvars action_lab, l = nextlab();
    asmf_printf('\tsub r12, pc, #8\n');
    asmf_printf(l, '\tldr pc, %p\n');
    asmf_printf('\tnop\n');
    asm_outlab(l);
    asm_outword(action_lab, 1);
enddefine;


/*
 *  extern_name_translate:
 *      translate an external symbol, using the same conventions as the
 *      C compiler and linker (e.g. add a leading '_', truncate to N
 *      characters, etc.)
 */

define global extern_name_translate(lang, symbol, type) -> symbol;
    lvars lang, symbol, type;
    returnif(lang = 'ASM');
    if lang = 'FORTRAN' then uppertolower(symbol) <> '_' -> symbol endif
enddefine;


    ;;; Use C compiler to link
constant
    cc_link_command_header = '$POP__cc -Wl,-export-dynamic -o $IM \\\n'
;

endsection;     /* $-Popas */

