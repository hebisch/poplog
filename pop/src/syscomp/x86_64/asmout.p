/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:        S.pcunix/src/syscomp/asmout.p
 > Purpose:     Output code procedures for Intel 80x86 (Unix-style assembler)
 > Author:      John Gibson (see revisions)
 >              (modified for 80386 by Robert Duncan, August 1988)
 */


#_INCLUDE 'common.ph'

section $-Popas;

constant macro CLOSURE_HDR_LEN = 6;

define global popint(n); lvars n; (n << 2) || 3; enddefine;

define global mcint(n); lvars n; n >> 2; enddefine;


;;; --- ASSEMBLER OUTPUT -----------------------------------------------

;;; Assemblers vary according to whether expressions are bracketed with
;;; [...] or (...)

#_IF DEF SYSTEM_V
;;; System V seems to prefer [...]
lconstant ASM_LPAR = '[', ASM_RPAR = ']';
#_ELSE
lconstant ASM_LPAR = '(', ASM_RPAR = ')';
#_ENDIF

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
#_IF false
    lconstant SEP_CHAR = `.`;
#_ELSE
        lconstant SEP_CHAR = `_`;
#_ENDIF
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
    $- ASM_SHORT_STR= '\t.value\t',
    $- ASM_INT_STR  = '\t.long\t',
        $- ASM_DOUBLE_STR  = '\t.quad\t',
    $- ASM_WORD_STR = ASM_DOUBLE_STR,
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
enddefine;
sysprotect("pop_max_filename_len");

constant procedure asm_endfile = identfn;


constant procedure (
    asm_startcode   = outcode(% '\n' <> ASM_TEXT_STR %),
    asm_startdata   = outcode(% '\n' <> ASM_DATA_STR %),
);

global constant procedure (
    asm_align_word =
             #_IF true
                        outcode(% '.align\t8' %)           
             #_ELSE    
        #_IF DEF LINUX and not(DEF LINUX_ELF)
            outcode(% '.align\t2' %)
        #_ELSE
            outcode(% '.align\t4' %)
                #_ENDIF
             #_ENDIF,
asm_align_double = 
#_IF false
       identfn
#_ELSE
       outcode(% '.align\t8' %)
#_ENDIF,
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
        asm_outdouble = outdatum(% ASM_DOUBLE_STR %), 
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
#_IF DEF SOLARIS
        explode(' \\/ 4'),
#_ELSE
        explode(' >> 3'),
#_ENDIF
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
    ;;; EBX is the user stack pointer
    asm_outlab(nextlab() ->> l);
    fast_for offs from nfroz*8 by -8 to 8 do
        asmf_pr('\tsubq\t$8, %rbx\n');
        asmf_printf(asm_expr(l, "-", offs), '\tmovq\t%p, %%rax\n');
        asmf_pr('\tmovq\t%rax, (%rbx)\n');
    endfast_for;
    ;;; Jump to -jmplab-
    asmf_printf(jmplab, '\tmovq\t$%p, %%rax\n');
    asmf_pr('\tjmp\t*%rax\n');
    ;;; Return the number of longwords planted
    asm_align_word();
    (nfroz*18+15)>>3;
enddefine;


/*
 *  Exfunc closure code -- jump to subroutine _exfunc_clos_action (aextern.s)
 *  with exfunc_closure record address in a reg, etc. This procedure is
 *  passed the label of _exfunc_clos_action. The code generated must be
 *  padded to exactly 4 words.
 */

define asm_gen_exfunc_clos_code(action_lab);
    lvars action_lab;
    ;;; leal _exfunc_clos_action, %eax (= 6 bytes)
    asmf_printf(action_lab, '\tleaq\t%p, %%r10\n');
    ;;; call *%eax (= 2 bytes)
    asmf_printf('\tcall\t*%%r10\n');
    ;;; pad to 16 bytes
#_IF DEF SYSTEM_V or DEF LINUX
    asmf_printf('\t.align\t8\n');
#_ELSE
    asmf_printf('\t.space\t5\n');
#_ENDIF
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
#_IF DEF LINUX and not(DEF LINUX_ELF)
    '_' <> symbol -> symbol;
#_ENDIF
    if lang = 'FORTRAN' then uppertolower(symbol) <> '_' -> symbol endif
enddefine;


#_IF DEF LINUX

    ;;; Use C compiler to link
constant
    cc_link_command_header = '$POP__cc -v -Wl,-export-dynamic -Wl,-no-as-needed -o $IM \\\n'
;

#_ELSE

    /*  String for first line of Unix "ld" command -- used in os_comms.p
        (Image name is in the environment variable "IM")
    */
constant
    unix_ld_command_header =
#_IF DEFV SYSTEM_V >= 4.0
        '/usr/ccs/bin/ld -o $IM -e _start \\\n'
    #_IF DEF NCR or DEF DGUX
        ;;; export global symbols for external load
        <> '-B export \\\n'
    #_ENDIF
#_ELSEIF DEF LINUX_ELF
        '/usr/bin/ld -S -x -o $IM -m elf_i386 -L/lib \\\n' <>
        '-export-dynamic -dynamic-linker /lib/ld-linux.so.1 \\\n'
#_ELSEIF DEF LINUX
        '/usr/bin/ld -S -x -static -o $IM \\\n'
#_ELSEIF DEF SCO
        '/bin/ld -x -o $IM -e _start \\\n'
#_ELSE
        '/bin/ld -x -o $IM -e start \\\n'
#_ENDIF
    ;

    /*  Location of crt*.o files used by "ld"
    */
define active unix_ld_crt_objects;

        ;;; Search a lib directory for crt{1,i,n}.o or crt0.o
    define SearchLib(dir) -> found;
        if sys_file_exists(dir dir_>< 'crt1.o') then
            dir dir_>< 'crt{1,i,n}.o' -> found;
        elseif sys_file_exists(dir dir_>< 'crt0.o') then
            dir dir_>< 'crt0.o' -> found;
        else
            false -> found;
        endif;
    enddefine;

        ;;; Use 'gcc -v' to locate GCC CRTs
    define TryGCC(cc) -> found;
        false -> found;
        if cc and sys_fname_path(cc) = nullstring then
            sys_search_unix_path(cc, systranslate('PATH')) -> cc;
        endif;
        returnunless(cc);
        lvars (_, dev, _, _, pid) =
            run_unix_program(cc, ['-v'], false, true, 1, true);
        lvars linerep = line_repeater(dev, 512);
        lvars line = linerep();
        syskill(pid) -> ;
        sysclose(dev);
        sys_wait(pid) -> (,);
        ;;; we expect the first line of output to read
        ;;;     Reading specs from <path>/specs
        ;;; where <path> is what we're looking for
        lconstant INTRO = 'Reading specs from ';
        lconstant FNAME = 'specs';
        returnunless(isstring(line) and isstartstring(INTRO, line));
        lvars specfile = allbutfirst(length(INTRO), line);
        returnunless(sys_fname_name(specfile) = FNAME);
        lvars gcclib = sys_fname_path(specfile);
        SearchLib(gcclib) -> found;
    enddefine;

        ;;; Look in the standard locations for SunPro CC
    define TrySunPro(cc) -> found;
        false -> found;
        if cc and sys_fname_path(cc) = nullstring then
            sys_search_unix_path(cc, systranslate('PATH')) -> cc;
        endif;
        returnunless(cc);
        ;;; we're expecting: <installdir>/bin/cc
        returnunless(isendstring('/bin/cc', cc));
        lvars installdir = allbutlast(6, cc);   ;;; includes trailing '/'
        lvars dir, procedure version_p =
            sys_file_match(installdir dir_>< 'SC*', nullstring, false, false);
        until (version_p() ->> dir) == termin do
            returnif(SearchLib(dir dir_>< 'lib/') ->> found);
            returnif(SearchLib(dir) ->> found);
        enduntil;
    enddefine;

        ;;; Not found
    define Fail();
        mishap(0, 'CANNOT FIND C RUNTIME STARTUP OBJECT FILES')
    enddefine;

        ;;; The C compiler to use
    lvars cc;
    unless systranslate('POP__cc') ->> cc then
        'cc' -> cc;
    endunless;

        ;;; Might it be a GNU compiler?
    lvars try_gcc =
        if sys_fname_nam(cc) = 'gcc' then
            true
        elseif DEF LINUX then
            true
        else
            false
        endif;

        ;;; Might it be a SunPro compiler?
    lvars try_sunpro =
        if DEF SUNOS then
            not(try_gcc)
        else
            false
        endif;

        ;;; Do the search
    try_gcc and TryGCC(cc) or
    try_sunpro and TrySunPro(cc) or
    SearchLib('/usr/ccs/lib/') or
    SearchLib('/usr/lib/') or
    Fail();
enddefine;

#_ENDIF

endsection;     /* $-Popas */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Feb 26 1999
        Added cc_link_command_header for Linux
--- Robert Duncan, Feb 26 1999
        Changed asm_pdr_len again
--- Robert Duncan, Feb 24 1999
        Changed asm_pdr_len for Linux
--- Robert Duncan, Nov  4 1998
        Generalised unix_ld_crt_objects for all systems, and to take
        account of $POP__cc
--- Robert Duncan, Aug 25 1998
        Changed asm_pdr_len not to use the right shift operator ">>" --
        Solaris x86 assembler doesn't like it.
        Added Solaris definition of unix_ld_crt_objects.
--- Julian Clinton, Aug 5 1998
        Added DGUX to NCR conditional of '-B' linker flag.
--- John Gibson, Aug 15 1996
        New version of extern_name_translate.
--- Robert Duncan, Aug  9 1996
        Changed to support NCR SVR4
--- Robert Duncan, Apr 25 1996
        Added cases for Linux ELF
--- Integral Solutions Ltd, Aug 31 1995 (Julian Clinton)
        Modified 'align' option for Linux.
--- John Gibson, Apr  3 1995
        Added asm_outint
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Modifications for Linux and SCO.
--- Robert John Duncan, Oct 11 1994
        Added definitions for asm_uselab and asm_pdr_len, used in
        "genproc.p"
--- Robert John Duncan, Sep  5 1994
        Renamed from C.80386: this is now strictly for Unix; cf. Windows
        version in S.pcwnt
--- Robert John Duncan, Mar 22 1994
        Changed asm_gen_poplink_code to plant the execute labels
--- Robert John Duncan, Jan 26 1994
        Revised to assume PC/Unix-type system
--- Robert John Duncan, May 20 1993
        Really added asm_gen_exfunc_clos_code
--- John Gibson, May 19 1993
        Added asm_gen_exfunc_clos_code
--- Robert John Duncan, Jul 27 1992
        Added -extern_name_translate-
--- John Gibson, Jul 17 1989
        OS command procedures commoned in C.unix/src/syscomp/os_comms.p
        (but this file still defines the "ld" command string since this
        is different for nearly every system).
--- John Gibson, Jun  7 1989
        Included common.ph
--- John Gibson, May 18 1989
        Thought the two uses of #_IF which didn't have newlines after
        the conditions were causing a problem, so changed them, but
        then realised they weren't, so changed them back!
--- John Gibson, Mar 23 1989
        Added -asm_addbits- and -asm_outbits-
--- Rob Duncan, Feb 16 1989
        Amalgamated with Sun 386 version
--- Rob Duncan, Feb  3 1989
        Fixed the code size returned by -asm_gen_poplink_code-
--- John Gibson, Feb  2 1989
        Changed -asm_gen_poplink_code- to allow any number of frozvals to
        be pushed.
--- John Gibson, Nov 16 1988
        Added procedure -unix_archive- (used by poplibr)
--- Rob Duncan, Oct 27 1988
        Changed -asm_startfile- to take the filename as argument
--- Rob Duncan, Oct 11 1988
        Removed a spurious `%` character from -asm_gen_poplink_code-
 */
