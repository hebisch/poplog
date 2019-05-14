/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.all/lib/lib/showcode.p
 > Purpose:        Tracing poplog virtual machine code planting procedures.
 > Author:         Jonathan Laventhol, Jan 30 1984 (see revisions)
 > Documentation:  HELP * SHOWCODE
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING or DEF showcode

;;; loading this file converts all the code planting procedures to print their
;;; name and their arguments, and makes sysnlabel make something printable.
;;; tracing is switched on and off by the boolean variable pop_show_code. the
;;; initial "sys" of the procedure names isn't printed, and calls to sysLABEL
;;; just print the argument, so sysSYNTAX(...); sysLABEL(sysnlabel) will print
;;; like this:
;;;
;;;     SYNTAX  hello procedure <false>
;;; label_3
;;;
;;; You can make the procedures do something other than print by redefining
;;; the variable procedure popshowcodepdr (default is popshowcodesyspdr).
;;; The new procedure should take 2 arguments, the name of the code planting
;;; procedure (e.g. "sysSYNTAX" or "sysLABEL") and a vector of arguments.


section $-showcode => pop_show_code, popshowcodepdr, popshowcodesyspdr;

vars
    pop_show_code       =  false,
    procedure popshowcodepdr,
    ;


lvars curr_indent = 0;

define lconstant label_name =
    newproperty([], 64, false, "tmparg")
enddefine;

define vars make_code_showable();

    ;;; NB anyone updating the list should retain alphabetical order to
    ;;; simplify checking for completeness (using ved_wordswith and ved_cdiff).

    applist([sysAND sysCALL sysCALLQ sysCALLS sysCOMPILE sysCONSTANT
            sysCONSTRUCT sysDLABEL sysDLVARS sysENDLBLOCK sysENDPROCEDURE
            sysERASE sysEXECUTE sysEXEC_COMPILE sysEXEC_OPTION_COMPILE
            sysFIELD sysGLOBAL sysGOTO sysGO_ON
            sysIDENT sysIFNOT sysIFSO sysLABEL sysLBLOCK sysLCONSTANT
            sysLOCAL sysLVARS
            ;;; sysNEW_LABEL handled below
            sysNEW_LVAR sysOR sysPASSIGN sysPLOG_ARG_PUSH sysPLOG_IFNOT_ATOM
            sysPLOG_RESTART sysPLOG_RESTORE sysPLOG_SAVE sysPLOG_TERM_SWITCH
            sysPOP sysPROCEDURE sysPUSH sysPUSHQ sysPUSHS
            sysSWAP sysSYNTAX sysUCALL sysUCALLQ sysUCALLS
            sysUFIELD sysUPASSIGN sysVARS ],

        procedure(word);
            lvars procedure pdr, word, new_p;

            define lconstant trace_instr() with_props false;
                lvars args;
                if pop_show_code and curr_indent >= 0 then
                    consvector(pdnargs(pdr)) -> args;
                    popshowcodepdr(word, args);
                    explode(args)
                endif;
                ;;; indent instructions run by code-planting procedures
                ;;; given to other instructions
                unless word == "sysEXECUTE" then
                    dlocal curr_indent = curr_indent+4;
                endunless;
                pdr()
            enddefine;

            define lconstant sys_comp() with_props false;
                lvars n, m, p;
                dlocal curr_indent;
                if iscaller(subsystem_libcompile) then
                    ;;; don't show code if compiling a library
                    chain(  procedure;
                                dlocal pop_show_code = false;
                                trace_instr()
                            endprocedure)
                endif;

                if iscaller(sys_comp, 1) ->> n then
                    fast_for m to n do
                        caller(m) -> p;
                        if isdlocal(ident pop_compiler_subsystem, p)
                        and p /== subsystem_compile then
                            false -> n;
                            quitloop
                        endif
                    endfor
                endif;
                unless n then -4 -> curr_indent endunless;
                trace_instr()
            enddefine;

            sysunprotect(word);
            valof(word) -> pdr;
            if word == "sysCOMPILE" then sys_comp else trace_instr endif
                                ->> new_p -> valof(word);
            pdprops(pdr) -> pdprops(new_p);
            updater(pdr) -> updater(new_p);
            sysprotect(word)
        endprocedure);

    sysunprotect("sysNEW_LABEL");
    procedure(old_p) -> lab_pair;
        old_p() -> lab_pair;
        gensym("label_") -> label_name(lab_pair)
    endprocedure(% valof("sysNEW_LABEL") %) -> valof("sysNEW_LABEL");
    "sysNEW_LABEL" -> pdprops(valof("sysNEW_LABEL"));
    sysprotect("sysNEW_LABEL");
    identfn -> make_code_showable
enddefine;


define popshowcodesyspdr(name, args);
    lvars args, name;
    returnif(pop_syntax_only);
    dlocal pop_pr_quotes = true;
    sp(max(0,curr_indent));
    if name == "sysLABEL" or name == "sysDLABEL" then
        pr(label_name(args(1)) or args(1));
        cucharout(`:`)
    else
        sp(4);
        pr(allbutfirst(3, name));
        sp(max(1, 8 - datalength(name) + 3));
        appdata(args, procedure(arg);
                        spr(label_name(arg) or arg)
                      endprocedure)
    endif;
    cucharout(`\n`)
enddefine;

popshowcodesyspdr -> popshowcodepdr;

make_code_showable();

endsection; /* showcode */

section;
constant showcode = true;   ;;; for "uses"
endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep 17 1996
        # sysNEW_LABEL pair labels are now retained and printed as temp
          names via a property (substituting the pairs with words changes
          their scope, cf isl-fr.4583).
        # Instructions executed by code-planting procedures given to other
          instructions are now printed with extra indentation.
        # pop_show_code is set false inside a sysCOMPILE run inside
          subsystem_libcompile (so that code is not printed while
          compiling a library).
--- John Gibson, Jul  7 1990
        Added -sysEXEC_COMPILE-
--- Aaron Sloman, Jul  2 1990
        Declared variable showcode
--- John Williams, May 14 1990
        Replaced sys(U)(EXT)FIELD_VAL and sys(U)(EXT)SUBSCR with sys(U)FIELD
--- John Williams, Mar 26 1990
        No longer makes -sysPOP- the updater of -sysPUSH- etc.
--- Aaron Sloman, Mar 18 1990
    Added sysEXEC_OPTION_COMPILE sysEXT_SUBSCR sysSUBSCR sysUEXT_SUBSCR
    sysUSUBSCR, and re-arranged list into alphabetical order to simplify
    checking for completeness.
--- Ian Rogers, May 24 1989 - added sysEXT_FIELD_VAL sysUEXT_FIELD_VAL
--- John Gibson, Nov 23 1988
        Added sysLBLOCK, sysENDLBLOCK
 */
