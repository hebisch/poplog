/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/slowprocs.p
 > Purpose:         completely remove the fast procedures, for debugging.
 > Author:          Jonathan Laventhol, Jun 17 1983 (see revisions)
 > Documentation:   REF *FASTPROCS, HELP *EFFICIENCY
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

;;; NB This file must be loaded BEFORE user files which use the fast
;;; procedures.

;;; See LIB * pop_nofast_trans for the fast to slow translation.

define vars 0 slowprocs(nofast);
    lvars flags = pop_vm_flags;
    set_global_valof(if nofast then flags || VM_NO_FAST
                     else flags &&~~ VM_NO_FAST
                     endif,
                     ident pop_vm_flags);
    pr( if nofast then ';;; Fast procedures removed\n'
        else ';;; Fast procedures are fast\n'
        endif)
enddefine;

;;; Initialise
slowprocs(true);

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Now redundant -- replaced by compile_mode global :vm [+|-]nofast
        (which slowprocs does the same as).
--- Robert John Duncan, May 15 1992
        Changed to exclude fast Prolog predicates which are semantically
        distinct from their slow versions (see BR isl-fr.4431)
--- Adrian Howard, Jun 25 1991
        Altered so fast/checking procedures are now automatically found by
        looking for procedures with fi/fast prefix
--- John Gibson, Jun 30 1989
        Added all the missing things
--- Simon Nichols, Nov  7 1988
        Added fast prolog procedures and increased property table size from
        31 to 63.
--- John Gibson, Apr  1 1988
        Added -fast_frozval-
--- John Gibson, Mar 20 1988
        Added -fast_repeat-
--- John Gibson, Mar 16 1988
        Added -fast_sysread- and -fast_syswrite-
--- John Gibson, Mar 14 1988
        Added -fi_div- and -fi_rem- (now in system)
--- Aaron Sloman, May 25 1987
    changed "ident" to "id" as it would no longer compile
--- Aaron Sloman, Jun  3 1986 restored fast_for, after redefining endfast_for
    as a macro equivalent to endfor
--- John Williams, May 31 1986
        Commented out entries for 'fast_for' and 'endfast_for', since
        using 'for' instead won't accept 'endfast_for' as a terminator
--- Aaron Sloman, May 19 1986
        Previously referred to non-existent HELP * SLOWPROCS
--- Aaron Sloman, May 15 1986
        Put in additional entries and sorted alphabetically.
--- John Williams, Feb 25 1986
        Added entries for 'fi_' bitwise operators, FI_MIN, and FI_MAX.
        Also made slow/fast procedure table an lconstant
--- Mark  Rubinstein, Jun 25 1985
        Mapping for FAST_APPPROPERTY added.
--- Jonathan Laventhol, July 23 1985
        Extended  so that you can switch from the fast to the slow procedures;
 */
