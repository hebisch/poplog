/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/seq_to_closure.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

compile_mode :pop11 +strict;

section;
vars procedure wrapper_kernel;
vars procedure wrapper_invoker;
endsection;

section $-objectclass;

;;;
;;; Is p a version of apply (or fast_apply) or a (recursive) empty
;;; closure of apply?
;;;
define lconstant procedure Is_apply( p ); lvars p;
    p == apply or
    p == fast_apply or
    p.isclosure and p.datalength == 0 and p.pdpart.Is_apply
enddefine;

define lconstant procedure seqclose( Kproc, Wlist ) ;
    if Wlist.null then
        Kproc
    else
        lvars p = Wlist.dest -> Wlist;
        if p.Is_apply then              ;;; optimise out simple appliers.
            seqclose( Kproc, Wlist )
        else
            p(% seqclose( Kproc, Wlist ) %);
        endif;
    endif
enddefine;

;;;
;;; Wrap the kernel k_proc with the wrappers in list wlist.
;;;
define wlist_to_closure( procedure k_proc, w_list, procedure ind_proc ) -> result;
    lvars procedure result = seqclose( k_proc, w_list );
    unless result == k_proc do
        procedure( proc, wrapper_kernel, wrapper_invoker );
            dlocal wrapper_kernel, wrapper_invoker;
            fast_apply( proc )
        endprocedure(% result, k_proc, ind_proc %) -> result;
        k_proc.pdprops -> result.pdprops;
        k_proc.pdnargs -> result.pdnargs;
    endunless;
enddefine;

endsection;

section;
sysprotect( "wrapper_kernel" );
sysprotect( "wrapper_invoker" );
endsection;


;;; -------------------------------------------------------------------------
;;; Modified, 19/9/02, sfkl
;;;     *   Rename seq_to_closure to wlist_to_closure (defensive).
;;;     *   Arrange for the wrapped procedure to bind wrapper_kernel
;;;         and wrapper_invoker.
;;;     *   Polishing: added compile_mode strict & removed superfluous
;;;         lvars declarations.
;;; -------------------------------------------------------------------------
;;; Modified, 2/7/93, sfk
;;;     *   Arranged for the result of seq_to_closure to have the same
;;;         pdprops and pdnargs as the ``innermost'' procedure.  This
;;;         simplifies nearly all of the uses of seq_to_closure.
;;; -------------------------------------------------------------------------
