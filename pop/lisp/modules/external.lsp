#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/external.lsp
 | Purpose:         Clisp interface to -external_load-
 | Author:          Aled Morris & John Williams, August 1986 (see revisions)
 | Documentation:   HELP * EXTERNAL
 | Related Files:
 |#

(cl:provide :external)

(cl:in-package :poplog)

(export '(external-load-files
          external-unload-files
          external-show
          external-call
          external-call-float
          external-procedure-p
          live-external-procedure-p))

(pop11)


section $-lisp;

lisp_compile_mode;


define external_load_files(files, symlist, load_id, nbytes) -> load_id;
    lvars item;
    [% for item in_cl_list symlist do
        if islistlength(item, 2) then
            check_name(fast_front(fast_back(item)), 'external procedure');
            conspair(get_simple_string(fast_front(item)), fast_back(item))
        elseif isvector(item) and datalength(item) == 2 then
            consvector(sym_to_word(fast_subscrv(1, item)),
                       sym_to_word(fast_subscrv(2, item)), 2)
        else
            mishap(item, 1, 'Malformed symbol list')
        endif
    endfor %] -> symlist;

    external_load(load_id,
                  checkr_filenames(files),
                  symlist,
                  if nbytes /== nil then check_positive(nbytes) endif);
enddefine;


define external_unload_files(load_id) -> load_id;
    if external_load_count() == 0 then
        mishap(0, 'No files are externally loaded');
    endif;
    defaults load_id external_load_mark(1);
    external_unload(load_id)
enddefine;


define external_call(extpdr, args);
    external_apply(destlist(args), true, extpdr)
enddefine;


define external_call_float(extpdr, args);
    external_apply(destlist(args), "ddecimal", extpdr)
enddefine;


lisp_export(external_load_files,       @SYS:EXTERNAL-LOAD-FILES,   [4 4 1]);
lisp_export(external_unload_files,     @EXTERNAL-UNLOAD-FILES,     [0 1 1]);
lisp_export(external_show,             @EXTERNAL-SHOW,             [0 0 0]);
lisp_export(external_call,             @EXTERNAL-CALL,             [1 ? 1]);
lisp_export(external_call_float,       @EXTERNAL-CALL-FLOAT,       [1 ? 1]);
lisp_export(isexternal_procedure,      @EXTERNAL-PROCEDURE-P,      "boolean");
lisp_export(islive_external_procedure, @LIVE-EXTERNAL-PROCEDURE-P, "boolean");


endsection;


/* Now define EXTERNAL-LOAD-FILES */

lisp

(in-package :poplog)

(setq *constant-functions* t)


(defun EXTERNAL-LOAD-FILES
        (files symbol-list
            &key
         nbytes (label (symbol-name (gensym "EXTERNAL-LOAD-"))))
    (sys:external-load-files files symbol-list label nbytes))


#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 |#
