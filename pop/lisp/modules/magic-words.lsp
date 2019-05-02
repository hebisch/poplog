#| --- Copyright University of Sussex 1992. All rights reserved. ----------
 | File:            C.all/lisp/modules/magic-words.lsp
 | Purpose:         Make Lisp 'magic word' facility accessible to users
 | Author:          John Williams, May 11 1988
 | Documentation:   HELP * MAGIC-WORDS
 | Related Files:   C.all/lisp/src/magic-words.p
 |#

(cl:provide :magic-words)

(cl:in-package :poplog)

(export '(magic-word-handler
          define-magic-word
          cancel-magic-word
          list-magic-words))

(pop11)


section $-lisp;

lispsynonym(@MAGIC-WORD-HANDLER, "magic_word_handler");

endsection;


lisp

(in-package :poplog)

(setq *constant-functions* t)


(defmacro DEFINE-MAGIC-WORD (magic-word &body (body decs doc))
    `(progn
        (setf (gethash ',magic-word magic-word-handler)
                        #'(lambda () ,@decs (block ,magic-word ,@body))
            (documentation ',magic-word 'magic-word)
                        ',doc)
        ',magic-word))


(defun CANCEL-MAGIC-WORD (magic-word)
    (setf (documentation magic-word 'magic-word) nil)
    (remhash magic-word magic-word-handler))


(defun LIST-MAGIC-WORDS (&aux list)
    (maphash #'(lambda (item value) (push item list)) magic-word-handler)
    list)
