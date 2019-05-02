#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/io.lsp
 | Purpose:         Common Lisp I/O functions
 | Author:          John Williams, May 29 1987 (see revisions)
 | Documentation:   CLtL, ch 21, 22, & 23
 | Related Files:   C.all/lisp/src/read.p, C.all/lisp/src/write.p
 |#

;;; Pathnames

(defun MAKE-PATHNAME (&key host device directory name type version
                           defaults (case :local))
    (sys:make-pathname host device directory name type version defaults case))


(defun PARSE-NAMESTRING (thing
                         &optional host (defaults *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
    (sys:parse-namestring thing host defaults start end junk-allowed))


(defun TRANSLATE-PATHNAME (source from-pathname to-pathname &key)
    (sys:translate-pathname source from-pathname to-pathname))


(defun PATHNAME-HOST (p &key (case :local))
    (sys:pathname-field (pathname p) case 1))


(defun PATHNAME-DEVICE (p &key (case :local))
    (sys:pathname-field (pathname p) case 2))


(defun PATHNAME-DIRECTORY (p &key (case :local))
    (sys:pathname-field (pathname p) case 3))


(defun PATHNAME-NAME (p &key (case :local))
    (sys:pathname-field (pathname p) case 4))


(defun PATHNAME-TYPE (p &key (case :local))
    (sys:pathname-field (pathname p) case 5))


(defun PATHNAME-VERSION (p)
    (sys:pathname-field (pathname p) :local 6))


;;; Streams

(defun MAKE-STRING-OUTPUT-STREAM (&key element-type)
    (sys:make-string-output-stream nil))


(defun STREAM-EXTERNAL-FORMAT (stream)
    :default)


(defun OPEN (filename
                &key (direction :input)
                     (element-type 'character)
                     (if-exists :new-version)
                     (if-does-not-exist
                         (if (or (eq direction :input)
                                 (eq if-exists :append)
                                 (eq if-exists :overwrite))
                             :error
                             (if (eq direction :probe) nil :create)))
                     (external-format :default))
    (unless (eq external-format :default)
         (cerror "Carry on using the default format"
                 "Unrecognised :EXTERNAL-FORMAT option ~S"
                 external-format))
    (sys:open filename direction element-type if-exists if-does-not-exist))


(defun CLOSE (stream &key abort)
    (sys:close stream))


(defun DELETE-FILE (file)
    (if (wild-pathname-p file)
        (progn
            (cerror "Delete all matching files"
                'file-error
                :message "Wild pathname given to DELETE-FILE"
                :involving (list file)
                :pathname file)
            (let ((files (directory file)))
                (dolist (f files files) (sys:delete-file f))))
        (sys:delete-file file)))


(defun RENAME-FILE (file new-name)
    (if (wild-pathname-p file)
        (progn
            (cerror "Re-name all matching files"
                'file-error
                :message "Wild pathname given to RENAME-FILE"
                :involving (list file)
                :pathname file)
            (let ((files (directory file)) r1 r2 r3)
                (dolist (f files (values (sys:nreverse-list r1)
                                         (sys:nreverse-list r2)
                                         (sys:nreverse-list r3)))
                    (multiple-value-bind
                      (x y z)
                      (sys:rename-file f (translate-pathname f file new-name))
                        (push x r1)
                        (push y r2)
                        (push z r3)))))
        (sys:rename-file file (merge-pathnames new-name file))))


(defvar *LOAD-PATHNAME* nil)

(defvar *LOAD-TRUENAME* nil)

(export '(*LOAD-PATHNAME* *LOAD-TRUENAME*))


(defun LOAD (file &key ((:verbose *load-verbose*)       *load-verbose*)
                       ((:print   *load-print*)         *load-print*)
                       ((:lock    poplog:*load-lock*)   poplog:*load-lock*)
                       (if-does-not-exist t)
                  &aux *load-pathname*)

    (setq *load-pathname*
          (if (streamp file)
              (and (typep file 'file-stream) (pathname file))
              (merge-pathnames file)))

    (labels ((do-load (file)
                (let ((*load-truename* (and *load-pathname*
                                            (probe-file *load-pathname*))))
                    (if (streamp file)
                        (sys:load file)
                        (if *load-truename*
                            (sys:load *load-truename*)
                            (if if-does-not-exist
                                (error 'file-error
                                        :message "File not found"
                                        :involving (list *load-pathname*)
                                        :pathname *load-pathname*)
                                nil))))))

        (if (wild-pathname-p *load-pathname*)
            (let ((files (directory *load-pathname*)))
                (if files
                    (dolist (*load-pathname* files t)
                        (do-load *load-pathname*))
                    (if if-does-not-exist
                        (error 'file-error
                            :message "No matching files: ~S"
                            :involving (list file)
                            :pathname *load-pathname*))))
            (do-load file))))


(defun SYS:FUNCALL-WITH-STANDARD-IO-SYNTAX (fn &aux
            (*package*              #.(find-package "COMMON-LISP-USER"))
            (*print-array*          t)
            (*print-base*           10)
            (*print-case*           :upcase)
            (*print-circle*         nil)
            (*print-escape*         t)
            (*print-gensym*         t)
            (*print-length*         nil)
            (*print-level*          nil)
            (*print-lines*          nil)
            (*print-miser-width*    nil)
            (*print-pprint-dispatch*  nil)
            (*print-pretty*         nil)
            (*print-radix*          nil)
            (*print-readably*       t)
            (*print-right-margin*   nil)
            (*read-base*            10)
            (*read-default-float-format* 'single-float)
            (*read-eval*            t)
            (*read-suppress*        nil)
            (*readtable*            #.*readtable*)
            (poplog:*read-prompt*   "== ")
           )
        (funcall fn))


(defmacro WITH-STANDARD-IO-SYNTAX (&body body)
    `(sys:funcall-with-standard-io-syntax #'(lambda () ,@body)))


(defun READ-FROM-STRING (string &optional eof-error-p eof-value
                                &key (start 0) end preserve-whitespace
                                &aux stream)
    (setq stream (make-string-input-stream string start end))
    (values
        (if preserve-whitespace
            (read-preserving-whitespace stream eof-error-p eof-value)
            (read stream eof-error-p eof-value))
        (sys:string-input-stream-index stream)))


(defun PARSE-INTEGER (string &key start end (radix 10) junk-allowed)
    (sys:parse-integer string start end radix junk-allowed))


;;; Temporary read macro #k for making definitions of WRITE etc. easier
;;;     #k :circle
;;; expands to ((:circle *print-circle*) *print-circle)

(set-dispatch-macro-character #\# #\k
    #'(lambda (stream char sub-char)
        (let ((keyword (read stream t)))
            (let ((var (intern (concatenate 'string
                                "*PRINT-" (symbol-name keyword) "*"))))
                `((,keyword ,var) ,var)))))


(defun WRITE (object
              &key
              ((:stream *standard-output*) *standard-output*)
                #k :escape      #k :radix
                #k :base        #k :circle
                #k :pretty      #k :level
                #k :length      #k :case
                #k :gensym      #k :array
                #k :readably    #k :lines
                #k :right-margin
                #k :miser-width
                #k :pprint-dispatch
              )
    (sys:write object *standard-output* nil nil))


(defun WRITE-TO-STRING (object
                        &key
                         #k :escape      #k :radix
                         #k :base        #k :circle
                         #k :pretty      #k :level
                         #k :length      #k :case
                         #k :gensym      #k :array
                         #k :readably    #k :lines
                         #k :right-margin
                         #k :miser-width
                         #k :pprint-dispatch

                        )
    (let ((*standard-output* (sys:make-string-output-stream nil)))
        (sys:write object *standard-output* nil nil)
        (get-output-stream-string *standard-output*)))


(set-dispatch-macro-character #\# #\k nil)


(defun WRITE-STRING (string &optional stream &key start end)
    (sys:write-string string stream start end nil))


(defun WRITE-LINE (string &optional stream &key start end)
    (sys:write-string string stream start end t))


(defun PRIN1 (object &optional stream &aux (*print-escape* t))
    (sys:write object stream nil nil))


(defun PRINC (object &optional stream &aux (*print-escape* nil))
    (sys:write object stream nil nil))


(defun PRINT (object &optional stream &aux (*print-escape* t))
    (sys:write object stream #\Newline #\Space))


(defun PPRINT (object &optional stream
                        &aux (*print-escape* t) (*print-pretty* t))
    (sys:write object stream #\Newline nil)
    (values))


(defun PRIN1-TO-STRING (item &aux (stream (sys:make-string-output-stream nil)))
    (prin1 item stream)
    (get-output-stream-string stream))


(defun PRINC-TO-STRING (item &aux (stream (sys:make-string-output-stream nil)))
    (princ item stream)
    (get-output-stream-string stream))


(defun TERPRI (&optional stream)
    (write-char #\Newline stream)
    nil)


(defmacro FORMATTER (string)
    `#'(lambda (#1=#:stream &rest #2=#:args)
        (nthcdr
            (sys:call-format-print #1# ,(sys:call-format-compile string) #2#)
            #2#)))


(defmacro PRINT-UNREADABLE-OBJECT ((object stream &key type identity)
                                   &body #(body decs))
    `(let ((#1=#:object ,object) (#2=#:stream ,stream))
        ,@decs
        (if *print-readably*
            (let ((*print-readably* nil))
                (error "Print not readable" #1#)))
        (write-char #\# #2#)
        (if ,type
            (sys:write (type-of #1#) #2# #\< #\Space)
            (write-char #\< #2#))
        ,@body
        (if ,identity
            (sys:write (sys:address-of #1#) #2# ,(and body #\Space) #\>)
            (write-char #\> #2#))
        nil))


(defmacro WITH-OPEN-FILE ((stream file &rest options) &body #(forms decs))
    `(let (,stream)
        ,@decs
        (unwind-protect
            (progn
                (setq ,stream (open ,file ,@options))
                ,@forms)
            (if (streamp ,stream) (close ,stream)))))


(defmacro WITH-OPEN-STREAM ((var stream) &body #(forms decs))
    `(let (,var)
        ,@decs
        (unwind-protect
            (progn
                (setq ,var ,stream)
                ,@forms)
            (if (streamp ,var)  (close ,var)))))


(defmacro WITH-INPUT-FROM-STRING ((var string &key (start 0) end index)
     &body #(forms decs))
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
        ,@decs
        ,@(if index
            `((prog1
                 (progn ,@forms)
                 (setf ,index (sys:string-input-stream-index ,var))))
            forms)))


(defmacro WITH-OUTPUT-TO-STRING ((var &optional string &key element-type)
                                 &body forms)
    `(let ((,var (sys:make-string-output-stream ,string)))
        ;; declarations come out ok here
        ,@forms
        ,@(unless string `((get-output-stream-string ,var)))))



(defun SYS:ASK-USER (string args yes no &aux poplog:*read-prompt* reply)
    (if string
        (fresh-line *query-io*)
        (setq string ""))
    (setq poplog:*read-prompt* (format nil "~? (~A/~A) " string args yes no))
    (loop
        (clear-input *query-io*)
        (setq reply (string-trim #.(coerce '(#\Space #\Tab) 'string)
                (read-line *query-io* t)))
        (if (string-equal reply yes)
            (return t))
        (if (string-equal reply no)
            (return nil))
        (unless (string= reply "")
            (format *query-io* "; Invalid response: ~S~%" reply))))


(defun Y-OR-N-P (&optional string &rest args)
    (sys:ask-user string args "y" "n"))


(defun YES-OR-NO-P (&optional string &rest args)
    (sys:vedscreenbell)
    (sys:ask-user string args "yes" "no"))



#| --- Revision History ---------------------------------------------------
--- John Williams, May 25 1995
        Fixed LOAD to cope with streams not of type file-stream.
--- John Williams, May 16 1995
        WITH-STANDARD-IO-SYNTAX now uses a function rather than an inline
        LET (for smaller code).
--- John Williams, May  1 1995
        RENAME-FILE now works for wild pathname.
--- John Williams, Apr 12 1995
        Changes for Steele 1990 pathnames.
--- John Williams, Apr  3 1995
        *PRINT-READABLY* now supported. Added macros PRINT-UNREADABLE-OBJECT
        and WITH-STANDARD-IO-SYNTAX.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Nov  4 1994
        Upgraded LOAD to Steele 1990 (p657-9).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun  7 1994
        Changes for POPLOG package.
 |#
