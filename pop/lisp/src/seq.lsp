#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/seq.lsp
 | Purpose:         Common Lisp sequence functions
 | Author:          Sam Valentine & John Williams, Oct 16 1986 (see revisions)
 | Documentation:   CLtL, ch 14.
 | Related Files:   C.all/lisp/src/seq.p
 |#

#| First we have the sequence-handling primitives. |#

(let ((*package* (find-package :system)))
    (defstruct
        (sys:gen-seq (:constructor sys:cons-gen-seq (index seq))
                     (:type vector)
                     (:predicate nil))
        "if index is nil we have a list, otherwise a vector starting at index"
        index seq))

(proclaim '(inline sys:gen-seq-index sys:gen-seq-seq))

(defun sys:create-gen-seq (x)
    (if (listp x)
        (sys:cons-gen-seq nil x)
        (sys:cons-gen-seq 0 x)))

(defun sys:seq-cdr (x)
    (progn
        (if (sys:gen-seq-index x)
            ;its a vector
            (incf (sys:gen-seq-index x))
            ; its a list
            (setf (sys:gen-seq-seq x) (cdr (sys:gen-seq-seq x))))
        x))

(defun sys:seq-nthcdr (n x)
    (progn
        (if (sys:gen-seq-index x)
            ;its a vector
            (incf (sys:gen-seq-index x) n)
            ; its a list
            (setf (sys:gen-seq-seq x) (nthcdr n (sys:gen-seq-seq x))))
        x))

(defun sys:seq-pop (x)
    (if (sys:gen-seq-index x)
        ; its a vector
        (prog1
            (elt (sys:gen-seq-seq x) (sys:gen-seq-index x))
            (incf (sys:gen-seq-index x)))
        ; its a list
        (pop (sys:gen-seq-seq x))))


;;; Now the Common Lisp sequence functions (see also C.all/lisp/src/seq.p)


(defun MAKE-SEQUENCE (type size &key initial-element)
    (sys:make-sequence type size initial-element))


(defun sys:map-into (build result fn seqs &aux len)
    (declare (optimize speed))
              ;;; For faster loops
    (setq
        len (if seqs
                (apply #'min (sys:map-1-list #'length seqs))
                most-positive-fixnum)
        seqs (sys:map-1-list #'sys:create-gen-seq seqs))
    (if build
        (if result (setq result (sys:make-sequence result len nil)))
        (if (array-has-fill-pointer-p result)
            (setf len (min len (array-total-size result))
                  (fill-pointer result) len)
            (setq len (min len (length result)))))
    (if result
        (if (listp result)
            (let ((res result))
                (dotimes (i len result)
                    (setf (car res)
                        (apply fn (sys:map-1-list #'sys:seq-pop seqs)))
                    (setq res (cdr res))))
            (dotimes (i len result)
                (setf (elt result i)
                    (apply fn (sys:map-1-list #'sys:seq-pop seqs)))))
        (dotimes (i len nil)
            (apply fn (sys:map-1-list #'sys:seq-pop seqs)))))


(defun MAP (result fn seq &rest seqs)
    (sys:map-into t result fn (cons seq seqs)))


(defun MAP-INTO (result fn &rest seqs)
    (and result (sys:map-into nil result fn seqs)))


(defun SOME (pred seq &rest seqs)
    (setq seqs (cons seq seqs))
    (do (   (len (apply #'min (sys:map-1-list #'length seqs)))
            (i 0 (1+ i))
            (val nil (apply pred (sys:map-1-list #'sys:seq-pop seqs)))
            (seqs (sys:map-1-list #'sys:create-gen-seq seqs)))
        ((if val t (>= i len)) val) ))


(defun EVERY (pred seq &rest seqs)
    (setq seqs (cons seq seqs))
    (do (   (len (apply #'min (sys:map-1-list #'length seqs)))
            (i 0 (1+ i))
            (val t (apply pred (sys:map-1-list #'sys:seq-pop seqs)))
            (seqs (sys:map-1-list #'sys:create-gen-seq seqs) ))
        ((if val (>= i len) t) val) ))


(setf (symbol-function 'NOTANY) (complement #'some))


(setf (symbol-function 'NOTEVERY) (complement #'every))


(defun REDUCE (fn seq &key from-end (start 0) end
                           (initial-value nil i-supp) (key #'identity))
    (unless end (setq end (length seq)))
    (if (eq start end)
        (return-from reduce (if i-supp initial-value (values (funcall fn)))))
    (if from-end
        (if (vectorp seq)
            ;; reducing a vector from the right
            (progn
                (unless i-supp
                    (setq end (1- end)
                        initial-value (funcall key (elt seq end))))
                (do ((j (1- end) (1- j)))
                    ((< j start) initial-value)
                    (setq initial-value (funcall fn (funcall key (elt seq j))
                                                    initial-value))))
            ;; reducing a list from the right
            (if i-supp
                (labels
                    ((right-reduce-list-i-v (l len i &aux r)
                        (if (zerop len)
                            i
                            (progn
                                (setq r (right-reduce-list-i-v (cdr l) (1- len) i))
                                (values
                                    (funcall fn (funcall key (car l)) r))))))
                    (right-reduce-list-i-v
                        (nthcdr start seq) (- end start) initial-value))
                (labels
                    ((right-reduce-list (l len &aux r)
                        (if (eq len 1)
                            (values (funcall key (car l)))
                            (progn
                                (setq r (right-reduce-list (cdr l) (1- len)))
                                (values
                                    (funcall fn (funcall key (car l)) r))))))
                    (right-reduce-list
                        (nthcdr start seq) (- end start)))))
        (if (vectorp seq)
            ;; reducing a vector from the left
            (progn
                (unless i-supp
                    (setq initial-value (funcall key (elt seq start))
                            start (1+ start)))
                (do ((j start (1+ j)))
                    ((>= j end) initial-value)
                    (setq initial-value (funcall fn initial-value
                                                (funcall key (elt seq j))))))
            ;; reducing a list from the left
            (progn
                (setq seq (nthcdr start seq) end (- end start))
                (unless i-supp
                    (setq initial-value (funcall key (car seq))
                            seq (cdr seq) end (1- end)))
                (dotimes (i end initial-value)
                    (setq initial-value (funcall fn initial-value
                                                (funcall key (pop seq)))))))))


(defun FILL (seq item &key (start 0) end)
    (unless end (setq end (length seq)))
    (if (vectorp seq)
        (do ((i start (1+ i)))
            ((>= i end) seq)
            (setf (elt seq i) item))
        (do ((list (nthcdr start seq) (cdr list))
             (i start (1+ i)))
            ((>= i end) seq)
            (setf (car list) item))))


(defun REPLACE (seq1 seq2 &key (start1 0) end1 (start2 0) end2 &aux len)
    (unless end1 (setq end1 (length seq1)))
    (unless end2 (setq end2 (length seq2)))
    (setq
        len (min (- end1 start1) (- end2 start2))
        end1 (+ start1 len)
        end2 (+ start2 len))
    (if (eq seq1 seq2)
        (if (< start2 start1 end2)
            (setq seq2 (subseq seq2 start2 end2)
                start2 0
                end2 len)))
    (if (vectorp seq2)
        (if (vectorp seq1)
            ;; vector replaced using vector
            (do (   (i1 start1 (1+ i1))
                    (i2 start2 (1+ i2)))
                ((>= i1 end1) seq1)
                (setf (elt seq1 i1) (elt seq2 i2)))
            ;; list replaced using vector
            (do (   (lis1 (nthcdr start1 seq1) (cdr lis1))
                    (i2 start2 (1+ i2)))
                ((>= i2 end2) seq1)
                (setf (car lis1) (elt seq2 i2))))
        (if (vectorp seq1)
            ;; vector replaced using list
            (do ((i1 start1 (1+ i1))
                    (lis2 (nthcdr start2 seq2) (cdr lis2)))
                ((>= i1 end1) seq1)
                (setf (elt seq1 i1) (car lis2)))
            ;; list replaced using list
            (do ((lis1 (nthcdr start1 seq1) (cdr lis1))
                    (lis2 (nthcdr start2 seq2) (cdr lis2))
                    (i 0 (1+ i)))
                ((>= i len) seq1)
                (setf (car lis1) (car lis2)) ) )) )


; Macro for generating code to check :COUNT and :FROM-END parameters
; Assumes variable names "count", "from-end", "seq"

(defmacro SYS:CHECK-COUNT (block-name)
    `(if count
        (progn
            (unless (typep count 'fixnum)
                    (error 'simple-type-error
                        :format-string "Bad value for :COUNT parameter: ~S"
                        :format-arguments (list count)
                        :datum count
                        :expected-type 'fixnum))
            (unless (sys:fix> count 0)
                    (return-from ,block-name seq)))
        (setq count -1 from-end nil)))


(defun REMOVE (item seq &key from-end test test-not start end count (key #'identity))
    (sys:check-count remove)
    (sys:remove-if
        (sys:choose-seq-test1 item test test-not)
        seq from-end start end count key nil))


(defun REMOVE-IF (pred seq &key from-end start end count (key #'identity))
    (sys:check-count remove-if)
    (sys:remove-if pred seq from-end start end count key nil))


(defun REMOVE-IF-NOT (pred seq &key from-end start end count (key #'identity))
    (sys:check-count remove-if-not)
    (sys:remove-if
        (complement pred)
        seq from-end start end count key nil))


(defun DELETE (item seq &key from-end test test-not start end count (key #'identity))
    (sys:check-count delete)
    (sys:remove-if
        (sys:choose-seq-test1 item test test-not)
        seq from-end start end count key t))


(defun DELETE-IF (pred seq &key from-end start end count (key #'identity))
    (sys:check-count delete-if)
    (sys:remove-if pred seq from-end start end count key t))


(defun DELETE-IF-NOT (pred seq &key from-end start end count (key #'identity))
    (sys:check-count delete-if-not)
    (sys:remove-if
        (complement pred)
        seq from-end start end count key t))


(defun REMOVE-DUPLICATES (seq &key from-end test test-not start end
                                    (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (sys:remove-duplicates seq start end from-end test key nil))


(defun DELETE-DUPLICATES (seq &key from-end test test-not start end
                                    (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (sys:remove-duplicates seq start end from-end test key t))


(defun sys:substitute-if (new pred seq from-end start end count key)
    (unless end (setq end (length seq)))
    (if (vectorp seq)
        (if from-end
            ;; vector from the right
            (do (   (seq (copy-seq seq))
                    (i (1- end) (1- i)))
                ((or (< i start) (zerop count)) seq)
                (if (funcall pred (funcall key (elt seq i)))
                    (setf (elt seq i) new
                        count (1- count))))
            ;; vector from the left
            (do (   (seq (copy-seq seq))
                    (i start (1+ i)))
                ((or (>= i end) (zerop count)) seq)
                (if (funcall pred (funcall key (elt seq i)))
                    (setf (elt seq i) new
                        count (1- count)))))
        (if from-end
            ;; list from the right
            (labels ((substitute-list-from-end (lis start end)
                        ;; returned values are lis and remaining count
                        (if (zerop end) (values lis count)
                            (multiple-value-bind (reslis newcount)
                                (substitute-list-from-end (cdr lis) (1- start) (1- end))
                                (values (cons
                                        (cond ((zerop newcount) (car lis))
                                            ((plusp start) (car lis))
                                            ((funcall pred (funcall key (car lis)))
                                                (progn (setq newcount (1- newcount)) new))
                                            (t (car lis)))
                                        reslis) newcount)))))
                (values (substitute-list-from-end seq start end )))
            ;; list from the left
            (labels ((substitute-list-from-front (lis start end)
                        ;; returned value is list
                        (if (zerop end) lis
                            (if (zerop count) lis
                                (cons (cond
                                        ((plusp start) (car lis))
                                        ((funcall pred (funcall key (car lis)))
                                            (progn (setq count (1- count)) new))
                                        (t (car lis)))
                                    (substitute-list-from-front
                                        (cdr lis) (1- start) (1- end)))))))
                (substitute-list-from-front seq start end )))))


(defun SUBSTITUTE-IF (new pred seq &key from-end (start 0) end
                                        count (key #'identity))
    (sys:check-count substitute-if)
    (sys:substitute-if
        new pred seq
        from-end start end count key))


(defun SUBSTITUTE-IF-NOT (new pred seq &key from-end (start 0) end
                                            count (key #'identity))
    (sys:check-count substitute-if-not)
    (sys:substitute-if
        new (complement pred) seq
        from-end start end count key))


(defun SUBSTITUTE (new old seq &key from-end test test-not (start 0) end
                                    count (key #'identity))
    (sys:check-count substitute)
    (sys:substitute-if
        new (sys:choose-seq-test1 old test test-not) seq
        from-end start end count key))


(defun sys:nsubstitute-if (new pred seq from-end start end count key)
    (unless end (setq end (length seq)))
    (if (vectorp seq)
        (if from-end
            ;; vector from the right
            (do ((i (1- end) (1- i)))
                ((or (< i start) (zerop count)) seq)
                (if (funcall pred (funcall key (elt seq i)))
                    (setf (elt seq i) new
                        count (1- count))))
            ;; vector from the left
            (do ((i start (1+ i)))
                ((or (>= i end) (zerop count)) seq)
                (if (funcall pred (funcall key (elt seq i)))
                    (setf (elt seq i) new
                        count (1- count)))))
        (if from-end
            ;; list from the right
            (labels ((nsubstitute-list-from-end (lis len)
                        ;; returned value is remaining count
                        (if (zerop len) count
                            (let ((count (nsubstitute-list-from-end
                                            (cdr lis) (1- len) )))
                                (if (zerop count) 0
                                    (if (funcall pred (funcall key (car lis)))
                                        (progn (setf (car lis) new)
                                            (1- count))
                                        count))))))
                (nsubstitute-list-from-end (nthcdr start seq) (- end start))
                seq)
            ;; list from the left
            (do (   (lis (nthcdr start seq) (cdr lis))
                    (i start (1+ i)))
                ((or (>= i end) (zerop count)) seq)
                (if (funcall pred (funcall key (car lis)))
                    (setf (car lis) new
                        count (1- count)))))))


(defun NSUBSTITUTE-IF (new pred seq &key from-end (start 0) end
                                         count (key #'identity))
    (sys:check-count nsubstitute-if)
    (sys:nsubstitute-if
        new pred seq
        from-end start end count key))


(defun NSUBSTITUTE-IF-NOT (new pred seq &key from-end (start 0) end
                                             count (key #'identity))
    (sys:check-count nsubstitute-if-not)
    (sys:nsubstitute-if
        new (complement pred) seq
        from-end start end count key))


(defun NSUBSTITUTE (new old seq &key from-end test test-not (start 0) end
                                     count (key #'identity))
    (sys:check-count nsubstitute)
    (sys:nsubstitute-if
        new (sys:choose-seq-test1 old test test-not) seq
        from-end start end count key))


(defun sys:find-if (pred seq from-end start end key &aux item)
    (if (vectorp seq)
        (progn
            (unless end (setq end (length seq)))
            (if from-end
                ;; vector from the right
                (do* ((i (1- end) (1- i)))
                    (nil (values nil nil))
                    (if (< i start)
                        (return (values nil nil))
                        (if (funcall pred (funcall key (setq item (elt seq i))))
                            (return (values item i)))))
                ;; vector from the left
                (do* ((i start (1+ i)))
                    (nil (values nil nil))
                    (if (>= i end)
                        (return (values nil nil))
                        (if (funcall pred (funcall key (setq item (elt seq i))))
                            (return (values item i)))))))
        (progn
            (unless end (setq end most-positive-fixnum))
            (if from-end
                ;; list from the right
                ;;  - work from the left anyway, but select the last
                (do* ((i start (1+ i))
                      (list (nthcdr start seq) (cdr list))
                      (result nil)
                      (position nil))
                     (nil (values nil nil))
                     (if (or (endp list) (>= i end))
                         (return (values result position)))
                         (if (funcall pred (funcall key (setq item (car list))))
                             (setq position i result item)))
                ;; list from the left
                (do* ((i start (1+ i))
                      (list (nthcdr start seq) (cdr list)))
                     (nil (values nil nil))
                     (if (or (endp list) (>= i end))
                         (return (values nil nil)))
                     (if (funcall pred (funcall key (setq item (car list))))
                         (return (values item i))))))))


(defun FIND-IF (pred seq &key from-end (start 0) end (key #'identity))
    (nth-value 0 (sys:find-if pred seq from-end start end key)))


(defun FIND-IF-NOT (pred seq &key from-end (start 0) end (key #'identity))
    (nth-value 0 (sys:find-if (complement pred) seq from-end start end key)))


(defun FIND (item seq &key from-end (start 0) end (key #'identity)
                            test test-not)
    (nth-value 0
        (sys:find-if
            (sys:choose-seq-test1 item test test-not)
            seq from-end start end key)))


(defun POSITION-IF (pred seq &key from-end (start 0) end (key #'identity))
    (nth-value 1 (sys:find-if pred seq from-end start end key)))


(defun POSITION-IF-NOT (pred seq &key from-end (start 0) end (key #'identity))
    (nth-value 1 (sys:find-if (complement pred) seq from-end start end key)))


(defun POSITION (item seq &key from-end (start 0) end (key #'identity)
                                test test-not)
    (nth-value 1
        (sys:find-if
            (sys:choose-seq-test1 item test test-not)
            seq from-end start end key)))


(defun sys:count-if (pred seq from-end start end key)
    (if (vectorp seq)
        ;; vector
        (progn
            (unless end (setq end (length seq)))
            (do ((i start (1+ i))
                 (count 0))
                ((>= i end) count)
                (if (funcall pred (funcall key (elt seq i)))
                    (setq count (1+ count)))))
        ;; list
        (progn
            (unless end (setq end most-positive-fixnum))
            (do ((i start (1+ i))
                 (count 0)
                 (list (nthcdr start seq) (cdr list)))
                ((or (endp list) (>= i end)) count)
                (if (funcall pred (funcall key (car list)))
                    (setq count (1+ count)))))))


(defun COUNT-IF (pred seq &key from-end (start 0) end (key #'identity))
    (sys:count-if pred seq from-end start end key))


(defun COUNT-IF-NOT (pred seq &key from-end (start 0) end (key #'identity))
    (sys:count-if (complement pred) seq from-end start end key))


(defun COUNT (item seq &key test test-not from-end (start 0) end (key #'identity))
    (sys:count-if
        (sys:choose-seq-test1 item test test-not) seq from-end start end key))


(defun MISMATCH (seq1 seq2 &key from-end test test-not (key #'identity)
                                (start1 0) (start2 0) end1 end2)
    (unless end1 (setq end1 (length seq1)))
    (unless end2 (setq end2 (length seq2)))
    (setq test (sys:choose-seq-test2 test test-not))
    (if from-end
        ;; from the right
        (let* ( (len1 (- end1 start1))
                (len2 (- end2 start2))
                (l (min len1 len2))
                (st1 (- end1 l))
                (st2 (- end2 l)))
            (do (   (answer (if (/= len1 len2) st1))
                    (s1 (sys:seq-nthcdr st1 (sys:create-gen-seq seq1)))
                    (s2 (sys:seq-nthcdr st2 (sys:create-gen-seq seq2)))
                    (i st1 (1+ i))
                    (j st2 (1+ j)))
                ((>= i end1) answer)
                (unless (funcall test
                            (funcall key (sys:seq-pop s1))
                            (funcall key (sys:seq-pop s2)))
                        (setq answer (1+ i)))))
        ;; from the left
        (do (   (s1 (sys:seq-nthcdr start1 (sys:create-gen-seq seq1)))
                (s2 (sys:seq-nthcdr start2 (sys:create-gen-seq seq2)))
                (i start1 (1+ i))
                (j start2 (1+ j)))
            (nil)
            (cond ((>= i end1) (return (if (/= j end2) i)))
                ((>= j end2) (return i))
                ((funcall test
                        (funcall key (sys:seq-pop s1))
                        (funcall key (sys:seq-pop s2))) nil)
                (t (return i))  ))))


(defun SEARCH (seq1 seq2 &key from-end test test-not (key #'identity)
                              (start1 0) (start2 0) end1 end2)
    (unless end1 (setq end1 (length seq1)))
    (unless end2 (setq end2 (length seq2)))
    (setq test (sys:choose-seq-test2 test test-not))
    (let ((lastj (- end2 (- end1 start1))))
        (if from-end
            ;; from the right
            (do (   (answer nil)
                    (s1 (sys:seq-nthcdr start1 (sys:create-gen-seq seq1)))
                    (s2 (sys:seq-nthcdr start2 (sys:create-gen-seq seq2))
                        (sys:seq-cdr s2))
                    (j start2 (1+ j)))
                ((> j lastj) answer)
                (do (   (finish nil)
                        (ss1 (sys:copy-gen-seq s1))
                        (ss2 (sys:copy-gen-seq s2))
                        (ii start1 (1+ ii))
                        (jj j (1+ jj)))
                    (finish)
                    (if (>= ii end1)
                        (setq answer j finish t)
                        (setq finish (not (funcall test
                                    (funcall key (sys:seq-pop ss1))
                                    (funcall key (sys:seq-pop ss2))))) )))
            ;; from the left
            (do (   (answer nil)
                    (s1 (sys:seq-nthcdr start1 (sys:create-gen-seq seq1)))
                    (s2 (sys:seq-nthcdr start2 (sys:create-gen-seq seq2))
                        (sys:seq-cdr s2))
                    (j start2 (1+ j)))
                ((or answer (> j lastj)) answer)
                (do (   (finish nil)
                        (ss1 (sys:copy-gen-seq s1))
                        (ss2 (sys:copy-gen-seq s2))
                        (ii start1 (1+ ii))
                        (jj j (1+ jj)))
                    (finish)
                    (if (>= ii end1)
                        (setq answer j finish t)
                        (setq finish (not (funcall test
                                    (funcall key (sys:seq-pop ss1))
                                    (funcall key (sys:seq-pop ss2))))) ))) )))


(defun SORT (seq pred &key key)
    (sys:sort seq pred key nil))


(defun STABLE-SORT (seq pred &key key)
    (sys:sort seq pred key t))


(defun MERGE (result-type seq1 seq2 pred &key (key #'identity)
                &aux
                (len1 (length seq1))
                (len2 (length seq2))
                (len (+ len1 len2))
                (result (sys:make-sequence result-type len nil))
                (res result)
                (v (vectorp result))
                (seq1 (sys:create-gen-seq seq1))
                (seq2 (sys:create-gen-seq seq2))
                (absent1 t) (absent2 t)
                elt1 elt2 kelt1 kelt2)
    (dotimes (i len result)
        ;; prepare element of seq1
        (if (and absent1 (plusp len1))
            (setq absent1 nil
                elt1 (sys:seq-pop seq1)
                kelt1 (funcall key elt1)))
        ;; prepare element of seq2
        (if (and absent2 (plusp len2))
            (setq absent2 nil
                elt2 (sys:seq-pop seq2)
                kelt2 (funcall key elt2)))
        ;; compare
        (if (or absent1 (and (not absent2) (funcall pred kelt2 kelt1)))
            ;; select from seq2
            (progn
                (if v
                    (setf (elt result i) elt2)
                    (setf (car res) elt2 res (cdr res)))
                (setq len2 (1- len2) absent2 t))
            ;; select from seq1
            (progn
                (if v
                    (setf (elt result i) elt1)
                    (setf (car res) elt1 res (cdr res)))
                (setq len1 (1- len1) absent1 t)))))



#| --- Revision History ---------------------------------------------------
--- John Williams, May  5 1995
        SYS:CHOOSE-SEQ-TEST1 & SYS:CHOOSE-SEQ-TEST2 moved to lists.lsp
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        FIND, FIND-IF, FIND-IF-NOT, POSITION, POSITION-IF, and POSITION-IF-NOT
        now all use NTH-VALUE instead of MULTIPLE-VALUE-BIND.
--- John Williams, Aug 27 1993
        Upgraded to Steele 1990.
--- John Williams, Jul 28 1993
        SYS:SORT now takes extra "stable" flag.
--- John Williams, Oct 24 1991
        Fixed BR isl-fr.4381
--- John Williams, Apr 25 1988
        REDUCE re-written so nresults inferrable.
 |#
