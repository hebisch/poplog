#| --- Copyright University of Sussex 1996. All rights reserved. ----------
 | File:            C.all/lisp/src/lists.lsp
 | Purpose:         Common Lisp list functions
 | Author:          John Williams & Sam Valentine, Feb 26 1986 (see revisions)
 | Documentation:   CLtL, p262-281
 | Related Files:   C.all/lisp/src/lists.p
 |#


(proclaim '(inline car cdr (setf car) (setf cdr) caar cadr cdar cddr))


(defun SYS:CHOOSE-SEQ-TEST1 (target test test-not)
    (if test
        (if test-not
            (error "Both :TEST and :TEST-NOT options supplied" test test-not)
            #'(lambda (seq-element) (funcall test target seq-element)))
        (if test-not
            (complement #'(lambda (seq-element)
                            (funcall test-not target seq-element)))
            #'(lambda (seq-element) (eql target seq-element)))))


(defun SYS:CHOOSE-SEQ-TEST2 (test test-not)
    (if test
        (if test-not
            (error "Both :TEST and :TEST-NOT options supplied" test test-not)
            test)
        (if test-not
            (complement test-not)
            #'eql)))


;;; List functions

(defun TREE-EQUAL (x y &key test test-not)
    (setq test (sys:choose-seq-test2 test test-not))
    (labels
        ((do-tree-equal (x y)
            (if (consp x)
                (and (consp y)
                     (do-tree-equal (car x) (car y))
                     (do-tree-equal (cdr x) (cdr y)))
                (and (atom y) (funcall test x y) t))))
        (do-tree-equal x y)))


(defun LIST-LENGTH (list)
    (do ((n 0 (+ n 2))
         (fast list (cddr fast))
         (slow list (cdr slow)))
        (nil)
        (when (endp fast) (return n))
        (when (endp (cdr fast)) (return (1+ n)))
        (when (and (eq fast slow) (plusp n)) (return nil))))


(defun MAKE-LIST (size &key initial-element &aux list)
    (dotimes (i size list)
        (setq list (cons initial-element list))))


(defun REVAPPEND (x y)
    (dolist (i x y) (setq y (cons i y))))


(defun NRECONC (x y &aux temp)
    (loop
        (when (endp x) (return y))
        ;; use rotatef ??
        (setf temp (cdr x) (cdr x) y y x x temp)))


(defun LDIFF (list sublist)
    (do ((list list  (cdr list))
         (result nil (cons (car list) result)))
        ((or (endp list) (eq sublist list)) (sys:nreverse-list result))))


;;; Association lists

(defun ACONS (x y alist)
    (cons (cons x y) alist))


(defun PAIRLIS (keys data &optional alist)
    (dolist (key keys alist)
        (when (endp data) (return alist))
        (setq alist (acons key (car data) alist) data (cdr data))))


(defun SYS:ASSOC (item alist test)
    ;; used by SUBLIS and NSUBLIS as well as ASSOC itself
    (dolist (p alist nil)
        (and (consp p) (funcall test item (car p)) (return p))))


(defun ASSOC (item alist &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (if key
        (dolist (p alist nil)
            (and (consp p) (funcall test item (funcall key (car p)))
                 (return p)))
        (sys:assoc item alist test)))


(defun ASSOC-IF (pred alist &key key)
    (if key
        (dolist (p alist)
            (and (consp p) (funcall pred (funcall key (car p))) (return p)))
        (dolist (p alist)
            (and (consp p) (funcall pred (car p)) (return p)))))


(defun ASSOC-IF-NOT (pred alist &key key)
    (if key
        (dolist (p alist)
            (and (consp p)
                 (not (funcall pred (funcall key (car p)))) (return p)))
        (dolist (p alist)
            (and (consp p) (not (funcall pred (car p))) (return p)))))


(defun SYS:RASSOC (item alist test)
    (dolist (p alist nil)
        (and (consp p) (funcall test item (cdr p)) (return p))))


(defun SYS:RASSOC-WITH-KEY (item alist test key)
    (dolist (p alist nil)
        (and (consp p) (funcall test item (funcall key (cdr p))) (return p))))


(defun RASSOC (item alist &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (if key
        (sys:rassoc-with-key item alist test key)
        (sys:rassoc item alist test)))


(defun RASSOC-IF (pred alist &key key)
    (if key
        (dolist (p alist)
            (and (consp p) (funcall pred (funcall key (cdr p))) (return p)))
        (dolist (p alist)
            (and (consp p) (funcall pred (cdr p)) (return p)))))


(defun RASSOC-IF-NOT (pred alist &key key)
    (if key
        (dolist (p alist)
            (and (consp p)
                 (not (funcall pred (funcall key (cdr p)))) (return p)))
        (dolist (p alist)
            (and (consp p) (not (funcall pred (cdr p))) (return p)))))


;;; SUBST etc

(defun SYS:CONS-IF-NEW (a d tree)
    (if (and (eq a (car tree)) (eq d (cdr tree)))
        tree
        (cons a d)))


(defun SYS:SUBST-IF (new pred tree)
    (if (funcall pred tree)
        new
        (if (consp tree)
            (sys:cons-if-new
                (sys:subst-if new pred (car tree))
                (sys:subst-if new pred (cdr tree))
                tree)
            tree)))


(defun SYS:SUBST-IF-WITH-KEY (new pred tree key)
    (if (funcall pred (funcall key tree))
        new
        (if (consp tree)
            (sys:cons-if-new
                (sys:subst-if-with-key new pred (car tree) key)
                (sys:subst-if-with-key new pred (cdr tree) key)
                tree)
            tree)))


(defun SUBST-IF (new pred tree &key key)
    (if key
        (sys:subst-if-with-key new pred tree key)
        (sys:subst-if new pred tree)))


(defun SUBST-IF-NOT (new pred tree &key key)
    (if key
        (sys:subst-if-with-key new (complement pred) tree key)
        (sys:subst-if new (complement pred) tree)))


(defun SUBST (new old tree &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (labels (
        (do-subst (tree)
            (if (funcall test old tree)
                new
                (if (consp tree)
                    (sys:cons-if-new
                        (do-subst (car tree))
                        (do-subst (cdr tree))
                        tree)
                    tree)))
        (do-subst-with-key (tree)
            (if (funcall test old (funcall key tree))
                new
                (if (consp tree)
                    (sys:cons-if-new
                        (do-subst-with-key (car tree))
                        (do-subst-with-key (cdr tree))
                        tree)
                    tree)))
            )
        (if key
            (do-subst-with-key tree)
            (do-subst tree))))


(defun SUBLIS (alist tree &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (labels
         ((do-sublis (tree &aux p)
            (if (setq p (sys:assoc tree alist test))
                (cdr p)
                (if (consp tree)
                    (sys:cons-if-new
                        (do-sublis (car tree))
                        (do-sublis (cdr tree))
                        tree)
                    tree)))
          (do-sublis-with-key (tree &aux p)
            (if (setq p (sys:assoc (funcall key tree) alist test))
                (cdr p)
                (if (consp tree)
                    (sys:cons-if-new
                        (do-sublis-with-key (car tree))
                        (do-sublis-with-key (cdr tree))
                        tree)
                    tree))))
        (if key
            (do-sublis-with-key tree)
            (do-sublis tree))))


;;; NSUBST etc

(defmacro SYS:SETCONS (a d tree)
    ;; Assumes tree is the name of a variable, not an expression
    `(progn
        (setf (car ,tree) ,a (cdr ,tree) ,d)
        ,tree))


(defun SYS:NSUBST-IF (new pred tree)
    (if (funcall pred tree)
        new
        (if (consp tree)
            (sys:setcons
                (sys:nsubst-if new pred (car tree))
                (sys:nsubst-if new pred (cdr tree))
                tree)
            tree)))


(defun SYS:NSUBST-IF-WITH-KEY (new pred tree key)
    (if (funcall pred (funcall key tree))
        new
        (if (consp tree)
            (sys:setcons
                (sys:nsubst-if-with-key new pred (car tree) key)
                (sys:nsubst-if-with-key new pred (cdr tree) key)
                tree)
            tree)))


(defun NSUBST-IF (new pred tree &key key)
    (if key
        (sys:nsubst-if-with-key new pred tree key)
        (sys:nsubst-if new pred tree)))


(defun NSUBST-IF-NOT (new pred tree &key key)
    (if key
        (sys:nsubst-if-with-key new (complement pred) tree key)
        (sys:nsubst-if new (complement pred) tree)))


(defun NSUBST (new old tree &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (labels (
        (do-nsubst (tree)
            (if (funcall test old tree)
                new
                (if (consp tree)
                    (sys:setcons
                        (do-nsubst (car tree))
                        (do-nsubst (cdr tree))
                        tree)
                    tree)))
        (do-nsubst-with-key (tree)
            (if (funcall test old (funcall key tree))
                new
                (if (consp tree)
                    (sys:setcons
                        (do-nsubst-with-key (car tree))
                        (do-nsubst-with-key (cdr tree))
                        tree)
                    tree)))
            )
    (if key
        (do-nsubst-with-key tree)
        (do-nsubst tree))))


(defun NSUBLIS (alist tree &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (labels
        ((do-nsublis (tree &aux p)
            (if (setq p (sys:assoc tree alist test))
                (cdr p)
                (if (consp tree)
                    (sys:setcons
                        (do-nsublis (car tree))
                        (do-nsublis (cdr tree))
                        tree)
                    tree)))
         (do-nsublis-with-key (tree &aux p)
            (if (setq p (sys:assoc (funcall key tree) alist test))
                (cdr p)
                (if (consp tree)
                    (sys:setcons
                        (do-nsublis-with-key (car tree))
                        (do-nsublis-with-key (cdr tree))
                        tree)
                    tree))))
        (if key
            (do-nsublis-with-key tree)
            (do-nsublis tree))))


;;; MEMBER etc

(defun SYS:MEMBER (item list test)
    (loop
        (when (endp list) (return nil))
        (if (funcall test item (car list))
            (return list)
            (setq list (cdr list)))))


(defun SYS:MEMBER-WITH-KEY (item list test key)
    (loop
        (when (endp list) (return nil))
        (if (funcall test item (funcall key (car list)))
            (return list)
            (setq list (cdr list)))))


(defun MEMBER (item list &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (if key
        (sys:member-with-key item list test key)
        (sys:member item list test)))


(defun MEMBER-IF (pred list &key key)
    (if key
        (loop
            (when (endp list) (return nil))
            (if (funcall pred (funcall key (car list))) (return list))
            (setq list (cdr list)))
        (loop
            (when (endp list) (return nil))
            (if (funcall pred (car list)) (return list))
            (setq list (cdr list)))))


(defun MEMBER-IF-NOT (pred list &key key)
    (if key
        (loop
            (when (endp list) (return nil))
            (unless (funcall pred (funcall key (car list))) (return list))
            (setq list (cdr list)))
        (loop
            (when (endp list) (return nil))
            (unless (funcall pred (car list)) (return list))
            (setq list (cdr list)))))


(defun TAILP (sublist list)
    (loop
        (if (eql sublist list)
            (return t)
            (if (atom list)
                (return nil)
                (setq list (cdr list))))))


(defun ADJOIN (item list &key test test-not key)
    (setq test (sys:choose-seq-test2 test test-not))
    (if key
        (if (sys:member-with-key (funcall key item) list test key)
            list
            (cons item list))
        (if (sys:member item list test)
            list
            (cons item list))))


(defun UNION (l1 l2 &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (dolist (i l1 l2)
        (unless (sys:member-with-key (funcall key i) l2 test key)
                (setq l2 (cons i l2)))))


(defun NUNION (l1 l2 &key test test-not (key #'identity) &aux temp)
    (setq test (sys:choose-seq-test2 test test-not))
    (do ()
        ((endp l1) l2)
        (if (sys:member-with-key (funcall key (car l1)) l2 test key)
            (setq l1 (cdr l1))
            ;; (rotatef l1 (cdr l1) l2)
            (setf temp (cdr l1) (cdr l1) l2 l2 l1 l1 temp))))


(defun INTERSECTION (l1 l2 &key test test-not (key #'identity) &aux result)
    (setq test (sys:choose-seq-test2 test test-not))
    (dolist (i l1 result)
        (if (sys:member-with-key (funcall key i) l2 test key)
            (setq result (cons i result)))))


(defun NINTERSECTION (l1 l2 &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (do* ((rem (cons nil l1))
            (result rem))
         ((endp (cdr rem)) (cdr result))
        (if (sys:member-with-key (funcall key (cadr rem)) l2 test key)
            (setq rem (cdr rem))
            (setf (cdr rem) (cddr rem)))))


(defun SET-DIFFERENCE (l1 l2 &key test test-not (key #'identity) &aux result)
    (setq test (sys:choose-seq-test2 test test-not))
    (dolist (i l1 result)
        (unless (sys:member-with-key (funcall key i) l2 test key)
                (setq result (cons i result)))))


(defun NSET-DIFFERENCE (l1 l2 &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (do* ((rem (cons nil l1))
            (result rem))
         ((endp (cdr rem)) (cdr result))
        (if (sys:member-with-key (funcall key (cadr rem)) l2 test key)
            (setf (cdr rem) (cddr rem))
            (setq rem (cdr rem)))))


(defun SET-EXCLUSIVE-OR (l1 l2 &key test test-not (key #'identity)
                               &aux result)
    (setq test (sys:choose-seq-test2 test test-not))
    (dolist (i l1)
        (unless (sys:member-with-key (funcall key i) l2 test key)
                (setq result (cons i result))))
    (dolist (i l2 result)
        (unless (sys:member-with-key (funcall key i) l1 test key)
                (setq result (cons i result)))))


(defun NSET-EXCLUSIVE-OR (l1 l2 &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (do* ((list1 (cons nil l1))
          (list2 (cons nil l2))
          (p1 list1)
          (p2 list2))
         ((endp (cdr p1)) (setf (cdr p1) (cdr list2)) (cdr list1))
        (if (endp (cdr p2))
            (setq p1 (cdr p1) p2 list2)
            (if (funcall test (funcall key (cadr p1))
                              (funcall key (cadr p2)))
                (setf (cdr p1) (cddr p1)
                      (cdr p2) (cddr p2) p2 list2)
                (setq p2 (cdr p2))))))



(defun SUBSETP (l1 l2 &key test test-not (key #'identity))
    (setq test (sys:choose-seq-test2 test test-not))
    (dolist (i l1 t)
        (unless (sys:member-with-key (funcall key i) l2 test key)
                (return nil))))


(proclaim '(notinline car cdr (setf car) (setf cdr) caar cadr cdar cddr))



#| --- Revision History ---------------------------------------------------
--- John Williams, Sep  6 1996
        sublis and nsublis improved as below.
--- John Williams, Sep  5 1996
        Re-writes for most functions that take &key parameter, to avoid
        calling it unless supplied. Also c*r and c**r declared inline.
--- John Williams, May  5 1995
        SYS:CHOOSE-SEQ-TEST1 & SYS:CHOOSE-SEQ-TEST2 moved here from seq.lsp
--- John Williams, May 16 1994
        SUBST-IF-NOT and NSUBST-IF-NOT now use COMPLEMENT.
--- John Williams, Aug 27 1993
        Upgraded to Steele 1990.
 |#
