#| --- Copyright University of Sussex 1993. All rights reserved. ----------
 | File:            C.all/lisp/src/strings.lsp
 | Purpose:         Common Lisp string functions
 | Author:          John Williams, Feb 26 1986 (see revisions)
 | Documentation:   CLtL, p299
 | Related Files:   C.all/lisp/src/strings.p
 |#

(defun STRING= (string1 string2 &key start1 start2 end1 end2)
    (sys:string= string1 string2 start1 end1 start2 end2))


(defun STRING-EQUAL (string1 string2 &key start1 start2 end1 end2)
    (sys:string-equal string1 string2 start1 end1 start2 end2))


(defun STRING< (string1 string2 &key start1 start2 end1 end2)
    (sys:string< string1 string2 start1 end1 start2 end2))


(defun STRING> (string1 string2 &key start1 start2 end1 end2)
    (sys:string> string1 string2 start1 end1 start2 end2))


(defun STRING<= (string1 string2 &key start1 start2 end1 end2)
    (sys:string<= string1 string2 start1 end1 start2 end2))


(defun STRING>= (string1 string2 &key start1 start2 end1 end2)
    (sys:string>= string1 string2 start1 end1 start2 end2))


(defun STRING/= (string1 string2 &key start1 start2 end1 end2)
    (sys:string/= string1 string2 start1 end1 start2 end2))


(defun STRING-LESSP (string1 string2 &key start1 start2 end1 end2)
    (sys:string-lessp string1 string2 start1 end1 start2 end2))


(defun STRING-GREATERP (string1 string2 &key start1 start2 end1 end2)
    (sys:string-greaterp string1 string2 start1 end1 start2 end2))


(defun STRING-NOT-GREATERP (string1 string2 &key start1 start2 end1 end2)
    (sys:string-not-greaterp string1 string2 start1 end1 start2 end2))


(defun STRING-NOT-LESSP (string1 string2 &key start1 start2 end1 end2)
    (sys:string-not-lessp string1 string2 start1 end1 start2 end2))


(defun STRING-NOT-EQUAL (string1 string2 &key start1 start2 end1 end2)
    (sys:string-not-equal string1 string2 start1 end1 start2 end2))


(defun MAKE-STRING (size &key (initial-element #\Space)
                              (element-type 'character))
    (sys:make-string size initial-element element-type))


(defun STRING-UPCASE (string &key start end)
    (sys:string-upcase string start end))


(defun STRING-DOWNCASE (string &key start end)
    (sys:string-downcase string start end))


(defun STRING-CAPITALIZE(string &key start end)
    (sys:string-capitalize string start end))


(defun NSTRING-UPCASE (string &key start end)
    (sys:nstring-upcase string start end))


(defun NSTRING-DOWNCASE (string &key start end)
    (sys:nstring-downcase string start end))


(defun NSTRING-CAPITALIZE(string &key start end)
    (sys:nstring-capitalize string start end))



#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 31 1993
        Upgraded to Steele 1990.
 |#
