#| --- Copyright University of Sussex 1992. All rights reserved. ----------
 | File:            C.all/lisp/src/mapping.lsp
 | Purpose:         Common Lisp mapping functions
 | Author:          John Williams, Oct 16 1986
 | Documentation:   CLtL, p98-99
 | Related Files:   C.all/lisp/src/lists.p
 |#


(defun MAPCAR (fn list &rest more-lists)
    (if more-lists
        (labels ((do-mapcar (lists)
                    (unless (sys:eq-member () lists)
                            (cons (apply fn (sys:map-1-list #'car lists))
                                  (do-mapcar (sys:map-1-list #'cdr lists))))))
            (do-mapcar (cons list more-lists)))
        (sys:map-1-list fn list)))


(defun MAPLIST (fn list &rest more-lists)
    (labels ((do-maplist (lists)
                 (unless (sys:eq-member () lists)
                     (cons (apply fn lists)
                           (do-maplist (sys:map-1-list #'cdr lists))))))
        (do-maplist (cons list more-lists))))


(defun MAPC (fn list &rest more-lists)
    (if more-lists
        (do ((lists (cons list more-lists) (sys:map-1-list #'cdr lists)))
            ((sys:eq-member () lists) list)
            (apply fn (sys:map-1-list #'car lists)))
        (dolist (item list list)
            (funcall fn item))))


(defun MAPL (fn list &rest more-lists)
    (if more-lists
        (do ((lists (cons list more-lists) (sys:map-1-list #'cdr lists)))
            ((sys:eq-member () lists) list)
            (apply fn lists))
        (do ((l list (cdr l)))
            ((endp l) list)
            (funcall fn l))))


(defun MAPCAN (fn list &rest more-lists)
    (apply #'nconc (apply #'mapcar fn list more-lists)))


(defun MAPCON (fn list &rest more-lists)
    (apply #'nconc (apply #'maplist fn list more-lists)))
