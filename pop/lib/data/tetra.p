/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/tetra.p
 *  Purpose:        Sample data for use with LIB WALTZ.
 *  Author:         Aaron Sloman, Jan 1981
 *  Documentation:  TEACH * WALTZ
 *  Related Files:  LIB * WALTZ
 */

vars junctions;
;;; a list of junctions from a picture of a tetrahedron with
;;; a corner of another object sticking out
;;;       2
;;;      /|\
;;;     / | \___6
;;;    /  | 5\ |
;;;   /   |   \|7
;;; 1/    |    \
;;;  \    |    / 3
;;;   \   |   /
;;;    \  |  /
;;;     \ | /
;;;      \|/4

[[junc ell j1 j4 j2]
 [junc arw j2 j1 j4 j5]
 [junc ell j3 j7 j4]
 [junc arw j4 j3 j2 j1]
 [junc tee j5 j7 j6 j2]
 [junc ell j6 j5 j7]
 [junc tee j7 j3 j6 j5]]
    -> junctions;

;;; note that since the Waltz programs don't use the co-ordinates of points
;;; they are merely given symbolic names here. If you wish, create a picture
;;; and run seepicture to get the database.
