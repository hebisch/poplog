/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/auto/pophost.p
 > Purpose:        Finding things out about the host computer
 > Author:         Jonathan Laventhol, Jan 25 1984 (see revisions)
 > Documentation:  HELP * POPHOST, REF * SYSTEM
 > Related Files:  C.all/lisp/src/util.p LIB * VED_REPLY
 */

;;; REMOVE THIS LINE AFTER CHECKING AND AMENDING THE DATA GIVEN BELOW
#_IF not(DEF POPC_COMPILING)
    nprintf(';;; LIB POPHOST SHOULD BE EDITED BY YOUR POPLOG ADMINISTRATOR');
#_ENDIF

;;; For finding out system information.  THIS FILE SHOULD BE CHANGED AT
;;; NEW SITES. You may add any information which you wish to have in this
;;; table, but it would be wise not to delete anything from it.
;;;
;;; The information in this file is also used by the Common Lisp subsystem.
;;;
;;; Use the following conventions:
;;;     All entries are either strings or numbers.
;;;     The names for the entries are always words.
;;;     Lowercase letters are used wherever possible.


section;

global constant procedure pophost;

[
 ;;; Generic operating system type (string)
 ;;;    (Common Lisp SOFTWARE-TYPE)
 [os                        % hd(sys_os_type) sys_>< '' %]

 ;;; Operating system version (string)
 ;;;  (Common Lisp SOFTWARE-VERSION)
 [osversion                 % allbutlast(1, allbutfirst(1, tl(sys_os_type)
                                sys_>< '')) %]

 ;;; Generic machine type (string)
 ;;;  (Common Lisp MACHINE-TYPE)
 [machinetype               % hd(sys_machine_type) sys_>< '' %]

 ;;; Actual machine type (string)
 ;;;  (Common Lisp MACHINE-VERSION)
 [machine                   '??????']

 ;;; Host machine name (string)
 ;;; (Common Lisp MACHINE-INSTANCE)
 [systemname                % sys_host_name() %]

 ;;; Amount of memory provided by host machine  (integer)
 [memory                    0]

 ;;; Serial number of host machine (integer)
 [machineserialnumber       % lvars id = sys_host_id();
                                if ispair(id) then front(id) else 0 endif %]

 ;;; Full site name (string)
 ;;;  (Common Lisp LONG-SITE-NAME)
 [fullsitename              '??????']

 ;;; Short site name (string)
 ;;;  (Common Lisp SHORT-SITE-NAME)
 [site                      '??????']

 ;;; Electronic mail address (string)
 [sitemailname              '??????']

];

newproperty((/* List on stack */), 10, false, true) -> pophost;


endsection;


/*
--- Revision History ---------------------------------------------------
--- John Williams, Aug 12 1992
        Now uses -sys_host_id- for "machineserialnumber"
--- John Williams, Apr  7 1989
        Now uses -sys_machine_type- for "machinetype"
--- John Williams, Feb 20 1989
        Now uses -syshostname- for "systemname" and -sys_processor_type-
        for"machinetype". Also prints warning message if not edited.
*/
