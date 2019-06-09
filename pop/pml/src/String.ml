(* --- Copyright University of Sussex 1994.  All rights reserved. ---------
 * File:            C.all/pml/src/String.ml
 * Purpose:         PML: Functions on strings
 * Author:          Robert Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * STRING
 *)


signature String = sig

    (* Substring Functions *)

    exception Substring
    exception Prefix
    exception Suffix
    exception Index

    val substring       : int -> int -> string -> string
    val prefix          : int -> string -> string
    val suffix          : int -> string -> string
    val index           : int -> string -> string
    val issubstring     : string -> string -> bool
    val isprefix        : string -> string -> bool
    val issuffix        : string -> string -> bool

    (* String Search Functions *)

    exception Locate
    exception Locater
    exception Skip
    exception Skipr

    val locate          : string -> string -> int
    val locater         : string -> string -> int
    val skip            : string -> string -> int
    val skipr           : string -> string -> int

    (* Character Testing Predicates *)

    val isalphabetic    : string -> bool
    val islowercase     : string -> bool
    val isuppercase     : string -> bool
    val isnumeric       : string -> bool
    val isspace         : string -> bool
    val iscontrol       : string -> bool

    (* Character Transformation Functions *)

    val lowercase       : string -> string
    val uppercase       : string -> string
    val control         : string -> string
    val detab           : string -> string
    val mapstring       : (string -> string) -> string -> string

    (* Miscellaneous Functions *)

    exception Stringof
    exception Stringint

    val stringof        : int -> string -> string
    val stringint       : string -> int

end;    (* signature String *)


pop11

section $-ml;

ml_structure String : String = struct

ml_exception Substring;
ml_exception Prefix;
ml_exception Suffix;
ml_exception Index;
ml_exception Locate;
ml_exception Locater;
ml_exception Skip;
ml_exception Skipr;
ml_exception Stringof;
ml_exception Stringint;


;;; substring n1 n2 s:
;;;     returns a string composed of the (n2 - n1) characters of s
;;;     beginning with character n1 (i.e. the substring of s starting at
;;;     character n1 and finishing at character (n2 - 1)).
;;;     Raises the exception -Substring- unless 0 <= n1 <= n2 <= size s

ml_val substring : int -> int -> string -> string =
procedure(n1, n2, s) with_props substring;
    lconstant substring_exn = exception("Substring");
    lvars n1, n2, s;
    if n1 < 0 or n2 < n1 or n2 > datalength(s) then
        raise(substring_exn);
    else
        substring(n1 fi_+ 1, n2 fi_- n1, s);
    endif;
endprocedure;

;;; prefix n s:
;;;     returns a string composed of the first n characters of s.
;;;     Raises the exception -Prefix- if n < 0 or n > size s

ml_val prefix : int -> string -> string =
procedure(n, s) with_props prefix;
    lconstant prefix_exn = exception("Prefix");
    lvars n, s;
    if n < 0 or n > datalength(s) then
        raise(prefix_exn);
    else
        substring(1, n, s);
    endif;
endprocedure;

;;; suffix n s:
;;;     returns a string composed of the last (size s - n) characters
;;;     of s (i.e the suffix of s beginning at character n).
;;;     Raises the exception "suffix" if n < 0 or n > size s.

ml_val suffix : int -> string -> string =
procedure(n, s) with_props suffix;
    lconstant suffix_exn = exception("Suffix");
    lvars n, s, l = datalength(s);
    if n < 0 or n > l then
        raise(suffix_exn);
    else
        substring(n fi_+ 1, l fi_- n, s);
    endif;
endprocedure;

;;; index n s:
;;;     returns the n'th character of s as a one-element string.
;;;     Raises the exception -Index- unless 0 <= n < size s

ml_val index : int -> string -> string =
procedure(n, s) with_props index;
    lconstant index_exn = exception("Index");
    lvars n, s;
    if n < 0 or n >= datalength(s) then
        raise(index_exn);
    else
        mlstring(Subscrs(n fi_+ 1, s), 1);
    endif;
endprocedure;

;;; issubstring s1 s2:
;;;     true iff s1 is a substring of s2

ml_val issubstring : string -> string -> bool =
procedure(s1, s2) with_props issubstring;
    lvars s1, s2;
    if issubstring(s1, 1, s2) then
        true;
    else
        false;
    endif;
endprocedure;

;;; isprefix s1 s2:
;;;     true iff s1 is a prefix of s2

ml_val isprefix : string -> string -> bool =
procedure(s1, s2) with_props isprefix;
    lvars s1, s2;
    if issubstring_lim(s1, 1, 1, false, s2) then
        true;
    else
        false;
    endif;
endprocedure;

;;; issuffix s1 s2:
;;;     true iff s1 is a suffix of s2

ml_val issuffix : string -> string -> bool =
procedure(s1, s2) with_props issuffix;
    lvars s1, s2, l1 = datalength(s1), l2 = datalength(s2);
    if l1 fi_> l2 then
        false;
    elseif issubstring(s1, l2 fi_- l1 fi_+ 1, s2) then
        true;
    else
        false;
    endif;
endprocedure;

;;; locate s1 s2:
;;;     returns the smallest integer n for which the condition
;;;         isprefix s1 (suffix n s2)
;;;     is true.
;;;     Raises the exception -Locate- if there is no such n,
;;;     i.e. if not(issubstring s1 s2)

ml_val locate : string -> string -> int =
procedure(s1, s2) with_props locate;
    lconstant locate_exn = exception("Locate");
    lvars s1, s2, n;
    if issubstring(s1, 1, s2) ->> n then
        n fi_- 1;
    else
        raise(locate_exn);
    endif;
endprocedure;

;;; locater:
;;;     for "locate right": returns the largest integer n for which the
;;;     condition
;;;         isprefix s1 (suffix n s2)
;;;     is true.
;;;     Raises the exception -Locater- if there is no such n,
;;;     i.e if not(issubstring s1 s2)

ml_val locater : string -> string -> int =
procedure(s1, s2) with_props locater;
    lconstant locater_exn = exception("Locater");
    lvars c, p, s1, s2, l1 = datalength(s1), l2 = datalength(s2);
    if l1 == 0 then
        return(l2);
    elseif l1 == 1 then
        if locchar_back(Subscrs(1, s1), l2, s2) ->> p then
            return(p fi_- 1);
        endif;
    else
        Subscrs(1, s1) -> c;
        l2 fi_- l1 fi_+ 1 -> p;
        while p fi_> 0 and locchar_back(c, p, s2) ->> p do
            if issubstring_lim(s1, p, p, false, s2) then
                return(p fi_- 1);
            endif;
            p fi_- 1 -> p;
        endwhile;
    endif;
    raise(locater_exn);
endprocedure;

;;; skip:
;;;     skips over leading occurrences of -s1- in -s2-. Raises the
;;;     exception -Skip- if -s1- is the empty string.

ml_val skip : string -> string -> int =
procedure(s1, s2) with_props skip;
    lconstant skip_exn = exception("Skip");
    lvars s1, s2, l = datalength(s1), p;
    if l == 0 then
        raise(skip_exn);
    elseif l == 1 then
        if skipchar(Subscrs(1, s1), 1, s2) ->> p then
            p fi_- 1;
        else
            datalength(s2);
        endif;
    else
        1 -> p;
        while issubstring_lim(s1, p, p, false, s2) do
            p fi_+ l -> p;
        endwhile;
        p fi_- 1;
    endif;
endprocedure;

;;; skipr:
;;;     skips trailing occurrences of -s1- in -s2-. Raises the exception
;;;     -Skipr- if -s1- is the empty string.

ml_val skipr : string -> string -> int =
procedure(s1, s2) with_props skipr;
    lconstant skipr_exn = exception("Skipr");
    lvars s1, s2, l = datalength(s1), p;
    if l == 0 then
        raise(skipr_exn);
    elseif l == 1 then
        if skipchar_back(Subscrs(1, s1), datalength(s2), s2) ->> p then
            p;
        else
            0;
        endif;
    else
        datalength(s2) fi_- l fi_+ 1 -> p;
        while p fi_> 0 and issubstring_lim(s1, p, p, false, s2) do
            p fi_- l -> p;
        endwhile;
        p fi_+ l fi_- 1;
    endif;
endprocedure;

;;; isalphabetic:
;;;     true if every character c in s is a letter

ml_val isalphabetic : string -> bool =
procedure(s) with_props isalphabetic;
    lvars i, c, s;
    For i to datalength(s) do
        returnunless(isalphacode(Subscrs(i, s)))(false);
    endfor;
    true;
endprocedure;

;;; islowercase:
;;;     true if every character c in s is a lower case letter

ml_val islowercase : string -> bool =
procedure(s) with_props islowercase;
    lvars i, c, s;
    For i to datalength(s) do
        returnunless(islowercode(Subscrs(i, s)))(false);
    endfor;
    true;
endprocedure;

;;; isuppercase:
;;;     true if every character c in s is an upper case letter

ml_val isuppercase : string -> bool =
procedure(s) with_props isuppercase;
    lvars i, c, s;
    For i to datalength(s) do
        returnunless(isuppercode(Subscrs(i, s)))(false);
    endfor;
    true;
endprocedure;

;;; isnumeric:
;;;     true if every character c in s is a digit

ml_val isnumeric : string -> bool =
procedure(s) with_props isnumeric;
    lvars i, c, s;
    For i to datalength(s) do
        returnunless(isnumbercode(Subscrs(i, s)))(false);
    endfor;
    true;
endprocedure;

;;; isspace:
;;;     true if every character c in s is a white space character,
;;;     i.e one of " ", "\t", "\n", "\r", "\f"

ml_val isspace : string -> bool =
procedure(s) with_props isspace;
    lvars i, c, s;
    For i to datalength(s) do
        unless (Subscrs(i, s) ->> c) == ` `
        or c == `\t`
        or c == `\n`
        or c == `\^M`
        or c == `\^L`
        then
            return(false);
        endunless;
    endfor;
    true;
endprocedure;

;;; iscontrol:
;;;     true if every character c in s is a control character,
;;;     i.e. c < " " or c >= "\127"

ml_val iscontrol : string -> bool =
procedure(s) with_props iscontrol;
    lvars i, c, s;
    For i to datalength(s) do
        unless (Subscrs(i, s) ->> c) fi_< ` ` or c fi_>= `\^?` then
            return(false);
        endunless;
    endfor;
    true;
endprocedure;

;;; lowercase:
;;;     maps every uppercase letter in s to its lowercase equivalent

ml_val lowercase : string -> string = uppertolower;

;;; uppercase:
;;;     maps every lowercase letter in s to its uppercase equivalent

ml_val uppercase : string -> string = lowertoupper;

;;; control:
;;;     transforms to control characters

ml_val control : string -> string =
procedure(s) with_props control;
    lvars i, c, s;
    For i to datalength(s) do
        if (Subscrs(i, s) ->> c) fi_>= `\(064)` and c fi_<= `\(095)` then
            c fi_- 64;
        else
            c;
        endif;
    endfor,
    mlstring(datalength(s));
endprocedure;

;;; detab:
;;;     replaces tabs in -s- with spaces.

ml_val detab : string -> string =
procedure(s) with_props detab;
    lvars i, c, s;
    returnunless(locchar(`\t`, 1, s))(s);
    For i to datalength(s) do
        if (Subscrs(i, s) ->> c) == `\t` then `\s` else c endif;
    endfor,
    mlstring(datalength(s));
endprocedure;

;;; mapstring:
;;;     transform each character in -s- with -f-.

ml_val mapstring : (string -> string) -> string -> string =
procedure(f, s);
    lvars i, procedure f, s, n = stacklength();
    For i to datalength(s) do
        deststring(f(mlstring(Subscrs(i, s), 1))) -> ;
    endfor,
    mlstring(stacklength() fi_- n);
endprocedure;

;;; stringof n s:
;;;     returns a string of size n where each character is equal to the
;;;     first character of s.
;;;     Raises the exception -Stringof- if n < 0 or size s < 1

ml_val stringof : int -> string -> string =
procedure(n, s) with_props stringof;
    lconstant stringof_exn = exception("Stringof");
    lvars c, n, s;
    if not(isinteger(n)) or n fi_< 0 or datalength(s) == 0 then
        raise(stringof_exn);
    else
        Subscrs(1, s) -> c;
        mlstring(Repeat n times c endrepeat, n);
    endif;
endprocedure;

;;; stringint:
;;;     converts a string s to an integer. Raises the exception -stringint-
;;;     if not(isnumeric s).

ml_val stringint : string -> int =
procedure(s) with_props stringint;
    lconstant stringint_exn = exception("Stringint");
    lvars i, c, s;
    0;
    For i to datalength(s) do
        unless (Subscrs(i, s) ->> c) fi_>= `0` and c fi_<= `9` then
            raise(stringint_exn);
        endunless;
        * 10 + (c fi_- `0`);
    endfor;
endprocedure;

ml_endstructure;

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Rob Duncan, Sep  4 1989
        Changed "Strings" to "String"
--- Rob Duncan, Sep  4 1989
        Added -skip-, -skipr-, -detab-, -control- and -mapstring-.
        Reordered definitions within the file.
 *)
