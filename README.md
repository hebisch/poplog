POPLOG is a free, open source, multi-language software development
environment providing incremental compilers for a number of interactive
programming languages, notably:

* Pop-11
    The core language of Poplog, including a rich interface to the X
    window system and a powerful Object Oriented programming extension,
    Objectclass, developed by Steve Leach now a standard part of the
    language (comparable to CLOS as an extension of LISP). 
* Prolog
    Standard prolog with the "Edinburgh" syntax.
* Common Lisp
    Compatible with most of CLTL2 (Common Lisp the language, 2nd
    Edition) by G.L. Steele
* Standard ML
    A powerful, strongly typed, polymorphic, functional language.

Poplog provides support for multi-paradigm software development in a
rapid prototyping environment, because of the use of (fast) incremental
compilers for all the languages.  There is substantial AI and teaching
material using Poplog, some included in this repository, some
in separate packages repository, some available on the net.

This is cleaned up version of Poplog sources, currently only
core part.  It misses binary needed for bootstrap and extensions
(packages).  Packages are in separate repository:

  https://github.com/hebisch/poplog_packages

You can find link to bootstrap binary and buildable tarball for
Intel/AMD 64-bit Linux at

  http://fricas.org/~hebisch/poplog

(this build version closely corresponding to repository).

For more detailed installation instructions see INSTALL.
