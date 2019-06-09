# --- Copyright University of Sussex 1990. All rights reserved. ----------
# File:             C.unix/x/Xpw/Makefile
# Purpose:          makefile for Poplog widget set
# Author:           Jonathan Meyer, December 1989 (see revisions)
# Documentation:
# Related Files:
#

# Change this if you do not use the default compilers  - for example, if you
# use gcc, and it is in /usr/local/bin, then CCROOT=/usr/local
# and CC=$(CCROOT)/bin/gcc

CCROOT=/
CC=$(CCROOT)/bin/cc
AS=$(CCROOT)/bin/as
AR=$(CCROOT)/bin/ar
LD=$(CCROOT)/bin/ld
# COMMENT OUT ONE OF THE FOLLOWING
# for BSD-type systems
RANLIB=$(CCROOT)/bin/ranlib
# for System V
# RANLIB=/bin/true

# XROOT Specifies the root of your X tree. This is usually /usr for most sites
# but may be for example /usr/local.  CHANGE THIS FOR YOUR SITE.

# Sussex - for CTN/CRN X11R4
XROOT=/usr/local/X11R4
# Sussex - for CTN/CRN X11R3
# XROOT=/usr/local/X11
# Sussex - for System V (Bobcat) X
# XROOT=/usr

# LIBFILE specifies the name of the widget library package.
# LIBDIR specifies where the library is to be installed.
LIBFILE=libXpw.a
LIBDIR=../bin

IDIRS = -I. -I$(XROOT)/include -I$(XROOT)/include/X11
CFLAGS = $(IDIRS)

all:    $(LIBFILE)

.PRECIOUS:  $(LIBFILE)

# we need dependencies for all of the header files -
# but I don't know an easy way to do this

$(LIBFILE): \
			$(LIBFILE)(XpwGraphics.o)\
			$(LIBFILE)(XpwText.o)\
			$(LIBFILE)(XpwAssoc.o)\
			$(LIBFILE)(PopCore.o) \
			$(LIBFILE)(PopComposit.o)\
			$(LIBFILE)(PopPixmap.o)\
			$(LIBFILE)(PopGraphic.o)\
			$(LIBFILE)(PopPixmap.o)\
			$(LIBFILE)(PopScrText.o)\
			$(LIBFILE)(PopText.o)
			$(RANLIB) $(LIBFILE)


# if your system doesn't have an implicit .o.a rule, then
# you should unmask the following lines - and delete this comment.
#           $(CC) -c $(CFLAGS) $(?:.o=.c)
#           $(AR) $(ARFLAGS) $(LIBFILE) $?
#           rm $?
#           @(RANLIB) $(LIBFILE)
#           @echo $(LIBFILE) is now up to date

install: $(LIBFILE)
	mv $(LIBFILE) $(LIBDIR)

clean:
	-rm -f *.o

clobber:    clean
	-rm -f $(LIBFILE)

# --- Revision History ---------------------------------------------------
# --- Simon Nichols, Jun 19 1990 - added $(XROOT)/include/X11 to IDIRS.
# --- Simon Nichols, Jun  7 1990 - added install option (and LIBDIR).
# --- Simon Nichols, May 31 1990 - moved from C.all to C.unix.
# --- James Goodlet, May 24 1990 - merged system V/BSD versions, and added
#           "support" for X11R4.
