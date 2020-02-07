/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.x/x/Xpw/LocBitmap.c
 * Purpose:     Locate and load bitmap, searching in default places
 * Author:          Jonathan Meyer, Jul 30 1991 (see revisions)
 * Documentation:   REF XpwCore/XpwLoadPixmap
 * Related Files:   XpwCore.c XpwCore.p
 */

/*
 * This file is based closely on the MIT Xmu LocBitmap.c source code, which is
 * Copyright 1989 Massachusetts Institute of Technology
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#ifndef VMS
#include <sys/param.h>          /* get MAXPATHLEN if possible */
#endif
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

/*
 * split_path_string - split a colon-separated list into its constituent
 * parts; to release, free list[0] and list.
 */

static char **split_path_string (src)
    register char *src;
  {
    int nelems = 1;
    register char *dst;
    char **elemlist, **elem;

    /* count the number of elements */
    for (dst = src; *dst; dst++) if (*dst == ':') nelems++;

    /* get memory for everything */
    dst = (char *) malloc (dst - src + 1);
    if (!dst) return NULL;
    elemlist = (char **) calloc ((nelems + 1), sizeof (char *));
    if (!elemlist)
      { free (dst);
        return NULL;
      }

    /* copy to new list and walk up nulling colons and setting list pointers */
    strcpy (dst, src);
    for (elem = elemlist, src = dst; *src; src++)
      { if (*src == ':')
          { *elem++ = dst;
            *src = '\0';
            dst = src + 1;
          }
      }
    *elem = dst;

    return elemlist;
  }

/*
 * lookup_file_paths - gets bitmapFilePath resource value and turns it into
 * list of file paths
 */

static char **lookup_file_paths(dpy)
Display *dpy;
{
    /* Look for bitmapFilePath resource */
    XrmName xrm_name[2];
    XrmClass xrm_class[2];
    XrmRepresentation rep_type;
    XrmValue value;
    xrm_name[0] = XrmStringToName ("bitmapFilePath");
    xrm_name[1] = (XrmName) 0;
    xrm_class[0] = XrmStringToClass ("BitmapFilePath");
    xrm_class[1] = (XrmClass) 0;
    /* need to initialize display database */
    if (!XtDatabase(dpy)) (void) XGetDefault (dpy, "", "");
    if (XrmQGetResource(XtDatabase(dpy), xrm_name, xrm_class, &rep_type, &value)
    && rep_type == XrmStringToQuark(XtRString))
        return split_path_string (value.addr);
    else
        return NULL;
}

/* Frees list returned by lookup_file_paths */

static void free_file_paths(file_paths)
char **file_paths;
  { if (file_paths)
      { if (file_paths[0]) free (file_paths[0]);
        free ((char *) (file_paths));
      }
  }

Pixmap XpwLocateBitmapFile (screen, name, srcname, srcnamelen,
                widthp, heightp, xhotp, yhotp)
    Screen *screen;
    char *name;
    char *srcname;          /* RETURN */
    int srcnamelen;
    int *widthp, *heightp, *xhotp, *yhotp;  /* RETURN */
  {
    Display *dpy = DisplayOfScreen (screen);
    Window root = RootWindowOfScreen (screen);

    Bool try_plain_name = True;
    Bool got_file_paths = False;
    char **file_paths, **fpaths;
    char filename[MAXPATHLEN];
    unsigned int width, height;
    int xhot, yhot;
    int i;


    /*
     * Search order:
     *    1.  name if it begins with / or ./
     *    2.  "each prefix in file_paths"/name
     *    3.  BITMAPDIR/name
     *    4.  name if didn't begin with / or .
     */

#ifndef BITMAPDIR
#define BITMAPDIR "/usr/include/X11/bitmaps"
#endif

    for (i = 1; i <= 4; i++)
      { char *fn = filename;
        Pixmap pixmap;

        switch (i)
          { case 1:
                if (!(name[0] == '/' || (name[0] == '.') && name[1] == '/'))
                  continue;
                fn = name;
                try_plain_name = False;
                break;
            case 2:
                if (!got_file_paths)
                  { file_paths = fpaths = lookup_file_paths(dpy);
                    got_file_paths = True;
                  }
                if (fpaths && *fpaths)
                  { sprintf (filename, "%s/%s", *fpaths, name);
                    fpaths++;
                    i--;
                    break;
                  }
                continue;
            case 3:
                sprintf (filename, "%s/%s", BITMAPDIR, name);
                break;
            case 4:
                if (!try_plain_name) continue;
                fn = name;
                break;
          }

        if (XReadBitmapFile (dpy, root, fn, &width, &height,
                     &pixmap, &xhot, &yhot) == BitmapSuccess)
          {
            if (widthp) *widthp = (int)width;
            if (heightp) *heightp = (int)height;
            if (xhotp) *xhotp = xhot;
            if (yhotp) *yhotp = yhot;
            if (got_file_paths) free_file_paths(file_paths);
            if (srcname && srcnamelen > 0)
              { strncpy (srcname, fn, srcnamelen - 1);
                srcname[srcnamelen - 1] = '\0';
              }
            return pixmap;
          }
      }

    if (got_file_paths) free_file_paths(file_paths);
    return None;
  }


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 18 1996
        Fixed bug in XpwLocateBitmapFile -- was trying to free the incremented
        file_paths pointer instead of its starting value.
--- Julian Clinton, Apr  7 1993
        Modified lookup_file_paths to use XtDatabase instead of accessing
        display structure directly.
--- John Gibson, Nov 21 1991
        VMS mod to exclude <sys/param.h>
 */
