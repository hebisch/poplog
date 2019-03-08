/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 * File:        C.unix/extern/lib/pop_stat.c
 * Purpose:     Pop wrappers for stat functions missing from some systems
 * Author:      Robert Duncan, Mar 29 1996 (see revisions)
 */

#include <unistd.h>
#include <sys/stat.h>

int pop_stat(const char *file_name, struct stat *buf) {
    return stat(file_name, buf);
}

int pop_fstat(int filedes, struct stat *buf) {
    return fstat(filedes, buf);
}

int pop_lstat(const char *file_name, struct stat *buf) {
    return lstat(file_name, buf);
}


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 25 1998
        This file previously only for PCs, now applied to all Unix systems.
--- Robert Duncan, Aug  9 1996
        Name changes
 */
