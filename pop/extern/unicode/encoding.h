/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:        C.all/extern/unicode/encoding.h
 * Purpose:
 * Author:      Robert John Duncan, Feb 27 1997
 * Documentation:
 * Related Files:
 */

#ifndef _ENCODING_H_
#define _ENCODING_H_

typedef unsigned char   CHAR8;
typedef unsigned short  CHAR16;

typedef void UNICODE_TO_X(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
);

typedef void X_TO_UNICODE(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
);

typedef struct _coding_pair {
    UNICODE_TO_X*   encode;
    X_TO_UNICODE*   decode;
} CODING_PAIR;

/* Flags */

#define BYTE_INPUT          0x1     /* UNICODE_TO_X */
#define INPUT_INCOMPLETE    0x1     /* X_TO_UNICODE */
#define STOP_AT_NEWLINE     0x2
#define CHAR8_INPUT         BYTE_INPUT

/* Special Unicode values */

#define U_BYTE_ORDER_MARK           0xFEFF
#define U_REPLACEMENT_CHARACTER     0xFFFD
#define U_REVERSE_BYTE_ORDER_MARK   0xFFFE
#define U_SENTINEL                  0xFFFF

#endif  /* _ENCODING_H_ */
