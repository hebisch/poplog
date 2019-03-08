/* --- Copyright Integral Solutions Ltd 1997. All rights reserved. --------
 * File:            C.all/extern/unicode/encoding_euc_jp.c
 * Purpose:         Unicode <-> EUC-JP
 * Author:          Todd Brye, NCR Corporation 1997 (see revisions)
 */

/*
 *
 *  EUC-JP
 *  =========
 *  Tables derived from JISX0208, JISX0212, JISX0201-1976, and ASCII.
 *
 *      Code written by Todd Brye, NCR Corporation 1997
 *  NCR Knowledge Discovery Workbench (TM)
 *  Copyright NCR Corporation 1997
 *
 *  Clementine
 *  Copyright 1994-1997 by Integral Solutions Limited
 */


#include "encoding.h"
#include "euc_jp_tables.h"

#define EUC_SUBSTITUTION_CODE 0x3F

static UNICODE_TO_X unicode_to_euc;
static X_TO_UNICODE euc_to_unicode;

CODING_PAIR encoding_euc_jp = { unicode_to_euc, euc_to_unicode };

#define SET_OUTPUT(c, set) {                    \
      switch(set)                               \
      {                                         \
        case 0:                                 \
            output[0] = (CHAR8)(c & 0xFF);      \
            ++output, --olen;                   \
            break;                              \
                                                \
        case 1: if(olen == 1) goto done;        \
            output[0] = (CHAR8)(c >> 8);        \
            output[1] = (CHAR8)(c & 0xFF);      \
            output += 2, olen -= 2;             \
            break;                              \
                                                \
        case 2: if(olen == 1) goto done;        \
            output[0] = (CHAR8) 0x8E;           \
            output[1] = (CHAR8)(c & 0xFF);      \
            output += 2, olen -= 2;             \
            break;                              \
                                                \
        case 3: if(olen <= 2) goto done;        \
            output[0] = (CHAR8) 0x8F;           \
            output[1] = (CHAR8)(c >> 8);        \
            output[2] = (CHAR8)(c & 0xFF);      \
            output += 3, olen -= 3;             \
            break;                              \
                                                \
        default:                                \
            output[0] = EUC_SUBSTITUTION_CODE;  \
            ++output, --olen;                   \
            break;                              \
      }                                         \
    }

static void
unicode_to_euc(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    /* set output bytes to value c */

    if (flags & CHAR8_INPUT) {
        /* input is 8-bit */
        const CHAR8* charInput = (CHAR8*)input;
        while (ilen > 0 && olen > 0) {
            CHAR16 cin = charInput[0];
            CHAR16 cout = encode[0][cin];
            SET_OUTPUT(cout, code_set[0][cin]);
            ++charInput, --ilen;
            if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
    }
    else
        while (ilen > 0 && olen > 0)
        {
            CHAR16 cin = input[0];
            CHAR16 cout = encode[cin >> 8][cin & 0xFF];
            SET_OUTPUT(cout, code_set[cin >> 8][cin & 0xFF]);
            ++input, --ilen;
            if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
    done:

    *inputLength = ilen;
    *outputLength -= olen;
}

static void
euc_to_unicode(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    while (ilen > 0 && olen > 0)
    {
        CHAR16 c = which_code_set[input[0]];
        CHAR16 cout = U_REPLACEMENT_CHARACTER;

        if (c == 0)
        {
            cout = input[0];
            ++input, --ilen;
        }
        else if (c == 1)
        {
            if (ilen > 1)
            {
                /*  decode tables are partial, so we have to check
                    second byte for validity */
                if (input[1] >= 0xa1 && input[1] <= 0xfe)
                    cout = decode_set1[input[0]-0xa1][input[1]-0xa1];
                input += 2, ilen -= 2;
            }
            else
            {
                if(flags & INPUT_INCOMPLETE) break;
            }
        }
        else if (c == 2)
        {
            if (ilen > 1)
            {
                if (input[1] >= 0xa1 && input[1] <= 0xdf)
                    cout = (0xff61 - 0xa1) + input[1];
                input += 2, ilen -= 2;
            }
            else
            {
                if(flags & INPUT_INCOMPLETE) break;
            }
        }
        else if (c == 3)
        {
            if (ilen > 2)
            {
                if (input[1] >= 0xa1 && input[1] <= 0xfe
                &&  input[2] >= 0xa1 && input[2] <= 0xfe)
                    cout = decode_set3[input[1]-0xa1][input[2]-0xa1];
                input += 3, ilen -= 3;
            }
            else
            {
                if(flags & INPUT_INCOMPLETE) break;
            }
        }

        output[0] = cout;
        ++output, --olen;
        if (cout == '\n' && (flags & STOP_AT_NEWLINE)) break;

    }

    *inputLength = ilen;
    *outputLength -= olen;
}


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 11 1997
        Various bug-fixes to do with range-checking on both input and
        output
 */
