#include <string.h>

#if defined(__arm__)

struct registers_buffer {
    int i_reg[4];
    union {double d; struct {float sl; float sh;} sf2;} f_reg[8];
};

typedef struct registers_buffer reg_buff;

typedef struct arg_state { int ni; int sfi; int dfi; int si;} arg_state;

#if 0

#include <stdio.h>

void
print_arg_state(arg_state * as) {
    printf("ni = %d, sfi = %d, dfi = %d, si = %d\n",
           as->ni, as->sfi, as->dfi, as->si);
}

#define PR_AS (print_arg_state(as))

#else

#define PR_AS do {} while(0)

#endif
static void
store_single(arg_state * as, void * sp, reg_buff * rp, float val) {
    float * dst;
    int sfi = as->sfi;
    PR_AS;
    if (sfi < 16) {
        int dfi1 = sfi >> 1;
        if (sfi & 1) {
            dst = &(rp->f_reg[dfi1].sf2.sh);
            as->sfi = (as->dfi)<<1;
        } else {
            dst = &(rp->f_reg[dfi1].sf2.sl);
            as->sfi++;
            as->dfi = (as->dfi > dfi1)?as->dfi:(dfi1 + 1);
        }
    } else {
        dst = (float *)sp + as->si;
        as->si++;
    }
    memcpy(dst, &val, sizeof(val));
}

static void
store_double(arg_state * as, void * sp, reg_buff * rp, double val) {
    int dfi = as->dfi;
    double * dst;
    PR_AS;
    if (dfi < 8) {
        dst = &(rp->f_reg[dfi].d);
        as->dfi++;
        if (as->sfi == (dfi<<1)) {
            as->sfi = (as->dfi<<1);
        }
    } else {
        int si = as->si;
        as->sfi = 16;
        si = (si + 1)&(~1);
        dst = (double *)sp + (si>>1);
        as->si = si + 2;
    }
    memcpy(dst, &val, sizeof(val));
}

static void
store_int(arg_state * as, void * sp, reg_buff * rp, int val) {
    int ni = as->ni;
    PR_AS;
    if (ni < 4) {
        rp->i_reg[ni] = val;
        as->ni++;
    } else {
        *((int *)sp + as->si) = val;
        as->si++;
    }
}

/* Must agree with syscomp/wordflags.p */
#define ET_DDEC 2

/* Must agree with offset in syscomp/symdefs.p */
#define ET_OFF (4*12+2)
void
copy_external_arguments(int n, int * ap, void * sp, reg_buff * rp,
                        int fltsingle) {
    int i;
    arg_state as = {0, 0, 0, 0};
    ap += (n - 1);
    for(i = 0; i < n; i++, fltsingle >>= 1) {
        int ai = *ap;
        ap--;
        if (ai & 1) {
            /* simple */
            if (ai & 2) {
                /* integer */
                store_int(&as, sp, rp, ai >>= 2);
            } else {
                /* single float */
                ai -= 1;
                float fval;
                memcpy(&fval, &ai, sizeof(ai));
                if (fltsingle & 1) {
                    store_single(&as, sp, rp, fval);
                } else {
                    double dval = fval;
                    store_double(&as, sp, rp, dval);
                }
            }
        } else {
            unsigned char * ak = ((unsigned char * *)ai)[-1];
            unsigned char et = *(ak + ET_OFF);
            if (et == ET_DDEC) {
                double dval;
                memcpy(&dval, (int *)ai, 4);
                memcpy(((int *)&dval)+1, (int *)ai - 2, 4);
                if (fltsingle & 1) {
                    float fval = dval;
                    store_single(&as, sp, rp, fval);
                } else {
                    store_double(&as, sp, rp, dval);
                }
            } else {
                int val;
                if (et == 0) {
                    val = ai;
                } else {
                    /* External pointer of bignum: derefernce */
                    val = *(int *)ai;
                }
                store_int(&as, sp, rp, val);
            }
        }
    }
}

#if 0

/* Test code */

reg_buff rb;

int
main(void) {
    void * sp = malloc(1000);
    arg_state as = {0, 0, 0, 0};
    float fval = 0.0;
    double dval = 100.0;
    int ival = 0;
/*
    store_int(&as, sp, &rb, (ival += 1));
    store_int(&as, sp, &rb, (ival += 1));
    store_int(&as, sp, &rb, (ival += 1));
    store_int(&as, sp, &rb, (ival += 1));
    store_int(&as, sp, &rb, (ival += 1));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
*/
    store_int(&as, sp, &rb, (ival += 1));
    store_single(&as, sp, &rb, (fval += 1.0));
    store_double(&as, sp, &rb, (dval += 1.0));
    store_single(&as, sp, &rb, (fval += 1.0));
    print_arg_state(&as);
    return 0;
}

#endif
#endif
