#include <R.h>

/* Utility functions */

/* Random integer */

#define IRAND(imax) (int) (((double) (imax + 1)) * unif_rand())

/* 2 different random integers */

void i2rand(int *vec, int imax)
{
    vec[0] = IRAND(imax);
    do {
	vec[1] = IRAND(imax);
    } while (vec[1] == vec[0]);
}


/*
 * Quasiswap or sum-of-squares reducing swap of Miklos & Podani. A quasiswap
 * step takes a random 2x2 submatrix and adds (-1,+1,+1,-1). If the submatrix
 * was (1,0,0,1) it is swapped to (0,1,1,0), but if it was, say, (2,0,0,1) it
 * is swapped to (1,1,1,0) which reduces sums-of-squares. We start with a
 * random matrix with given marginal totals (from R r2dtable) but possibly
 * some values >1. Then we perform quasiswaps on random 2x2 submatrices so
 * long that only 1 and 0 are left.  The function only does the quasiswaps,
 * and it assumes that input matrix 'm' (dimensions 'nr', 'nc') was produced
 * by r2dtable or some other function with given marginal totals, but some
 * values possibly > 1.
 */

/* row & col indices to a vector index */

#define INDX(i, j, nr) (i) + (nr)*(j)

void quasiswap(int *m, int *nr, int *nc)
{
    int i, n, mtot, ss, row[2], col[2], nr1, nc1, a, b, c, d;

    nr1 = (*nr) - 1;
    nc1 = (*nc) - 1;

    /* Get matrix total 'mtot' and sum-of-squares 'ss' */

    n = (*nr) * (*nc);
    for (i = 0, mtot = 0, ss = 0; i < n; i++) {
	mtot += m[i];
	ss += m[i] * m[i];
    }

    /* Get R RNG */
    GetRNGstate();

    /* Quasiswap while there are entries > 1 */

    while (ss > mtot) {
	i2rand(row, nr1);
	i2rand(col, nc1);
	/* a,b,c,d notation for a 2x2 table */
	a = INDX(row[0], col[0], *nr);
	b = INDX(row[0], col[1], *nr);
	c = INDX(row[1], col[0], *nr);
	d = INDX(row[1], col[1], *nr);
	if (m[a] > 0 && m[d] > 0 && m[a] + m[d] - m[b] - m[c] >= 2) {
	    ss -= 2 * (m[a] + m[d] - m[b] - m[c] - 2);
	    m[a]--;
	    m[d]--;
	    m[b]++;
	    m[c]++;
	} else if (m[b] > 0 && m[c] > 0 &&
		   m[b] + m[c] - m[a] - m[d] >= 2) {
	    ss -= 2 * (m[b] + m[c] - m[a] - m[d] - 2);
	    m[a]++;
	    m[d]++;
	    m[b]--;
	    m[c]--;
	}
    }

    /* Set R RNG */
    PutRNGstate();
}

/* Trial swap: try 'thin' times and swap when you can. This gives zero
 * to many swaps for one call.
 */

void trialswap(int *m, int *nr, int *nc, int *thin)
{

    int i, a, b, c, d, row[2], col[2];

    GetRNGstate();

    for (i=0; i < *thin; i ++) {
	i2rand(row, (*nr) - 1);
	i2rand(col, (*nc) - 1);
	a = INDX(row[0], col[0], *nr);
	b = INDX(row[0], col[1], *nr);
	c = INDX(row[1], col[0], *nr);
	d = INDX(row[1], col[1], *nr);
	if (m[a] == 1 && m[d] == 1 && m[b] == 0 & m[c] == 0) {
	    m[a] = 0;
	    m[d] = 0;
	    m[b] = 1;
	    m[c] = 1;
	} else if (m[c] == 1 && m[b] == 1 && m[d] == 0 &&
		   m[a] == 0) {
	    m[a] = 1;
	    m[d] = 1;
	    m[b] = 0;
	    m[c] = 0;
	}
    }

    PutRNGstate();
}

/* Ordinary swap: swap if you can, stop after you swapped, or repeat
 * thin times. The data matrix 'm' must be binary: this is not
 * checked.
 */

void swap(int *m, int *nr, int *nc, int *thin)
{

    int i, a, b, c, d, row[2], col[2];

    GetRNGstate();

    for (i=0; i < *thin; i ++) {
	for(;;) {
	    i2rand(row, (*nr) - 1);
	    i2rand(col, (*nc) - 1);
	    a = INDX(row[0], col[0], *nr);
	    b = INDX(row[0], col[1], *nr);
	    c = INDX(row[1], col[0], *nr);
	    d = INDX(row[1], col[1], *nr);
	    if (m[a] == 1 && m[d] == 1 && m[b] == 0 & m[c] == 0) {
		m[a] = 0;
		m[d] = 0;
		m[b] = 1;
		m[c] = 1;
		break;
	    } else if (m[c] == 1 && m[b] == 1 && m[d] == 0 &&
		       m[a] == 0) {
		m[a] = 1;
		m[d] = 1;
		m[b] = 0;
		m[c] = 0;
		break;
	    }
	}
    }
    PutRNGstate();
}

#undef IRAND
#undef INDX
