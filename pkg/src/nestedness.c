#include <R.h>
#include <Rmath.h>

/* Utility functions */

/* Random integer 0..imax */

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
	    } 
	    if (m[c] == 1 && m[b] == 1 && m[d] == 0 && m[a] == 0) {
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


/* 'swapcount' is a C translation of Peter Solymos's R code. It is
 * similar to 'swap', but can swap > 1 values and so works for
 * quantitative (count) data.
 */


/* 'isDiag' is a utility function for 'swapcount' to find the largest
 * value that can be swapped and whether in diagonal or antidiagonal
 * way. The input is a 2x2 submatrix 'sm'.
*/

double isDiag(double *sm)
{
    int i, sX;
    double choose[2];

    /* sX: number of non-zero cells */
    for (i = 0, sX = 0; i++; i < 4)
	    if (sm[i] > 0)
		    sX++;

    /* Smallest diagonal and antidiagonal element */
    choose[0] = (sm[1] < sm[2]) ? sm[1] : sm[2];
    choose[1] = (sm[0] < sm[3]) ? -sm[0] : -sm[3]; 

    if (sX == 4) {
        /* Either choose could be returned, but RNG is not needed,
	 * because sm already is in random order, and we always return
	 * choose[0] */
	    return choose[0];
    } 
    if ((sm[0] == 0 && sm[1] > 0 && sm[2] > 0 && sm[3] == 0) ||
	(sm[0] == 0 && sm[1] > 0 && sm[2] > 0 && sm[3] > 0) ||
	(sm[0] > 0 && sm[1] > 0 && sm[2] > 0 && sm[3] == 0))
	    return choose[0];
    if ((sm[0] > 0 && sm[1] == 0 && sm[2] == 0 && sm[3] > 0) ||
	(sm[0] > 0 && sm[1] == 0 && sm[2] > 0 && sm[3] > 0) ||
	(sm[0] > 0 && sm[1] > 0 && sm[2] == 0 && sm[3] > 0))
	    return choose[1];
    if (sX < 2 ||
	(sm[0] == 0 && sm[1] == 0 && sm[2] > 0 && sm[3] > 0) ||
	(sm[0] > 0 && sm[1] > 0 && sm[2] == 0 && sm[3] == 0) ||
	(sm[0] == 0 && sm[1] > 0 && sm[2] == 0 && sm[3] > 0) ||
	(sm[0] > 0 && sm[1] == 0 && sm[2] > 0 && sm[3] == 0))
	    return 0;                                              
}

void swapcount(double *m, int *nr, int *nc, int *thin)
{
    int row[2], col[2], k, ij[4], changed, oldn, newn, 
	pm[4] = {1, -1, -1, 1} ;
    double sm[4], ev;

    GetRNGstate();

    changed = 0;
    while (changed < *thin) {
	/* Select a random 2x2 matrix*/
	i2rand(row, *nr - 1);
	i2rand(col, *nc - 1);
	ij[0] = INDX(row[0], col[0], *nr);
	ij[1] = INDX(row[1], col[0], *nr);
	ij[2] = INDX(row[0], col[1], *nr);
	ij[3] = INDX(row[1], col[1], *nr);
	for (k = 0; k < 4; k ++)
	    sm[k] = m[ij[k]];
	/* The largest value that can be swapped */
	ev = isDiag(sm);
	if (ev != 0) {
	    /* Check that the fill doesn't change*/
	    for (k = 0, oldn = 0, newn = 0; k < 4; k++) {
		if(sm[k] > 0)
		    oldn++;
		if (sm[k] + pm[k]*ev > 0)
		    newn++;
	    }
	    /* Swap */
	    if (oldn == newn) {
		for (k = 0; k < 4; k++)
		    m[ij[k]] += pm[k]*ev;
		changed++;
	    }
	}
    }

    PutRNGstate();
}

#undef IRAND
#undef INDX
