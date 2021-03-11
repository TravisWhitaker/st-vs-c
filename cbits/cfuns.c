#include <stdint.h>
#include <stdlib.h>
#include <math.h>

uint64_t call_overhead(uint64_t x)
{
	return x + 1;
}

void cfree(void *p)
{
	free(p);
}

uint64_t *cAllocWord(uint64_t len)
{
	uint64_t *x = malloc(sizeof(*x) * len);
	return x;
}

double *cAllocDouble(uint64_t len)
{
	double *x = malloc(sizeof(*x) * len);
	return x;
}

void cPopWord(uint64_t *x, uint64_t len)
{
	for(uint64_t i = 0; i < len; i++)
	{
		x[i] = i;
	}
}

void cPopDouble(uint64_t *x, uint64_t len)
{
	for(uint64_t i = 0; i < len; i++)
	{
		x[i] = (i / 10000);
	}
}

void cMapTimes2Word(uint64_t *p, uint64_t len)
{
	for(uint64_t i = 0; i < len; i++)
	{
		p[i] = p[i] * 2;
	}
};

void cMapTimes2Double(double *p, uint64_t len)
{
	for(uint64_t i = 0; i < len; i++)
	{
		p[i] = p[i] * 2;
	}
};

void cMapExpDouble(double *p, uint64_t len)
{
	for(uint64_t i = 0; i < len; i++)
	{
		p[i] = exp(p[i]);
	}
};

void cSwapInPlace(uint64_t *p, uint64_t len)
{
	uint64_t e = len - 1;
	uint64_t t;
	for(uint64_t i = 0; i < (len / 2); i++)
	{
		t = p[i];
		p[i] = p[e];
		p[e] = t;
		e--;
	}
};
