/*
  gpclib:  General Polygon Clipping library for R
  Copyright (C) 2003 Roger D. Peng <rpeng@stat.ucla.edu>
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "gpc.h"


#ifndef MALLOC
#define MALLOC(p, b, s)    {if ((b) > 0) { \
                            p= malloc(b); if (!(p)) { \
                            fprintf(stderr, "gpc malloc failure: %s\n", s); \
		            exit(0);}} else p= NULL;}
#endif

#ifndef FREE
#define FREE(p)            {if (p) {free(p); (p)= NULL;}}
#endif


int compute_polygon_size(gpc_polygon *p);
void gpc_polygon_to_double(double *a, int na, gpc_polygon *p);
void double_to_gpc_polygon(gpc_polygon *p, double *a, int na);

SEXP gpc_polygon_intersect(SEXP subjpoly, SEXP clippoly)
{
    gpc_polygon subject, clip, result;
    int polysize, nsubj, nclip;
    SEXP returnval;
    double *xreturnval;
    double *xsubjpoly, *xclippoly;

    PROTECT(subjpoly = AS_NUMERIC(subjpoly));
    PROTECT(clippoly = AS_NUMERIC(clippoly));
    nsubj = LENGTH(subjpoly);
    nclip = LENGTH(clippoly);

    /* Rprintf("nsubj: %d\nnclip: %d\n", nsubj, nclip); */

    xsubjpoly = NUMERIC_POINTER(subjpoly);
    xclippoly = NUMERIC_POINTER(clippoly);

    double_to_gpc_polygon(&subject, xsubjpoly, nsubj);
    double_to_gpc_polygon(&clip, xclippoly, nclip);
    gpc_polygon_clip(GPC_INT, &subject, &clip, &result);

    /* Rprintf("polygons clipped\n"); */

    polysize = compute_polygon_size(&result);

    PROTECT(returnval = NEW_NUMERIC(polysize));
    xreturnval = NUMERIC_POINTER(returnval);

    gpc_polygon_to_double(xreturnval, polysize, &result);

    gpc_free_polygon(&subject);
    gpc_free_polygon(&clip);
    gpc_free_polygon(&result);

    UNPROTECT(3);

    return(returnval);
}



SEXP gpc_polygon_difference(SEXP subjpoly, SEXP clippoly)
{
    gpc_polygon subject, clip, result;
    int polysize, nsubj, nclip;
    SEXP returnval;
    double *xreturnval;
    double *xsubjpoly, *xclippoly;

    PROTECT(subjpoly = AS_NUMERIC(subjpoly));
    PROTECT(clippoly = AS_NUMERIC(clippoly));
    nsubj = LENGTH(subjpoly);
    nclip = LENGTH(clippoly);

    /* Rprintf("nsubj: %d\nnclip: %d\n", nsubj, nclip); */

    xsubjpoly = NUMERIC_POINTER(subjpoly);
    xclippoly = NUMERIC_POINTER(clippoly);

    double_to_gpc_polygon(&subject, xsubjpoly, nsubj);
    double_to_gpc_polygon(&clip, xclippoly, nclip);
    gpc_polygon_clip(GPC_DIFF, &subject, &clip, &result);

    /* Rprintf("polygons clipped\n"); */

    polysize = compute_polygon_size(&result);

    PROTECT(returnval = NEW_NUMERIC(polysize));
    xreturnval = NUMERIC_POINTER(returnval);

    gpc_polygon_to_double(xreturnval, polysize, &result);

    gpc_free_polygon(&subject);
    gpc_free_polygon(&clip);
    gpc_free_polygon(&result);

    UNPROTECT(3);

    return(returnval);
}


SEXP gpc_polygon_union(SEXP subjpoly, SEXP clippoly)
{
    gpc_polygon subject, clip, result;
    int polysize, nsubj, nclip;
    SEXP returnval;
    double *xreturnval;
    double *xsubjpoly, *xclippoly;

    PROTECT(subjpoly = AS_NUMERIC(subjpoly));
    PROTECT(clippoly = AS_NUMERIC(clippoly));
    nsubj = LENGTH(subjpoly);
    nclip = LENGTH(clippoly);

    /* Rprintf("nsubj: %d\nnclip: %d\n", nsubj, nclip); */

    xsubjpoly = NUMERIC_POINTER(subjpoly);
    xclippoly = NUMERIC_POINTER(clippoly);

    double_to_gpc_polygon(&subject, xsubjpoly, nsubj);
    double_to_gpc_polygon(&clip, xclippoly, nclip);
    gpc_polygon_clip(GPC_UNION, &subject, &clip, &result);

    /* Rprintf("polygons clipped\n"); */

    polysize = compute_polygon_size(&result);

    PROTECT(returnval = NEW_NUMERIC(polysize));
    xreturnval = NUMERIC_POINTER(returnval);

    gpc_polygon_to_double(xreturnval, polysize, &result);

    gpc_free_polygon(&subject);
    gpc_free_polygon(&clip);
    gpc_free_polygon(&result);

    UNPROTECT(3);

    return(returnval);
}




void double_to_gpc_polygon(gpc_polygon *p, double *a, int na)
{
    int i, j, k;

    p->num_contours = a[0];
    MALLOC(p->hole, p->num_contours * sizeof(int), "hole flag array creation");
    MALLOC(p->contour, p->num_contours * sizeof(gpc_vertex_list), "contour creation");
    i = 1;
  
    for(j=0; j < p->num_contours; j++) {
	p->contour[j].num_vertices = a[i++];    
	MALLOC(p->contour[j].vertex, p->contour[j].num_vertices * sizeof(gpc_vertex), "vertex creation");
    
	for(k=0; k < p->contour[j].num_vertices; k++) {
	    p->contour[j].vertex[k].x = a[i++];
	    p->contour[j].vertex[k].y = a[i++];
	}
	if(i > na) {
	    Rprintf("index out of range: %d\n", i);
	    return;
	}     
    }
}


void gpc_polygon_to_double(double *a, int na, gpc_polygon *p)
{
    int i, j, k;

    a[0] = p->num_contours;
    i = 1;

    for(j=0; j < p->num_contours; j++) {
	a[i++] = p->contour[j].num_vertices;

	if(i > na) {
	    Rprintf("index out of range: %d\n", i);
	    return;
	}
	for(k=0; k < p->contour[j].num_vertices; k++) {
	    a[i++] = p->contour[j].vertex[k].x;

	    if(i > na) {
		Rprintf("index out of range: %d\n", i);
		return;
	    }
	    a[i++] = p->contour[j].vertex[k].y;

	    if(i > na) {
		Rprintf("index out of range: %d\n", i);
		return;
	    }
	}
    }
}


int compute_polygon_size(gpc_polygon *p) 
{
    int psize = 1, i;

    psize += p->num_contours;
  
    for(i=0; i < p->num_contours; i++) {
	psize += 2 * p->contour[i].num_vertices;
    }
    return(psize);
}
