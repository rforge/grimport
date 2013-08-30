#include <stdio.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

/*
 * Conditionally include headers based on whether we want to apply
 * the related feature
 */
#if HAVE_LIBPOPPLER
#include <poppler/glib/poppler.h>
#endif
#if HAVE_LIBSPECTRE
#include <libspectre/spectre.h>
#endif
#if HAVE_LIBRSVG
#include <librsvg/rsvg.h>
#if LIBRSVG_MAJOR_VERSION <= 2 && LIBRSVG_MICRO_VERSION <= 31 && LIBRSVG_MICRO_VERSION <= 1
#include <librsvg/rsvg-cairo.h>
#endif
#endif

#include <cairo.h>
#include <cairo-ps.h>
#include <cairo-svg.h>

#include <R.h>
#define USE_RINTERNALS 1
#include <Rinternals.h>

SEXP pdf_get_n_pages(SEXP filename);
SEXP ps_get_n_pages(SEXP filename);
SEXP ps_to_pdf(SEXP ps_file, SEXP pdf_file);
SEXP pdf_to_ps(SEXP pdf_file, SEXP ps_file, SEXP page_num);
SEXP svg_to_ps(SEXP svg_file, SEXP cairo_ps_file);
SEXP pdf_to_svg(SEXP pdf_file, SEXP svgfile, SEXP page_num);
SEXP svg_to_svg(SEXP svg_file, SEXP cairo_svg_file);