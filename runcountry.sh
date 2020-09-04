#!/bin/bash

COUNTRY=$1
RUN=$2

FILE="/shared/covid/covid_lmic/interventions/reviewer_comments/covidm_interventions_run_scenario.R"

COVIDM="/shared/covid/covidm"
HELPER_FILE="/shared/covid/covid_lmic/interventions/reviewer_comments/helper_functions.R"

INPUT_DIR="/shared/covid/covid_lmic/interventions/reviewer_comments/inputs"
OUTPUT_FILE1="/shared/covid/covid_lmic/interventions/reviewer_comments/sceni_${RUN}_${COUNTRY}_cases.rds"

sudo CFLAGS="-I/shared/gsl/bin/include" LDFLAGS="-L/shared/gsl/bin/lib -lgsl -lgslcblas -lm" GSL_CONFIG="/shared/gsl/bin/bin/gsl-config" LD_LIBRARY_PATH=/shared/gsl/bin/lib:$LD_LIBRARY_PATH PATH=/shared/gsl/bin/lib:$PATH PKG_CPPFLAGS="-I/shared/gsl/bin/include" PKG_LIBS="-L/shared/gsl/bin/lib -lgsl -lgslcblas -lm" /shared/R/R-3.6.0/bin/Rscript ${FILE} ${COVIDM} ${HELPER_FILE} ${RUN} ${COUNTRY} ${INPUT_DIR} ${OUTPUT_FILE1}
