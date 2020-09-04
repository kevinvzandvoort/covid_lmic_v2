#!/bin/bash
#SBATCH --job-name=covidm_lmic_nigeria --array=2-86

/shared/covid/covid_lmic/interventions/reviewer_comments/runcountry.sh nigeria $SLURM_ARRAY_TASK_ID
