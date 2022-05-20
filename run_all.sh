#!/bin/bash
open POQ_replication.Rproj

Rscript code/conjoint.R

Rscript code/analysis_SI.R

Rscript code/assumptions_diagnostics.R
