# initialisation
source(".init_local.R")

# anything else?
library(RColorBrewer)
library(tidyverse)
library(foreach)
options(stringsAsFactors = FALSE)

FIGS_PATH <- file.path(DATA_PATH_FLDCAL, "figs")
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)