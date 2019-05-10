# initialisation
source(".init_local.R")

# anything else?
library(RColorBrewer)
library(tidyverse)
library(foreach)
options(stringsAsFactors = FALSE)

FIGS_PATH <- file.path("/Users/bguillod/Dropbox/ETH_WORK/figs")
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)