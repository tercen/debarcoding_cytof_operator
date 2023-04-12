suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(tim)
  library(stringr)
  
  library(CATALYST)
  library(openCyto)
  library(flowCore)
  
  library(imager)
  library(grid)
  library(gridExtra)
  library(ggplot2)
})

# library(profvis)

# profvis({
source("op_functions.R") 
# options("tercen.stepId"     = "30808f32-dbb1-4c49-9293-ccd2594aba59")
# Single file
# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/30808f32-dbb1-4c49-9293-ccd2594aba59
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "2ffba54a-df15-4e73-9fad-4f8bc547072c")

# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/30808f32-dbb1-4c49-9293-ccd2594aba59
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "30808f32-dbb1-4c49-9293-ccd2594aba59")

# Multi file
# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/38c67b64-02e0-4b80-8dc4-3994d00cf5ee
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "38c67b64-02e0-4b80-8dc4-3994d00cf5ee")

# http://127.0.0.1:5400/test/w/4644e0c114a8edd474bc8f893a050075/ds/a2d1a8f2-1e3a-4eec-8425-0e005dc7bbb5
# options("tercen.workflowId" = "4644e0c114a8edd474bc8f893a050075")
# options("tercen.stepId"     = "a2d1a8f2-1e3a-4eec-8425-0e005dc7bbb5")

# http://127.0.0.1:5400/test/w/e87260d62c621a13272407f6ff0043cb/ds/19c7f23a-79fd-4174-ae8f-3a8a2ac00754
# options("tercen.workflowId" = "e87260d62c621a13272407f6ff0043cb")
# options("tercen.stepId"     = "19c7f23a-79fd-4174-ae8f-3a8a2ac00754")

ctx = tercenCtx()

ctx$requestResources(nCpus=1, ram=26000000000, ram_per_cpu=26000000000)

cutoff <- ctx$op.value('Separation_Cutoff', as.double, -1)

# source("op_functions.R") 

res <- debarcoding_op(ctx, cutoff)

# })





barcode_df <-  res[[2]] %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids )


# SAVE
res[[1]] %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
  left_join_relation( ctx$rrelation, ".r",ctx$rrelation$rids ) %>%
  left_join_relation( barcode_df, ".i", ".i" ) %>%
  left_join_relation( res[[3]], list(), list() ) %>%
  as_join_operator( unlist(append(ctx$cnames, ctx$rnames)), unlist(append(ctx$cnames, ctx$rnames))) %>%
  save_relation(ctx)

