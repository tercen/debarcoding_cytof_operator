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

ctx = tercenCtx()

cutoff <- ctx$op.value('Separation_Cutoff', as.double, -1)

res <- debarcoding_op(ctx, cutoff)

assay_df <- res[[1]]
barcode_df <- res[[2]]
img_df <- res[[3]]

barcode_df <- barcode_df %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids )


# SAVE
assay_df %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
  left_join_relation( ctx$rrelation, ".r",ctx$rrelation$rids ) %>%
  left_join_relation( barcode_df, ".i", ".i" ) %>%
  left_join_relation( img_df, list(), list() ) %>%
  as_join_operator( append(ctx$cnames, ctx$rnames), append(ctx$cnames, ctx$rnames)) %>%
  save_relation(ctx)

