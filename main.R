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
  library(jsonlite)
})

# library(profvis)

# get_workflow_id <- function(ctx) {
#   if(is.null(ctx$task)) {
#     return(ctx$workflowId)
#   } else {
#     workflowIdPair <- Find(function(pair) identical(pair$key, "workflow.id"), ctx$task$environment)
#     workflowId <- workflowIdPair$value
#     return(workflowId)
#   }
# }
# 
# get_step_id <- function(ctx) {
#   if(is.null(ctx$task)) {
#     return(ctx$stepId)
#   } else {
#     stepIdPair <- Find(function(pair) identical(pair$key, "step.id"), ctx$task$environment)
#     stepId <- stepIdPair$value
#     return(stepId)
#   }
# }



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

# http://127.0.0.1:5400/test/w/462bec31fcad0c7eb8af65440e003fc9/ds/0ed730d2-e5ba-490f-b636-fe7a47ab8912
# options("tercen.workflowId" = "462bec31fcad0c7eb8af65440e003fc9")
# options("tercen.stepId"     = "0ed730d2-e5ba-490f-b636-fe7a47ab8912")

# Multistep
# http://127.0.0.1:5400/test/w/462bec31fcad0c7eb8af65440e003fc9/ds/acdc8625-7db7-47c1-b3fd-cb08a634b1da
# options("tercen.workflowId" = "462bec31fcad0c7eb8af65440e003fc9")
# options("tercen.stepId"     = "acdc8625-7db7-47c1-b3fd-cb08a634b1da")

ctx = tercenCtx()
#ctx$task$siblings$id
# MultiStep step2
# ctx2 = tercenCtx(workflowId = "462bec31fcad0c7eb8af65440e003fc9", stepId = "3edda5da-633d-42ab-bf20-2488e909b21e")

if(is.null(ctx$task)) {
  stop("task is null")
} else {
  pair <- Find(function(pair) identical(pair$key, "task.siblings.id"), ctx$task$environment)
  task_siblings_id <- jsonlite::fromJSON(pair$value)
  ctx2 <- tercenCtx(taskId = task_siblings_id)
}


ctx$requestResources(nCpus=1, ram=26000000000, ram_per_cpu=26000000000)

cutoff <- ctx$op.value('Separation_Cutoff', as.double, -1)

# source("op_functions.R") 

res <- debarcoding_op(ctx, ctx2, cutoff)

# })

# browser()
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

