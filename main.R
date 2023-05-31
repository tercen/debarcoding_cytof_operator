suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(tim)
  library(stringr)

  library(CATALYST)
  library(openCyto)
  library(flowCore)
  library(grid)
  library(gridExtra)
  library(ggplot2)
  library(jsonlite)
})

ctx = tercenCtx()

if(is.null(ctx$task)) {
  stop("task is null")
  # ctx2 = tercenCtx(workflowId = "1c63c8b5db028040a96f4122c1003436", stepId = "0fd72916-a3f9-4f70-bfa8-a012387353e0")
} else {
  pair <- Find(function(pair) identical(pair$key, "task.siblings.id"), ctx$task$environment)
  task_siblings_id <- jsonlite::fromJSON(pair$value)
  ctx2 <- tercenCtx(taskId = task_siblings_id)
}

cutoff <- ctx$op.value('Separation_Cutoff', as.double, -1)

source("op_functions.R") 

res <- debarcoding_op(ctx, ctx2, cutoff)

barcode_df <-  res[[2]] %>%
  as_tibble() %>%
  dplyr::rename(.barcode_id = ".i") %>%
  as_relation() %>%
  left_join_relation(ctx$crelation, ".barcode_id", ctx$crelation$rids ) %>%
  as_join_operator(ctx$cnames, ctx$cnames)

img_df <-  res[[3]] %>%
  as_relation() %>%
  as_join_operator(list(), list())
  

save_relation(list(barcode_df, img_df), ctx)

