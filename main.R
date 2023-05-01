suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(tim)
  library(stringr)

  library(CATALYST)
  library(openCyto)
  library(flowCore)

  # library(imager)
  library(grid)
  library(gridExtra)
  library(ggplot2)
  library(jsonlite)
})
#sudo docker build -t tercen/debarcoding_cytof_operator:latest .
# Multistep
# http://127.0.0.1:5400/test/w/26de5ac41d7c8df655f7dd256407b39a/ds/a5b3be9d-93f5-47de-a477-e009ac03d15c
# http://127.0.0.1:5400/test/w/26de5ac41d7c8df655f7dd256407b39a/ds/9ef92b20-f52e-450c-b169-5ebd4c4da269
# options("tercen.workflowId" = "26de5ac41d7c8df655f7dd256407b39a")
# options("tercen.stepId"     = "a5b3be9d-93f5-47de-a477-e009ac03d15c")

#  http://127.0.0.1:5400/test/w/462bec31fcad0c7eb8af65440e003fc9/ds/acdc8625-7db7-47c1-b3fd-cb08a634b1da
# http://127.0.0.1:5400/test/w/462bec31fcad0c7eb8af65440e003fc9/ds/3edda5da-633d-42ab-bf20-2488e909b21e
# options("tercen.workflowId" = "462bec31fcad0c7eb8af65440e003fc9")
# options("tercen.stepId"     = "acdc8625-7db7-47c1-b3fd-cb08a634b1da")

ctx = tercenCtx()
# ctx2 = tercenCtx(workflowId = "462bec31fcad0c7eb8af65440e003fc9", stepId = "3edda5da-633d-42ab-bf20-2488e909b21e")
# ctx2 = tercenCtx(workflowId = "26de5ac41d7c8df655f7dd256407b39a", stepId = "9ef92b20-f52e-450c-b169-5ebd4c4da269")

if(is.null(ctx$task)) {
  # stop("task is null")
} else {
  pair <- Find(function(pair) identical(pair$key, "task.siblings.id"), ctx$task$environment)
  task_siblings_id <- jsonlite::fromJSON(pair$value)
  ctx2 <- tercenCtx(taskId = task_siblings_id)
}


# ctx$requestResources(nCpus=1, ram=26000000000, ram_per_cpu=0)
cutoff <- ctx$op.value('Separation_Cutoff', as.double, -1)
ctx$log(ctx2$taskId)


source("op_functions.R") 
res <- debarcoding_op(ctx, ctx2, cutoff)


scale_df <- res[[1]] %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
  left_join_relation( ctx$rrelation, ".r",ctx$rrelation$rids ) %>%
  as_join_operator( unlist(append(ctx$cnames, ctx$rnames)), unlist(append(ctx$cnames, ctx$rnames))) 


barcode_df <-  res[[2]] %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
  as_join_operator(ctx$cnames, ctx$cnames) #%>%

img_df <-  res[[3]] %>%
  as_relation() %>%
  as_join_operator(list(), list()) #%>%
  
save_relation(list(scale_df, barcode_df, img_df), ctx)


# SAVE
# res[[1]] %>%
#   as_relation() %>%
#   left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
#   left_join_relation( ctx$rrelation, ".r",ctx$rrelation$rids ) %>%
#   left_join_relation( barcode_df, ".i", ".i" ) %>%
#   left_join_relation( res[[3]], list(), list() ) %>%
#   as_join_operator( unlist(append(ctx$cnames, ctx$rnames)), unlist(append(ctx$cnames, ctx$rnames))) %>%
#   save_relation(ctx)