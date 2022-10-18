library(tercen)
library(dplyr, warn.conflicts = FALSE)

# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/49eff7e6-4647-4b56-8721-2e7f28888619
options("tercen.workflowId" = "wwww")
options("tercen.stepId"     = "dddd")

ctx = tercenCtx()

# ctx %>%
#   select(.y, .ci, .ri) %>%
#   group_by(.ci, .ri) %>%
#   summarise(mean = mean(.y)) %>%
#   ctx$addNamespace() %>%
#   ctx$save()