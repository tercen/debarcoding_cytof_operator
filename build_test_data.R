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

source('op_functions.R')



lapply( Sys.glob("tests/*.csv"), function(x) { unlink(x) } )
lapply( Sys.glob("tests/*.json"), function(x) { unlink(x) } )
lapply( Sys.glob("tests/*.Rda"), function(x) { unlink(x) } )
lapply( Sys.glob("tests/*.csv.schema"), function(x) { unlink(x) } )


# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "30808f32-dbb1-4c49-9293-ccd2594aba59")

# Test cases
wkfId <- "7537973a65f87297878b1dd4e80015bb"
options("tercen.workflowId"= wkfId)

# stepIdList <- c("30808f32-dbb1-4c49-9293-ccd2594aba59",
#                 "38c67b64-02e0-4b80-8dc4-3994d00cf5ee")

stepIdList <- c("30808f32-dbb1-4c49-9293-ccd2594aba59")

# Steps with properties
propDictList <- list()
# propDictList <- list("SeparationCutoff"=list(stepId="30808f32-dbb1-4c49-9293-ccd2594aba59",
#                                      sepCuttof=0.4),
#                      "Default"=list(stepId="30808f32-dbb1-4c49-9293-ccd2594aba59"))


for( i in seq(1, length(stepIdList))){
  options("tercen.stepId"=stepIdList[i])  
  ctx = tercenCtx()
  
  # Get step name
  step_name <- tim::get_step_name(ctx, wkfId, stepIdList[i])
  print(step_name)
  
  
  has_property_list <- lapply(  propDictList, function(x){
    x$stepId == stepIdList[i]
  })
  
  if( any(unname(unlist(has_property_list)))){
    
    prop_list_idx <- which(unname(unlist(has_property_list)))
    for(j in prop_list_idx){
      
      props <- propDictList[j]
      test_suff <- unlist(unname(names(props)))[1]
      props <- props[[test_suff]]
      props$stepId <- NULL
      
      plist <- props
      pnames <- unlist(unname(names(props)))
      
      
      step_name_ex <- paste0(step_name, "_", test_suff)
      
      
      params <- c(ctx, setNames(plist,pnames))
      res <- do.call('debarcoding_op', params )
      
      tim::build_test_data( res, ctx, paste0(step_name_ex, "_absTol"),
                            version = '0.0.1',
                            absTol = 0.001,
                            gen_schema = TRUE,
                            skipCols=c(".content"),
                            props=setNames(plist,pnames))
      
      
    }
  }else{
    res <- debarcoding_op( ctx )
    
    tim::build_test_data( res, ctx, paste0(step_name, "_absTol"),
                          version = '0.0.1',
                          skipCols=c(".content"),
                          gen_schema = TRUE,
                          absTol = 0.001)
  }
}
        


# Single file
# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/30808f32-dbb1-4c49-9293-ccd2594aba59
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "30808f32-dbb1-4c49-9293-ccd2594aba59")