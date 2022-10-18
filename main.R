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


source("op_functions.R")

# Single file
# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/30808f32-dbb1-4c49-9293-ccd2594aba59
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "30808f32-dbb1-4c49-9293-ccd2594aba59")
# 

# Multi file
# http://127.0.0.1:5402/admin/w/7537973a65f87297878b1dd4e80015bb/ds/38c67b64-02e0-4b80-8dc4-3994d00cf5ee
# options("tercen.workflowId" = "7537973a65f87297878b1dd4e80015bb")
# options("tercen.stepId"     = "38c67b64-02e0-4b80-8dc4-3994d00cf5ee")

ctx = tercenCtx()

docId <- ctx$select( ctx$labels[[1]], nr = 1 ) # Assumes there is only 1 label, and they are all equal
docId <- docId[[1]]
doc <- ctx$client$fileService$get(docId)

filename = tempfile()
writeBin(ctx$client$fileService$download(docId), filename)
sample_key <- read.csv(filename)
unlink(filename)


sk_dm <- data.matrix(sample_key[ , seq(2, ncol(sample_key))])
colnames(sk_dm) <- unlist(lapply( colnames(sk_dm), function(x){
  as.numeric(substr(x, 2,nchar(x)))
} ))

rownames(sk_dm) <- sample_key[,1]


# Use this if ZIP files are to be supported
# # unzip if archive
# if (length(grep(".zip", doc$name)) > 0) {
#   tmpdir <- tempfile()
#   unzip(filename, exdir = tmpdir)
#   f.names <- list.files(tmpdir, full.names = TRUE)
# } else {
#   f.names <- filename
# }


row_df <- ctx$rselect() %>%
  mutate(.ri = seq(0, ctx$rschema$nRows-1))

col_df <- ctx$cselect() %>%
  mutate(.ci = seq(0, ctx$cschema$nRows-1))


df <- ctx$select(c(".y", ".ri", ".ci")) %>%
  left_join(row_df, by=".ri") %>%
  left_join(col_df, by=".ci") 

# Remove column name prefix
names(df) <- unlist(lapply( names(df), function(x){
  if( str_starts(x, "[.]") ){
    return(x)
  }else{
    sp <- str_split_fixed(x, "[.]", Inf)
    return(sp[length(sp)]  )
  }
}))

res <- df %>%
  dplyr::group_by(filename) %>%
  group_map( ~ do.debarcoding(., sk_dm), .keep=TRUE )


nfiles <- length(res)


assay_df <- NULL
barcode_df <- NULL
img_df <- NULL
for( i in seq(1, nfiles) ){
  if( is.null( assay_df ) ){
    assay_df <- res[[i]]$assay_df
    barcode_df <- res[[i]]$barcode_df
    img_df <- res[[i]]$img_df
  }else{
    assay_df <- rbind(assay_df, res[[i]]$assay_df)
    barcode_df <- rbind(barcode_df, res[[i]]$barcode_df)
    img_df <- rbind(img_df, res[[i]]$img_df)
  }
}


barcode_df <- barcode_df %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids )


# SAVE
assay_df %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  left_join_relation( ctx$rrelation, ".r",ctx$rrelation$rids ) %>%
  left_join_relation( ctx$crelation, ".i",ctx$crelation$rids ) %>%
  left_join_relation( barcode_df, ".i", ".i" ) %>%
  left_join_relation( img_df, list(), list() ) %>%
  as_join_operator( append(ctx$rnames, ctx$cnames), append(ctx$rnames, ctx$cnames)) %>%
  save_relation(ctx)