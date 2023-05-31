library(tidyr)

debarcoding_op <- function( ctx, ctx2, Separation_Cutoff=-1 ){
  # 1. check for document Id
  # If ther eis, do it like it is
  # if not, read the table
  isDocumentObj <- any(lapply(ctx2$cnames, function(x){
    pts <- strsplit(x, "\\.")[[1]]
    pts[length(pts)] == "documentId"
  }))
  if( isDocumentObj == T ){
    docId <- ctx2$cselect(  ) # Assumes there is only 1 label, and they are all equal
    docId <- docId[[1]]
    doc <- ctx2$client$fileService$get(docId)
    
    filename = tempfile()
    writeBin(ctx$client$fileService$download(docId), filename)  
    sample_key <- read.csv(filename)
    on.exit(unlink(filename))
  }else{
    fact <- ctx2$rselect()[[1]]
    fact_name <- ctx2$rnames[[1]]
    
    sample_key <- ctx2$select(c(".y", ".ci", ".ri")) %>%
      tidyr::pivot_wider(names_from = c(".ri"), values_from = ".y") %>%
      select(-".ci") %>%
      t() 
      
    sample_key <- cbind(ctx2$rselect(), sample_key)
    colnames(sample_key) <- append('X', paste('X', ctx2$cselect()[[1]], sep = ''   ))
  }
  
  sk_dm <- data.matrix(sample_key[ , seq(2, ncol(sample_key))])
  colnames(sk_dm) <- unlist(lapply( colnames(sk_dm), function(x){
      as.numeric(substr(x, 2,nchar(x)))
  } )) 
  
  rownames(sk_dm) <- sample_key[,1]

  # 3.55mb
  row_df <- ctx$rselect() %>%
    mutate(.ri = seq(0, ctx$rschema$nRows-1))
    
  col_df <- ctx$cselect() %>%
    mutate(.ci = seq(0, ctx$cschema$nRows-1))  

  
  # 4.92
  df <- ctx$select(c(".y", ".ri", ".ci")) %>%
    left_join(row_df, by=".ri") %>%
    left_join(col_df, by=".ci")  

  col_df <- NULL
  row_df <- NULL
  
  row_factor <- ctx$rnames[[1]] #names(row_df)
  
  # First row factor different from ci.
  # NOTE, only one row factor allowed
  for( i in seq(1, length(row_factor))){
    if( row_factor[i] != ".ci"){
      row_factor <- row_factor[i]
      break
    }
  }
  row_factor <- str_split_fixed(row_factor, "[.]", Inf)
  row_factor <- row_factor[length(row_factor)]

  

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
    group_map( ~ do.debarcoding(., sk_dm, Separation_Cutoff, row_factor), .keep=TRUE )

  df <- NULL
  ctx$log("After debarcoding")
  
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

  assay_df <- assay_df %>%
    ctx$addNamespace()

  barcode_df <- barcode_df %>%
    ctx$addNamespace()

  
  return(lst(assay_df, barcode_df, img_df))
}


do.debarcoding <- function( df, sk_dm, sepCuttof, row_factor='variable' ){

  # 0.08
  data_chans <- unique(unlist(as.list(df[row_factor])))  

  # 1.9mb
  dm_ff <- matrix( unlist(lapply(data_chans, function(x){
    df %>%
      dplyr::filter( !!sym(row_factor) == x ) %>%
      select(.y) %>%
      data.matrix()
  })), ncol= length(data_chans) )   
  

  
  colnames(dm_ff)  <- data_chans
  

  sce <- new("flowFrame", exprs=dm_ff)
  dm_ff <- NULL

  sce <- prepData(sce)
  sce <- assignPrelim(sce, sk_dm)
  sce <- estCutoffs(sce)

  pops <- rownames(sk_dm)
  np <- ceiling((length(pops)+1)/3)

  plot_list <- list()
  plot_list <- append( plot_list, list(plotYields(sce, which = c(0)) ))
  for( i in seq(1, length(pops))){
    plot_list <- append( plot_list, list(plotYields(sce, which = c(pops[i]) )) )
  }


  plot_file <- tempfile()
  png(filename=plot_file, width = 1500, height=500*np)
  do.call("grid.arrange", c(plot_list, ncol=3))
  dev.off()
  plot_list <- NULL


  if( sepCuttof == -1 ){
    sce <- CATALYST::applyCutoffs(sce)
  }else{
    sce <- CATALYST::applyCutoffs(sce,  sep_cutoffs = sepCuttof)
  }
  
  ctx$log("Apply end")

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output 1: Scaled values per .ci and .ri
  assay_values <- assay( sce, "scaled")


  chnames <- rownames(sce)
  assay_df <- NULL

  for( i in seq(1, length(chnames))){
    chname <- chnames[i]
    ci_ri <- df %>%
                dplyr::filter(!!sym(row_factor) == chname) %>%
                dplyr::select(c(".ci", ".ri"))
    
    tmp_df <- tibble(assay_values[i,], ci_ri)

    
    if(is.null(assay_df)){
      assay_df <- tmp_df
    }else{
      assay_df <- rbind(assay_df, tmp_df)
    }
  }

  names(assay_df) <- c("Scaled", ".i", ".r")

  # head(assay_df)
  # END of output 1 -> assay_df
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output 2: Assignment values per barcode (.ci)
  bc_assigns <- sce$bc_id
  barcodes <- unique( bc_assigns )


  # Unique per column, removes repetition from rows
  barcode_df <- cbind( unique(df$.ci), tibble(bc_assigns))

  names(barcode_df) <- append( ".i", "Barcodes")


  # END of output 2 -> barcode_df
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output 3: Diagnostic plot (yield)
  img_df <- tim::plot_file_to_df(plot_file, filename = plot_file)
  img_df$mimetype <- 'image/png'
  
  if( "filename" %in% names(df)){
    img_df$filename <- df$filename[[1]]
  }else{
    img_df$filename <- "IMG"
  }
  # END of output 3 -> img_df
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  return(lst(assay_df, barcode_df, img_df))
}