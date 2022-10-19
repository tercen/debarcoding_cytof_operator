debarcoding_op <- function( ctx, sepCuttof=-1 ){
  
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
    group_map( ~ do.debarcoding(., sk_dm, sepCuttof), .keep=TRUE )
  
  
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
  
  

  
  return(lst(assay_df, barcode_df, img_df))
}

do.debarcoding <- function( df, sk_dm, sepCuttof ){
  
  df_ff <- NULL
  
  data_chans <- unique(unlist(as.list(df$variable)))

  for( chan in data_chans ){
    df_tmp <- df %>%
      dplyr::filter( variable == chan ) %>%
      select(.y)

    names(df_tmp) <- c(chan )

    if( is.null(df_ff) ){
      df_ff <- df_tmp
    }else{
      df_ff <- cbind(df_ff, df_tmp)
    }
  }


  # To demonstrate the debarcoding workflow with CATALYST, we provide sample_ff which follows
  # a 6-choose-3 barcoding scheme where mass channels 102, 104, 105, 106, 108, and 110
  # were used for labeling such that each of the 20 individual barcodes are positive for exactly
  # 3 out of the 6 barcode channels. Accompanying this, sample_key contains a binary code of
  # length 6 for each sample, e.g. 111000, as its unique identifier.
  sample_ff <- new("flowFrame", exprs=data.matrix( df_ff ))
  sce <- prepData(sample_ff)

  sce <- assignPrelim(sce, sk_dm)
  sce <- estCutoffs(sce)

  
  pops <- rownames(sk_dm)
  np <- ceiling((length(pops)+1)/3)
  # browser()
  plot_list <- list()
  plot_list <- append( plot_list, list(plotYields(sce, which = c(0)) ))
  for( i in seq(1, length(pops))){
    plot_list <- append( plot_list, list(plotYields(sce, which = c(pops[i]) )) )
  }


  plot_file <- tempfile()
  png(filename=plot_file, width = 1500, height=500*np)
  do.call("grid.arrange", c(plot_list, ncol=3))
  dev.off()


  if( sepCuttof == -1 ){
    sce <- CATALYST::applyCutoffs(sce)
  }else{
    sce <- CATALYST::applyCutoffs(sce,  sep_cutoffs = sepCuttof)
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output 1: Scaled values per .ci and .ri
  assay_values <- assay( sce, "scaled")

  chnames <- rownames(sce)
  assay_df <- NULL

  for( i in seq(1, length(chnames))){
    chname <- chnames[i]
    ci_ri <- df %>%
                dplyr::filter(variable == chname) %>%
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

  barcode_df <- NULL
  for( bc in barcodes ){
    bc_flag <- unlist(lapply( bc_assigns, function(x){
      if( x == bc){
        1
      }else{
        0
      }
    }))

    tmp_df <- tibble( bc_flag )
    if(is.null(barcode_df)){
      barcode_df <- tmp_df
    }else{
      barcode_df <- cbind( barcode_df, tmp_df )
    }
  }

  # barcode_df <- cbind( col_df[".ci"], barcode_df)
  barcode_df <- cbind( df$.ci, barcode_df)

  names(barcode_df) <- append( ".i", barcodes)

  # END of output 2 -> barcode_df
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output 3: Diagnostic plot (yield)
  img_df <- tim::plot_file_to_df(plot_file, filename = plot_file)
  img_df$mimetype <- 'image/png'
  
  img_df$filename <- df$filename[[1]]
  # END of output 3 -> img_df
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  return(lst(assay_df, barcode_df, img_df))
}