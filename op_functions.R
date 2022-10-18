do.debarcoding <- function( df, sample_key  ){
  
  df_ff <- NULL
  data_chans <- unlist(as.list(row_df[,1]))

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

  pops <- names(sample_key[,1])
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


  cutoff <- ctx$op.value('Separation Cutoff', as.double, -1)
  if( cutoff == -1 ){
    sce <- CATALYST::applyCutoffs(sce)
  }else{
    sce <- CATALYST::applyCutoffs(sce,  sep_cutoffs = 0.35)
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