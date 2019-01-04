#' @import rtracklayer
#' @import GenomicRanges

#' @title normalize gene expressions for sra metadata database
#' @param file file:filename
#' @param path path used in the .sh file
#' @param chr chr specify which chromosome is inspected
#' @param start start specify the starting position of the inspected gene
#' @param end end specify the ending position of the inspected gene
#' @return a normalized gene expression dataframe 
#'
#' @export
#' 
#' @examples 
#' normalized(file = scan("sra_samples.txt", what="", sep="\n"), path = path, chr = "chrY", start = 14813160, end = 14972768)



normalized <- function(file,path,chr,start,end) {
  
  extract.block <- function(files, chr, start, end, verbose = TRUE){
    rl <- IRanges::RangesList(IRanges::IRanges(start=start, end=end))
    names(rl) <- chr
    rles <- lapply(files, function(xx) {
      import(xx, as = "Rle", format = "bw", selection = BigWigSelection(rl))
    })
    megaMatrix <- do.call(cbind, lapply(rles, function(xx)      as.numeric(xx[[chr]][start:end])))
    
  }
  
  counts <- read.table(file = 'counts.tsv.gz',header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  normalized_values <- rep(NA,length(file))
  
  if(length(file)>=1)
  {
    for(i in 1:length(file))
    {
     
      c <- grep((strsplit(file[i],'.bw')[[1]][1]),counts$X)
      total <- as.numeric(strsplit(as.character(counts[c,'total.mapped.reads']),',')[[1]][1])
      
      block <- extract.block(files = paste0(path, file[i]),
                             chr = chr,
                             start = start,
                             end = end,
                             verbose = TRUE)
      
      x <- mean(block/(total))* 40000000
      normalized_values[i] <- x
    }
   
    gene  <- data.frame(f,normalized_values)
    return(gene)
  }
}

