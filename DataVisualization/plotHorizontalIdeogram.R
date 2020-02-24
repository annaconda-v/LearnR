## Plot position of genomic ranges on a horizontal ideogram ##
##############################################################
## Load required libraries
library(GenomicRanges)
library(BSgenome.Hsapiens.UCSC.hg38)
library(biovizBase)

## Load the data
SDs <- read.table("/home/porubsky/Rintro/UCSCtracks/segDups_GRCh38.gz", stringsAsFactors = FALSE)
# head(SDs) To check column names in the data.frame
## Convert data.frame to Genomic ranges
SDs.gr <- makeGRangesFromDataFrame(SDs, seqnames.field = 'V2', start.field = 'V3', end.field = 'V4')
## Call plotting function
ideo.plt <- plotHorizontalIdeogram(granges = SDs.gr, bsgenome = BSgenome.Hsapiens.UCSC.hg38)
# ideo.plt To visualize ideogram

## Function definition
plotHorizontalIdeogram <- function(granges=NULL, bsgenome=NULL) {
  ## Use standard chromosomes only
  chroms <- paste0('chr', c(1:22, 'X','Y'))
  
  ## Prepare ideogram plot ##
  ## Get chromosome lengths
  seq.len <- seqlengths(bsgenome)[chroms]
  ## Prepare data.frame to plot
  ideo.df <- data.frame(seqnames=names(seq.len), length=seq.len)
  ## Define factor to store chromosome names
  ideo.df$seqnames <- factor(ideo.df$seqnames, levels=chroms)
  ## Remove chromosomes that are not defined in chromosomes
  granges <- keepSeqlevels(granges, value = chroms, pruning.mode = 'coarse')
  ## Convert user supplied GRanges object into the data.frame
  plt.df <- as.data.frame(granges)
  
  ## Plot contigs on ideogram
  plt <- ggplot2::ggplot() + geom_rect(data = ideo.df, aes(xmin=0, xmax=length, ymin=0, ymax=1), fill="white", color="black") +
    facet_grid(seqnames ~ ., switch = 'y') +
    geom_rect(data=plt.df , aes(xmin=start, xmax=end, ymin=0, ymax=1), fill='orange') +
    scale_x_continuous(expand = c(0,0)) +
    theme_void() +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(strip.text.y = element_text(angle = 180))

  ## Return final plot
  return(plt)
}
