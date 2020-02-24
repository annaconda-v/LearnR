## Plot position of assembly gaps in the human reference genome ##
##################################################################
## Load required libraries
library(GenomicRanges)
library(biovizBase)
library(ggplot2)
library(ggbio)

## Load the data
gaps <- read.table("/home/porubsky/Rintro/UCSCtracks/gaps_GRCh38.gz", stringsAsFactors = FALSE)
# head(gaps) To check column names in the data.frame
## Convert data.frame to Genomic ranges
gaps.gr <- makeGRangesFromDataFrame(gaps, seqnames.field = 'V2', start.field = 'V3', end.field = 'V4')
## Call plotting function
ideo.plt <- plotBandedIdeogram(granges = gaps.gr, genome = 'hg38', title = "Assembly gaps positions in the human genome")
# ideo.plt To visualize ideogram

## Function definition
plotBandedIdeogram <- function(granges=NULL, genome='hg38', title=NULL) {
  
  ## Prepare data for plotting
  ## Set desired chromosomes to plot chr1-22 and X
  chromosomes <- paste0('chr', c(1:22,'X'))
  ## Get coressponding ideogram from the database
  suppressMessages( hg38IdeogramCyto <- getIdeogram(genome, cytobands = TRUE) )
  ## Remove chromosomes that are not defined in chromosomes
  hg38IdeogramCyto <- keepSeqlevels(hg38IdeogramCyto, value = chromosomes, pruning.mode = 'coarse')
  seqlevels(hg38IdeogramCyto) <- chromosomes
  ## Set the ggplot theme for final chromosome ideogram
  theme_vertical <- theme(legend.position ="top",
                          axis.line = element_blank(),
                          axis.text.x=element_blank(), 
                          axis.ticks.x=element_blank(),   
                          strip.text.y = element_text(angle = 180),
                          strip.text.x = element_text(size = 7, face="bold"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          panel.spacing.x=unit(1.5, "lines"))
  ## Convert ideogram bands stored in GRanges object into the data.frame
  ideo.df <- as.data.frame(hg38IdeogramCyto)
  
  ## Get chromosome breaks and labels
  max.len <- signif(max(ideo.df $end), digits = 2)
  breaks <- seq(from = 0, to = max.len, length.out = 6)
  labels <- breaks / 1000000
  labels <- paste0(labels, 'Mb')
  chr.num <- length(unique(ideo.df $seqnames))
  
  ## Plot the ideogram using ggplot2
  ideo <- ggplot() + 
    geom_rect(ideo.df, aes(ymin=start, ymax=end, xmin=-0.1, xmax=0.1, fill=gieStain), color='black', show.legend=FALSE) + 
    scale_fill_giemsa() +
    facet_grid(. ~ seqnames, switch = 'x') + 
    scale_y_continuous(breaks = breaks, labels = labels) +
    theme_vertical +
    xlab("") +
    ylab("")
  
  ## Remove chromosomes that are not defined in chromosomes
  granges <- keepSeqlevels(granges, value = chromosomes, pruning.mode = 'coarse')
  ## Convert user supplied GRanges object into the data.frame
  ranges.df <- as.data.frame(granges)
  ## Get the mid position of each genomic ranges
  ranges.df$midpoint <- ranges.df$start + ((ranges.df$end - ranges.df$start) / 2)
  ranges.df$xpos <- 0.15
  ranges.df$shape <- 60
  ideo <- ideo + geom_point(data=ranges.df, aes(y=midpoint, x=xpos), color="red", size=6, shape=60, inherit.aes=FALSE)
  
  ## Add title
  if (!is.null(title)) {
    ideo <- ideo + ggtitle(title)
  }
  
  ## Return final plot
  return(ideo)
}
