# Created: 2019.09.02
# By: Stephanie R Hilz
# Usage: Takes a vector of values and returns an analagous vector of colors from a heatmap gradient of the vector values

library(RColorBrewer)

colorByFeatureMain <- function(vector){
  rbPal <- colorRampPalette(c("blue","red"))
  mappedColors <- rbPal(length(vector))[as.numeric(cut(vector,breaks = length(vector)))][1:length(vector)]
  names(mappedColors) <- names(vector)
  return(mappedColors)
}