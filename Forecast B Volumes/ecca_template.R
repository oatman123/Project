# Impose the ECCA Chart style. 
# This function works when you create a plot using the 'ggplot2' package.
# It basically removes the background color and positions the legend horizontally in the bottom and allows to change the font and its size
# You can call the function eccaTheme(fontSize,fontFamily)
eccaTheme <- function(base_size = 20, base_family = "",...){
  require(ggplot2)
  theme_set(theme_bw(base_size, base_family))
  theme_update(legend.position="bottom", legend.box = "horizontal")
}
eccaChartColors <- c("#329169","#F58C3C","#1E50AA","#FFC832","#76A305","#965096", "#91969B","#46BEF5","#D70087","#3C0082","#D20F46","#C3C8CD") # From ECCA Chart Style Guide
