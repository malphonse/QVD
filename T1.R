##Preparing to graph Growth curve
##Open Library
library(tidyverse)
library(RColorBrewer)

?RColorBrewer
display.brewer.all()

#Convert to tibble
GC_lac <- as.tibble(OD600_LAC)

GC_lac
view(GC_lac)

##Select columns to plot----
 
plot <- GC_lac %>%
  select(Time, minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`)

plot

##Wrangle (Pivot) to make 2 variable to plot

graph <- plot %>%
pivot_longer(c(minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`), 
             names_to = "condition", values_to = "reading"
             )
#graph ggplot

a <- ggplot(graph, aes(x = Time, y = reading, color = condition))

b <- a + geom_point() + geom_rug()

b + theme_minimal()










