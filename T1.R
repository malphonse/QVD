##Preparing to graph Growth curve
##Open Library
library(tidyverse)
library(RColorBrewer)

display.brewer.all()

OD600_LAC


##Select columns to plot----
 
plot <- OD600_LAC %>%
  select(Time, minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`)

##View plot
plot

##Wrangle (Pivot) to make 2 variable to plot

graph <- plot %>%
pivot_longer(c(minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`), 
             names_to = "condition", values_to = "reading"
             )
##View graph

graph

#graph ggplot

a <- ggplot(graph, aes(x = Time, y = reading, color = condition))

Figure1 <- a + geom_smooth() +
  facet_wrap(~condition, nrow = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

Figure1A <- a + geom_smooth() +
  geom_rug(color = "grey50") +
  theme_minimal()

##View Figure
Figure1
Figure1A








