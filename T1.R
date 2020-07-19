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

#graph with ggplot

a <- ggplot(graph, aes(x = Time, y = reading, color = condition))

Figure1 <- a + geom_smooth() +
  facet_wrap(~condition, nrow = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

Figure1A <- a + geom_smooth(method = "loess", size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View Figure
Figure1
Figure1A + theme(legend.position = "top")

##Using mutate to calculate mean----

##Calcuate mean and add new column with values 
av <- OD600_LAC %>%
  mutate(minusQVD = c(`LAC::lux - QVDOPH` + `LAC::lux - QVDOPH_1` +`LAC::lux - QVDOPH_2`)/ 3 ) %>%
  mutate(`10ugQVD` = c(`LAC::lux +10ug/mL QVDOPH` + `LAC::lux +10ug/mL QVDOPH_1` + `LAC::lux +10ug/mL QVDOPH_2`)/3 ) %>%
  mutate(`100ugQVD` = c(`LAC::lux +100ug/mL QVDOPH` + `LAC::lux +100ug/mL QVDOPH_1` + `LAC::lux +100ug/mL QVDOPH_2`)/3) %>%
  mutate(`20mgQVD` = c(`LAC::lux + 20mg/kg (35gms) QVDOPH` + `LAC::lux + 20mg/kg (35gms) QVDOPH_1` + `LAC::lux + 20mg/kg (35gms) QVDOPH_2`)/3)

av

##Preparing a sub-table for plotting graph
av_graph <- av %>%
  select(`Time`, minusQVD, `10ugQVD`, `100ugQVD`, `20mgQVD`)

##Pivot the coloumns for graph
renderav_graph <- av_graph %>%
  pivot_longer(c(minusQVD, `10ugQVD`, `100ugQVD`, `20mgQVD`), 
               names_to = "condition", values_to = "reading")

##Graphing
lacfig <- ggplot(renderav_graph, aes(x = Time, y = reading, color = condition))


FigureA1 <- lacfig + geom_smooth(method = "loess", size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View graph
Figure1A + theme(legend.position = "top")









