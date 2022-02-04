library(apexcharter)
library(dplyr)
library(forcats)


plotdata <- data.frame(x=c("a","b", "c"), y=5:7, z=1:3, mycolors=c('#FDE7B5','#AAE7B6','#FDA7B6'))

## This works!
apex(
  data = plotdata, 
  type = "column",
  mapping = aes(x = x, y = y, fillColor=mycolors)
) 


## This also works, but one needs to blow up the dataset:
library(tidyr)

plotdata1 <- complete(plotdata,x,z)
plotdata1

apex(
  data = plotdata1, 
  type = "column",
  mapping = aes(x = x, y = y, fill=z)
) %>%
  ax_chart(stacked=TRUE) %>%
  ax_colors(c('#FDE7B5','#AAE7B6','#FDA7B6'))





library("echarts4r")

e_common(
  font_family = "Arial",
  theme = "dark"
)

plotdata <- data.frame(x=c("a","b"), y=5:6)

e_charts(data=plotdata, x=x) %>%
  e_bar(y) %>%
  e_color(c("red", "blue")) 

%>%
  #e_theme("dark") %>%
  e_title("") %>%  # Add title & subtitle
  e_tooltip(trigger = "axis")
  
##########################3

N <- 2 # data points

opts <- list(
  xAxis = list(
    type = "category",
    data = list("a","b")
  ),
  yAxis = list(
    type = "value"
  ),
  series = list(
    list(
      type = "bar",
      data = c(7,9),
      color = "red"
      # itemStyle = list(
      #     color = "blue" #list("blue","red")
      # )
    )
    # list(
    #   type = "bar",
    #   data = list(x="b",y=5),
    #   color = "blue"
    #   # itemStyle = list(
    #   #     color = "blue" #list("blue","red")
    #   # )
    # )
  )
)

e_charts() %>% 
  e_list(opts)
## Only works for funnel charts:

funnel <- data.frame(
  stage = c("View", "Click", "Purchase"),
  value = c(80, 30, 20),
  color = c("blue", "red", "green")
)

funnel %>%
  e_charts() %>%
  e_bar(value, stage) %>%
  e_add("itemStyle", color)

#####
df <- tibble::tibble(
  x = 0:2,
  y = c(6,5,9)
)

library(echarts4r)

e_common(
  font_family = "Arial",
  theme = "light"
)

df %>%
  e_charts(x) %>%
  e_visual_map(
    type = "piecewise",
    calculable = FALSE,
    pieces = list(
      list(gt = 1, color = "green"),
      list(lte = 1, color = "red")
    )
  ) %>%
  e_bar(y)

library(ggplot2)
plotdata <- data.frame(x=c("a","b"), y=5:6, z=factor(1:2))
ggplot(data=plotdata, aes(x=x, y=y, fill=z)) +
  geom_col()
