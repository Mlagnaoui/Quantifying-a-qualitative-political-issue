library(maps)
library(ggplot2)
library(plyr)

#load the map drawing requirements

eu <- c("Austria", "Belgium", "Bulgaria",
        "Croatia","Cyprus","Czech Republic",
        "Denmark","Estonia","Finland","France",
        "Germany","Greece","Hungary","Ireland",
        "Italy","Latvia","Lithuania","Luxembourg",
        "Malta","Netherlands","Poland","Portugal",
        "Romania","Slovakia","Slovenia","Spain","Sweden"
)

eu_map <- map_data("world", region=eu)
####################################



# setting a clean background for the map
theme_clean <- function(base_size = 12) {
  require(grid) 
  theme_grey(base_size) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "cm"),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      complete = TRUE
    )
}
####################################

#build the drawing function
f <- function(){
  
  politics <- data.frame(region=eu,
                         orientation = sample(c('left','right'),27,replace=T)
  )
  
  eu_politics_map <- merge(eu_map,politics,by.x='region',by.y='region')
  eu_politics_map <- arrange(eu_politics_map,group,order)
  
  ggplot(eu_politics_map, aes(x=long, y=lat, group=group,fill=orientation)) + geom_polygon(colour="black") + theme_clean() #+ scale_fill_manual(values=c('darkred','darkblue'))
}

####################################


