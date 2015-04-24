## This function compute the proportion of female/male for a given name.
## The proportion of female for a given name is called propF. 
## To compare gender ratio, I compute the absolute difference between propF and 0.5,
## where 0.5 is the case when equal number of male/female have the given name.
## This absolute difference is called deviance, and the smaller the deviance, the
## more gender-neutral the name is (more gender ambiguous).

genderNeutral <- function(data, year) {
  #subset data
  data <- data[(data$year == year),]
  #combine counts from different states and drop year
  data <- aggregate(data$count, by = list(data$year, data$name, data$gender), FUN = sum)
  data <- data[,-1]
  names(data) <- c("name", "gender", "count")
  # spread the data for two gender level
  byName <- spread(data, gender, count)
  # replace NA with 0
  byName[is.na(byName)] <- 0
  # create new variable that records proportion of female for each name
  byName <- mutate(byName, propF = F/(F+M), deviance = abs(F/(F+M) - 0.5))
  print(head(arrange(byName,deviance, desc(F)), n = 10))
}

## This function plot the trend for a given name
plotNameTrend <- function(data, name) {
  nameData <- data[(data$name == name),]
  g <- ggplot(nameData, aes(year, prop)) + geom_line() + labs(x = "Year", y = "Popularity", title = nameData$name)
  return(g)
}

## This function create a multiplot for a list of name using the plotNameTrend() function.
plotNameList <- function(data, nameList) {
  plots <- list()
  for (i in seq(along = nameList)) {
    g <- plotNameTrend(data, nameList[i])  
    plots[[i]] <- g
  }
  multiplot(plotlist = plots, cols = 2)
}

## This function find the top names for a year in a given region.
topName <- function(data, region, year, top) {
  temp <- data[(data$state %in% region & data$year == year),]
  temp <- ddply(temp, .(name), summarise, total = sum(count))
  temp <- arrange(temp, desc(total))[1:top,]
  return(temp)
}

## This function compute the popularity of a name in a state 
propNameInState <- function(data, state) {
  temp <- select(data[(data$state == state),], -state)

  # combine counts from different gender
  temp <- aggregate(temp$count, by = list(temp$year, temp$name), FUN = sum)
  names(temp) <- c("year", "name", "count")
  
  # compute the total number of names for each year in NY
  temp_countByYear <- ddply(temp, .(year), summarise, total = sum(count))
  # join allYear with countByYear by "year"
  temp <- merge(temp, temp_countByYear, by.x = "year", by.y = "year")
  # create new variable: proportion of a given name relative to all names in that year
  temp <- mutate(temp , prop = count / total)
  return(temp)
}

## This function plot the name trend for three given states.
plotNameTrendInState <- function(data1, data2, data3, state1, state2, state3, name) {
  temp1 <- data1[(data1$name == name),]
  temp1$state <- state1
  temp2 <- data2[(data2$name == name),]
  temp2$state <- state2
  temp3 <- data3[(data3$name == name),]
  temp3$state <- state3
  temp <- rbind(temp1, temp2, temp3)
  
  myTitle <- paste("Trend in", name)
  g <- ggplot(temp, aes(year, prop, group = state, color = state)) + geom_line() + labs(x = "Year", y = "Popularity in state", title = myTitle)
  return(g)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
