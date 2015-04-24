# check data set
str(babyname)
summary(babyname)

# What is the most popular name of all time? (Of either gender.)
head(sort(tapply(babyname$count, babyname$name, sum), decreasing = TRUE))

# what is the most gender ambiguous name in 2013? 1945?
genderNeutral(babyname, 2013)
genderNeutral(babyname, 1945)

# Of the names represented in the data, find the name that has had the largest 
# percentage increase in popularity since 1980. Largest decrease?

# combine counts from different states and different gender
allYear <- aggregate(babyname$count, by = list(babyname$year, babyname$name), FUN = sum)
names(allYear) <- c("year", "name", "count")

# compute the total number of names for each year
countByYear <- ddply(allYear, .(year), summarise, total = sum(count))
# join allYear with countByYear by "year"
allYear <- merge(allYear, countByYear, by.x = "year", by.y = "year")
# create new variable: proportion of a given name relative to all names in that year
allYear <- mutate(allYear, prop = count / total)

# compute differences of prop between 1980 and 2013 for each name.
year1980 <- allYear[(allYear$year == 1980),]
year2013 <- allYear[(allYear$year == 2013),]
# inner join two data sets using name
combineTable <- merge(year1980, year2013, by.x = "name", by.y = "name")
combineTable <- mutate(combineTable, prop.diff = prop.y - prop.x)

# Top 10 names increase in popularity
head(arrange(combineTable, desc(prop.diff)), n = 10)

# Top 10 names decrease in popularity
head(arrange(combineTable, desc(-prop.diff)), n = 10)


# Can you identify names that may have had an even larger increase or decrease 
# in popularity?

year1910 <- allYear[(allYear$year == 1910),]
# inner join two data sets using name
combineTable2 <- merge(year1910, year2013, by.x = "name", by.y = "name")
combineTable2 <- mutate(combineTable2, prop.diff = prop.y - prop.x)

# Top 10 names increase in popularity
head(arrange(combineTable2, desc(prop.diff)), n = 10)
topNameIncrease <- arrange(combineTable2, desc(prop.diff))$name[1:10]

# Top 10 names decrease in popularity
head(arrange(combineTable2, desc(-prop.diff)), n = 10)
topNameDecrease <- arrange(combineTable2, desc(-prop.diff))$name[1:10]

# drop count and total
propAllYear <- select(allYear, -count, -total)

plotNameList(propAllYear, topNameIncrease)
plotNameList(propAllYear, topNameDecrease)


# Do people prefer longer names? nchar()
propAllYear$name <- as.character(propAllYear$name)
# create new variable: length of each name
nameLength <- mutate(propAllYear, nchar = nchar(name))
# delete name column
select(nameLength, -name)
# combine prop for the same length of name for each year
nameLength <- ddply(nameLength, .(year, nchar), summarise, total = sum(prop))

# plot the name length trend 
plots <- list()
g <- ggplot(nameLength, aes(year, total, group = nchar, color = nchar))
plots[[1]] <- g + geom_line() + scale_color_gradient2(midpoint = 9, low = "blue", mid = "white", high = "red")

# plot the trend for each name length (easier to check the trend)
for (i in 2:15) {
  nameLength_sub <- nameLength[(nameLength$nchar == i),]
  myTitle <- paste("Name length = ", i)
  g <- ggplot(nameLength_sub, aes(year, total)) + geom_line() + labs(x = "Year", y = "Popularity", title = myTitle)
  plots[[i]] <- g
}
multiplot(plotlist = plots, cols = 3)
#temp <- mutate(propAllYear, nchar = nchar(name))


# most regional names: top 5 names by gender for each region in 2013
top5_NW2013 <- topName(babyname, NorthWest, 2013, 5)
top5_MW2013 <- topName(babyname, MidWest, 2013, 5)
top5_S2013 <- topName(babyname, South, 2013, 5)
top5_W2013 <- topName(babyname, West, 2013, 5)

region <- c("NorthWest", "MidWest", "South", "West")
plots <- list()
tops <- list(top5_NW2013, top5_MW2013, top5_S2013, top5_W2013)
for (i in seq(along = region)) {
  myTitle <- region[i]
  g <- ggplot(tops[[i]], aes(x = factor(name), y = total, fill = name)) + geom_bar(stat = "identity") + labs(x = "Name", y = "Count", title = myTitle)
  plots[[i]] <- g
}
multiplot(plotlist = plots, cols = 2)

# does new york or california lead the trend?
top5_NY <- topName(babyname, "NY", 2013, 5)
top5_CA <- topName(babyname, "CA", 2013, 5)

# Their top 2 are both Sophia and Jacob.
# Let's check these two name trend in NY and CA.

NY_data <- propNameInState(babyname, "NY")
CA_data <- propNameInState(babyname, "CA")
NC_data <- propNameInState(babyname, "NC")

plotNameTrendInState(NY_data, CA_data, NC_data, "NY", "CA", "NC", "Sophia")
plotNameTrendInState(NY_data, CA_data, NC_data, "NY", "CA", "NC", "Jacob")

# how is the prediction by Freaknomics?



