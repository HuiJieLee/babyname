# subset data with year >= 1980
#babyname1980 <- babyname[(babyname$year >= 1980),]
#babyname1980$year <- factor(babyname1980$year)
#babyname1980 <- tbl_df(babyname1980)

#ddply(babyname1980, .(year), summarise, total = sum(count))

# create new variable that records total number of names for that year and percentage of name relative to all names for that year.
#babyname1980_merge <- babyname1980 %>%
#    group_by(year) %>%  # count total number of names for each year
#    mutate(total = sum(count), prop = count / total) %>%
#    select(-total) %>%  # drop total
#    group_by(year, name) %>% # combine counts from different states
#    summarise(count = sum(count), prop = sum(prop))


