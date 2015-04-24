# Get the data from the web
fileUrl <- "http://www.ssa.gov/oact/babynames/state/namesbystate.zip"
download.file(fileUrl, destfile = "namesbystate.zip")
unzip("namesbystate.zip")

# Batch import files
files <- list.files(pattern = ".TXT$")
babyname <- lapply(files, read.table, sep = ",")
babyname <- do.call(rbind,babyname)

# Set column names
names(babyname) <- c("state", "gender", "year", "name", "count")

NorthWest <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
MidWest <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
South <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
West <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
