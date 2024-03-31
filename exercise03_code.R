# Exercise 3
# Quizzes
# 1
# yes

# 2
# No

# 3
# Melt

# 4
# melt, unite and cast

# 5
# Left, all.x = TRUE

# Tutorial
# Section 00
library(data.table)
library(magrittr)
library(tidyr)

# Section 01 - Tidy Data Warm Up
# 1
# d

# 2
# b

# 3
# it is tidy

# 4
product_dt <- fread(file.path("C:","Users","usertest","Documents","Data Analysis","Lectures","extdata","extdata","example_product_data.csv"))

# 5
product_dt
product_dt <- melt(product_dt, id.vars = "name", measure.vars = c("producta","productb"), variable.name = "product_type", value.name = "number")

# 6
product_dt
dcast(product_dt, ... ~ product_type, value.var = "number")
# It is equal to the original data table, only the order of rows has changed

# Section 02 - Merge Warm Up
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, carname := rownames(mtcars)]
dt1 <- mtcars_dt[5:25,.(carname, mpg, cyl)]
dt2 <- mtcars_dt[1:10, .(carname, gear)]
# 1
merge(dt1, dt2, by = "carname", all = FALSE)
# 6 rows

# 2
merge(dt1, dt2, by = "carname", all.x = TRUE)
# 21 rows

# 3
merge(dt1, dt2, by = "carname", all = TRUE)
# 25 rows

# Section 03 - Weather dataset
# 1
weather_dt <- fread(file.path("C:","Users","usertest","Documents","Data Analysis","Lectures","extdata","extdata","weather.txt"))
weather_dt

# 2
# The dataset is messy as dates are separated (year, month and each day is a column)
# also there are a lot of NA values, making it not very visible

# 3
# In order to make the dataset tidy, I would create a new column with the temperature
# of each data, group up year, month and data

# 4
weather_dt <- melt(weather_dt, id.vars = c("id","year","month","element"), measure.vars = c("d1","d2","d3","d4","d5","d6","d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22",
                                                              "d23", "d24", "d25", "d27", "d28", "d29", "d30", "d31"), variable.name = "day", value.name = "temp")
weather_dt <- dcast(weather_dt,... ~ element, value.var = "temp")  
weather_dt <- na.omit(weather_dt)
weather_dt <- separate(weather_dt, col = day, into = c("d","day"),sep = "(?<=[A-Za-z])(?=[0-9])")
weather_dt[,d := NULL]
weather_dt <- unite(weather_dt, col = date, year, month,day, sep = "/")

# Homework
# Section 04 - Scattered data across many files
# 1
files <- list.files(file.path("C:","Users","usertest","Documents","Data Analysis","Lectures","extdata","extdata","baby-names"), full.names = TRUE)

# 2
names(files) <- basename(files)
baby_names_dt <- lapply(files, fread)
head(baby_names_dt[[1]])

# 3
babys_dt <- rbindlist(baby_names_dt, idcol = "filepath")
head(babys_dt)

# 4 
babys_dt <- separate(babys_dt, col = filepath, into = c("year","gender","path"))
babys_dt <- babys_dt[,path := NULL]

# Section 05 - Small case-study: cleaning up a gene-expression dataset in yeast
# 1
gt <- fread(file.path("C:","Users","usertest","Documents","Data Analysis","Lectures","extdata","extdata","eqtl","genotype.txt"))
growth <- fread(file.path("C:","Users","usertest","Documents","Data Analysis","Lectures","extdata","extdata","eqtl","growth.txt"))

head(gt[,1:5])
class(colnames(gt[,-1]))
gt <- melt(gt, id.vars = "strain", measure.vars = colnames(gt[,-1]),variable.name = "marker", value.name = "gt")

growth <- melt(growth, id.vars = "strain", measure.vars = c("YPD","YPD_BPS","YPD_Rapa", "YPE", "YPMalt"), variable.name = "media", value.name = "growth_rate")

final_dt <- merge(gt, growth, by = "strain", all = FALSE, allow.cartesian = TRUE)


# 4
library(ggplot2)
ggplot(final_dt[marker %in% c('mrk_5211', 'mrk_1653')], aes(marker, growth_rate, color=gt)) +
  geom_boxplot() + facet_wrap(~media)




