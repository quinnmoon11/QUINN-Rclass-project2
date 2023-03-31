###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the Processed_data folder
#
# Note the ## ---- name ---- notation
# This is called "code chunk labels" and is done so one can 
# pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html
#
# We are also using some tidyverse packages to do the 
# same base R operations weʻve been learning (dplyr, tidyr, skimr)

## ---- packages --------
#load needed packages. make sure they are installed.
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 

## ---- loaddata --------
#path to data
# If you need to check your working directory, use getwd()
# Either restart R in the correct directory, or navigate to it 
# with setwd(...relative path to the correct directory...). 
# NEVER put a setwd() inside your code, it is bad ettiquite as it 
# probably wonʻt work for anyone else. 

data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"
data_path <- "../../Data/Raw_data/"

#load data. 
# I am using check.names=F because these names have spaces and parentheses
# and I want to preserve the original names.

rawdata <- read.csv(data_location, check.names=FALSE)

# We can look in the data dictionary for a variable explanation
# I am using the paste function here to add the path to the filename. 
# sep="" adds no space when pasting. 
# I could have also just saved the complete path to the file and saved 
# it as dictonary_path. It is up to you. 

dictionary <- read.csv(paste(data_path, "datadictionary.csv", sep=""))
print(dictionary)


## ---- exploredata --------

#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
# package::function() that's not required one could just call the function
# specifying the package makes it clearer where the function "lives",
# but it adds typing. You can do it either way.

#take a look at the data
dplyr::glimpse(rawdata)

#another way to summarize the data
summary(rawdata)

#yet another way to get an idea of the data
head(rawdata)

#this is a nice way to look at data
skimr::skim(rawdata)

#See all the data by just typing `rawdata` at the R console.

#Note that the names here have spaces and () which are usable, but 
# force us to quote the character strings. You can either keep doing so 
# rawdata$`Culmen Length (mm)` or rename to something more convenient.
# Personally I would probably shorten everything, but to keep it more
# recognizable for this exercise, I will leave it. Itʻs up to you. 

# names(rawdata) <- c("study", "sampleN", "species", "region", "island", "stage", "id", ... )

## ---- cleandata1 --------
# Inspecting the data, we find some problems that need addressing:
#  First, we know that this is a dataset for three species of penguin, 
#  we notice that there are 9 unique species.

#check skimr or 
unique(rawdata$Species)

# Notice that some of the species names have typos? 
# Letʻs save rawdata as d1, and modify d1 so we can compare versions. 

d1 <- rawdata

#Use the techniques we learned in class to fix these errors. For example,
# we can find the mispelled entry, and replace the whole thing:

ii <- grep("PengTin", d1$Species)
d1$Species[ii] <- "Adelie Penguin (Pygoscelis adeliae)"

#Another way:

d1$Species <- sub("gTin", "guin", d1$Species)

# look at partially fixed data again
unique(d1$Species)

## ---- comment1 --------

# Fix all of the errors. 

#  Also, letʻs shorten Species just keeping the three common names "Adelie", 
#  "Gentoo", and "Chinstrap" and delete the rest of the Species character string. 

# NOTE: Check your work with each change.  Debug as you go, never all at once. 
# Make sure that the code you save in your script works without error. 

# There is an entry for `Culmen Length (mm)` which says "missing" instead of a number or NA. 
# Should we delete this record (and all of the variables)?
# This "missing" entry also turned all culmen length entries into characters instead of numeric.
# That conversion to character also means that our summary function isn't very meaningful.
# So let's fix that first.

## ---- cleandata2 --------

cl <- d1$`Culmen Length (mm)` # OK so typing `Culmen Length (mm)` is really annoying. 
                              # Letʻs make a temporary variable `cl` and save it 
                              # back to d1$`Culmen Length (mm)` when weʻre done. 

cl[ cl == "missing" ] <- NA  # find cl=="missing and replace "missing" with NA
cl <- as.numeric(cl)  # coerce to numeric
d1$`Culmen Length (mm)` <- cl

# another way using dplyr from the tidyverse

# d1 <- d1 %>% 
#			dplyr::mutate( culmenL = replace(culmenL, culmenL=="missing", NA)) %>% 
#			dplyr::mutate( culmenL = as.numeric(culmenL))


# look at partially fixed data again
skimr::skim(d1)
hist(d1$`Culmen Length (mm)`)

# letʻs also do a bivariate plot with mass
plot(d1$`Body Mass (g)`, d1$`Culmen Length (mm)`)

# Notice anything funny? 

# Now we see that there are three penguins with really really long culmens (300+mm) 
#  that could be typos. If we don't know, we might need to remove these penguins
#  But let's suppose that we somehow know that this is because of a misplaced decimal 
#  point (for example if we could verify with field records), and letʻs fix this:

## ---- cleandata3.1 --------
d2 <- d1 
cl[ cl > 300 ] 


## ---- comment2 --------
# notice that NAʻs will match the condition cl>300, because we donʻt really know, 
# so R returns it to be conservative. We donʻt want NAs, so letʻs exclude them with 
# !is.na()  where the ! is "not" or the opposite of is.na.
# The logical & which requires both conditions to be true (i.e., I want to be rich AND famous):

## ---- cleandata3.2 --------
cl[ !is.na(cl) & cl>300 ]

# now replace with the same divided by 10:

cl[ !is.na(cl) & cl>300 ] <- cl[ !is.na(cl) & cl>300 ]/10  

d2$`Culmen Length (mm)` <- cl


#culmen length values seem ok now
skimr::skim(d2)
hist(d2$`Culmen Length (mm)`)

plot(d2$`Body Mass (g)`, d2$`Culmen Length (mm)`)


## ---- comment3 --------
Look better?

# Now let's look at body mass. 
#  There are penguins with body mass of <100g when the others are over 3000. 
#  Perhaps these are new chicks? But they are supposed to be adults. Letʻs remove them.

## ---- cleandata4.1 --------
hist(d2$`Body Mass (g)`)

## ---- comment4 --------
# Mass is the main size variable, so we will probably need to remove the individuals with 
# missing masses in order to  be able to analyze the data. 
# Note: Some analysis methods can deal with missing values, so it's not always necessary 
# Or it may be fine to have it in some of the variables but probably not the size variable. 
# This should be adjusted based on your planned analysis approach. 

## ---- cleandata4.2 --------
d3 <- d2
mm <- d3$`Body Mass (g)`

mm[ mm < 100 ] <- NA       # replace tiny masses with NA
nas <- which( is.na(mm) )  # find which rows have NA for mass

d3 <- d3[ -nas, ]   # drop the penguins (rows) with missing masses


skimr::skim(d3)
hist(d3$`Body Mass (g)`)

plot(d3$`Body Mass (g)`, d3$`Culmen Length (mm)`)

## ---- comment5 --------
# Does it look better?

# We also want to have Species, Sex, and Island coded as a categorical/factor variable

## ---- cleandata5 --------
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
skimr::skim(d3)

## ---- bivariateplots --------
# Make bivariate plots for any remaining continous data to ensure there are no further
# errors. It is a good check on the distribution of the data as well. 

# Make histograms or densities of at least mass by discrete category, to check for any 
# potential category errors, extra categories, etc.  

# You should look through all of the data, variable by variable, or by pairs of variables.


## ---- finalizedata --------
# Finalize your cleaned dataset. Drop any variables (columns) from the dataframe 
# that you wonʻt analyze. If you have extra levels after dropping some values, 
# you may want to relevel your factors (this may or may not happen). 

# To specify new levels or orders of levels:
# new_factor <- factor(vec_extra_levels, levels=c("level1", "level2", ... ))

# to drop empty levels:
# new_factor <- droplevels(vec_extra_levels)


## ---- comment6 --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above

## ---- savedata --------
processeddata <- d3      # change if you did more steps

# location to save file

save_data_location <- "../../Data/Processed_data/penguins.rds"
saveRDS(processeddata, file = save_data_location)

save_data_location_csv <- "../../Data/Processed_data/penguins.csv"
write.csv(processeddata, file = save_data_location_csv, row.names=FALSE)


## ---- notes --------
# anything you don't want loaded into the Quarto file but 
# keep in the R file, just give it its own label and then don't include that label
# in the Quarto file. 
# For example, you may try excluding some of the excessive comments. 

# Dealing with NA or "bad" data:
# removing anyone who had "faulty" or missing data is one approach.
# it's often not the best. based on your question and your analysis approach,
# you might want to do cleaning differently (e.g. keep individuals with some missing information)

# Saving data as RDS:
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# However, CSV is better for sharing with others since it's plain text. 

# If you do CSV, you must to write down somewhere what each variable is.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
