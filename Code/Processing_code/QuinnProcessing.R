###############################
# The following script serves to visualize and summarize the data followed with some data cleaning
#The first step is to install the following packages

#To source an R script, open a console in the same directory as the .R file and type 'source("File.R")'

## ---- packages --------
#Install needed packages
#install.packages(dplyr) 
#install.packages(tidyr) 
#install.packages(skimr)
#install.packages(ggplot2) 

#load needed packages. make sure they are installed.
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(skimr) #for nice visualization of data 
require(ggplot2) #for making plots

#Ensure you are working in Code/Processing_code
getwd()

## ---- loaddata --------
#path to data
getwd()
data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"
data_path <- "../../Data/Raw_data/"
processed_data_path <- "../../Data/Processed_data/"

#load data. 
# I am using check.names=F because these names have spaces and parentheses
# and I want to preserve the original names.

rawdata <- read.csv(data_location, check.names=FALSE)

# We can look in the data dictionary for a variable explanation
# I am using the paste function here to add the path to the filename. 
# sep="" adds no space when pasting. 

dictionary <- read.csv(paste(data_path, "datadictionary.csv", sep=""))
print(dictionary)


## ---- exploredata --------

#take a look at the data
dplyr::glimpse(rawdata)

#another way to summarize the data
summary(rawdata)

#yet another way to get an idea of the data
head(rawdata)
tail(rawdata)

#this is a nice way to look at data
skimr::skim(rawdata)

# rawdata #If you want to see all the raw data - just type 'rawdata'

## ---- cleandata1 --------
# Inspecting the data, we find some problems that need addressing:
#  First, we know that this is a dataset for three species of penguin, 
#  we notice that there are 9 unique species.

#check skimr or 
unique(rawdata$Species)

# Notice that some of the species names have typos 
# Letʻs save rawdata as d1, and modify d1 so we can compare versions. 

d1 <- rawdata

#fix these errors in Species names

d1$Species <- sub("gTin", "guin", d1$Species)

# look at partially fixed data again- repeat everytime (if needed)
unique(d1$Species)

d1$Species <- sub("gTin", "guin", d1$Species)
#unique(d1$Species)
d1$Species <- sub("PeOg", "Peng", d1$Species)
#unique(d1$Species)
d1$Species <- sub("AdeKie", "Adelie", d1$Species)
#unique(d1$Species)
d1$Species <- sub("lieM", "lie ", d1$Species)
#unique(d1$Species)
d1$Species <- sub("Ven", "Gen", d1$Species)
unique(d1$Species) #verify names corrected

#shorten names
ii <- grep("(Pygoscelis adeliae)", d1$Species)
d1$Species[ii] <- "Adelie"
#unique(d1$Species) #to verify name was shortened
iii <- grep("(Pygoscelis papua)", d1$Species)
d1$Species[iii] <- "Gentoo"
#unique(d1$Species) only check if needed
iiii <- grep("(Pygoscelis antarctica)", d1$Species)
d1$Species[iiii] <- "Chinstrap"
unique(d1$Species) #verify names shortened

## ---- comment1 --------

# There is an entry for `Culmen Length (mm)` which says "missing" instead of a number or NA. 
# This "missing" entry turned all culmen length entries into characters instead of numeric.
# That conversion to character also means that our summary function isn't very meaningful.
# So let's fix that first.

## ---- cleandata2 --------

cl <- d1$`Culmen Length (mm)` # OK so typing `Culmen Length (mm)` is really annoying. 
                              # Letʻs make a temporary variable `cl` and save it 
                              # back to d1$`Culmen Length (mm)` when weʻre done. 

cl[ cl == "missing" ] <- NA  # find cl=="missing and replace "missing" with NA
cl #ensure "missing" was replaced
cl <- as.numeric(cl)  # coerce to numeric
class(cl) #ensure numeric
head(cl) #quotes should now be gone from around numbers
d1$`Culmen Length (mm)` <- cl

# another way using dplyr from the tidyverse

# d1 <- rawdata %>% dplyr::filter( `Culmen Length (mm)` != "missing" ) %>% 
#             dplyr::mutate( `Culmen Length (mm)` = as.numeric(`Culmen Length (mm)`))


# look at partially fixed data again
skimr::skim(d1)
hist(d1$`Culmen Length (mm)`)

# letʻs also do a bivariate plot with mass
plot(d1$`Body Mass (g)`, d1$`Culmen Length (mm)`)


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

# Now let's look at body mass. 
#  There are penguins with body mass of <100g when the others are over 3000. 
#  Perhaps these are new chicks? But they are supposed to be adults. Letʻs remove them.

## ---- cleandata4.1 --------
hist(d2$`Body Mass (g)`)

## ---- comment4 --------
# Mass is the main size variable, so we will probably need to remove the individuals with 
# missing masses in order to  be able to analyze the data.  

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

# We also want to have Species, Sex, and Island coded as a categorical/factor variable

## ---- cleandata5 --------
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
skimr::skim(d3)

## ---- bivariateplots --------
# Make bivariate plots for any remaining continous data to ensure there are no further
# errors. It is a good check on the distribution of the data as well. 
#check distributions
hist(d3$`Culmen Depth (mm)`)
hist(d3$`Flipper Length (mm)`)
hist(d3$`Body Mass (g)`)
hist(d3$`Delta 15 N (o/oo)`)
hist(d3$`Delta 13 C (o/oo)`)
#creating bivariate plots- comparing to Body Mass
plot(d3$`Body Mass (g)`, d3$`Culmen Depth (mm)`)
plot(d3$`Body Mass (g)`, d3$`Flipper Length (mm)`)
plot(d3$`Body Mass (g)`, d3$`Culmen Depth (mm)`)
plot(d3$`Body Mass (g)`, d3$`Delta 15 N (o/oo)`)
plot(d3$`Body Mass (g)`, d3$`Delta 13 C (o/oo)`)
plot(d3$`Delta 15 N (o/oo)`, d3$`Delta 13 C (o/oo)`)
# Make barplots or densities of least mass by discrete category
#Can also do this visualization (and others) in ggplot2
#ggplot2 was loaded earlier

#map densities- can be done on all continous data
#ggplot(d3, aes(x = Island)) +
 # geom_density()
  
#using ggplot histogram/barplots of body mass vs discrete categories 
ggplot(d3, aes(x = Species, y = `Body Mass (g)`)) + 
	geom_col()
ggplot(d3, aes(x = Sex, y = `Body Mass (g)`)) + 
	geom_col()
ggplot(d3, aes(x = Island, y = `Body Mass (g)`)) + 
	geom_col()


## ---- finalizedata --------
# Finalize your cleaned dataset. Drop any variables (columns) from the dataframe 
# Drop  
d4 = subset(d3, select = -c(studyName,Comments))


# To specify new levels or orders of levels:
# new_factor <- factor(vec_extra_levels, levels=c("level1", "level2", ... ))

# to drop empty levels:
# new_factor <- droplevels(vec_extra_levels)


## ---- comment6 --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above

## ---- savedata --------
processeddata <- d4      # change if you did more steps

# location to save file

save_data_location <- "../../Data/Processed_data/processeddata.rds"
saveRDS(processeddata, file = save_data_location)

save_data_location_csv <- "../../Data/Processed_data/processeddata.csv"
write.csv(processeddata, file = save_data_location_csv, row.names=FALSE)

## ---- newdatadictionary --------
#If needed, the updated data dictionary can be loaded
newdictionary <- read.csv(paste(processed_data_path, "Quinndatadictionary.csv", sep=""))
save_newdic_location_csv <- "../../Data/Processed_data/Quinndatadictionary.csv"
write.csv(newdictionary, file = save_newdic_location_csv, row.names=FALSE)
## ---- notes --------
# Finished Project 1! 

