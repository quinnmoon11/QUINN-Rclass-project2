###############################
# Penguin analysis script
#
# This script loads the processed, cleaned data, 
# does a simple analysis and saves the results
# to the results folder
###############################

#To source an R script, open a console in the same directory as the .R file and type 'source("File.R")'

## ---- setup -----
#load needed packages. make sure they are installed.
require(ggplot2) #for plotting
require(magrittr) #for piping
require(knitr) #for formatting output
require(dplyr) #for selecting data

#path to data and results 
data_path <- "../../Data/Processed_data/"
results_path <- "../../Results/"

## ---- functions ----
# function to paste path to output filenames

addpath <- function( filename, path=data_path ) {
    location <- paste( path, filename, sep="")
	return( location )
}

## ---- loaddata ----
# load data. 
dat <- readRDS( addpath("penguins.rds", data_path) )


## ---- summarize ----
# create summary table of the data using skimr to use in paper
# variables, sample size, mean, standard error

sk <- skimr::skim(dat)  # save skim object
sk <- as.data.frame(sk) # save as data.frame
head(sk)  # see the variable names

nrows <- dim(dat)[1] # total number of rows
sk$N <- nrows - sk$n_missing  # sample size of each variable

## ---- summary.table ----
# select only the variable, N, mean, sd, and category counts

sk.table <- sk[c("skim_variable", "N", "numeric.mean", "numeric.sd", "factor.top_counts")]
names(sk.table) <- c("Variable", "N", "Mean", "SE", "Counts") # rename SD as SE
sk.table$SE <- sk.table$SE/sqrt(sk.table$N) # calculate SE

options(knitr.kable.NA = "")
knitr::kable(sk.table, digits=2)


# save summary table
saveRDS(sk.table, file = addpath("summary_table.rds", results_path))


## ---- header ----
######################################
# Data fitting/statistical analysis
######################################

############################
#### First model fit

## ---- mass_sex_aov ----
# fit linear model using mass as outcome, sex as predictor
# notice that some Sex values are set at NA, we want to only include
#samples with known sex, create data subset removing NA
sex_subset <- select(dat, c('Body Mass (g)', Sex))
sex_subset2 <- na.omit(sex_subset)
#sex_subset2 #if you want to verify

lm.fit.s <- lm(`Body Mass (g)` ~ Sex, sex_subset2)  
anova.table.s <- anova(lm.fit.s)

# print to screen the anova table
print(anova.table.s)

# save anova table to file in Results folder  
saveRDS(anova.table.s, file = addpath("mass_sex_anova.rds", results_path))

## ---- mass_sex_boxplot ----
sex_subset <- select(dat, c('Body Mass (g)', Sex))
sex_subset2 <- na.omit(sex_subset)
# plot to screen
with(sex_subset2, plot(`Body Mass (g)` ~ Sex))

# plot to .png file, can also do pdf using `pdf()` function 
png(filename = addpath("mass_sex_bars.png", results_path))
  with(sex_subset2, plot(`Body Mass (g)` ~ Sex))
dev.off()

## ---- mass_sex_density ----
sex_subset <- select(dat, c('Body Mass (g)', Sex))
sex_subset2 <- na.omit(sex_subset)
# create plot and send to screen
p <- sex_subset2 %>%    # mass density by sex
       ggplot( aes(x=`Body Mass (g)`)) + 
       geom_density( aes(fill=Sex), alpha=.5) 
p

# save ggplot objects using `ggsave()` 
ggsave(filename = addpath("mass_sex_density.png", results_path), plot=p) 

## ---- comment2 ----
############################
#### Second model fit
#Make a data set with sex (excluding NA), Mass, and species
second_subset <- select(dat, c('Body Mass (g)', Sex, Species))
second_subset2 <- na.omit(second_subset)
#second_subset2 #if you want to verify 
## ---- mass_sex_species_aov ----
# fit linear model using mass as outcome, species and sex as predictors
second_subset <- select(dat, c('Body Mass (g)', Sex, Species))
second_subset2 <- na.omit(second_subset)

lm.fit.si <- lm(`Body Mass (g)` ~ Species + Sex, second_subset2)  
anova.table.si <- anova(lm.fit.si)
print(anova.table.si)
saveRDS(anova.table.si, file = addpath("mass_species_sex_anova.rds", results_path))

## ---- mass_species_sex_aov ----
# fit linear model again, but fit species first then sex as predictors
second_subset <- select(dat, c('Body Mass (g)', Sex, Species))
second_subset2 <- na.omit(second_subset)

lm.fit.is <- lm(`Body Mass (g)` ~ Sex + Species, second_subset2)  
anova.table.is <- anova(lm.fit.is)
print(anova.table.si)
saveRDS(anova.table.si, file = addpath("mass_sex_species_anova.rds", results_path))

## ---- mass_species_sex_density ----
second_subset <- select(dat, c('Body Mass (g)', Sex, Species))
second_subset2 <- na.omit(second_subset)

# create plot, subseted by species and faceted by species
q <- second_subset2 %>%    # mass density by species
       ggplot( aes(x=`Body Mass (g)`)) + 
       geom_density( aes(fill=c(Sex)), alpha=.5) +
       facet_grid(Species ~ .)
q

ggsave(filename = addpath("mass_species_sex_density.png", results_path), plot=q) 



## ---- comment3 ----
############################
#### Third model fit
## ---- mass_flipper_aov ----
# How do flipper length and mass relate
#we can change column name 
dat2<- dat %>% 
    rename("Flipper_Length" = "Flipper Length (mm)")

lm.fit.f <- lm(`Body Mass (g)` ~ Flipper_Length, dat2)  
anova.table.f <- anova(lm.fit.f)

# print to screen the anova table
print(anova.table.f)

# save anova table to file in Results folder  
saveRDS(anova.table.f, file = addpath("mass_flipper_anova.rds", results_path))

## ---- mass_flipper_boxplot ----

# plot to screen
#with(dat2, plot( Flipper_Length ~ `Body Mass (g)`))


# plot to .pdf file, using `pdf()` function 
#png(filename = addpath("mass_flipper_bars.png", results_path))
  #with(sex_subset2, plot(`Body Mass (g)` ~ Sex))
#dev.off()


#png(filename = addpath("mass_flipper_bars.png", results_path))
 # with(dat2, plot(`Body Mass (g)` ~ Flipper_Length))
#dev.off()

## ---- mass_flipper_plot ----

# create plot and send to screen

p <- ggplot(dat2, aes(x=`Body Mass (g)`, y=Flipper_Length)) +
    geom_point() +
    geom_smooth(method=lm)
p
# save ggplot objects using `ggsave()` 
ggsave(filename = addpath("mass_flipper_density.png", results_path), plot=p) 
