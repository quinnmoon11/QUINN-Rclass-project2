# README.md

This folder contains the raw data for this project. We begin with the data provided with the Palmerpenguins R package, which you can install in R using `install.packages("palmerpenguins")`

Generally, any dataset should contain some meta-data explaining what each variable in the dataset is, or a **Data Dictionary**. 

This dataset contains 17 columns, 16 data columns and one comment field, as explained in `datadictionary.csv` or the table below. 

Raw data sets should **generally not be edited by hand**. It should instead be loaded and processed/cleaned using code.

Not all of these columns are interesting for analysis (e.g., `studyName`) and you may want to drop them in your processed data. 

As the raw data that comes with the package `penguins_raw` are pretty clean, I purposely prepared a dirtier version for you to work on that will provide a more realistic data analysis pipeline experience.

`penguins_raw_dirty.csv` contains some random data entry errors that need to be cleaned. If you want to see how this was created, check out `Project_template/Code/Processing_code/typos.R`

Your first task is to find the faulty values and clean them using the techinques weʻve been learning in class, and save the cleaned dataset in `../Processed_data/`. Please save both `.rda` and `.csv` versions, and include a data dictionary in the `README.md`.  

The code for processing the raw data should be in **Project_template > Code > Processing_code**, and should run from that working directory.  Please use **relative paths** from that working directory to load and save files. 


# Data Dictionary `penquins_raw.csv`

|variable| description|
|----------|--------------|
| studyName | Sampling expedition from which data were collected, generated, etc.| 
| Sample Number | an integer denoting the continuous numbering sequence for each sample| 
| Species | a character string denoting the penguin species| 
| Region | a character string denoting the region of Palmer LTER sampling grid| 
| Island | a character string denoting the island near Palmer Station where samples were collected| 
| Stage | a character string denoting reproductive stage at sampling| 
| Individual ID | a character string denoting the unique ID for each individual in dataset| 
| Clutch Completion | a character string denoting if the study nest observed with a full clutch, i.e., 2 eggs| 
| Date Egg | a date denoting the date study nest observed with 1 egg (sampled)| 
| Culmen Length | a number denoting the length of the dorsal ridge of a bird's bill (millimeters)| 
| Culmen Depth | a number denoting the depth of the dorsal ridge of a bird's bill (millimeters)| 
| Flipper Length | an integer denoting the length penguin flipper (millimeters)| 
| Body Mass | an integer denoting the penguin body mass (grams)| 
| Sex | a character string denoting the sex of an animal| 
| Delta 15 N | a number denoting the measure of the ratio of stable isotopes 15N:14N| 
| Delta 13 C | a number denoting the measure of the ratio of stable isotopes 13C:12C| 
| Comments | a character string with text providing additional relevant information for data| 


