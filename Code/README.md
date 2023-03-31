# Code Folder

## Code Location:

Place your various `.R` or `.qmd` scripts in the appropriate folders:

- Processing_code for cleaning raw data to processed data
- Analysis_code or your analyses on cleaned data. 

Depending on your specific project, you might want to have further sub-folders.

## Code Design:

You can either have fewer large scripts, or multiple scripts that do only specific actions. Those can be R scripts or qmd files. In either case, document the scripts and what goes on in them so well that someone else (including future you) can easily figure out what is happening.

The scripts should load the appropriate data (e.g. raw or processed), perform actions, and save results (e.g. processed data, figures, computed values) in the appropriate folders. 

Be sure to use **relative paths** so that each script works from the working directory set to the folder that the script is in. 

## Required Documentation:

-  What inputs each script takes and where output is placed. 
-  If scripts need to be run in a specific order, document this. 
-  Documentation could be either as comments in the script, or in a separate text file such as this readme file. Ideally of course in both locations, especially for more complex projects. 

