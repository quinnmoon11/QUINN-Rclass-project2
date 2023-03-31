# Data Folder

The folders in Data should contain all of the data used in this project. Subfolders are used for the various stages. 

The `Data` is being loaded/manipulated/reshaped/saved using code from the `Code` folder. 

## Raw Data

Raw data goes in the `Raw_data` folder and do not edit it! It should be archived as you received it. It is the starting point, so do not change it. 

Any changes you need to make should be done in code, which is a natural archive of all of the steps you took. Following this practice will ensure that the data analysis pipeline is reproducible and documented. 

### Data Entry Check

A note: If this is YOUR data, especially if you collected and entered it, do a data entry check and even a double check prior to putting it into the raw data folder.  

The idea is to have accurate data. If  there is a known typo or other error that you know or can confirm is a misake (perhaps by checking your field notes, other records, etc.), by all means fix that in the raw data. We do not want to propagate confirmed errors. However, if you are a data analyst and were given the raw data, you have nothing to check against, and so you hope that the original author did the data entry check. Leave this raw data as is.

## Cleaned Data

`Processed_data` contains the cleaned version of the data. 

Sometimes you will have to edit the files outside of R. This happens, for example if the data come in an Excel file, which can be impossible to read into R if they are poorly formatted (not very rectangular), or if people used color highlighting to code some information.  In that case, save the original file as the raw data, and **make a copy** and place the copies in a different folder. Only **edit the copies**, and document all of the changes somehwere -- in the README.md is fine. The bottom line is **always keep the original files as you received them or created them** to have a solid starting point. 


## Tips

Add as many subfolders as you need. If you only have one processing step, a single subfolder for processed data is fine. If you have multiple stages of cleaning or reshaping, make subfolders to reflect your process. Keep things clearly organized so you know what stage each file is at. 

I suggest you save your processed data as .RDS or .Rdata files, as well as .csv files.  The R data format for saving your data is useful because it preserves all of the object attributes such as factor levels, etc. However, the .csv files are useful for long-term archiving, opening outside of R, and for sharing with others. So save them both ways. 

You should also save a data dictionary that defines all of the variables.  For some suggestions on saving your data see:

http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata