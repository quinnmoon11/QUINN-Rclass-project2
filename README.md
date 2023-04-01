
# Repo url

<https://github.com/quinnmoon11/QUINN-Rclass-project>

# Overview

A solution for the Palmer Penguins project done with R/Quarto/Github. The structure promotes a data science pipeline for reproducibility in research. 

# History

2023-02-16 First release with Data and Code folders with materials for data cleaning and initial exploration.  
2023-03-20 Solutions added to the repository.

# Acknowledgement

 This repository was cloned and modified from <https://github.com/mbutler808/Project-template>

# Software requirements

This repo contains a data analysis project and report writing using R, Quarto, Github and a reference manager for bibtex. A plain text editor is also necessary, and wordprocessing software to open any .docx files (e.g. MS Word, MacOS Pages, or [LibreOffice](https://www.libreoffice.org/)). 

For more R packages supporting reproducible research check out the taskview <https://cran.r-project.org/web/views/ReproducibleResearch.html>

# Repository structure

The repository supports the notion that there should _only be one copy of code and any outputs_. Any time the intellectual content needs to be reused, it is referred to or linked from the one copy. This way we can prevent different copies of the same content from accidentally diverging, and it is easier to maintain the project. 

This template also uses the convention that Folder names begin with a capital letter. 

* All data goes into the `Data` folder and any subfolders.
* All code goes into the `Code` folder or subfolders.
* All results (figures, tables, computed values) go into `Results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `Products` subfolders.
* See the various `README.md` files in those folders for some more information.

# Repository content

Please see the `README.md` files in each folder for more details.

* This repo begins with the _Palmer Penguins_ morphology and life history data in the `Raw_data` folder. 
* The `Processing_code` folder contains several files that load the raw data, perform a bit of cleaning, and save the result in the `Processed_data` folder. 

Planned additions:
* The `Analysis_code` folder will contains several files that load the processed data, do an exploratory analysis, and fit a simple model. These files produce figures and some numeric output (tables), which are saved to the `results` folder.
* The `Products` folder will contain an example `bibtex` and CSL style files for references. Those files are used by the example manuscript and slides.
* The  `Manuscript` folder will contain a template for a report written as Quarto file. There is also a sub-folder containing an example template for a supplementary material file as is common in scientific articles these days.
* The `slides` folder contains a basic example of slides made with Quarto. 

**For the first project, we only have Data and Code folders.**
The rest will come soon. 

# Getting started

The project can be reproduced by executing the code in the following order: 

1.  First run the processing code, which will produce the processed data. 

Planned additions:  

2.  Then run the analysis scripts, which will take the processed data and produce some results. 
3.  Then you can run the manuscript, poster and slides example files in any order. Those files pull in the generated results and display them. These files also pull in references from the `bibtex` file and format them according to the CSL style.
