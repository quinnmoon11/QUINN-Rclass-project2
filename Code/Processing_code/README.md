# Processing_code folder

This folder contains code for processing data.

It's the same code done 2 times:

* First, there is an R script that you can run which does all the cleaning. (QuinnProcessing.R)
* Second, my current favorite, is a Quarto file with an approach where the code is pulled in from the R script and run. (QuinnProcessingfile_v2.qmd)

The last version has the advantage of having code in one place for easy maintenance (writing/debugging), and reusing by pulling the code chunk into the Quarto file for a nice combination of text/commentary and code. Either file will clean up and save the clean data- choose your favorite!


The folder also contains a html file to visualize the code in a web browser (created using quarto)
