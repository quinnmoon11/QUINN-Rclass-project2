# Processing_code folder

This folder contains code for processing data.

It's the same code done 3 times:

* First, there is an R script that you can run which does all the cleaning.
* Second, there is a Quarto file which contains exactly the same code as the R script.
* Third, my current favorite, is a Quarto file with an approach where the code is pulled in from the R script and run.

The last version has the advantage of having code in one place for easy maintenance (writing/debugging), and reusing by pulling the code chunk into the Quarto file for a nice combination of text/commentary and code.

Each way of doing this is a reasonable approach, pick whichever one you prefer or makes the most sense for your setup. Whichever approach you choose, add ample documentation/commentary so you and others can easily understand the project and how it works.
