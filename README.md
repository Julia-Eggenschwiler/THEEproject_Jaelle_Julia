
## A computational model for antibiotic resistance - An evolutionary rescue model developed by Julia Eggenschwiler and Jaelle Häfliger

This project was created by **Jaelle Häfliger and Julia Eggenschwiler**  in the Theoretical Ecology and Evolution research practical of fall 2025.

The live website for 2025 is at [(https://github.com/Julia-Eggenschwiler/THEEproject_Jaelle_Julia)]
 
# Project description and main results

This readme-file connects to the Evolutionary rescue project, which was done during the Theoretical Reserach Practical course in autumn 2025. This project addresses the following research question: 
How do 2 bacterial strains, which can alternate between beneficial and deleterious versions of the same strength via mutation and back-mutation, influence the probability of rescue and how does their frequency in the population change?

In order to answer this question the trade-off between having a smaller disadvantage in the beginning but only being able to mutate to having a smaller advantage and starting with a larger disadvantage in the beginning but being able to mutate to having a larger advantage is a key componant.

The most important result found during the analysing-process of the simulation is the following: The model tends to be quite stochastic, but different plots and calculations show: in most cases, the strong bacterial strain achieves a higher rescue-probability and therefore "wins" compared to the weak bacterial strain.

# How to install

To create and run the simulation we used RStudio version 4.4.1. To run the code, it is required that the R-packages “dplyr”, “ggplot2”, “tidyr” and installed and loaded.  

In order to "knit" (print) the R-markdown to a .html file, it is required to install the knitr package with the function `install.packages("knitr")`

# File structure

In the github-repository: "THEEproject_Jaelle_Julia" you can find our R Markdown file: "Evolutionary rescue_discrete time_Jaelle_Julia.Rmd". This file contains all our annotated code which is needed to execute our simulations. 
Furthermore, there is the knitted hmtl-file: "Evolutionary-rescue_discrete-time_Jaelle_Julia.html", which is an already knitted version of our code.
The powerpoint presentation: "THEE_project.pptx" contains the slides of our the project-presentation, which was held at the THEE-lab on the 30th of October 2025.
The word-document: "Report THEE - draft_Julia Eggenschwiler_Jaelle Häfliger", contains the final version of our project-report, all results and outcomes and the interpretation and discussion of the results.

The subfolder: "r-files with separate plots" contains four additional R-files and two word-documents, with separate plots and notes, which were mainly used during the coding-stage of the project. These are not necesary to execute the code and visualize the results of this project.


# Contact

This repository and its contents were developed and are maintained by Jaelle Häfliger and Julia Eggenschwiler druing the Research Practical in Theoretical Ecology and Evolution, led by Prof. Claudia Bank, Institute of Ecology & Evolution, University of Bern.
If you have any questions or need any help regarding the project, please contact Julia Eggenschwiler or Jaelle Häfliger via their corresponding email-addresses of the University of Bern (julia.eggenschwiler@students.unibe.ch, jaelle.haefliger@students.unibe.ch).

*Note: check out the **website branch** if you are looking for the code to build and deploy the website.*
