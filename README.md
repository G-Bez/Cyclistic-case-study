# Cyclistic-case-study

Instructions:

1) Download the source code from here
2) Open the project file
3) Run renv::restore() to restore the project library from the lockfile
4) Take a look at .R scripts to understand how the targets pipeline works
5) Run targets::tar_make() to run the pipeline and create targets objects
6) Look at the resulting report .html file

Feel free to update the pipeline with more statistics and visualizations.
Remember to re run tar_make() everytime you make changes. Use 
targets::tar_visNetwork() to look for outdated objects.

Warning: datasets are quite large, and code is not optimized for performance.
Running the entire pipeline might take much time with non recent hardware, 
even more than 1 hr. Be patient.


