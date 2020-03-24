The code in this folder can be used to reproduce all figures and analyses in Vansteelant et al. 2020 - Journal of Ornithology. 

In order to replicate the analyses download this Github repository and run each of the scripts in the order indicated by the first 2-3 characters of each script:
p1_…, p2a_…, p2b_…, p3…, p3a_…, etc. 

The first script (p1_...) will automatically install and load all of the packages needed to replicate the analyses. 

All other scripts are annotated with detailed explanations of what each chunk of code is doing. 

Scripts that do not start with a numeric code contain supporting code that will automatically be sourced by the main scripts. For example, the script 'pt2pt_fxns.R' contains a number of custom-made functions for calculating movement statistics. This script is called by the main code as if it were a package containing pre-built functions. 

In case you run into trouble contact Wouter Vansteelant at w.m.g.vansteelant@uva.nl