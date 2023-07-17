# Analysis-assessing-impact-of-perception-of-breastfeeding-support

This repo contains all of the R analysis for assessing the impact of a mothers perception
of breastfeeding support on breastfeeding intensity.

The work leverages the Infant Feeding Practices Study II (IFPS II) dataset.  The modeling
work and outputs are generated from the Rmarkdown file 1_Code/modeling_script_publ_version.rmd. 
The repo also contains various additional R scripts under both 1_Code/ and Archive that contain
both older versions of the main analysis and supplemental analyses.

The data wrangling leverages the data.table package in conjunction with the magrittr
package.  

The modeling primarily leverages the multinom() function from the nnet package.