# PHSM_SI_package

R package to compute the severity index (SI) of PHSM dataset. 

The package contains the following functions to calculate the SI. They are executed in the following order.
```
# prepare the input data (mistress, master)
dataprep.R 
# Compute the binary score 	
binaryindex.R
# Compute the ordinal score for mask
ordmasks.R
# Compute the ordinal score for schools
ordschools.R
# Compute the ordinal score for businesses
ordbusiness.R
# Compute the ordinal score for gathering
ordgather.R
# Compute the ordinal score for movements
ordmove.R
# Compute the ordinal score for travel
ordtravel.R
# Compute the severity index for all the six categories
calc_sev.R
# Call all the above funtions and output the severity index for each new measure introduced. Final output if only measure change file is needed.
sevindex.R
# Read the output from {sevindex.R} and generate the SI for all days
prep_plot.R
# Run all the functions and output final file
forplot.R
```

## To use
Clone this repository .

```https://github.com/lshtm-gis/PHSM-SI_package.git```

Go into package directory and load the functions
```
library(devtools)
load_all(".")
```
# PHSM_SI_package

R package to compute the severity index (SI) of PHSM dataset. 

The package contains the following functions to calculate the SI. They are executed in the following order.
```
# prepare the input data (mistress, master)
dataprep.R 
# Compute the binary score 	
binaryindex.R
# Compute the ordinal score for mask
ordmasks.R
# Compute the ordinal score for schools
ordschools.R
# Compute the ordinal score for businesses
ordbusiness.R
# Compute the ordinal score for gathering
ordgather.R
# Compute the ordinal score for movements
ordmove.R
# Compute the ordinal score for travel
ordtravel.R
# Compute the severity index for all the six categories
calc_sev.R
# Call all the above funtions and output the severity index for each new measure introduced. Final output if only measure change file is needed.
sevindex.R
# Read the output from {sevindex.R} and generate the SI for all days
prep_plot.R
# Run all the functions and output final file
forplot.R
```

## To use
Clone this repository .

```https://github.com/lshtm-gis/PHSM-SI_package.git```

Go into package directory and load the functions
```
library(devtools)
load_all(".")
```
Libraries required
``` tidyverse,
rlist,
lubridate```
To compute the SI for all the countries
```
SI <- forplot('mistress.csv')
```
To compute the SI for singular country.
```
SI <- forplot('mistress.csv','France')
```
If only SI for measure change is needed(i.e. for only days a measure is introduced).
```
SI <- sevindex('mistress.csv','France')
```
To calculate the binary and ordinal scores for individual categories.
```
# Start with this to prepare the file
df_a <- dataprep('mistress.csv','France')
# Then calculate the binary score
df_b <- binaryindex(df_a)
# Then ordinal score for mask
df_c <- ordmask(df_b)
# Continue for others following the order 
```
SI <- forplot('mistress.csv')
```
To compute the SI for singular country.
```
SI <- forplot('mistress.csv','France')
```
If only SI for measure change is needed(i.e. for only days a measure is introduced).
```
SI <- sevindex('mistress.csv','France')
```
To calculate the binary and ordinal scores for individual categories.
```
# Start with this to prepare the file
df_a <- dataprep('mistress.csv','France')
# Then calculate the binary score
df_b <- binaryindex(df_a)
# Then ordinal score for mask
df_c <- ordmask(df_b)
# Continue for others following the order 
```
