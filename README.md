# PHSM_SI_package

R package to compute the severity index of PHSM dataset. 

The package contains the following functions representing the individual categories in the dataset.
```
dataprep.R
binaryindex.R
ordmasks.R
ordschools.R
ordbusiness.R
ordgather.R
ordmove.R
ordtravel.R
calc_sev.R
sevindex.R
prep_plot.R
forplot.R
```

## To use
Clone this repository and extract into your computer maintaining the same file structure.

```https://github.com/lshtm-gis/PHSM-SI_package.git```

Go into package directory and load the functions
```
library(devtools)
load_all(".")
```
To compute the SI for all the countries
```
SI <- forplot('mistress.csv')
```
To compute the SI for singular country.
```
SI <- forplot('mistress.csv','France')
```

