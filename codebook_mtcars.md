Codebook created on 2020-11-14 at 2020-11-14 08:56:33
================

## Dataset description

The data contains 32 cases and 11 variables.

## Codebook

| name | type    |  n | missing | unique |   mean | median |   mode |     sd |   min |    max |  range |   skew | skew\_2se |   kurt | kurt\_2se |
| :--- | :------ | -: | ------: | -----: | -----: | -----: | -----: | -----: | ----: | -----: | -----: | -----: | --------: | -----: | --------: |
| mpg  | numeric | 32 |       0 |     25 |  20.09 |  19.20 |  19.20 |   6.03 | 10.40 |  33.90 |  23.50 |   0.61 |      0.74 | \-0.37 |    \-0.23 |
| cyl  | numeric | 32 |       0 |      3 |   6.19 |   6.00 |   6.00 |   1.79 |  4.00 |   8.00 |   4.00 | \-0.17 |    \-0.21 | \-1.76 |    \-1.09 |
| disp | numeric | 32 |       0 |     27 | 230.72 | 196.30 | 196.30 | 123.94 | 71.10 | 472.00 | 400.90 |   0.38 |      0.46 | \-1.21 |    \-0.75 |
| hp   | numeric | 32 |       0 |     22 | 146.69 | 123.00 | 123.00 |  68.56 | 52.00 | 335.00 | 283.00 |   0.73 |      0.88 | \-0.14 |    \-0.08 |
| drat | numeric | 32 |       0 |     22 |   3.60 |   3.69 |   3.69 |   0.53 |  2.76 |   4.93 |   2.17 |   0.27 |      0.32 | \-0.71 |    \-0.44 |
| wt   | numeric | 32 |       0 |     29 |   3.22 |   3.33 |   3.33 |   0.98 |  1.51 |   5.42 |   3.91 |   0.42 |      0.51 | \-0.02 |    \-0.01 |
| qsec | numeric | 32 |       0 |     30 |  17.85 |  17.71 |  17.71 |   1.79 | 14.50 |  22.90 |   8.40 |   0.37 |      0.45 |   0.34 |      0.21 |
| vs   | numeric | 32 |       0 |      2 |   0.44 |   0.00 |   0.00 |   0.50 |  0.00 |   1.00 |   1.00 |   0.24 |      0.29 | \-2.00 |    \-1.24 |
| am   | numeric | 32 |       0 |      2 |   0.41 |   0.00 |   0.00 |   0.50 |  0.00 |   1.00 |   1.00 |   0.36 |      0.44 | \-1.92 |    \-1.19 |
| gear | numeric | 32 |       0 |      3 |   3.69 |   4.00 |   4.00 |   0.74 |  3.00 |   5.00 |   2.00 |   0.53 |      0.64 | \-1.07 |    \-0.66 |
| carb | numeric | 32 |       0 |      6 |   2.81 |   2.00 |   2.00 |   1.62 |  1.00 |   8.00 |   7.00 |   1.05 |      1.27 |   1.26 |      0.78 |

### Legend

  - **Name**: Variable name
  - **type**: Data type of the variable
  - **missing**: Proportion of missing values for this variable
  - **unique**: Number of unique values
  - **mean**: Mean value
  - **median**: Median value
  - **mode**: Most common value (for categorical variables, this shows
    the frequency of the most common category)
  - **mode\_value**: For categorical variables, the value of the most
    common category
  - **sd**: Standard deviation (measure of dispersion for numerical
    variables
  - **v**: Agresti’s V (measure of dispersion for categorical variables)
  - **min**: Minimum value
  - **max**: Maximum value
  - **range**: Range between minimum and maximum value
  - **skew**: Skewness of the variable
  - **skew\_2se**: Skewness of the variable divided by 2\*SE of the
    skewness. If this is greater than abs(1), skewness is significant
  - **kurt**: Kurtosis (peakedness) of the variable
  - **kurt\_2se**: Kurtosis of the variable divided by 2\*SE of the
    kurtosis. If this is greater than abs(1), kurtosis is significant.

This codebook was generated using the [Workflow for Open Reproducible
Code in Science (WORCS)](https://osf.io/zcvbs/)
