Codebook created on 2020-11-14 at 2020-11-14 08:50:30
================

## Dataset description

The data contains 150 cases and 5 variables.

## Codebook

| name         | type    |   n | missing | unique | mean | median |  mode | mode\_value |   sd |    v | min | max | range |   skew | skew\_2se |   kurt | kurt\_2se |
| :----------- | :------ | --: | ------: | -----: | ---: | -----: | ----: | :---------- | ---: | ---: | --: | --: | ----: | -----: | --------: | -----: | --------: |
| Sepal.Length | numeric | 150 |       0 |     35 | 5.84 |   5.80 |  5.80 |             | 0.83 |      | 4.3 | 7.9 |   3.6 |   0.31 |      0.78 | \-0.61 |    \-0.77 |
| Sepal.Width  | numeric | 150 |       0 |     23 | 3.06 |   3.00 |  3.00 |             | 0.44 |      | 2.0 | 4.4 |   2.4 |   0.31 |      0.79 |   0.14 |      0.18 |
| Petal.Length | numeric | 150 |       0 |     43 | 3.76 |   4.35 |  4.35 |             | 1.77 |      | 1.0 | 6.9 |   5.9 | \-0.27 |    \-0.68 | \-1.42 |    \-1.80 |
| Petal.Width  | numeric | 150 |       0 |     22 | 1.20 |   1.30 |  1.30 |             | 0.76 |      | 0.1 | 2.5 |   2.4 | \-0.10 |    \-0.25 | \-1.36 |    \-1.73 |
| Species      | factor  | 150 |       0 |      4 |      |        | 50.00 | setosa      |      | 0.67 |     |     |       |        |           |        |           |

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
