# compsizer

Package for calculating CompetitorSize, as in Jakab (2018) "Competitor Scale and Mutual Fund Behavior." 

#### `GenCompSize`

The workhorse function. It eats two datasets as principal arguments: (i) a long-format data.table (or data.frame) of portfolio weights (or other characteristic used for measuring similarity), and (ii) a long-format data.table (or data.frame) of fund sizes. Returns a long-format data.table of CompetitorSize. 

Cosine similarity is used as the measure of fund similarity. (In the future other measures might be implemented as well.)


#### `SplitByDate`

Utility for converting the long-format portfolio weight dataset and long-format fund size dataset into a list of matrices. Each list element corresponds to a date and includes an N x K matrix of portfolio weights (columns corresponding to funds and rows to securities), a K x 1 matrix of fund sizes, and a string specifying the date.

#### `CosSim`

Calculate cosine similarities. Returns a symmetric matrix of cosine similarities.

#### `CompSize`

Calculate CompetitorSize for a given date, based on a symmetric similarity matrix and a fund size matrix.
