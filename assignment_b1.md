Assignment B-1: Making a function
================
Berke Ucar
2023-10-30

# Assignment B-1: Making a function

*Caution:* If you do not have tidyverse, datateachr, and testthat
packages installed please run `install.packages(package_name)` to be
able to reproduce the following code. It is not good practice to keep
`install.packages` in the source code, so I do not include them in a
separate code block.

In the mini data analysis project of STAT545A, I used a lot of string
extraction tasks. For this assignment, I thought it would be fun and
useful to gather those steps in a function. This is important since
those kind of tasks (string extraction from a dataset) is a common
practice in data science.

One concern that I have is to allow this function to extract as many
regular expression as possible from a single column, which is both
flexible and sturdy for the extraction since extractions are done column
by column generally with their specific extraction patterns. Since I do
not perform the extraction for lots of columns, I believe this function
is not too generalized. The function adds a new column to the extracted
patterns named <column_name>\_<the_number_of_the_argument>. Since this
function only requires a single column to be a string column, the
function is not restrictive.

As a design choice, I let columns that have NA values to be a proper
input since they could possess a non-NA value as well. I check this with
counting the number of non NA values and checking if the count is equal
to 0. Furthermore, it is possible to extract a pattern from a double
column with `str_extract` method. So, I let the input column to be any
kind of column as well.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#' Extract patterns from a column
#' 
#' @details 
#' Extracts the given regular expression(s) from the column of the tibble and makes a new column or new columns out of extraction(s). The column of extraction can be any type of column since str_extract allows extraction on every data type and returns a string as a result. This function returns the resulted tibble.
#' 
#' @param dataset tibble that holds the data for extraction. I believe "dataset" represents the input well since user needs to use a tibble that represents a dataset.
#' @param column base variable which indicates the column that the extraction will be performed on. The reason I named this "column" is that user can specify only one column and column is explanatory for that case
#' @param ... string(s) that hold(s) the regular expression pattern(s) for the extraction. Since user can specify different patterns for the column, I chose ellipsis for these parameters.
#'
#' @return dataset tibble with the addition of resulted columns from the extraction
#' @example 
#' extract_from_column(steam_games, recent_reviews, "\\w+ \\w+")
#' extract_from_column(steam_games, recent_reviews, "\\w+", "\\d+%")
extract_from_column <- function(dataset, column, ...){
  arguments <- c(...)
  
  # checks
  if (length(arguments) == 0) stop("Sorry, you did not enter a pattern or patterns.")
  if (!is_tibble(dataset)) stop("Sorry, the dataset you passed is not a tibble, it is: ", class(dataset))
  if (nrow(dataset)==0) stop("Sorry, you entered an empty dataset.")
  if (summarise(dataset, n=sum(!is.na({{column}})))[1] == 0) stop("Sorry, you entered a full NA column.")
  
  
  resulted_dataset <- dataset
  for (i in 1:length(arguments)){ # for every input pattern
    name <- paste("added_col_", i,sep="") # generate the name for the column
    resulted_dataset <- resulted_dataset %>% mutate( !!name := str_extract(pattern=arguments[i], string={{column}}))  # extract the pattern and put it into corresponding column
  }
  
  return(resulted_dataset)
}
```

In order to demonstrate my function, I will use different datasets,
columns, and patterns as well. Here it goes:

``` r
library(datateachr)

print(extract_from_column(building_permits, address, "BC .*") %>% select(address, added_col_1)) # basic usage, extract a pattern from a string column. I only show the extracted column and the resource column here for the sake of the reader.
```

    ## # A tibble: 20,680 × 2
    ##    address                                       added_col_1
    ##    <chr>                                         <chr>      
    ##  1 4378 W 9TH AVENUE, Vancouver, BC V6R 2C7      BC V6R 2C7 
    ##  2 1111 RICHARDS STREET, Vancouver, BC V1V 1V1   BC V1V 1V1 
    ##  3 3732 W 12TH AVENUE, Vancouver, BC V6R 2N6     BC V6R 2N6 
    ##  4 88 W PENDER STREET, Vancouver, BC V6B 6N9     BC V6B 6N9 
    ##  5 492 E 62ND AVENUE, Vancouver, BC V5X 2G1      BC V5X 2G1 
    ##  6 3332 W 28TH AVENUE, Vancouver, BC V6S 1R9     BC V6S 1R9 
    ##  7 2873 W 33RD AVENUE, Vancouver, BC V6N 2G3     BC V6N 2G3 
    ##  8 3579 E 26TH AVENUE, Vancouver, BC V5R 1M5     BC V5R 1M5 
    ##  9 620 CARDERO STREET, Vancouver, BC V1V 1V1     BC V1V 1V1 
    ## 10 1868 W 5TH AVENUE #409, Vancouver, BC V6J 1P3 BC V6J 1P3 
    ## # ℹ 20,670 more rows

``` r
print(extract_from_column(steam_games, original_price, "\\d+\\.")%>%select(original_price, added_col_1)) # extraction of the decimal part of a double column, I only show the extracted column and the resource column here for the sake of the reader. !!! For this method since 0 is 0 in double as well, it cannot find the . hence it cannot extract the pattern
```

    ## # A tibble: 40,833 × 2
    ##    original_price added_col_1
    ##             <dbl> <chr>      
    ##  1           20.0 19.        
    ##  2           30.0 29.        
    ##  3           40.0 39.        
    ##  4           45.0 44.        
    ##  5            0   <NA>       
    ##  6           NA   <NA>       
    ##  7           60.0 59.        
    ##  8           15.0 14.        
    ##  9           30.0 29.        
    ## 10           50.0 49.        
    ## # ℹ 40,823 more rows

``` r
print(extract_from_column(steam_games, all_reviews, "\\(.*\\)", "\\d+%")%>%select(all_reviews, added_col_1, added_col_2)) # extraction of the multiple parts of a string column, I only show the extracted columns and the resource column here for the sake of the reader.
```

    ## # A tibble: 40,833 × 3
    ##    all_reviews                                           added_col_1 added_col_2
    ##    <chr>                                                 <chr>       <chr>      
    ##  1 Very Positive,(42,550),- 92% of the 42,550 user revi… (42,550)    92%        
    ##  2 Mixed,(836,608),- 49% of the 836,608 user reviews fo… (836,608)   49%        
    ##  3 Mostly Positive,(7,030),- 71% of the 7,030 user revi… (7,030)     71%        
    ##  4 Mixed,(167,115),- 61% of the 167,115 user reviews fo… (167,115)   61%        
    ##  5 Mostly Positive,(11,481),- 74% of the 11,481 user re… (11,481)    74%        
    ##  6 NaN                                                   <NA>        <NA>       
    ##  7 Very Positive,(9,645),- 92% of the 9,645 user review… (9,645)     92%        
    ##  8 Very Positive,(23,763),- 91% of the 23,763 user revi… (23,763)    91%        
    ##  9 Very Positive,(12,127),- 85% of the 12,127 user revi… (12,127)    85%        
    ## 10 Mixed,(904),- 44% of the 904 user reviews for this g… (904)       44%        
    ## # ℹ 40,823 more rows

Now, since I showed the general usage of the function, it is turn for
the testing and edge case checking. The following cells demonstrate the
testing. I test the following:

1.  Without any patterns
2.  A non-tibble dataset
3.  An empty dataset
4.  A column with all NA values

I know 3 and 4 are kind of the same but I would explain the difference
as this: if the dataset is only declared but not initiated, it is empty.
If the dataset has defined columns but no values or has NA values all
across the colum, it is full NA column according to my definition.

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
na_column_steam_games <- steam_games %>% filter(is.na(mature_content))
df <- data.frame(col_1 = c("berke", "ucar", "bioinformatics"))
empty <-tibble(data.frame())


test_that( "Testing for the function extract_from_column", {
  expect_error(extract_from_column(steam_games, all_reviews), "Sorry, you did not enter a pattern or patterns.")
  expect_error(extract_from_column(df, col_1, ".*"), "Sorry, the dataset you passed is not a tibble, it is:")
  expect_error(extract_from_column(empty, col_1, ".*"), "Sorry, you entered an empty dataset.")
  expect_error(extract_from_column(na_column_steam_games, mature_content, ".*"), "Sorry, you entered a full NA column.")
})
```

    ## Test passed 😸
