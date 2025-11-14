# Convert `mcc` object to data.frame

Extracts the MCC estimates from an `mcc` object and returns them as a
standard data.frame. This is useful for further analysis or when working
with packages that expect standard data.frame objects.

## Usage

``` r
# S3 method for class 'mcc'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `mcc` object

- ...:

  Additional arguments (currently unused)

## Value

A data.frame with MCC estimates

## Examples

``` r
# Create sample data
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
) |>
  arrange(id, time)

# Calculate MCC
mcc_result <- mcc(df, "id", "time", "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Convert to data.frame
mcc_df <- as.data.frame(mcc_result)
print(mcc_df)
#>   time  mcc
#> 1    0 0.00
#> 2    2 0.25
#> 3    3 0.50
#> 4    6 0.75
#> 5    7 1.00
class(mcc_df)  # "data.frame"
#> [1] "data.frame"

# This is equivalent to extracting mcc_final
identical(mcc_df, as.data.frame(mcc_result$mcc_final))
#> [1] TRUE

# Useful for further analysis with base R functions
summary(mcc_df)
#>       time          mcc      
#>  Min.   :0.0   Min.   :0.00  
#>  1st Qu.:2.0   1st Qu.:0.25  
#>  Median :3.0   Median :0.50  
#>  Mean   :3.6   Mean   :0.50  
#>  3rd Qu.:6.0   3rd Qu.:0.75  
#>  Max.   :7.0   Max.   :1.00  
plot(mcc_df$time, mcc_df$mcc, type = "s")


# Clean up
rm(df, mcc_result, mcc_df)
```
