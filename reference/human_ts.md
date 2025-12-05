# Create human readable timestamps

This helper function creates human readable timestamps in the form of
`%Y%m%d-%H%M%OS` at the current time.

## Usage

``` r
human_ts()
```

## Value

`human_ts()` returns a character value with the respective timestamp.

## Details

Human readable timestamps are appended to file names and fields when
metadata are shared with the Open Specy community.

## See also

[`format.Date`](https://rdrr.io/r/base/as.Date.html) for date conversion
functions

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
human_ts()
#> [1] "20251205-230516"
```
