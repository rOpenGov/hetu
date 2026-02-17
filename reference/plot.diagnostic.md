# Plotting method for diagnostic class objects

Creates a concise plot that visualizes TRUE and FALSE cases in a
diagnostics data frame

## Usage

``` r
# S3 method for class 'diagnostic'
plot(x, negate.logicals = FALSE, labels = TRUE, ...)
```

## Arguments

- x:

  a "summary.diagnostic" object

- negate.logicals:

  negate TRUE and FALSE logicals, default is FALSE. Sometimes it may be
  beneficial to emphasize FALSE cases instead of TRUE

- labels:

  include column labels on y-axis, default is TRUE

- ...:

  Arguments to be passed to methods, such as graphical parameters. For
  example:

  - **type** what type of plot should be drawn. Default is "h" for
    histogram / high density vertical lines.

  - **lwd** line width as double Default is 1.0

  See [`plot`](https://rdrr.io/r/base/plot.html) and
  [`par`](https://rdrr.io/r/graphics/par.html) for more options

## Details

There seems to be no canonical answer on what to call this type of plot.
Some of the names that can be found online when describing a plot for
binary response value on an axis are: a one-dimensional scatterplot, a
sparkline, a rug plot, or a strip plot / strip chart.
