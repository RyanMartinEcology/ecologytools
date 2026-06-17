# Time the execution of an expression

Wraps any R expression and reports how long it took to execute, printing
a formatted message to the console with the start time, end time, and
elapsed duration. Useful for profiling long-running operations in
scripts or cluster jobs.

## Usage

``` r
time_it(expr, label = NULL)
```

## Arguments

- expr:

  An R expression to evaluate.

- label:

  An optional character string describing the expression, used in the
  console output. Defaults to `NULL`, in which case the deparsed
  expression is used as the label.

## Value

Invisibly returns the result of evaluating `expr`. Called primarily for
its side effect of printing timing information to the console.

## Examples

``` r
if (FALSE) { # \dontrun{
# Time a simple operation
result <- time_it(Sys.sleep(2), label = "sleeping")

# Time a model run
micropointa <- time_it(
  runpointmodela(...),
  label = "micropoint model"
)
} # }
```
