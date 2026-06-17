# Print information about loaded packages

Prints a formatted summary of the current R session, including the R
version, platform, and the names and versions of all currently loaded
packages. Useful for reproducibility logging at the top of cluster
scripts or in analysis pipelines where tracking package versions is
important.

## Usage

``` r
package_info()
```

## Value

Invisibly returns a named list of loaded package information, where each
element is the package description object returned by
[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html). Called
primarily for its side effect of printing to the console.

## Details

Only packages attached via
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) are reported — base
packages and packages loaded but not attached are excluded. For a full
session snapshot including base packages, see
[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html).

## See also

[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html),
`print_script_header`

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)
library(microclimf)

package_info()
} # }
```
