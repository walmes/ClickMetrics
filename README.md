<img src="https://raw.githubusercontent.com/walmes/ClickMetrics/master/inst/hexsticker/ClickMetrics.png" width="120px" align="right" display="block" style="padding-top: 2em; -webkit-filter: drop-shadow(0px 0px 3px #111);">

ClickMetrics
============

<!-- [![Build Status](https://travis-ci.org/walmes/ClickMetrics.svg?branch=master)](https://travis-ci.org/walmes/ClickMetrics) -->

This project is a R package. The aim of `ClickMetrics` is to help on the
extraction of data by mouse clicking on images. First it will be covered
common situations in agronomy/biology: length, area, counts.

This package has shiny dashboards built to help scientists measure
things in images/photos, e.g. mycelial diameter, nematode body length,
number of spores on plate dishes, damaged leaf area, distribution of
point patterns. Based on coordinate of mouse clicks, the measures are
obtained after data pos processing.

This package is under continuous development, so function names,
function headers and return values, object classes, etc, can be modified
at any moment to improve usability and cohesion.

Please report bug and feature request by opening a
[Issue](https://github.com/walmes/ClickMetrics/issues).

To install the package from its GitHub repository, run the code below.

```r
# Install `devtools` if you don't have it.
install.packages("devtools")

# Load `devtools` package.
library(devtools)

# Install from source on GitHub.
install_github("walmes/ClickMetrics")

# Load `ClickMetrics`.
library(ClickMetrics)
packageDescription("ClickMetrics")
```

To install from compressed files, visit
<http://leg.ufpr.br/~walmes/pacotes/ClickMetrics> and download a
`*.tar.gz` if Linux or `*.zip` if Windows, if them are available. Set
the working directory to the folder where is the file and run the code
below. Replace `x.y-z` for the current version. Remember to install all
package dependencies also.

```r
# Installing from tarball (Linux).
install.packages(pkgs = "ClickMetrics_x.y-z.tar.gz",
                 repos = NULL,
                 type = "source")

# Installing from tarball (Windows).
install.packages(pkgs = "ClickMetrics_x.y.z.zip",
                 repos = NULL,
                 type = "source")
```

To do a tour on the package, run the code below.

```r
# Load the package.
library(ClickMetrics)

# Show all visible objects.
ls("package:ClickMetrics")

# Open the documentation in browser.
help(package = "ClickMetrics", help_type = "html")

# Show all vignettes.
browseVignettes(package = "ClickMetrics")

# Open a vignette.
vignette(topic = "vignette_name_here", package = "ClickMetrics")

# Get the citation.
citation("ClickMetrics")
```
