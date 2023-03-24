
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treeda

A ‘tree exploratory data analysis’ repository for workflows with tree
canopy height models (CHM). Essentially collating scripts to help
wildlife, fuels and vegetation with exploring underlying patterns and
structure in canopy height models.

## Installation

You can install the development version of treeda from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/treeda")
```

## Requirements

You must have a new-ish version of GDAL installed locally with
appropriate environment variables.

## Example in R

Here is an example in R. Provide polygons, file where the CHM is and
where the masked CHM (from the polygons) will go.

``` r
library(treeda)

aoi_polygons <- sf::read_sf('path_to_some_polygons.shp')

infile ='C:/some_path_to_CHM/Canopy_Height_Raster1m.tif'

outfile = 'D:/some_path_to_put_masked_CHM/test_tif/'

chm_eda <- get_tree_eda(aoi_polygons,
                   infile,
                   outfile,
                   FUN = lidR::lmf(ws = function(x) { x * 0.3 + 10}))
```
