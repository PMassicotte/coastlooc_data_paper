# COASTLOOC data paper

The COSTLOOC oceanic expeditions took place in 1997 and 1998. This project (a data paper) aims at presenting an overview of the collected data. A lot of data is about radiometric quantities and nutrients collected at the surface of the water column.

<https://pmassicotte.github.io/coastlooc_data_paper/>

## Using the project

The project uses the `renv` R package to make this data project reproducible. After forking or downloading the project, make sure that `renv` is installed:

```r
install.packages("renv")
```

Withing the project directory, simply run the following command to install all the needed R packages.

More information on can be found on the `renv` [website](https://rstudio.github.io/renv/).

```r
renv::restore()
```

## TODO

- Use `piggyback` <https://docs.ropensci.org/piggyback/>

- Release the final report PDF with a DOI and cite it in the main text.

- Make sure that all variables (columns) in the CSV files have units specified in their names (ex.: `depth_m`, `a_cdom_1_m`, ...).

- Remove unneeded scripts. For example, 20 and 22.
