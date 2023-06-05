# COASTLOOC data paper

The COSTLOOC oceanic expeditions took place in 1997 and 1998. This project (a data paper) aims at presenting an overview of the collected data. A lot of data are about radiometric quantities and nutrients collected at the surface of the water column.

<https://pmassicotte.github.io/coastlooc_data_paper/>

## Associated paper

The COASTLOOC data has been peer-reviewed and published in Earth System Science Data (ESSD).

- TODO: provide the URL to the published papers.

Public reviews are available:

1. https://doi.org/10.5194/essd-2023-83-CC1
2. https://doi.org/10.5194/essd-2023-83-RC1
3. https://doi.org/10.5194/essd-2023-83-RC2
4. https://doi.org/10.5194/essd-2023-83-RC3

## Using the project

The project uses the `renv` R package to make this data project reproducible. After forking or downloading the project, make sure that `renv` is installed:

```r
install.packages("renv")
```

Within the project directory, simply run the following command to install all the needed R packages.

More information can be found on the `renv` [website](https://rstudio.github.io/renv/).

```r
renv::restore()
```

The R script `R/000_main.R` contains all the steps to re-run all the analyses.

## Docker

There is also a `Dockerfile` included in the directory. It includes everything needed to recreate the environnment used for the project. It also contain the a [devcontainer](https://code.visualstudio.com/docs/devcontainers/containers) that can be used within [VSCode](https://code.visualstudio.com/).

An introcutio

One can refer to [How to test against almost any R version with VSCode and Docker](https://milesmcbain.micro.blog/2021/05/05/how-to-test.html) for a nice introduction to R/VSCode and devcontainer.
