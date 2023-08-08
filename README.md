# COASTLOOC data paper

<!--toc:start-->

- [COASTLOOC data paper](#coastlooc-data-paper)
  - [Associated paper](#associated-paper)
  - [Using the project](#using-the-project)
    - [renv](#renv)
    - [Docker and VSCode devcontainer](#docker-and-vscode-devcontainer)

The COASTLOOC oceanic expeditions took place in 1997 and 1998. This project (a data paper) aims at presenting an overview of the collected data. A lot of data are about radiometric quantities and nutrients collected at the surface of the water column.

<https://pmassicotte.github.io/coastlooc_data_paper/>

## Associated paper

The COASTLOOC data has been peer-reviewed and published in Earth System Science Data (ESSD).

- https://essd.copernicus.org/articles/15/3529/2023/

### Open reviews

1. https://doi.org/10.5194/essd-2023-83-CC1
2. https://doi.org/10.5194/essd-2023-83-RC1
3. https://doi.org/10.5194/essd-2023-83-RC2
4. https://doi.org/10.5194/essd-2023-83-RC3

### How to cite

*Massicotte, P., Babin, M., Fell, F., Fournier-Sicre, V., and Doxaran, D.: The Coastal Surveillance Through Observation of Ocean Color (COASTℓOOC) dataset, Earth Syst. Sci. Data, 15, 3529–3545, https://doi.org/10.5194/essd-15-3529-2023, 2023.*

## Using the project

The R script `R/000_main.R` contains all the steps to re-run all the analyses.

### renv

The project uses the `renv` R package to make this data project reproducible. After forking or downloading the project, make sure that `renv` is installed:

```r
install.packages("renv")
```

Within the project directory, simply run the following command to install all the needed R packages.

More information can be found on the `renv` [website](https://rstudio.github.io/renv/).

```r
renv::restore()
```

### Docker and VSCode devcontainer

There is also a `Dockerfile` included in the directory. It includes everything needed to recreate the development environment used for the project. It also contains a [devcontainer](https://code.visualstudio.com/docs/devcontainers/containers) folder that can be used within [VSCode](https://code.visualstudio.com/).

One can refer to [How to test against almost any R version with VSCode and Docker](https://milesmcbain.micro.blog/2021/05/05/how-to-test.html) for a nice introduction to R/VSCode and devcontainer.
