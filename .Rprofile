if (interactive() && Sys.getenv("RSTUDIO") == "") {
  Sys.setenv(TERM_PROGRAM = "vscode")
  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = FALSE)
    })
  }
}

# rspm::enable()

# For Linux and Windows users, we'll use RStudio Package Manager (RSPM).
if (Sys.info()[["sysname"]] %in% c("Linux")) {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"))
} else {
  # For Mac users, we'll default to installing from CRAN/MRAN instead, since
  # RSPM does not yet support Mac binaries.
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/latest"))
  # options(renv.config.mran.enabled = TRUE) # TRUE by default
}
options(
  renv.config.repos.override = getOption("repos"),
  renv.config.auto.snapshot = TRUE, # Attempt to keep renv.lock updated automatically
  renv.config.rspm.enabled = TRUE, # Use RStudio Package manager for pre-built package binaries
  renv.config.install.shortcuts = TRUE, # Use the existing local library to fetch copies of packages for renv
  renv.config.cache.enabled = TRUE, # Use the renv build cache to speed up install times
  renv.config.cache.symlinks = TRUE, # Keep full copies of packages locally than symlinks to make the project portable in/out of containers
  renv.config.install.transactional = FALSE,
  renv.config.synchronized.check = FALSE
)
