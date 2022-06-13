# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a github release with the piggyback package. Using this
# package, I can upload data to an existing release.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(piggyback)

pb_new_release(tag = "v0.0.1")

tmpfile <- "~/Desktop/data.zip"

zip(tmpfile, fs::dir_ls("data/", recurse = TRUE, type = "file"))

pb_upload(tmp, overwrite = FALSE)

unlist(tmpfile)
