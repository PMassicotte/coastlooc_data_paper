# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a github release with the piggyback package. Using this
# package, I can upload data to an existing release.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(piggyback)

# Create a zip file with all the data
dir_path <- tempdir()
tmpfile <- paste0(dir_path, "/data.zip")

zip(tmpfile, fs::dir_ls("data/", recurse = TRUE, type = "file"))

# Do not forget to create the release on Github first
pb_upload(file = tmpfile, overwrite = TRUE, tag = "v1.1.1")

unlink(tmpfile)
