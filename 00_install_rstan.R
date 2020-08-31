remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
# Restart R

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
pkgbuild::has_build_tools(debug = TRUE)
