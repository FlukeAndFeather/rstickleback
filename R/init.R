# Internal package environment to facilitate Python module access
.sbenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Manage Python dependencies automatically
  # https://rstudio.github.io/reticulate/articles/python_dependencies.html#-onload-configuration
  # Retrieved 2021-10-05
  reticulate::configure_environment(pkgname)

  # Check stickleback availability (Config/reticulate doesn't always work)
  if (reticulate::py_module_available("stickleback")) {
    # Import stickleback modules
    .sbenv$sb <- reticulate::import("stickleback.stickleback")
    .sbenv$sb_data <- reticulate::import("stickleback.data")
    .sbenv$sb_util <- reticulate::import("stickleback.util")
    .sbenv$sb_viz <- reticulate::import("stickleback.visualize", convert = FALSE)

    # Import utility functions
    util_path <- system.file("python", package = "rstickleback")
    .sbenv$util <- reticulate::import_from_path("util",
                                                util_path,
                                                delay_load = TRUE)
  } else {
    message("Python package stickleback not found.")
  }
}
