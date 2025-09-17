# ------------------------
# --- Packages Imports ---
# ------------------------

box::use(
  utils[menu]
)

# --------------------------------------------
# --- Project Sctructure Utility Functions ---
# --------------------------------------------

#' @title Create a Project Section
#' @md
#' @description
#' Generates files and directories to contain a Tidy Tuesday project within the **projects**
#' directory.
#' @details
#' This function is called for its side effects. It takes no function arguments, but does prompt
#' the user in an interactive session for input to create the necessary elements of a Tidy Tuesday
#' project directory.
#'
#' The user input requested consists of:
#'
#' - Year Input (e.g. 2025)
#' - Month Input (e.g. 1 for January)
#' - Project Name (e.g. tidy_project)
#'
#' The above example input will create the following directories/files:
#' + **<projects/2025/January/tidy_project/python/>** (for python code, if desired)
#' + **<projects/2025/January/tidy_project/r/>** (for R code)
#' + **<projects/2025/January/tidy_project/static/>** (for static assets like png files of graphs)
#' + **<projects/2025/January/tidy_project/README.md>** (for a README file describing the project)
build_project_section <- function(){

  # Console input for year
  year <- readline(prompt = "What year was the Tidy dataset released? (e.g. 2025): ")

  # Console input for month
  month <- menu(
    choices = month.name,
    title = "Which month was it released? (e.g. type 1 for January)"
  ) |>
    sapply(function(num) month.name[num])

  # Console input for section name
  name <- readline(prompt = "Give the section a name: " )

  # Construct project directory
  combined_path <- paste("projects", year, month, name, sep = "/")

  # Recursively create child directories
  dir_names <- c("r/", "python/", "static/") |>
    sapply(
      function(dir){paste(combined_path, dir, sep = "/")},
      USE.NAMES = FALSE
    )

  for (item in dir_names) {
    dir.create(path = item, recursive = TRUE)
  }

  # Create README file
  file.path(combined_path, "README.md") |>
    file.create()
}
