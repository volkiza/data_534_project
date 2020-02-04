library(devtools)
library(usethis)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "Luftpack")

#Set your name
my_desc$set("Authors@R",
              c('person("Elizaveta",
                       "Volkova",
                       role = c("aut", "cre")),
                person(given = "Sharukh",
                       family = "Alvi",
                       role = "ctb"),
                person(given = "Jay",
                       family = "Cho",
                       role = "ctb")'))

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.0.9000")

# The title of your package
my_desc$set(Title = "This package is an API wrapper for the Lufthansa Open API")
# The description of your package
my_desc$set(Description = "API wrapper for the Lufthansa Open API that allows user to select and download as csv file the availability of lounges in selected airport and also see the location of the airport on the map.")

# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, and lifecycle badge
use_mit_license(name = "Elizaveta Volkova")
use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()

# Get the dependencies
use_package("dplyr")
use_package("jsonlite")
use_package("httr")
use_package("tibble")
use_package("tidyr")
use_package("stringr")
use_package("purrr")
use_package("ggplot2")
use_package("tidyverse")
use_package("ggmap")
use_package("plyr")
use_package("maps")
use_package("mapdata")
use_package("MUCflights")

# Clean your description
use_tidy_description()
