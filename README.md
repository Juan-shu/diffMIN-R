# diffMIN : example data

## Data

In the `data` branch of our GitHub repository, we provide two data files for users to reference and demonstrate:

1. **`exp.rda`**: This is a sample dataset containing single-cell sequencing data from cardiomyocytes after the mouse TAC model. This dataset can be used to demonstrate and test the data processing capabilities of the `diffMIN` package.

2. **`obj.rda`**: This is an intermediate data file containing results computed using the `diffMIN` method. As the network computation step is very time-consuming, we have provided the computed results in this file so that users can directly load and proceed with further analysis.

## Using the Data

To download and load these data files in R, follow the steps below:

First, ensure you have the `httr` and `usethis` packages installed. These packages will help you download the data from GitHub and save it locally.

```r
# Install httr and usethis packages if not already installed
install.packages(c("httr", "usethis"))

# Load the necessary library
library(httr)

# exp.rda
exp_url <- "https://github.com/Juan-shu/diffMIN-R/raw/data/exp.rda"
GET(exp_url, write_disk("exp.rda", overwrite = TRUE))
load("exp.rda")

# obj.rda
obj_url <- "https://github.com/Juan-shu/diffMIN-R/raw/data/obj.rda"
GET(obj_url, write_disk("obj.rda", overwrite = TRUE))
load("obj.rda")
