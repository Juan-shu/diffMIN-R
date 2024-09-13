# diffMIN : example data

## Data

In the `data` branch of our GitHub repository, we provide two data files for users to reference and demonstrate:

1. **`exp.rda`**: This is a sample dataset containing single-cell sequencing data from cardiomyocytes after the mouse TAC model. This dataset can be used to demonstrate and test the data processing capabilities of the `diffMIN` package.

2. **`obj.rda`**: This is an intermediate data file containing results computed using the `diffMIN` method. As the network computation step is very time-consuming, we have provided the computed results in this file so that users can directly load and proceed with further analysis.

## Using the Data

```bash
# Create a directory to store the data files
mkdir -p data

# Download the data files using wget
wget -c https://github.com/Juan-shu/diffMIN-R/raw/data/exp.rda -P data/
wget -c https://github.com/Juan-shu/diffMIN-R/raw/data/obj.rda -P data/

# If you encounter any issues while downloading, please visit the GitHub repository and download the files directly.
