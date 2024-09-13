# diffMIN: Reconstructing sample-specific differential gene regulatory networks based on mutual information

## Motivation

Gene regulatory mechanisms are pivotal in the study of biological systems. Sample-specific networks (SSNs) have recently proven highly effective in scrutinizing diverse biological processes more precisely. Utilizing single-sample networks as features for clustering, personalized diagnosis, and treatment has gained increasing significance, surpassing the conventional reliance on differential gene expression. Several computational methods have been developed for the inference of SSNs. However, the existing methods are currently limited to constructing gene regulatory networks for individual samples.

## Results

We propose a new method for reconstructing sample-specific differential gene regulatory networks based on mutual information, named **diffMIN**. 

Key findings include:
- **Improved Performance**: Analysis of various single-cell datasets demonstrated that diffMIN outperforms existing SSNs algorithms regarding cell clustering. Moreover, it is more effective than clustering based solely on gene expression.
- **Application in Heart Failure**: We utilized diffMIN to analyze transverse aortic constriction (TAC) single-cell data, enabling us to generate differential networks highlighting the changes in pressure-overload, ultimately identifying a monotonic change in regulatory networks during heart failure.
- **Cancer Analysis**: Applying diffMIN on breast cancer samples from The Cancer Genome Atlas (TCGA) revealed that diffMIN can effectively identify important genes that may not exhibit significant expression differences but are highly correlated with patient prognosis. This discovery highlights the potential utility of diffMIN in identifying crucial genes in cancer.

In conclusion, our method is widely applicable and performs well in cell clustering, identification of key genes, investigation of biological processes, and disease diagnosis by constructing sample-specific differential networks.

## Help 

For detailed documentation on functions and usage, refer to https://juan-shu.github.io/diffMIN-R/diffMIN_tutorial.html

## Installation

You can install the `diffMIN` R package from GitHub using the `remotes` package. In R, run:

```r
# Install the remotes package if you don't have it
install.packages("remotes")

# Install the diffMIN package from GitHub
remotes::install_github("Juan-shu/diffMIN-R", ref = "master")
