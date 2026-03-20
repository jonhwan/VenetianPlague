# Spread of the plague in Venice, 1630–1631: epidemic entropy in a “natural experiment”

## Project Overview

Precise modeling of epidemic spread is difficult. One explanation is that disease spread is inherently stochastic. This would suggest that the distribution of cases across geographic regions would progress towards that more favored by chance. If the epidemic proceeds long enough, the allocation of cases could approach that most expected, maximizing Boltzmann–Gibbs–Shannon entropy. Here, we tested these hypotheses on mortality data from the Venetian 1630–1631 plague epidemic. Entropy per case (intensive) of the quantile function (distribution of parishes ranked by case rates) increased from an effective number of 7.32 parishes (95% CI 3.32–12.55 parishes) to 47.9 parishes (47.5–48.9 parishes) out of 50 total, indicating that the quantile function approached a uniform maximum entropy distribution. Intensive entropy of the probability density function (parishes categorized by cumulative case rate) increased from 0.63 nats (0.32–0.93 nats) to 1.75 nats (1.53–1.87 nats). The PDF approached a Gaussian distribution. The Kullback–Leibler divergence decreased from 0.84 nats (0.71–1.42 nats) to 0.12 nats (0.083–0.35 nats). These findings quantify how disease spreads and demonstrate that observed heterogeneity in infections between regions may in some circumstances be explained by chance alone.

* Pre-print: https://www.medrxiv.org/content/10.1101/2025.10.06.25335371v2
* Dataset: https://github.com/ggrrll/Venice-plague-epidemic-paper

If you have any questions about the files in this repository, please contact Jonathan Hwang or Thomas Lietman at UCSF (jonathan.l.hwang@gmail.com; tom.lietman@ucsf.edu)

## Setup

### System Requirements
```> sessionInfo()```  
```R version 4.2.0 (2022-04-22)```  
```Platform: aarch64-apple-darwin20 (64-bit)```  
```Running under: macOS Monterey 12.2.1```  

All analyses were run using R version 4.2.0 (2022-04-22) on Mac OS Monterey (12.2.1) using the RStudio IDE (https://www.rstudio.com). In this repository we have used the ```renv``` package to archive the package versions so that you can reproduce the exact compute environment, should you wish to do so.

### Installation Guide and Instructions for Use

* You can download and install R from CRAN: https://cran.r-project.org
* You can download and install RStudio from their website: https://www.rstudio.com
* All R packages required to run the analyses are sourced in the file 00-pkg-config.R.
The installation time should be < 10 minutes total on a typical desktop computer.
To reproduce all analyses in the paper, we recommend that you:

Clone the GitHub repository to your computer using git clone https://github.com/jonhwan/VenetianPlague.git in the terminal

Recreate the exact package environment using the renv package.

You can do this by opening the R project file (VenetianPlague_Entropy.Rproj) in RStudio, loading the ```renv``` package, and typing ```renv::restore()``` to restore the package environment from the projects ```renv.lock``` file.

Download the public data from the OSF repository by running the script ```00-download-public-data.R```.

All of the analysis scripts can be run sequentially using the script ```run-all.R```.

## Contents

* The main analysis:
* Exploratory analysis:
