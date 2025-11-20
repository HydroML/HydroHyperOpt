# HydroHyperOpt
To download this package, run the following code:
1. Install the devtools package with install.package("devtools")
2. Load the devtools package with library(devtools)
3. Install this package with install_github("HydroML/HydroHyperOpt")
4. Load the HydroHyperOpt package with library(HydroHyperOpt)



Our new optimal defaults mentioned in our paper were carfully devised trying to find a large area in the space of hyperparameter sets that were consistently better than the previous optimal defaults. We used the evtree package in R to run this classification, though this sometimes failed, perhaps due to neeing linear instead of constant cuts in space. In these cases we performed manual classification.

![rf_kge_geD.jpg](rf_kge_geD.jpg)


# How to cite
Bilolikar, D. K., More, A., Gong, A., & Janssen, J. (2023). How to out-perform default random forest regression: choosing hyperparameters for applications in large-sample hydrology. arXiv preprint arXiv:2305.07136.
