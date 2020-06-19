# Topics class - Econometrics & Statistics

In this repository we store the material used for the presentation for the topics class
in econometrics and statistics at uni bonn, summer 2020. In particular we consider the
extension from classical random forest algorithms to the causal setting, as well as
new functionalities of generalized random forests and local linear forests in
comparison to the classical random forest algorithm.

See our introduction notebook for more infomation:
<a href="https://nbviewer.jupyter.org/github/timmens/topics-econometrics/blob/master/introduction.ipynb" 
    target="_parent">
    <img align="center" 
   src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png" 
       width="109" height="20">
</a> 

## Working in this Repository (or running the scripts locally)

For complete reproducibility we provide a (conda) environment file, that can be used
to install a complete environment in which all programs can be executed. Otherwise
all packages in the environment file can of course also be installed manually.

### Installation of the conda kernel

```bash
$ conda env create -f environment.yml
$ conda activate topics-econometrics
$ Rscript -e 'devtools::install_github("soerenkuenzel/causalToolbox")'
$ Rscript -e 'IRkernel::installspec(name="topics-econometrics")'
```
Where the last step might be necessary to run an R kernel in a jupyter notebook.

## Methods

We compare the following methods:

- Constant estimator
- k-nearest neighbors
- Polynomial model (with interactions)
- T-learner using RandomForest as base learner
- Generalized Random Forests
- Local Linear Forests

See our methods notebook for more information:
<a href="https://nbviewer.jupyter.org/github/timmens/topics-econometrics/blob/master/methods.ipynb" 
    target="_parent">
    <img align="center" 
   src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png" 
       width="109" height="20">
</a> 

## Data Generating Processes

We consider three important data processes, two of which fall in the category of
predicting (heterogeneous) treatment effects and the other one considers the estimation
of a conditional mean function.

See our data processes notebook for more information:
<a href="https://nbviewer.jupyter.org/github/timmens/topics-econometrics/blob/master/data_processes.ipynb" 
    target="_parent">
    <img align="center" 
   src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png" 
       width="109" height="20">
</a> 


## Simulation

The results of a Monte Carlo simulation comparing the above methods on the data
processes from above are compared in this notebook:
<a href="https://nbviewer.jupyter.org/github/timmens/topics-econometrics/blob/master/simulation.ipynb" 
    target="_parent">
    <img align="center" 
   src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png" 
       width="109" height="20">
</a> 


## References

- Recursive partitioning for heterogeneous causal effects; Athey and Imbens (2016)
- Estimation and Inference of Heterogeneous Treatment Effects using Random Forests;
Athey and Wager (2017)
- Generalized random forests; Athey, Wager and Tibshirani (2019)
- Local Linear Forests; Athey, Wager, Tibshirani and Friedberg (2019)
- Metalearners for estimating heterogeneous treatment effects using machine learning;
Künzel, Sekhon, Bickel, and Yu (2019)
