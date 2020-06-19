# Monte Carlo Simulation

In this repository we compare various methods which can be used to estimate
heterogeneous treatment effects in a classic Neyman-Rubin potential outcomes setting.

For all important functions we provide *docstrings* which can be used as reference.
Importantly when using the package [docstring](https://github.com/Dasonk/docstring)
you can access the docstring by typing ``?func``.

## Working in this Repository

For complete reproducibility we provide a (conda) environment file, that can be used
to install a complete environment in which all programs can be executed. Otherwise
all packages in the environment file can be installed manually.

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

- Naive estimator
- k-nearest neighbors
- T-learner using RandomForest as base learner
- Generalized Random Forests
- Local Linear Forests

## Data Generating Processes

We consider four important data processes.

1. First, we look at complex heterogeneity in the treatment effect which is well
behaved at the boundaries.
2. Second, we look at a treatment effect which exhibits curvature at the boundaries.
3. Third, we consider a setting in which only a few individuals receive treatment.
4. Fourth, we examine a case in which the treatment effect is very simple but the
potential outcome functions are both highly complex.

## Presentation

The presentation notebook contains some first drafts for the presentation. You can see
a notebook-version of the presentation here.

<a href="https://nbviewer.jupyter.org/github/timmens/topics-econometrics/blob/master/presentation.ipynb" 
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
