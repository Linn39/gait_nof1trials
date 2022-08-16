## About
This is a repository for the publication Analyzing Population-Level Trials as N-of-1 Trials: an Application to Gait.

## Usage
Save data to:\
```./data/processed/features/```

Retrieve and install packages required for this project:\
```renv::restore()```

Get statistical summary of the data:\
run ```src/main_statistical_summary.R```

Run sampling with JAGS:\
run ```src/main_run_models.R```

Plot results:\
run ```src/plot_results.Rmd```

Quality control analyses:\
run ```mcmc_diagnosis.Rmd```

Compare posterior estimates of different models:\
run ```src/compare_models.Rmd```







