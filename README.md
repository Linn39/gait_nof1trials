## About
This is a repository for the publication Analyzing Population-Level Trials as N-of-1 Trials: an Application to Gait.

## Getting Started
Clone the repository:\
```git clone https://github.com/HIAlab/gait_nof1trials.git```\
```cd gait_nof1trials```

Download data [here](../../wiki/Data) and save data to:\
```/data/processed/features/```

Retrieve and install packages required for this project:\
```renv::restore()```

## Running the analyses
Get statistical summary of the data:\
run ```src/main_statistical_summary.R```

Sampling with JAGS to obtain the posterior estimates:\
run ```src/main_run_models.R```\
Alternatively, posteriors can be directly downloaded [here](../../wiki/Data) for subsequent analyses.

Plot results:\
run ```src/plot_results.Rmd```

Quality control analyses:\
run ```mcmc_diagnosis.Rmd```

Compare posterior estimates of different models:\
run ```src/compare_models.Rmd```

## License
Distributed under the MIT License. See ```LICENSE``` for more information.





