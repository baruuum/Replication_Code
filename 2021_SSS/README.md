# Replication code for Park (2021)

To replicate the results, your directory structure should look as the following:

```
.
├── rawdata
│   └── GSS7214_R3.DTA
├── scripts
│    ├── 0_Functions.R
│    ├── 1-1_Read_GSS_&_Pop_Est.R
│    ├── 1-2_Data_Setup.R
│    ├── 1-3_Predictor_Setup.R
│    ├── 1-4_Pop_Ests.R
│    ├── 2-1_Model_Codes.R
│    ├── 2-2_Fit_Models.R
│    ├── 2-3_Analysis_Dimensions_old.R
│    ├── 2-3_Analysis_GOF.R
│    ├── 2-4_Conv_Stats.R
│    ├── 3-1_Analysis_Salience_and_Dist.R
│    ├── 3-1_Analysis_old.R
│    ├── 3-2_Analysis_Space.R
│    ├── 3-2_Analysis_Space_old.R
│    └── 3-3_Analysis_Dvals.R
├── 0_Setup_Data_and_Models.R
├── 0_Setup_Data_and_Models.sh
├── 1_Fit_Dim0.R
├── 1_Fit_Dim0.sh
├── 2_Fit_Dim2.R
├── 2_Fit_Dim2.sh
├── 3_Fit_Dim3.R
├── 3_Fit_Dim3.sh
├── 4_Fit_Dim4.R
├── 4_Fit_Dim4.sh
├── 5_Fit_Dim5.R
├── 5_Fit_Dim5.sh
├── 6_Analyze_Results.R
├── 6_Analyze_Results.sh
└── README.md
```

The `./rawdata/GSS7214_R3.DTA` is the cummulative GSS datafile from 1972 to 2014, which can be downloaded from [this website](https://gss.norc.org/). As the analysis uses only the 2006 data, downloading only the 2006 dataset and renaming it should produce the same results.

Running the bash (`.sh`) files in turn will replicate all the results in the paper. For example, to run `0_Setup_Data_and_Models.sh`, simply type into the command line:

```bash
bash 0_Setup_Data_and_Models.sh
```

The scripts depend on the following `R` packages:

```
dplyr
data.table
survey
readstata13
rstan
cmdstanr
bayesplot
here
scales
ggplot2
plotly
reshape2
loo
mclust
MASS
```
as well as command-line version of Stan,  [CmdStan](https://mc-stan.org/users/interfaces/cmdstan.html). The script will automatically try to install these dependencies.

Notice that the analysis was run on two different platforms. While `0_Setup_Data_and_Models.sh` and `6_Analyze_Results.sh` were run on a MacOS machine, fitting of the Stan models was done on a Linux machine.

The log of my last run of each of the scripts can be found in the `./logs` directory, which includes, at the end, the session information.
