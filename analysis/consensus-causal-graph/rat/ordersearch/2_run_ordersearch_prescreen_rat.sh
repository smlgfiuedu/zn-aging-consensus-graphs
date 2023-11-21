#!bin/bash
data="rat_representative"
runtime="4"
current_time=$(date "+%Y.%m.%d")
config_file="./configs/${data}.${runtime}h.preprior.config"
results_file="./results/${data}_${runtime}h_mcmc_prescreen_results_${current_time}.txt"

echo "configuration file at $config_file"
echo "results located in $results_file"
mkdir -p "results"
nohup echo $config_file | mcmc_prescreen > $results_file &
