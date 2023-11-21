#!bin/bash
data="d1ks9v2"
runtime="1"
current_time=$(date "+%Y.%m.%d-%H.%M.%S")
config_file="./configs/${data}.${runtime}h.preprior.config"
results_file="./results/${data}_${runtime}h_results_${current_time}.txt"

echo "configuration file at $config_file"
echo "results located in $results_file"

nohup echo $config_file | mcmc_prescreen > $results_file &
