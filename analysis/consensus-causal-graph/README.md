# Consensus-Causal Graph Workflow


## Order Search
*Determine best order via `MCMC_PrescreeningSwap.cpp`*

### Steps
Compile C++ code: 
`g++ -x c++ -std=c++11 -o ./bin/mcmc_prescreen ./order_score/MCMC_PrescreeningSwap.cpp`

Preprocess input `human_representative.txt` 
- [[0_convert_data_human.R]]

Generate configuration file `human_representative.4h.preprior.config`
- [[1_generate_config_human.R]]

Run `mcmc_prescreen`:
- [[2_run_ordersearch_prescreen_human.sh]]

# Get Structures
*Determine the best structures derived from the highest scoring order from Order Search.*


