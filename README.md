# Consensus-Causal Graph Workflow
*Generate consensus-causal graphs for representative Bayesian networks for https://github.com/smlgfiuedu/Paper-RL*
## Order Search
*Determine best order via `MCMC_PrescreeningSwap.cpp`*

<u>Repository</u>: https://github.com/smlgfiuedu/Order-Score
### Steps
1. Compile C++ code: 
	- `g++ -x c++ -std=c++11 -o ./bin/mcmc_prescreen ./order_score/MCMC_PrescreeningSwap.cpp`

2. Preprocess input **human_representative.txt**
	- [[0_convert_data_human.R]]

3. Generate configuration file **human_representative.4h.preprior.config**
	- [[1_generate_config_human.R]]

4. Run `mcmc_prescreen`:
	- [[run_ordersearch_prescreen_human.sh]]

## Get Structures
*Determine the best structures derived from the highest scoring order from Order Search.*

<u>Repository</u>: https://github.com/smlgfiuedu/Structures-and-MB

### Steps
1. Compile C++ code:
	- `g++ -x c++ -std=c++11 -o ./bin/getstructure ./structures_mb/GetStructure.cpp`

2.  Generate configuration file **human_representative.structmb.config**
	- [[2_generate_struct_config_human.R]]

3. Run `getstructure`
	- [[run_structures_human.sh]]

## Consensus Causal Graph
*Generate a consensus network graph based on the best structures from Structures and MB*

<u>Repository:</u> https://github.com/smlgfiuedu/Consensus-Graph

### Steps
1. Run [[3_generate_consensus_graph_human.R]]

Final output is a pdf file containing the consensus network of a given structure, with edges weighted by the average strength of the relationship between nodes.