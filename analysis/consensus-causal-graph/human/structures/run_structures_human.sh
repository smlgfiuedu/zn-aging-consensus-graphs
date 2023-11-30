#!bin/bash
data="human_representative"
config_file="./configs/${data}.structmb.config"
current_time=$(date "+%Y.%m.%d")

nohup echo $config_file | getstructure > getstruct.${data}.${current_time}.log &