# Campus Connections

## About

This repository captures the work performed in support of the Colorado State University Campus Connections program.
The contained scripts summarize, visualize, model, and simulate the social networks represented in the campus connection data.
Please direct any questions to alex.fout@colostate.edu

## Setup

All required R packages are loaded in config.R, so look there for a list of packages that need to be loaded. 
You also may want to set the output and data directories in config.R as well. 

## Running

These scripts are intended to be run somewhat independent of one another. 
First you must load in the data using load_and_clean.R, but then you can run any of the other scripts as desired. 
One exception is that model_mc.R requires that modeling be run, in order to access fitted graphical models which are used in the Monte Carlo simulation.
