#!/bin/bash

# make sure conda is accessable
CONDA_BASE=$(conda info --base)
source $CONDA_BASE/etc/profile.d/conda.sh

# use virtual env
VENV=comp-logic
conda activate $VENV
python prolexa/prolexa_plus.py
