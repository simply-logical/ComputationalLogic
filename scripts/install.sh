#!/bin/bash
# script to setup python virtual env with conda and docker envs
# requirements - conda
#              - docker
#              - gcc
#              - swi-prolog

# =========== Python ===============
# make sure conda is accessable
CONDA_BASE=$(conda info --base)
source $CONDA_BASE/etc/profile.d/conda.sh

# create virtual env
VENV=comp-logic
conda create -n $VENV python=3.6
conda activate $VENV

# install dependancies
pip install --upgrade pip
pip install -r requirements.txt
pip install -e .

# =========== Docker ===============
sudo docker build . -t prolexa
sudo docker build -t prolexa-plus -f Dockerfile-prolexa-plus ./
