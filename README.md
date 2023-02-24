# IPACS_MODEL
The IPACS models for D2A resource planning
If on linux, may get error when installing flextable, as require the following linux Ubuntu dependencies for the following flextable dependencies:
systemfonts - libfontconfig1-dev
textshaping - libfribidi-dev libharfbuzz-dev
ragg - libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
gdtools - libcairo2-dev
For each of these, on terminal, run command: sudo apt -y install dependencyname (e.g. sudo apt -y install libfontconfig1-dev)