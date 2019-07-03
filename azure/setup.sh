# Needed only if the image is not the standard `linux-data-science-vm`
sudo apt update
sudo apt -y install r-base
sudo apt -y install r-cran-rstan

# Add LibSSL for installing curl and devtools, see:
# https://stackoverflow.com/questions/44228055/r-rstudio-install-devtools-fails
sudo apt-get install libcurl4-openssl-dev libssl-dev
