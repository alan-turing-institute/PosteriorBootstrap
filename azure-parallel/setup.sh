# Needed only if the image is not the standard `linux-data-science-vm`

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu/bionic-cran35/'
sudo apt-get update
sudo apt-get -y install r-base

# Install RStan
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Linux
sudo add-apt-repository -y "ppa:marutter/rrutter"
sudo add-apt-repository -y "ppa:marutter/c2d4u"
sudo apt-get update
sudo apt-get -y install r-cran-rstan
