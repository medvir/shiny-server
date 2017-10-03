# Shiny Server on UZH SienceCloud

## Installation

I created a shiny server on ScienceCloud. The IP is 172.23.175.116, the port 3838. It should be accessible within the University of Zurich network.

There is an image of the instance saved called shiny server in case we have to restart the instance.

Here are the instruction I followed to install the server: https://www.rstudio.com/products/shiny/download-server/

R packages can be installed with the following commands:
`sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('tidyverse', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('cowplot', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('seqinr', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('knitr', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('rmarkdown', repos='https://cran.rstudio.com/')\""`

I also installed latex
`wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz` and/or `sudo apt-get install texlive-full`

In case the shiny server has to be re-started use:
`sudo systemctl start shiny-server`
`sudo systemctl status shiny-server`

## Deployment apps
Shiny apps can be put into this repository and synchronized with the `/srv/shiny-server/` directory on the ScienceCloud shiny server. Other subfolders than `IMV` can be created.

`http://172.23.175.116:3838`
or
`http://172.23.175.116:3838/folder_name/app_name`

