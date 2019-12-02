# Shiny Server on UZH ScienceCloud

## Installation

I created an open source shiny server on ScienceCloud. The IP is 172.23.175.116, shiny server uses the port 3838. It should be accessible within the University of Zurich network.

((Permissions: user ubuntu, group shiny))  

There is an image called "shiny server 170928" in ScienceCloud in case we have to restart the instance. This are the instruction I followed to install the server: https://www.rstudio.com/products/shiny/download-server/

R packages can be installed with the following commands:  
`sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('tidyverse', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('cowplot', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('seqinr', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('knitr', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('rmarkdown', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('DT', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('ape', repos='https://cran.rstudio.com/')\""`  
`sudo su - -c "R -e \"install.packages('plotly', repos='https://cran.rstudio.com/')\""`
`sudo su - -c "R -e \"install.packages('readxl', repos='https://cran.rstudio.com/')\""`   

I also installed latex
`wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz` and/or (?) `sudo apt-get install texlive-full`

In case the shiny server has to be re-started:  
`sudo systemctl start shiny-server`  
`sudo systemctl status shiny-server`

## Deployment of shiny apps
Shiny apps can be put into in a new directory in this repository and synchronized with `/srv/shiny-server/` on the ScienceCloud shiny server.  
`ssh ubuntu@172.23.175.116`

## Web adress 
http://172.23.175.116:3838 or http://172.23.175.116:3838/folder_name/app_name
