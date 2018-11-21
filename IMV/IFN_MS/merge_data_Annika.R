library(readr)
library(tidyverse)

path = "/Volumes/Research/Common/personal folders/Annika/Michael_Roundlbots/MQ/"

files = list.files(path, pattern = ".csv")

data = data.frame()
for (f in files) {
    file_f = read_delim(paste0(path, f), 
               ";",
               escape_double = FALSE,
               trim_ws = TRUE, 
               skip = 0) %>% ### skipping first n lines!! ###
        #mutate(treatment = gsub("[^/]+$", "", treatment)) %>% ### modify treatment ###
        select(time, treatment, full_id, gene, log_fc, pvalue, qvalue)
        
    data = rbind(data, file_f)
    }

write.csv(data, file = paste0(path, "MQ.csv"))