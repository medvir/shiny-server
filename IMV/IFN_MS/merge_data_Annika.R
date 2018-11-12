library(readr)
library(tidyverse)

path = "/Volumes/Research/Common/personal folders/Annika/Michael_Roundlbots/"

files = list.files(path, pattern = ".csv")

data = data.frame()
for (f in files) {
    file_f = read_delim(paste0(path, f), 
               ";",
               escape_double = FALSE,
               trim_ws = TRUE, 
               skip = 2) %>% ### skipping first two lines!!
        select(time, treatment, full_id, gene, log_fc, pvalue, qvalue) %>%
        mutate(treatment = gsub("[^/]+$", "", treatment))
    data = rbind(data, file_f)
    }

write.csv(data, file = paste0(path, "all_data_Annika.csv"))
