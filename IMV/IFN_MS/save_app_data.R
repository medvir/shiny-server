library(tidyverse)
library(forcats)
library(qvalue)

data_path <- 'data'
files <- dir(path = data_path, pattern = "*.csv")

selected_cols <- cols_only(Anova = col_double(), `Highest mean condition` = col_character(),
                           `Max fold change` = col_double(), Modifications = col_character(),
                           `Peptide identifier` = col_character(), Sequence = col_character(),
                           Description = col_character())
# create a data frame holding the file names
data <- data_frame(filename = files) %>%
  # a new data column
  mutate(file_contents = map(filename, ~ read_csv(file.path(data_path, .), col_types = selected_cols, skip = 2))) %>%
  # extract time and treatment from filename
  separate(filename, c('a', 'time', 'b', 'c', 'treatment', 'd')) %>%
  select(c(-a, -b, -c, -d)) %>%
  unnest() %>%
  filter(!grepl('FGCZ', Description)) %>%  # remove contaminants
  mutate(fc = ifelse(`Highest mean condition` %in% c('Control', 'control'),
                     1/`Max fold change`, `Max fold change`)) %>%
  mutate(log_fc = log2(fc)) %>%
  mutate(gene = str_match(Description, "GN=([[:punct:][:alnum:]]*)($|[:space:])")[, 2]) %>%
  filter(grepl('Phospho', Modifications)) %>%
  filter(is.finite(log_fc)) %>%
  mutate(mods = gsub(" ", "", Modifications)) %>%
  mutate(full_id = paste(Sequence, mods, sep = '-')) %>%
  mutate(time = fct_relevel(time, "30min", "90min", "4h")) %>%
  rename(pvalue = Anova)
  
data <- data %>%
  group_by(time, treatment) %>%
  mutate(qvalue = qvalue(pvalue)$qvalues, pi0 = qvalue(pvalue)$pi0) %>%
  ungroup() %>%
  select(c(time, treatment, full_id, gene, log_fc, pvalue, qvalue))

#data[, names(data) %in% c('log_fc', 'Anova', 'qvalue')] <- data[, names(data) %in% c('log_fc', 'Anova', 'qvalue')] %>%
#  map(~round(.x, 5))
data %>%
  write_csv('display/appdata/selected.csv')
