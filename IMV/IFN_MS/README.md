## Content of this app

`ui.R` and `server.R` are the standard files defining the app. Besides
shiny and tidyverse, they only require package **circlize** to be
installed.

`save_app_data.R` is the script that takes original mass spec data,
performs some basic manipulation and saves into `appdata/selected.csv`.

This file `selected.csv` is not included
in the git repo because of its size (~20Mb for the first experiment).
Its columns are:

- `time`
- `treatment`
- `full_id`
- `gene`
- `log_fc`
- `pvalue`
- `qvalue`
