# VirMetRunAnalysis
Shiny app to visualise and report sequencing runs analysed by VirMet tidytable.

Open `ui.R` and `server.R` in R to run the app. A warning appears if the tidytable output files `run_reads_summary.tsv` and `orgs_species_found.tsv` are not from the same run.

`RunReport.Rmd` is the template for the report in pdf format.

Multiple samples can be selected for visualization and report generation, e.g. for merging duplicates into a single report. If the MOLIS numbers (10 first digits) of the selected samples differ, a warning appears.
