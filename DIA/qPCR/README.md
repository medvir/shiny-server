# qPCR
Interactive shiny app to convert ct values in copies/ml based on standard curves for
- different viruses
- different qPCR machines
- manual input or uploaded raw data files

Open ui.R and server.R in R to run the app.
Report.Rmd is the template for the report in pdf format.

Multiple samples can be selected from uploaded files for calculaiton and report generation.
Ct values below or above a certain threshold will be ignored (NA).
