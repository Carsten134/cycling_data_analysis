# Cycling behaviour in Düsseldorf: an empirical analysis
This repository holds R-Code for an application at TU-Dortmund. The objective of the exercise (as part of proof for special technical skills) was to analyse a cycling dataset from city Düsseldorf.

There are three main files, that contain the code and a 'Data' folder, which contains three datasets. The R-files are:
- `data_processing.R`
    + contains all steps needed to clean the raw dataset
- `decriptive_data_analysis.R`
    + contains building of aggregrate datasets and a descriptive analysis
- `inductive_data_analysis.R`
    + contains building of aggregate datasets and application of tests and correlation coefficients

The datasets are:
- `cyclist_counts.csv`
    + the raw dataset from opendata Düsseldorf
- `locations_of_count.csv`
    + geodata of the countings-stations
- `raw_data.csv`
    + the processed raw data (from `cyclist_count.csv`)

