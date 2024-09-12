# Meta-analysis

This repository contains the code and data for a meta-analysis project.

## Repository Structure

- **Input**: Contains input data files
  - `filtered_database.xlsx`: The main database file used for analysis

- **Plots**: Contains generated plot files
  - `complete`: Folder for complete plots
  - `descriptive`: Folder for descriptive plots
  - `summary`: Folder for summary plots

- **Script**: Contains R scripts for data analysis and visualization
  - `Data_cleaning_and_visualisation.R`: Script for data cleaning and initial visualizations
  - `Forestplots_for_publication.R`: Script for generating forest plots for publication

## Usage

1. Ensure you have R and necessary packages installed.
2. Clone this repository to your local machine.
3. Open the R scripts in the `Script` folder and run them in the following order:
   - First, run `Data_cleaning_and_visualisation.R` to clean the data and generate initial visualizations.
   - Then, run `Forestplots_for_publication.R` to create forest plots for the meta-analysis.

## Data

The main data file is `filtered_database.xlsx` located in the `Input` folder. This file contains the filtered and prepared data for the meta-analysis.

## Outputs

The scripts generate various plots which are saved in the `Plots` folder, organized into `complete`, `descriptive`, and `summary` subfolders.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For any questions or feedback, please open an issue in this repository.