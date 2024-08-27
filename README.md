# Epidemiological Analysis of Birth Cohort Data

This project contains R code for performing epidemiological analyses on existing birth cohort data. The analyses focus on understanding developmental trajectories and the impact of sociodemographic factors on the diagnosis of autism in children. These codes support the findings of part of a paper deposited on medRxiv. 


## Manuscript Information

For a detailed explanation of the research, see our manuscript on medRxiv:  
**Title:** An axis of genetic heterogeneity in autism is indexed by age at diagnosis and is associated with varying developmental and mental health profiles
(https://www.medrxiv.org/content/10.1101/2024.07.31.24311279v1.full-text)  


## Birth Cohort Information

The analyses are based on data from birth cohorts in three countries:
- **MCS (UK)**: Millennium Cohort Study, with detailed sociodemographic variables and a larger sample of autistic children.
- **GUI (Ireland)**: Growing up in Ireland (98'), providing comparative insights.
- **LSAC (Australia)**: Longitudinal Study of Australian CHildren, data from two cohorts (Baby and Kindergarten) in Australia, allowing for a broad analysis across different age groups.

The study investigates the developmental trajectories of autistic children and the influence of sociodemographic factors on the age at diagnosis.


## Project Structure

The project is organised into three main folders, each corresponding to a different birth cohort:
- MCS: Contains detailed R scripts for the UK cohort, including sensitivity analyses for specific subgroups (e.g., children with ADHD, autistic males).
- GUI: Contains R scripts for the Irish cohort analysis.
- LSAC: Contains R scripts for the Australian cohorts (Baby and Kindergarten).

Each folder includes scripts for:
- Data extraction and preprocessing
- Latent Growth Curve Modelling (LGCM) to analyse developmental trajectories
- Growth Mixture Modelling (GMM) for identifying latent trajectories
- Mediation analysis using GMM-identified groups (from the optimal model)

## Installation

To run the analyses, you need to have R installed along with the packages specified at the beginning of each file.


## Usage

To get started with the analyses, you can explore the following folders and scripts:

1. **MCS** 
   - If you are interested in detailed, step-by-step analysis scripts, start here. Each type of analysis is separated into its own script, for example:
     - `MCS_LGCM.Rmd`: Performs Latent Growth Curve Modelling to examine developmental trajectories.
     - `MCS ADHD LGCM GMM.Rmd`: Conducts Latent Growth Curve Modelling within the ADHD only sample.
     - All files are autistic children focused anlaysis unless specified in the title.

2. **LSAC**
   - The LSAC folder is structured to accommodate the two cohorts within the study:
     - `LSAC-B analysis.Rmd`: Scripts for the Baby cohort, including LGCM, GMM, mean trajectory summary statistics generation, mediation anlaysis, and difference analysis of mental health outcomes.
     - `LSAC-B sociodemo factors.Rmd`: Scripts for the extraction and harmonisation of sociodemographic factors for the Baby cohort.
     - `LSAC-K analysis.Rmd`: Scripts for the Kindergarten cohort, similar to the Baby cohort, including all analyses.
     - `LSAC-K sociodemo factors.Rmd`: Scripts for the extraction and harmonisation of sociodemographic factors for the Kindergarten cohort.


3. **GUI**
   - The GUI folder contains a single file (`GUI_analyses`) that includes all main analyses. Due to the smaller autistic sample size, no mediation analysis was performed.


## Other Information

For sociodemographic factors, we selected specific variables and approaches based on previous studies on similar topics, the sources of which are specified in the scripts and properly cited in our manuscript.

