# MHD Patient Comorbidity and Malnutrition Study

## Project Overview
This project is an academic research project I conducted in Grade 3, systematically investigating the comorbidity and malnutrition status of maintenance hemodialysis (MHD) patients at Tianjin Hospital. Through a questionnaire-based survey and data analysis, I explored the prevalence of comorbidities (e.g., cardiovascular diseases, diabetes) and malnutrition indicators (e.g., BMI, serum albumin levels), culminating in a comprehensive research report.

## Repository Structure
```text
├── results/               # Contains all analysis output files
├── .Rhistory              # R command history
├── 01.data_clean.r        # Data cleaning script
├── 02.description.r       # Data description script
├── 03.dis_statistics.r    # Descriptive statistics script
├── 04.Reliability_and_valibility.r  # Reliability and validity analysis
├── 05.lm_and_glm.r        # Linear and generalized linear models
├── 06.mediation.r         # Mediation analysis
├── 07.dis_all_vs_nul.r    # Analysis: all vs null
├── 08.dis_certain_vs_nul.r # Analysis: certain vs null
├── 09.dialysis_vs_nul.r   # Analysis: dialysis vs null
├── anal_data_row.xlsx     # Raw analytical data
├── article.docx           # Final research paper
├── data_clean.rdata       # Cleaned dataset
├── des.docx               # Description document
├── row_data.xlsx         # Original raw data
└── framework.pptx         # Project workflow presentation
```

## Usage Instructions

1. **Data Files**:
   - `row_data.xlsx`: Original raw data collected from questionnaires
   - `anal_data_row.xlsx`: Processed analytical data
   - `data_clean.rdata`: Cleaned dataset in R format

2. **Analysis Scripts**:
   - Run the R scripts sequentially from `01.data_clean.r` to `09.dialysis_vs_nul.r`
   - Each script performs a specific analysis step on `data_clean.rdata`
   - Analysis outputs are saved in the `results/` directory

3. **Final Outputs**:
   - `article.docx`: Complete research paper with findings
   - `framework.pptx`: Visual presentation of research workflow

## Analysis Workflow

1. Data cleaning and preparation (`01.data_clean.r`)
2. Descriptive statistics (`02.description.r`, `03.dis_statistics.r`)
3. Reliability and validity testing (`04.Reliability_and_valibility.r`)
4. Statistical modeling (`05.lm_and_glm.r`)
5. Mediation analysis (`06.mediation.r`)
6. Comparative analyses (`07-09.*.r`)

## Notes
- All analysis was conducted using R statistical software
- Scripts should be run in numerical order for proper data flow
- See `article.docx` for complete research methodology and results