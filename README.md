
All the works are contained in the **workflows** folder here.

This is the summary what's in there. **Number 1-2** are more related to the current stage.

If running **1. Missing_Value_Analysis.Rmd** offline, you will need at least to download **2. R scripts (folder)** and **4. Data (folder)** as well.

**1. Missing_Value_Analysis.Rmd**
  - This is the main report file
  - Including summary results like tables and plots

**2. R scripts (folder)**
  - This contains all r scripts used in the mail report file Missing_Value_Analysis.Rmd
  - Here is a brief summary of each scripts for
    - Function.R (functions like LOCF, MOCF, and to_long_format)
    - VITAL data formats.R (loading VITAL data set, formatting to long, and MICE imputing)
    - Acupuncture data formats.R (The same for acupuncture data set)
    - Summary analysis.R (The code for Data summary section)
    - Category time analysis.R (The code for Category time analysis section)
    - Continuous time analysis.R (The code for Treat time as continuous variable section)
    - Different Imputation Method.R (The code for Different Imputation Method section)
    - Sensitive Analysis.R (The code for section)
  
3. final report.Rmd
  - This is the final thesis to be sumitted
  - Not completed
  
4. Data (folder)
  - Contains the raw data for both acupuncture and VITAL studies
  
5. Backup Codes (folder)
  - Contains codes that may be used in the future
  
6. Outdated (folder)
  - Codes are not likely to be used anymore
