# Casco Bay, Maine Wastewater Discharges

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Data analysis archive examining four years of coastal acidification data from Casco Bay, Maine. 

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data archives ensure the science underlying the 2020 State of the Bay report is documented and reproducible by others. The purpose of these archives is to release raw data and data analysis code whenever possible to allow others to review, critique, learn from, and build upon CBEP science. 

# Archive Structure
 CBEP 2020 State of the Bay data analysis repositories are divided into from two to four sub-folders.  All archives contain at least an "Original_Data" and a "Graphics" folder.  The other two foldersare only included if strictly necessary. 

- Original Data.  Original data, with a "DATA_SOURCES.md" or "READ ME.txt" file
  that documents data sources.
  **DATA IN THIS FOLDER IS AS ORIGINALLY PROVIDED OR ACCESSED.** 

- Derived Data.  Data derived from the original raw data.  Includes
documentation of data reorganization steps, either in the form of files (R
notebooks, Excel files, etc.) that embody data transformations, or via another
README.txt file.

- Analysis.  Contains one or more R Notebooks proceeding through the data
analysis steps. - Graphics.  Contains R Notebooks stepping through development
of related graphics, and also raw copies of resulting graphics, usually in
\*.png and \*.pdf formats.  These graphics may differ from graphics as they
appear in final State of the Bay graphical layouts.

# Summary of Data Sources

Geographic data was derived from data on MEPDES-permitted outfalls provided by
Maine DEP through the Maine Geolibrary.  While our data was downloaded from a
different link, the equivalent file is now available at:
(https://hub.arcgis.com/datasets/maine::mainedep-pollutant-discharge-elimination-system-outfall)

Data on permitted discharges and were derived from active permits,  The dates of
the permits vary.  We have tried to find permit document relevant to 2019.
**CONFIRM DATES**
Data on permitted discharges was extracted from the permit documents by hand.

Data on monitored discharges was derived from PDFs of documetnation submitted to
Maine DEP, and usually refers to discharges in CY 2017 or 2018.
Data on actual discharges was extracted from the PDF documents by hand.
