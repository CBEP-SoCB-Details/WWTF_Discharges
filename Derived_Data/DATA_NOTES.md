# Total Discharges
Original PDFs in "flowsforcascobaydischarges.zip', in "Original_Data" folder
e-mailed to me by Gregg Wood, August 24, 2018.

Monthly data was assembled based on PDF files in the "Original_Data" folder.
Data was hand entered into "Total_Discharge_Volumes.xlsx"" file 
as the tab "Monthly Discharge Data".  Interpretation followed notes from Greg 
Wood on the PDF data sheets.

Falmouth WWTF discharge N came in e-mails directly from the Falmouth Plant staff
back in 2018.  Date and details of the e-mail are now missing.

Monthly data from May 2016 through April 2018 was aggregated to average annual
totals in the R Notebook "Wastewater_Discharges_2016-2018.Rmd".  Annual
summary data was exported as "annual_discharges_2016-2018.csv"."  All values in 
that table are close to (within 5% of) the values reported in the data from 
Maine DEP as average discharges from 2012-2016.

Note that raw data in the PDF sheets has no units.  We know from other sources,
and because results match the DEP data, that values represent discharges in 
millions of gallons per day (MGD).

# Nitrogen Data and Alternate Discharge Data
"Casco Bay effluent N data for CB 091018.xlsx" received from Angie Brewer 
of Maine DEP,  via e-mail dated 9/10/2018.  Includes two tabs.  The "QC final" 
tab contains "raw" data in NOx and TN concentrations from several Casco Bay
wastewater facilities, based largely on 2008 data.  The "summary" tab shows 
calculations for estimating average daily and annual load from each plant.  

The values in the "summary" tab for `FACILITY_ MEAN_ TN (MG/L)` do not exactly
match the mean based on the data on the "QC final" tab, but they are close.

The principal data of  interest on this tab for our purposes is the column
containing `MONTHLY_ AVG_ FLOW_2012-2016 (MGD)`, which provides the estimate we 
use in  initial calculations to estimate total discharges from each plant.

"Curtis Bohlen 043021.xlsx" contains updated data in the "QC final" tab.  The 
updated data  adds data from 2017, 2018, and 2019, and is a superset of the 
data just described.  The data on the "summary" tab is unchanged, and thus all 
values except `MONTHLY_ AVG_ FLOW_2012-2016 (MGD)` are out of date.

Summaries of TN concentrations, expressed as sample sizes, means, SDs, and SE of
the mean of available monthly data for each site, are exported from the R
notebook "Annual_Nitrogen_Concetration_Datae.Rmd"as "Annual_Summary_TN.csv".
Data is total nitrogen concentration, in units of mg/l.  Data includes an 
alternate estimate of mean annual TN concentrations for Portland's East End WWTF
that incorporates older data as a surrogate for (unmeasured here) winter 
concentrations.
