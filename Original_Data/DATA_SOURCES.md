# Discharge Data
Original PDFs in "flowsforcascobaydischarges.zip' e-mailed to me
by Gregg Wood, August 24, 2018

Monthly data was hand entered into "Total_Discharge_Volumes.xlsx"" file 
(in the "Derived Data" folder) as the tab "Monthly Discharge Data", 
following notes from Greg Wood on the PDF data sheets.  Falmouth 
WWTF discharge N data came directly from the Falmouth Plant but
information on the date and sender of the e-mail have been misplaced.

That data was aggregated to annual totals, in the R Notebook
"Wastewater Discharge Data 2016-2018.Rmd", also in the Derived Data folder.


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

That e-mail included the following commentary:

>  I have attached effluent N data from the Casco Bay municipal facilities for
which we have data. ... I figured you would want the raw data ..., and I ...
included a summary tab with a couple plots and a table that may be interesting.
The table is updated for DEP discharge permit renewals relative to the
Reasonable Potential assessments we have done. Falmouth is pending right now and
I believe out for comment.

>  With the exception of South Portland, which monitored their effluent while we
were conducting Fore River ambient monitoring in 2016, the other Casco Bay
facilities were not asked to participate in the larger effluent N monitoring
project that I coordinated in 2015 and 2016. We had the 2008 data and had no
reason to think that effluent TN concentration would be different enough in 2015
or 2016 to warrant more sampling. The values for East End are of course
currently inaccurate since their voluntary load reduction efforts starting in
2017 (officially in 2018). The Department should have effluent N data since May
2018, though I have yet to see these. Let me know if you want me to request
those data for you.

>  Re: Cape Elizabeth and Peaks, those facilities were not part of the studies 
the Department completed for effluent N testing....


"Curtis Bohlen 043021.xlsx" contains updated data in the "QC final" tab.  The 
updated data  is based in part on data from 2017, 2018, and 2019.  The data 
on the "summary" tab is unchanged from the prior file, and thus all values 
except `MONTHLY_ AVG_ FLOW_2012-2016 (MGD)` are now out of date, based on
older data.







