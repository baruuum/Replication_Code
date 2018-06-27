

Park, Barum. 2018. “How Are We Apart? Continuity and Change in the Structure of Ideological Disagreement in the American Public, 1980-2012,” _Social Forces_, Volume 96, Issue 4, pp. 1757-1784.

- The following datasets should be placed in a directory named "~/raw_data" for the code to run.
	1) ANES cumulative datafile should be saved under the name "anes_timeseries_cdf_rawdata.txt" (dataset is downloadable from https://www.icpsr.umich.edu)
	2) CF scores should be saved under "cfscores.csv" (downloadable from http://data.stanford.edu/dime)
	3) Common Space DW-NOMINATE scores should be saved under "commonspace2012.DTA" (downloadable from http://www.voteview.com)
	4) As the liberal-conservative placements of political stimuli were not included in the ANES cumulative data file, separate files for each measured year of the ANES should be saved in the folder under the name "NESyear.dta". For example, the data set for year 1980 has to be saved under the name "NES1980.dta". (each of the files is also downloadable from https://www.icpsr.umich.edu)

- Codes:
	1) All codes are written in R
	2) codes are organized in order
	3) codes might depend on previous codes (e.g., you have to run "GRM\_Data\_Management.R" first to be able to run "GRM_Fitting\_&\_Analysis.R")
	4) each code has a "Working Directory Here!" string. Replace this string with the folder in which you have unzipped the replication materials
	5) most of the codes contain also a "Path to directory where figures should be placed" string. Replace this string with the path where you want the figures to be saved.
	6) Running the code for fitting the GRM models is extremely time consuming (most models take over 5 days to run on a Westmere x86_64 2.67GHz machine). 

- "codebook.xlsx" file contains information regarding issue items that were used in the analysis of operational ideology
	1) Variable labels as presented in the paper
	2) Original variable labels in the ANES file
	3) Classification of variables
	4) Years each item was measured
	5) Recoding history (on Sheet 2)
