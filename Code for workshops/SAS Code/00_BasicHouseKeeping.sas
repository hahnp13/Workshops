*=== Run this first and you won't have to worry about it later ===;
dm'log;clear;output;clear;odsresults;clear;';	*clears most output and all log;
options nodate nocenter  formdlim = '-' ls=240 ps=5000 symbolgen;

*==================================== Edzard's path ===============================================;
%let path 	= T:\Demard_Emilie_Stat_workshop\;
%let path_out = T:\Demard_Emilie_Stat_workshop/Results\;

*================================= Emilie's path ================================================;
/*%let path = C:\Users\Emilie\OneDrive - University of Florida\Desktop\Postdoc\Conferences and Awards\FES 2025\Stat workshop\;*Home PC;*/
/*%let path_out = C:\Users\Emilie\OneDrive - University of Florida\Desktop\Postdoc\Conferences and Awards\FES 2025\Stat workshop\Results\;*/
%let path =	C:\Users\edemard\OneDrive - University of Florida\Desktop\Postdoc\Conferences and Awards\FES 2025\Stat workshop\;*Work PC;
%let path_out = C:\Users\edemard\OneDrive - University of Florida\Desktop\Postdoc\Conferences and Awards\FES 2025\Stat workshop\Results\;


*================================= Participant's path ================================================;
%let path 	= ;*path to where you excel data file is;
%let path_out = ;*path to export results from analysis, please create a result folder;


*Using macro to call file in 01_cleaning;
%let XL_in = beall.webworms;

libname AGR "&path";


*use for graphing in step 8 from 02_Glimmix;
Proc format;
	value $trt 		"NN"="T1" "YN"="T2" "NY"="T3" "YY"="T4";
run;
