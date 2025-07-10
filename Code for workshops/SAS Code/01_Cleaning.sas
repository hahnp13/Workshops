dm'log;clear;output;clear;odsresults;clear;';	*clears most output and all log;
proc datasets library=work kill memtype=data nolist;quit;

*Import data with xlsx file;
Libname xl xlsx "&Path.&XL_in..xlsx";
  Data Basedata;	
	format  row 2. col 2.  block $3. trt $2. spray $2. lead $2. ;
	set  xl.beall_webworms (firstobs=1)  indsname=ds;
	rename col=col1;
run;
proc print data=Basedata (obs=15);*where rep_10=.;run;
Libname xl clear;

*Alternative: import data with csv file;
data Basedata;
	infile "&path.beall.webworms.csv" dlm=',' firstobs=2;
	input row col	y	block$	trt$	spray$	lead$	logy; *must put $ on variables that are characters. Otherwise, SAS will won't include them;
run;
proc print data= Basedata(obs=5);run;

*Linerazing the data and creating a response name "resp_name" for logy and y ;
data Linear;
set Basedata;
array raw(*)  logy y;
	do resp_n=1 to dim(raw);
		col=Vname(raw(resp_n));
		resp_name=col;
		response=raw(resp_n)/1;
		output;
	end;
	drop resp_n col logy y;
run;
proc print data=Linear (OBS=6);run;

*Visualizing the data using proc tabulate;
proc tabulate data=linear;
	class row col1 block trt spray lead  ;
	where resp_name="y";*logy;
	var response;
/*	table trt*spray*lead,block*response*N/Nocellmerge;*/
	table trt*spray*lead,block*response*Mean/Nocellmerge;
run;

*Creating a permanent dataset (i.e. saved on your PC) set with cleaned data;
proc sort data=linear out=AGR.Webworm_clean;
	by resp_name trt block spray lead;
run;
proc print data= _Last_(obs=6);run;
