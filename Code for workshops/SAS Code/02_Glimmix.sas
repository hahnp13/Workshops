dm 'log;clear;output;clear;odsresults;clear;';
proc datasets library=work kill memtype=data nolist;quit;
title;
/*%let resp_name= y;*/
/*%let resp_name= logy;*/

*Renaming permanant data set;
Data Selected;
	set AGR.Webworm_clean;
/*	where resp_name="&resp_name";*/
/*	if response=0 then response= 0.01;*/
run;
proc print data= _Last_(obs=16);run;

**Step 1 Seting a basic model in Sas;
dm'log;clear;output;clear;odsresults;clear;';*Clearing the output;	
Proc datasets library=work nolist;delete Fout CondFout Cout Mout ANOVA Sout;quit;

ods select StudentPanel ;
PROC GLIMMIX data=selected plots=Studentpanel ; 
where resp_name="logy";
	Class row col1 trt block spray lead ;
	Model response=spray|lead;
	ods output Tests3=ANOVA fitstatistics=fout;
	output out =resid pred(ilink)=pred student=student;
run;
ODS select all;
Proc print data=Fout; run;
Proc print data=ANOVA ; run; 


**Step 1b Seting up a GLM in Sas and 2b check residues and overdispersion;
dm'log;clear;output;clear;odsresults;clear;';*Clearing the output;	
Proc datasets library=work nolist;delete Fout CondFout Cout Mout ANOVA Sout;quit;

ods select StudentPanel ;
PROC GLIMMIX data=selected plots=Studentpanel ; 
where resp_name="y";
	Class row col1 trt block spray lead ;
	Model response=spray|lead/dist=poisson;
	ods output Tests3=ANOVA fitstatistics=fout;
	output out =resid pred(ilink)=pred student=student;
run;
ODS select all;
Proc print data=Fout; run;* check overdispersion with Fout;
Proc print data=ANOVA ; run; 

*Step 1c. Update the GLM to correct overdispers. and step 2c check for overdispersion;
dm'log;clear;output;clear;odsresults;clear;';*Clearing the output;	
Proc datasets library=work nolist;delete Fout CondFout Cout Mout ANOVA Sout;quit;

ods select StudentPanel ;
PROC GLIMMIX data=selected plots=Studentpanel ; 
where resp_name="y";
	Class row col1 trt block spray lead ;
	Model response=spray|lead/dist=nb;
	ods output Tests3=ANOVA fitstatistics=fout;
	output out =resid pred(ilink)=pred student=student;
run;
ODS select all;
Proc print data=Fout; run;* check overdispersion with Fout;
Proc print data=ANOVA ; run; 

*Step 1d. Update the GLM to correct overdisp. and step 2d and 2c check residues and overdisp.;
*Step 3 check random effects and model basics; 
*Step 4 check significance and step 5 calculate treamtmant means;
dm'log;clear;output;clear;odsresults;clear;';*Clearing the output;	
Proc datasets library=work nolist;delete Fout CondFout Cout Mout ANOVA Sout;quit;

ods select StudentPanel ;
PROC GLIMMIX data=selected plots=Studentpanel method=laplace; 
where resp_name="y";
	Class row col1 trt block spray lead ;
	Model response=spray|lead/dist=nb;
	Random Intercept/subject=block;
	Lsmeans spray*lead/ilink slicediff=spray cl adjdfe=row adjust=tukey;
	ods output Tests3=ANOVA fitstatistics=fout covparms=Cout Lsmeans=Lsmeans condfitstatistics=condfout;
	output out =resid pred(ilink)=pred student=student;
run;
ODS select all;
Proc print data=Fout; run;
Proc print data=ANOVA ; run; *step 4: check for singificance;
Proc print data=Cout ; run; *step 3: check random effects and model basics;
Proc print data=Lsmeans ; run; *Step 5: treatment means;
proc print data=condfout;run;*Step 2c: check for overdispersion;

*To export ANOVA table and lsmeans in excel sheet, unblcok lines 88-91;
/*libname xl xlsx "&path_out.Results.xlsx";*/
/*	data xl.ANOVA_Table;set ANOVA;run;*/
/*	data xl.Lsmeans;set Lsmeans;run;*/
/*libname xl clear;*/

*Step 6. Conduct contrasts;
dm'log;clear;output;clear;odsresults;clear;';	
Proc datasets library=work nolist;delete Fout CondFout Cout Mout ANOVA Sout Dout contrasts estimates lsmestimates;quit;

ods select StudentPanel ;
PROC GLIMMIX data=selected plots=Studentpanel method=laplace;
where resp_name="y";	
	Class row col1 trt block spray lead ;
	Model response= spray*lead/dist=nb solution cl;* ddfm=kr noint htype=1;
	Random Intercept/subject=block;
/*	Random Intercept/subject=block*Trt;*/
	Lsmeans spray*lead/diff=all ilink cl adjust=tukey;
		contrast 'T1 vs T2' spray*lead 1 -1 0 0;
	    contrast 'T1 vs T3' spray*lead 1 0 -1 0;
	    contrast 'T1 vs T4' spray*lead 1 0 0 -1;
	    contrast 'T2 vs T3' spray*lead 0 1 -1 0;
	    contrast 'T2 vs T4' spray*lead 0 1 0 -1;
	    contrast 'T3 vs T4' spray*lead 0 0 1 -1;

		estimate 'T1 vs T2' spray*lead 1 -1 0 0;
	    estimate 'T1 vs T3' spray*lead 1 0 -1 0;
	    estimate 'T1 vs T4' spray*lead 1 0 0 -1;
	    estimate 'T2 vs T3' spray*lead 0 1 -1 0;
	    estimate 'T2 vs T4' spray*lead 0 1 0 -1;
	    estimate 'T3 vs T4' spray*lead 0 0 1 -1;

	 	LSMestimate Spray*Lead
		 'T1 vs T2'  1 -1 0 0,
	     'T1 vs T3'  1 0 -1 0,
	     'T1 vs T4'  1 0 0 -1,
	     'T2 vs T3'  0 1 -1 0,
	     'T2 vs T4'  0 1 0 -1,
	     'T3 vs T4'  0 0 1 -1;

	ods output Tests3=ANOVA Lsmeans=lsmeans slicediffs=Sout covparms=Cout diffs=diffs fitstatistics=fout 
	parameterestimates=pout contrasts=contrasts estimates=estimates LSMestimates=LSMestimates;
	output out =resid pred(ilink)=pred student=student;
run;
ODS select all;
Proc print data=lsmeans; run;
Proc print data=Pout; run;
Proc print data=contrasts; run;* contrast, estimates and LSMestimates gave same results;
Proc print data=estimates; run;
Proc print data=LSMestimates; run;


*Step 6. Conduct contrasts=letters;
* this macro expects datasets to be named lsmeans and diffs (see table naming in proc glimmix);
%include "&path.mult_letter.sas";
ods select none;
%mult(trt=spray, by=., by2=, by3=., alpha=0.05, p=probt, descending=1); *by use for interaction,This macro finds a letters display for all pairwise comparisons;
quit;
ODS select all;
proc print data=MEANS_AND_LETTERS (obs=15);
run;

*Step to clean the table, keep only variables used for graphing and add a variable treatment;
Data to_graph;
	set MEANS_AND_LETTERS;
	trt=catt(spray, lead);*use function to concatenates 2 character variables into trt;
	keep trt resp_name spray lead  Mu StdErrMu  Lower Upper LowerMu UpperMu Letter ;
/*	Format trt $trt.;*unblock this line to use format and rename your treatments with numbers like T1, T2, t3;*/
run;
Proc print data=_last_(obs=14); run;
*since you are making all possible comparisons you don't really need contrasts;
	*However, since you have a factorial treatment structure does it really make sense to compare all treatment;


*Step 8: make graphs;

*unblock lines 160-162 to export graph in tiff;
/*options reset = all device = TIFF;*/
/*ods listing device = TIFF gpath ="&path_out" image_dpi = 300 file = "Graph" ;*/
/*title;*/

*unblock line 165 to export powerpoint graph;
/*ods powerpoint file="&path_out.webworm_bargraph.pptx";*/

ODS graphics/ reset= all height=5in width=8 in attrpriority=none; title;
Proc sgplot data=To_graph;
	vbarparm category=trt response=Mu/group=trt groupdisplay=cluster  limitlower=lowerMu limitupper=UpperMu
		name="Name" limitattrs=(color=black)grouporder=ascending;
	Scatter x=trt y=UpperMu / datalabel=letter markerattrs=(symbol=circlefilled size=0) datalabelpos=top
								  datalabelattrs=(family='TimesNewRoman' size=12);
	xaxis label="Treatment" 
										labelattrs=(family='TimesNewRoman' size=14 weight=bold color=black)
										valueattrs=(family='TimesNewRoman' size=12 weight=normal color=black);
	yaxis label="Mean count of webworms"
										labelattrs=(family='TimesNewRoman' size=14 weight=bold color=black)
										valueattrs=(family='TimesNewRoman' size=12 weight=normal color=black);

	Styleattrs datacolors=(DELG BILG BIGB VLIBG) 
				datacontrastcolors=(black black black black) 
				datalinepatterns=(solid solid solid solid ) ;*BIGB BILG DELG VLIBG VIGB;
	keylegend "Name"/ valueattrs=(family=TimesNewroman size=11pt)position=bottom;

run;
ODS graphics/ reset= all;
ods powerpoint close;
ods printer close;
ods listing close;
ods html;
*to split y-axis title into two lines add this code to where you want to split the text(*ESC*){unicode '000a'x}
example: 'yaxis label= "mean count of psyllid (*ESC*){unicode '000a'x} per tap sample"' ;



