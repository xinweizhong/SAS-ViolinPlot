********************************************************;
Options notes nomprint nosymbolgen nomlogic nofmterr nosource nosource2 missing=' ' noquotelenmax linesize=max noBYLINE;
dm "output;clear;log;clear;odsresult;clear;";
proc delete data=_all_; run;
%macro rootpath;
%global program_path program_name;
%if %symexist(_SASPROGRAMFILE) %then %let _fpath=%qsysfunc(compress(&_SASPROGRAMFILE,"'"));
	%else %let _fpath=%sysget(SAS_EXECFILEPATH);
%let program_path=%sysfunc(prxchange(s/(.*)\\.*/\1/,-1,%upcase(&_fpath.)));
%let program_name=%scan(&_fpath., -2, .\);
%put NOTE: ----[program_path = &program_path.]----;
%put NOTE: ----[program_name = &program_name.]----;
%mend rootpath;
%rootpath;

%inc "&program_path./ViolinPlot-macro.sas";
********************************************************;

options noxwait xsync;
x "attrib -r ""&program_path.\Violinplot-macro-test.pdf"" ";

ods _all_ close;
title;footnote;
options device=pdf;
options topmargin=0.1in bottommargin=0.1in leftmargin=0.1in rightmargin=0.1in;
options orientation=landscape nodate nonumber;
ods pdf file="&program_path.\Violinplot-macro-test.pdf"  style=trial nogtitle nogfoot ;

ods graphics on; 
ods graphics /reset  noborder maxlegendarea=55  outputfmt =pdf height =7 in width = 7in  attrpriority=none;
ods escapechar='^';

proc format;
	value xfmt 1='Group 01' 2='Group 02' 3='Group 03';
run;

proc sort data = sashelp.cars out = cars;
    by Cylinders Horsepower;
	where cylinders in (4 6 8);
run;


ods graphics/antialiasmax=4200 discretemax=2400;
%let var=Horsepower;

%violinPlot(indat = cars
		            ,outcomeVar      = &var.
		            ,groupVar        = Cylinders
					,groupcd= %str(4#8#6)
					,xfmt=xfmt.
					,Yby=
					,xaxisopts=
					,yaxisopts=
					,densityopts=
					,quartileYN = N
				    ,quartileSymbolsYN = N
					,quartileopts=
				    ,jitterYN = Y
				   	,jitteropts=%str(jitterwidth=0.8/SIZE=4px/SYMBOL=circlefilled/color=blue)
					,boxplotYN=Y
					,boxplotopts=%str(boxwidth=0.3/LCOLOR=CX000000/DISPLAY=outline/FILLTRANSPARENCY=0.5)
				    ,meanYN= 
					,meanopts=
				    ,trendLineYN= 
				    ,trendStatistic= 
					,trendLineopts=
					,add_annods=
				    ,debug=0);
ods pdf close;
ods listing;

x "attrib +r ""&program_path.\Violinplot-macro-test.pdf"" ";

