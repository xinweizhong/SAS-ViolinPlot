/*************************************************************************************************
File name:      violinplot-macro.sas
 
Study:          NA
 
SAS version:    9.4
 
Purpose:        plot violinplot
 
Macros called:
 
Notes:
 
Parameters:
 
Sample:
 
Date started:    16MAY2024
Date completed:  
 
Mod     Date            Name            Description
---     -----------     ------------    -----------------------------------------------
1.0     16MAY2024       Xinwei.Zhong    Created
 
 
************************************ Prepared by hengrui************************************/

****** Check whether the variable exists *********;
%macro varlsexist(data=,varls=);
    %let dsid = %sysfunc(open(&data)); 
	%let _dsn=%eval(%sysfunc(count(&varls.,|))+1);
	%let noexistvar=;
    %if &dsid %then %do; 
	   %do _i_v=1 %to &_dsn.;
	   	   %let _var=%scan(&varls.,&_i_v.,|);
		   %global typ_&_var.;
	       %let varnum = %sysfunc(varnum(&dsid,&_var.));
	       %if &varnum.=0 %then %let noexistvar=%str(&noexistvar. &_var.);
		   	%else %let typ_&_var. = %sysfunc(vartype(&dsid., &varnum.));
	   %end;
	   %let rc = %sysfunc(close(&dsid));
    %end;%else %do;
       %put ERR%str()OR: Dataset[&data.] not exist, please check!!;
       %abort cancel;
    %end;
    %if %length(&noexistvar.)>0 %then %do;
       %put ERR%str()OR: Variable[&noexistvar.] not exist in dataset[&data.], please check!!;
       %abort cancel;
    %end;
%mend varlsexist;

****** deal with attribute default value *********;
%macro split_attrib(attrib_info=,info_ds=);
options noquotelenmax;
%if %length(%nrbquote(&attrib_info.))=0 %then %do;
	%put ERR%str()OR: Parameter[attrib_info] uninitialized, please check!!;
	%abort cancel;
%end;
%if %length(&info_ds.)=0 %then %let info_ds=%str(__split_attrib);
data &info_ds.(drop=_id1 _id2);
	length __info $2000. subcol $500. class attrib $32. attribval $200.;
	__info=tranwrd(tranwrd(tranwrd(tranwrd("%nrbquote(&attrib_info.)",'%/','$#@'),'%\','$##@'),'%|','*#@'),'%=','*##@');
	__info=tranwrd(tranwrd(__info,'%[','$###@'),'%]','$####@');
	if substr(__info,lengthn(__info),1)='|' then __info=substr(__info,1,lengthn(__info)-1);
	do classn=1 to (count(__info,'|')+1);
		_id1=prxparse('/(\w+)=\[([^\[\]]+)\]/');
		subcol=strip(scan(__info,classn,'|'));
		if prxmatch(_id1,subcol) then do;
			class=strip(upcase(prxposn(_id1, 1, subcol)));
			subcol=prxposn(_id1, 2, subcol);
		end; 
		if substr(subcol,lengthn(subcol),1)='/' then subcol=substr(subcol,1,lengthn(subcol)-1);
		start = 1; 
	    finish = length(subcol);
		_id2=prxparse('/(\w+)=([^\/\\]+)/');
	    do ord = 1 by 1 until(start > finish); 
	        call prxnext(_id2, start, finish, subcol, position, length); 
	        if position > 0 then do;
	            attrib = strip(upcase(prxposn(_id2, 1, subcol))); 
	            attribval = prxposn(_id2, 2, subcol); 
				attribval=tranwrd(tranwrd(tranwrd(tranwrd(tranwrd(tranwrd(attribval,'$#@','/'),'$##@','\'),'*#@','|'),'*##@','='),'$###@','['),'$####@',']');
	            output;
	        end;
	        else leave;
	    end;
	end;
run;
%mend split_attrib;

%macro attrib_vmacro(default_attrib=,set_attrib_info=,vmacro_prestr=,debug=);
%if %length(%nrbquote(&default_attrib.))=0 or %length(%nrbquote(&set_attrib_info.))=0 %then %do;
	%put ERR%str()OR: Parameter[default_attrib/set_attrib_info] uninitialized, please check!!;
	%abort cancel;
%end;
%split_attrib(attrib_info=%nrbquote(&default_attrib.),info_ds=%str(__default_attrib));
%split_attrib(attrib_info=%nrbquote(&set_attrib_info.),info_ds=%str(__split_attrib));
%if %length(&debug.)=0 %then %let debug=0;

proc sql undo_policy=none;
	create table __attribval as select a.class,b.class as class0,a.ord,b.ord as ord0
		,a.attrib,b.attrib as attrib0,a.attribval as defaultval,b.attribval
		from __default_attrib as a
		left join __split_attrib as b on a.class=b.class and a.attrib=b.attrib
		order by a.classn,a.class,a.ord;
quit; 
data __attribval;
	set __attribval;
	if attribval='' then do;
		attribval=defaultval; impute=1;
	end;
	length vmacroname $50.;
	vmacroname=cats(class,"_&vmacro_prestr.",attrib);
	if lengthn(vmacroname)>32 then put "ERR" "OR:" vmacroname= "macro variable is too long!";
	call symputx(vmacroname,attribval,'g');
run;
%if "&debug."="0" %then %do;
	proc datasets nolist;
		delete __split_attrib: __default_attrib:;
	quit;
%end;
%mend attrib_vmacro;

****************************;
%macro violinPlot(indat= 
					,outcomeVar= 
				    ,groupVar= 
					,groupcd=
					,xfmt=
					,Yby=
					,xaxisopts=
					,yaxisopts=
					,densityopts=
					,quartileYN = 
				    ,quartileSymbolsYN = 
					,quartileopts=
				    ,jitterYN = 
				   	,jitteropts=
					,boxplotYN=
					,boxplotopts=
				    ,meanYN= 
					,meanopts=
				    ,trendLineYN= 
				    ,trendStatistic= 
					,trendLineopts=
					,add_annods=
					,outcomeVar_stdway=
					,kde_NGRID=
				    ,debug=) / minoperator;

%put NOTE: -------------------- Macro[&SYSMACRONAME.] Start --------------------;
*****parameter control****;
%if %length(&indat.)=0 or %length(&outcomeVar.)=0 %then %do;
	%put ERR%str()OR: Parameter[indat/outcomeVar] uninitialized, please check!!;
	%return;
%end;
%if %sysfunc(exist(&indat.))=0 %then %do;
	%put ERR%str()OR: DataSet[indat = &indat.] no exist, please check!!;
	%return;
%end;

%let _varls=%str(&outcomeVar.);
%if %length(&groupVar.)>0 %then %let _varls=%str(&_varls.|&groupVar.);

%varlsexist(data=%str(&indat.),varls=%str(&_varls.));
%if "&&typ_&outcomeVar."^="N" %then %do;
	%put ERR%str()OR:  Variable[&=outcomeVar.] in dataset[&indat.] no numeric variable, please check!!;
	%return;
%end;

%if %length(&boxplotYN.)=0 %then %let boxplotYN=%str(Y);
%let boxplotYN=%upcase(%substr(&boxplotYN.,1,1));
%if %length(&jitterYN.)=0 %then %let jitterYN=%str(Y);
%let jitterYN=%upcase(%substr(&jitterYN.,1,1));
%if %length(&quartileYN.)=0 %then %let quartileYN=%str(Y);
%let quartileYN=%upcase(%substr(&quartileYN.,1,1));
%if %length(&quartileSymbolsYN.)=0 %then %let quartileSymbolsYN=%str(N);
%let quartileSymbolsYN=%upcase(%substr(&quartileSymbolsYN.,1,1));
%if %length(&meanYN.)=0 %then %do;
	%if &boxplotYN.=Y %then %let meanYN=%str(Y);
		%else %let meanYN=%str(N);
%end;
%let meanYN=%upcase(%substr(&meanYN.,1,1));
%if %length(&trendLineYN.)=0 %then %let trendLineYN=%str(Y);
%let trendLineYN=%upcase(%substr(&trendLineYN.,1,1));
%if %length(&trendStatistic.)=0 %then %let trendStatistic=%str(Median);
%if %length(&outcomeVar_stdway.)=0 %then %let outcomeVar_stdway=%str(ALL);
%let outcomeVar_stdway=%upcase(&outcomeVar_stdway.);
%if "&outcomeVar_stdway."^="ALL" and "&outcomeVar_stdway."^="BYGROUP" %then %do;
	%put ******************************************************************************************************;
    %put *                                                                                                    *;
	%put The assignment parameter[outcomeVar_stdway] value must be [ALL|BYGROUP], The [ALL] value will be used!;
	%put *                                                                                                    *;
    %put ******************************************************************************************************;
%end;

%if %length(&kde_NGRID.)=0 %then %let kde_NGRID=%str(600);
%if %length(&debug.)=0 %then %let debug=%str(0);

************ Data manipulation *****************;

data __indat;
	set &indat.(keep=&groupVar. &outcomeVar. where = (&outcomeVar. gt .z));
	if "&groupVar." ="" then do;
		dummyVariable = 1;
		call symputx('groupVar', 'dummyVariable');
	end;
	length __&groupvar. $200.;
	__&groupvar.=strip(vvalue(&groupvar.));
run;


%if %length(&groupvar.)>0 %then %do;
	%if %length(&groupcd.)>0 %then %do;
		%let nGroupVarValues=%eval(%sysfunc(countc(&groupcd.,%str(#| )))+1);
		data __gr(where=(__&groupvar.>''));
			length __&groupvar. $200.;
			%do _i=1 %to &nGroupVarValues.;
				__grn=&_i.*2; __&groupvar.="%scan(&groupcd.,&_i.,#|)"; output;
			%end;
		run;
	%end;%else  %do;
		proc sort data =__indat out = __gr(keep=&groupvar.) nodupkey;
			where ^missing(&groupvar.);
		    by &groupvar.;
		run;
		data __gr;
			set __gr;
			by &groupvar.;
			retain __grn 0;
			__grn+1;
			length __&groupvar. $200.;
			__&groupvar.=strip(vvalue(&groupvar.));
		data __gr;
			set __gr;
			__grn=__grn*2;
		run;
	%end;
	%let __chk_gr=0;
	proc sql undo_policy=none noprint;
		create table __indat as select a.*,b.__grn from __indat as a
			left join __gr as b on strip(a.__&groupvar.)=strip(b.__&groupvar.);
		create table __chk_gr as select * from __indat where __grn=.;
		select count(*) into: __chk_gr from __chk_gr;
	quit;
	%if &__chk_gr.>0 %then %do;
		%put ERR%str()OR: The subject is unique identification or grouping information does not match. Please check the dataset [__chk_gr]!!!;
		%return;
	%end;
data __myfmt;
	set __gr;
	length fmtname label $200. ; 
    fmtname='groupVar';    
    start=__grn/2;   
    label=strip(__&groupvar.); 
	type='N';
	keep start fmtname label type;
run;
proc sort ;
	by fmtname;
run;
proc format cntlin=__myfmt; run; 
proc sql noprint;
    select count(*) into : nGroupVarValues trimmed from __gr;
quit;
%end; 
%let xoffset=%sysevalf(1/%eval(&nGroupVarValues.+ 1));

************************************************************************;
******* xaxisopts *******;
%attrib_vmacro(vmacro_prestr=%str(axis_),set_attrib_info=%nrbquote(X=[&xaxisopts.]),
default_attrib=%nrbquote(X=[label=&groupVar./LABELFT=Arial%/SimSun/LABELFS=9pt/LABELFW=bold/OFFSETMIN=&xoffset./OFFSETMAX=&xoffset./TICKFT=Arial%/SimSun/TICKFS=9pt/TICKFW=bold]));
data __attribval_all;
	set __attribval(keep=vmacroname attribval);
	%if "%upcase(&groupvar.)"="DUMMYVARIABLE" %then %do;
		if impute=1 and strip(attrib)='LABEL' then call symputx('x_axis_LABEL',' ');
	%end;
run;

******* yaxisopts *******;
%attrib_vmacro(vmacro_prestr=%str(axis_),set_attrib_info=%nrbquote(Y=[&yaxisopts.]),
default_attrib=%nrbquote(Y=[label=&outcomeVar./LABELFT=Arial%/SimSun/LABELFS=9pt/LABELFW=bold/OFFSETMIN=0.02/OFFSETMAX=0.02/TICKFT=Arial%/SimSun/TICKFS=9pt/TICKFW=bold]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

******* densityopts *******;
%attrib_vmacro(vmacro_prestr=%str(dnst_),set_attrib_info=%nrbquote(D=[&densityopts.]),
default_attrib=%nrbquote(D=[DISPLAY=(FILL OUTLINE)/densitywidth=0.8/TRANSPARENCY=0/colorls=CXDEEBF7#CX9ECAE1#CX4292C6#CX08519C
/OLColor=CX000000/OLPattern=1/OLthickness=1]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

******* quartileopts *******;
%attrib_vmacro(vmacro_prestr=%str(qlt_),set_attrib_info=%nrbquote(Q=[&quartileopts.]),
default_attrib=%nrbquote(Q=[median_COLOR=black/median_SIZE=9px/median_SYMBOL=diamondFilled/median_TRANSPARENCY=0/median_WEIGHT=NORMAL/Q1_COLOR=black/Q1_SIZE=6px/Q1_SYMBOL=diamondFilled
/Q1_TRANSPARENCY=0/Q1_WEIGHT=NORMAL/Q3_COLOR=black/Q3_SIZE=6px/Q3_SYMBOL=diamondFilled/Q3_TRANSPARENCY=0/Q3_WEIGHT=NORMAL]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

******* jitteropts *******;
%attrib_vmacro(vmacro_prestr=%str(JTR_),set_attrib_info=%nrbquote(J=[&jitteropts.]),
default_attrib=%nrbquote(J=[COLOR=black/SIZE=6px/SYMBOL=circle/TRANSPARENCY=0/WEIGHT=NORMAL/jitterwidth=0.8]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

******* boxplotopts *******;
%attrib_vmacro(vmacro_prestr=%str(box_),set_attrib_info=%nrbquote(B=[&boxplotopts.]),
default_attrib=%nrbquote(B=[boxwidth=0.18/display=outline/CAP=Y/CAPWEIGHT=0.6/LCOLOR=CX000000/LTHICKNESS=1/LPATTERN=1/MCOLOR=CX000000/MSIZE=6px/MSYMBOL=circle/MTRANSPARENCY=0/MWEIGHT=NORMAL
/FILLCOLOR=CXCCFF99/FILLTRANSPARENCY=0]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

******* meanopts *******;
%attrib_vmacro(vmacro_prestr=%str(mean_),set_attrib_info=%nrbquote(M=[&meanopts.]),
default_attrib=%nrbquote(M=[COLOR=CX000000/SIZE=6px/SYMBOL=diamondFilled/TRANSPARENCY=0/WEIGHT=NORMAL]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;
******* trendLineopts *******;
%attrib_vmacro(vmacro_prestr=%str(trl_),set_attrib_info=%nrbquote(T=[&trendLineopts.]),
default_attrib=%nrbquote(T=[DISPLAY=ALL/LCOLOR=red/LTHICKNESS=1/LPATTERN=1/MCOLOR=red/MSIZE=6px/MSYMBOL=circleFilled/MTRANSPARENCY=0/MWEIGHT=NORMAL]));
data __attribval_all;
	set __attribval_all __attribval(keep=vmacroname attribval IMPUTE);
run;

************* Statistics:  Descriptive statistics (stats) *************;
proc means noprint nway data = __indat;
	class __grn &groupVar.;
    var &outcomeVar.;
    output out=__statistics %str(mean= mean p25 = quartile1 median = median p75 = quartile3 min=min max=max);
run;

************* Statistics: Kernel density estimation (KDE) *************;
ods exclude all;
proc sort data = __indat;
	by __grn &groupVar. &outcomeVar.;
proc kde data = __indat;
	by __grn &groupVar. ;
	univar &outcomeVar. /NGRID=&kde_NGRID. noprint out = __KDE;
run;
ods exclude none;

%let boxwidth=%str(&B_BOX_boxwidth.);
************* Merge KDE and stats to assign quartiles *************;
proc sql undo_policy=none;
	%if "&outcomeVar_stdway."="ALL" %then %do;
	 create table __KDE as select *,min(density) as min_density,max(density) as max_density 
		from __KDE;
	%end;%else %do;
	 create table __KDE as select *,min(density) as min_density,max(density) as max_density 
		from __KDE group by __grn,&groupVar.;
	%end;
    create table __KDEstatistics as select a.*, 
/*			density+ b.__grn/2 as upperBand,*/
/*	        (-density) + b.__grn/2 as lowerBand,*/
			(((density- min_density)/( max_density- min_density)))/2*&d_dnst_densitywidth.+ b.__grn/2 as upperBand,
	        ((-((density- min_density)/( max_density- min_density))))/2*&d_dnst_densitywidth.+ b.__grn/2 as lowerBand,
            value as yBand,
            case when             yBand le quartile1 then  25 + 100*b.__grn
	            when quartile1 lt yBand le median    then  50 + 100*b.__grn
	            when median lt    yBand le quartile3 then  75 + 100*b.__grn
	            else 100 + 100*b.__grn
            end as quartile
		from __KDE a inner join __statistics b
             on a.&groupVar. = b.&groupVar.
             order by __grn,&groupVar.,yBand;

     create table __inDatStatistics as select a.*,
             case when         &outcomeVar. le quartile1   then  25 + 100*b.__grn
                  when quartile1 lt &outcomeVar. le median then  50 + 100*b.__grn
                  when median lt &outcomeVar. le quartile3 then  75 + 100*b.__grn
                           else 100 + 100*b.__grn
             end as quartile,
			 case when         &outcomeVar. le quartile1   then  1
                  when quartile1 lt &outcomeVar. le median then  2
                  when median lt &outcomeVar. le quartile3 then  3
                           else 4
             end as _qlcgr,
             case when              &outcomeVar. le quartile1 then b.__grn/2 + ifn(mod(monotonic(), 2), 1, -1)*ranuni(1)*(&boxwidth./2*&J_JTR_jitterwidth.*0.35)
                  when quartile1 lt &outcomeVar. le median    then b.__grn/2 + ifn(mod(monotonic(), 2), 1, -1)*ranuni(1)*(&boxwidth./2*&J_JTR_jitterwidth.)
                  when median    lt &outcomeVar. le quartile3 then b.__grn/2 + ifn(mod(monotonic(), 2), 1, -1)*ranuni(1)*(&boxwidth./2*&J_JTR_jitterwidth.)
                        else b.__grn/2 + ifn(mod(monotonic(), 2), 1, -1)*ranuni(2357)*(&boxwidth./2*&J_JTR_jitterwidth.*0.35)
             end as jitter
			 ,b.quartile1 AS Q1,b.median AS median_,b.mean as mean_ ,b.quartile3 as q3	

            from __indat a inner join __statistics b
                  on a.&groupVar. = b.&groupVar.
              order by __grn,&groupVar., &outcomeVar.;
quit;
data __inDatStatistics;
	set __inDatStatistics;
	%if &quartileYN. ^= Y %then %do;
		if quartile>. then quartile=ceil(quartile/100)*100;
		_qlcgr=1;
	%end; 
run;
data __fin;
    set __KDEstatistics(in = KDE)
        __statistics(in = stats)
        __inDatStatistics(in = data);

    groupVar_div_2 = __grn/2;
    label quartile1 = 'First Quartile' median = 'Median' quartile3 = 'Third Quartile' mean = 'Mean';
	%if &quartileYN. ^= Y %then %do;
		if quartile>. then quartile=ceil(quartile/100)*100;
	%end;
run;

***************** Figure generation ********************;
%let colorls =%str(&D_dnst_COLORLS.);

proc sort data=__inDatStatistics out=__attrmap(keep=quartile _qlcgr) nodupkey;
	where _qlcgr>.;
	by quartile _qlcgr;
run;
data __attrmap;
    set __attrmap;
	length value linecolor fillcolor markercolor $50.;
	ID="myid1"; value=strip(vvalue(quartile));
    
    %do _i=1 %to 4;
        if _qlcgr=&_i. then linecolor=strip(scan("&colorls.",&_i.,"#| "));
    %end;
	fillcolor=linecolor;
	markercolor=linecolor;
run;

data __statistics;
	set __statistics;
	min_=min;
	max_=max;
run;
data __anno;
run;
%IF "&boxplotYN."="Y" %THEN %do;
data __statistics;
	set __statistics;
	groupVar_div_2 = __grn/2;
	lower=groupVar_div_2-(&boxwidth./2);
	upper=groupVar_div_2+(&boxwidth./2);
	iqr=quartile3-quartile1;
	min_=quartile1-1.5*IQR;
	max_=quartile3+1.5*IQR;
	minx_lower=groupVar_div_2-(&boxwidth./2)*&B_BOX_CAPWEIGHT.;
	minx_upper=groupVar_div_2+(&boxwidth./2)*&B_BOX_CAPWEIGHT.;
run;
data anno_POLYGON;
	set __statistics;
	length id function x1space y1space LINECOLOR LINEPATTERN FILLCOLOR $50.;
    id="myid"; x1space="datavalue"; y1space="datavalue"; transparency= 0; display="&B_box_display."; 
	LINECOLOR="&B_box_LCOLOR."; LINETHICKNESS=&B_box_LTHICKNESS.; LINEPATTERN="&B_box_LPATTERN.";
	FILLCOLOR="&B_box_FILLCOLOR."; FILLTRANSPARENCY=&B_box_FILLTRANSPARENCY.;
	function="POLYGON";  x1=lower; y1=quartile1; output;
	function="POLYCONT"; x1=lower; y1=quartile3; output;
	function="POLYCONT"; x1=upper; y1=quartile3; output;
	function="POLYCONT"; x1=upper; y1=quartile1; output;
run;
data anno_line;
	set __statistics;
	length id function x1space y1space x2space y2space LINECOLOR LINEPATTERN $50.;
    id="myid"; x1space="datavalue"; y1space="datavalue"; x2space="datavalue"; y2space="datavalue";
	function="line"; transparency= 0;  LINEPATTERN="&B_box_LPATTERN.";
	LINECOLOR="&B_box_LCOLOR."; LINETHICKNESS=&B_box_LTHICKNESS.; 
	x1=groupVar_div_2; y1=min_; x2=x1; y2=quartile1; output;
	x1=groupVar_div_2; y1=max_; x2=x1; y2=quartile3; output;
%IF "%upcase(%substr(&b_box_CAP.,1,1))"="Y" OR "%upcase(%substr(&b_box_CAP.,1,1))"="1" %then %do;
	x1=minx_lower; y1=min_; x2=minx_upper; y2=y1; output;
	x1=minx_lower; y1=max_; x2=minx_upper; y2=y1; output;
%end;
	x1=lower; y1=median; x2=upper; y2=y1; output;
run;
data __anno;
	set anno_POLYGON anno_line;
run;
%end;

data __anno;
	set __anno &add_annods.;
run;
%let var=&outcomeVar.;
data __fin;
	set __fin;
	if nmiss(q3,q1)=0 then iqr=q3-q1;
	if iqr>. then do;
		&var._1=&var.;
		if ((q1-1.5*IQR)<=&var.<=(q3+1.5*IQR)) then &var._1=.;
	end;
run;
data __statistics;
	set __statistics;
	min_1=min(min,min_);
	max_1=max(max,max_);
	if nmiss(max_1,min_1)=0 then max_min=max_1-min_1;
run;
%let _outliern=;
proc sql noprint;
	select ceil(max(max_min)/10/5)*5 into: _yby trimmed from __statistics;
	select max(&var._1) into: _outliern trimmed from __fin;
quit;
%put &=_yby.;

%if %length(&yby.)=0 %then %let yby=%str(&_yby.);
proc sql noprint;
	select floor(min(min_1)/&yby.)*&yby.,ceil(max(max_1)/&yby.)*&yby. 
		into: _ymin trimmed,: _ymax trimmed from __statistics;
quit;
%put &=_ymin. &=_ymax.;
***********************;
data _yaxis;
	length yc $200.;
	do y= &_Ymin. to &_Ymax. by &Yby.;
		yc=strip(put(y,best.)); output;
	end;
run;
data _xaxis;
	set __gr;
	length xc $200. ;   
    x=__grn/2;    
	%if %length(&xfmt.) %then %do;
		xc=strip(put(x,&xfmt.)); 
	%end;%else %do;
		xc=strip(put(x,groupVar.));
	%end;
run;

proc sql noprint;
	select x,"'"||strip(xc)||"'",min(x),max(x) into: _xaxisls separated by ' ',:_xaxislsc separated by ' '
		,:_xmin trimmed,:_xmax trimmed from _xaxis;
	select y,"'"||strip(yc)||"'" into: _yaxisls separated by ' ',:_yaxislsc separated by ' ' from _yaxis;
quit;

%if "&debug."="0" %then %do;
	proc datasets nolist;
		delete __attribval __indat: __kde: __chk_gr __gr __myfmt: __stat: anno_POLYGON: anno_line: _yaxis _xaxis;
	quit;
%end;

*********** violinplot ************;
proc template;
  define statgraph violinplot;
    begingraph;
	  discreteattrvar attrvar=_qlc var=quartile attrmap="myid1"; 
	  annotate / id="myid0";
      layout overlay /OUTERPAD=0 BORDER=false 
		xaxisopts=(display=all labelattrs=(family="&x_axis_LABELFT." size=&X_axis_LABELFs. weight=&X_axis_LABELFw.) offsetmin=&X_axis_offsetmin. offsetmax=&X_axis_offsetmax. 
	        linearopts =( tickvaluelist=(&_xaxisls.) tickdisplaylist=(&_xaxislsc.)  viewmin =&_xmin. viewmax=&_xmax. ) 
			label="&x_axis_LABEL." tickvalueattrs=(family="&X_axis_TICKFT." size=&X_axis_TICKFs. weight=&X_axis_TICKFw.) ) 
		yaxisopts=(display=all labelattrs=(family="&y_axis_LABELFT." size=&y_axis_LABELFs. weight=&y_axis_LABELFw.) offsetmin=&y_axis_offsetmin. offsetmax=&y_axis_offsetmax. 
	        linearopts =( tickvaluelist=(&_yaxisls.) tickdisplaylist=(&_yaxislsc.)  viewmin =&_ymin. viewmax=&_ymax. ) 
			label="&y_axis_LABEL." tickvalueattrs=(family="&y_axis_TICKFT." size=&y_axis_TICKFs. weight=&y_axis_TICKFw.) )  
			walldisplay=none;

		BandPlot y=yBand LimitUpper=upperBand LimitLower=lowerBand / Group=_qlc Display=&D_dnst_DISPLAY.
			FILLATTRS=(TRANSPARENCY=&D_dnst_TRANSPARENCY.)
			OutLineAttrs=(Color=&D_dnst_OLCOLOR. Pattern=&D_dnst_OLPattern. thickness=&D_dnst_OLthickness.) ;

		annotate / id="myid";
		%if &jitterYN. = Y %then %do;
			scatterplot x=jitter y=&var. / name="mean" legendlabel=" "
                 markerattrs=(COLOR=&J_JTR_COLOR. SIZE=&J_JTR_SIZE. SYMBOL=&J_JTR_SYMBOL. TRANSPARENCY=&J_JTR_TRANSPARENCY. WEIGHT=&J_JTR_WEIGHT.) ;
		%end;
		%if &quartileSymbolsYN. = Y %then %do;
			scatterplot x=groupVar_div_2 y=MEDIAN / name="median" legendlabel=" "
                 markerattrs=(COLOR=&Q_qlt_MEDIAN_COLOR. SIZE=&Q_qlt_MEDIAN_SIZE. SYMBOL=&Q_qlt_MEDIAN_SYMBOL. TRANSPARENCY=&Q_qlt_MEDIAN_TRANSPARENCY. WEIGHT=&Q_qlt_MEDIAN_WEIGHT.) ;
			scatterplot x=groupVar_div_2 y=quartile1 / name="Q1" legendlabel=" "
                 markerattrs=(COLOR=&Q_qlt_Q1_COLOR. SIZE=&Q_qlt_Q1_SIZE. SYMBOL=&Q_qlt_Q1_SYMBOL. TRANSPARENCY=&Q_qlt_Q1_TRANSPARENCY. WEIGHT=&Q_qlt_Q1_WEIGHT.) ;
			scatterplot x=groupVar_div_2 y=quartile3 / name="Q3" legendlabel=" "
                 markerattrs=(COLOR=&Q_qlt_Q3_COLOR. SIZE=&Q_qlt_Q3_SIZE. SYMBOL=&Q_qlt_Q3_SYMBOL. TRANSPARENCY=&Q_qlt_Q3_TRANSPARENCY. WEIGHT=&Q_qlt_Q3_WEIGHT.) ;
		%end;
		%if &meanYN. = Y %then %do;
             scatterplot x=groupVar_div_2 y=MEAN / name="mean" legendlabel=" "
                 markerattrs=(COLOR=&M_mean_COLOR. SIZE=&M_mean_SIZE. SYMBOL=&M_mean_SYMBOL. TRANSPARENCY=&M_mean_TRANSPARENCY. WEIGHT=&M_mean_WEIGHT.) ;
        %end;

		%if &trendLineYN. = Y %then %do;
             seriesplot x = groupVar_div_2 y = &trendStatistic. /DISPLAY=&T_trl_DISPLAY. lineattrs = (color=&T_trl_LCOLOR. thickness=&T_trl_LTHICKNESS. PATTERN=&T_trl_LPATTERN.)
                 markerattrs=(COLOR=&T_trl_MCOLOR. SIZE=&T_trl_MSIZE. SYMBOL=&T_trl_MSYMBOL. TRANSPARENCY=&T_trl_MTRANSPARENCY. WEIGHT=&T_trl_MWEIGHT.) ;
        %end;
		%IF "&boxplotYN."="Y" and &jitterYN. ^= Y and &_outliern.>0 %THEN %do;
			scatterplot x=groupVar_div_2 y=&var._1 / name="outlier" legendlabel=" "
                 markerattrs=(COLOR=&B_box_MCOLOR. SIZE=&B_box_MSIZE. SYMBOL=&B_box_MSYMBOL. TRANSPARENCY=&B_box_MTRANSPARENCY. WEIGHT=&B_box_MWEIGHT.) ;
		%end;
		 
      endlayout;
    endgraph;
  end;
run;

proc sgrender data=__fin template=violinplot sganno=__anno dattrmap=__attrmap;
run;


********* delete global macro varibale *********;
proc sql noprint;
	select strip(vmacroname) into : _drpmvarls separated by ' ' from __attribval_all;
quit;
/*%PUT &=_drpmvarls.;*/
%symdel &_drpmvarls.;

%if "&debug."="0" %then %do;
	proc datasets nolist;
		delete __fin __attrmap: __anno: __attribval_all;
	quit;
%end;
%put NOTE: -------------------- Macro[&SYSMACRONAME.] End --------------------;
%mend  violinPlot;

