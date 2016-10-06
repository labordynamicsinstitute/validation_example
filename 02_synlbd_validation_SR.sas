/*
 This file is auto-generated by the statrep package.
 Do not edit this file or your changes will be lost.
 Edit the LaTeX file instead.
 
 See the statrep package documentation and the file
 statrep.cfg for information on these settings.
 */
 
 
%include "./02_synlbd_validation_SR_preamble.sas" /nosource;
/* Remove all output files. */
%cleandir(., tex, tex);
%cleandir(., png, png);
%cleandir(., lst, lst);


/* Start program with a null title. */
title;

options nocenter;
libname home ".";

/* basic parameters */
%let indcnt=40;
%let maxestabcnt=10000;
%let minestabcnt=100;
%let seed1=123456;

%let maxemp=42000;
%let minemp=1;
%let minyear=1979;
%let maxyear=1981;
%let seed2=1234567;

%let printobs=10;

/* draw estab count distribution across industries*/
/* use log normal */
data industries;
  do industry = 1 to &indcnt.;
  estabs= exp(ranuni(&seed1.)*
                (log(&maxestabcnt.)
                -log(&minestabcnt.))
                + log(&minestabcnt.));
  output;
  end;
  run;


/* now draw employment for each estab in each industry */

data fakelbd;
  set industries;
  by industry;
  drop i;
  do lbdnum=100000*industry+1 to 100000*industry+estabs;
    do year=&minyear. to &maxyear.;
       emp= exp(ranuni(&seed2.)
         *(log(&maxemp.)-log(&minemp.))
         + log(&minemp.));
    payroll  = emp*30*ranuni(&seed2.);
    output;
  end; end;
run;

%output(univarA)
ods graphics on;
  proc univariate data=industries;
  var estabs;
  run;
  ods graphics off;
 
%endoutput(univarA)

%write(figunivarA,store=univarA,objects=BasicMeasures,type=listing) 

%output(univarB)
ods graphics on;
proc univariate data=fakelbd;
var emp;
run;
ods graphics off;

%endoutput(univarB)

%write(figunivarB,store=univarB,objects=BasicMeasures,type=listing) 

%output(obsperind)
ods graphics on;
proc univariate data=fakelbd;
var industry;
histogram industry;
run;

%endoutput(obsperind)

%write(obsperind,store=obsperind,objects=Histogram,type=graphic) 

/*Prepare data*/
/* program: 01_prepdata.sas */
data analysis1;
set fakelbd;
by industry lbdnum year;
wage = payroll/emp;
if first.lbdnum then do;
lagE = .;
lagp = .;
lagw = .;
end;
else                 do;
lagE = lag(emp);
lagp=lag(payroll);
lagw = lag(wage);
end;
empgrowth = emp/lage;
wagegrowth= wage/lagw;
run;

%output(regA)
/*Regression of interest*/
/* program: 02_regression1.sas */
proc reg data=analysis1;
by industry;
where industry le 2;
model empgrowth = lagE lagw;
output out=obsds1 r=inc;
ods output parameterestimates=param1;
run;
ods trace off;

%endoutput(regA)

%write(regAparms,store=regA,objects=Reg.ByGroup1.MODEL1.Fit.empgrowth.ParameterEstimates,type=listing) 

/* Prepare disclosure avoidance analysis */
/* program: 03_prep_daa.sas */

/* create a count by industry */
proc sql;
create table discreview1 as
select industry,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds1
where inc ne .
group by industry
;
quit;
/* merge with parameter estimates */
/* flag possible problematic cases */
%let mincount=10;
data discreview1;
  merge discreview1(in=_a)
               param1(in=_b);
   by industry;
   flag_problem=(nEstabs<&mincount.);
   label flag_problem=" (nEstabs<&mincount.)";
 run;

%output(daa1lst)
proc freq data=discreview1;
table flag_problem;
run;

%endoutput(daa1lst)

%write(daa1lst,store=daa1lst,type=listing) 

/*Export validation table and sample size table*/
proc export data=param1 file="./validationtable1.csv" dbms=csv replace;
run;

%output(paramAcsv)
proc print data=param1;
run;

%endoutput(paramAcsv)

%write(paramAcsv,store=paramAcsv,type=listing) 

proc export data=discreview1 file="./discreview1.csv" dbms=csv replace;
run;

%output(discreviewA)
proc print data=discreview1;
run;

%endoutput(discreviewA)

%write(discreviewA,store=discreviewA,type=listing) 

/*Prepare data*/
data analysis;
set fakelbd;
by industry lbdnum year;

wage = payroll/emp;

if first.lbdnum then do;
 lagE = .;         lagp = .;         lagw = .;
 end;
else                 do;
 lagE = lag(emp);
 lagp=lag(payroll);
 lagw = lag(wage);

 empgrowth = emp/lage;
 wagegrowth= wage/lagw;
end;

/*Specialized establishment size category*/
if 45 le emp le 49 then empcat = 1;
else if 51 le emp le 55 then empcat =2;
else empcat = .;
run;

data analysis2;
set analysis;
by industry lbdnum year;
if not first.lbdnum;
run;

%output(tabB)
/*Tabulation of interest*/
proc means data=analysis2 n mean variance;
class industry empcat year;
where  year ge 2;
var empgrowth wagegrowth;
ods output summary=model2;
run;

proc print data=model2(obs=&printobs.);
where industry=5;
run;

%endoutput(tabB)

%write(tabBparms,store=tabB,objects=print.print,type=listing) 

/* Export analysis --effective sample size of each cell*/
proc sql;
create table exreview2 as
select  industry, empcat, year,  nObs
from model2
;quit;

%output(exreview)
proc univariate data=exreview2;
var nObs;
run;

%endoutput(exreview)

%write(exreview,store=exreview,objects=Moments Quantiles,type=listing) 

/*Export table for export, export analysis table*/
data export2;
set model2;
where (NObs>5);
run;
proc export data=export2
 file="./validationtable2.csv" dbms=csv replace;
run;

%output(paramBcsv)
proc print data=export2(drop=vname_: obs=&printobs.);
where industry=5;
run;

%endoutput(paramBcsv)

%write(paramBcsv,store=paramBcsv,type=listing) 

proc export data=exreview2
  file="./exreview2.csv" dbms=csv replace;
run;

%output(exBcsv)
proc print data=exreview2(obs=&printobs.);
run;

%endoutput(exBcsv)

%write(exBcsv,store=exBcsv,type=listing) 

data analysis2b;
set analysis2;
empcat2=int(emp/5)+1;
run;

%output(tabB)
/*Tabulation of interest*/
proc means data=analysis2b n mean variance;
class industry empcat2 year;
where year ge 2;
var empgrowth wagegrowth;
ods output summary=model2b;
run;

proc print data=model2b(obs=10);
where industry=5;
run;

%endoutput(tabB)

%write(tabBparms,store=tabB,objects=print.print,type=listing) 

/*Disclosure avoidance review--effective sample size of each cell*/
proc sql;
create table discreview2b as
select industry
      ,empcat2
      ,year
      ,count(distinct lbdnum)as nEstabs
      ,count(*) as nObs
from analysis2b
where n(wagegrowth,empgrowth)=2
group by industry,empcat2,year
;
quit;

/*Export table for export, disclosure avoidance table*/
proc export data=model2b(drop=empgrowth_N wagegrowth_N)
 file="./validationtable2b.csv" dbms=csv replace;
run;

%output(paramBcsv)
proc print data=model2b(drop=empgrowth_N wagegrowth_N vname_: obs=&printobs.);
where industry=5;
run;

%endoutput(paramBcsv)

%write(paramBcsv,store=paramBcsv,type=listing) 

proc export data=discreview2b
  file="./discreview2b.csv" dbms=csv replace;
run;

%output(discBcsv)
proc print data=discreview2b(obs=&printobs.);
run;

%endoutput(discBcsv)

%write(discBcsv,store=discBcsv,type=listing) 

%output(exreview2b)
proc univariate data=discreview2b;
var nObs;
run;

%endoutput(exreview2b)

%write(exreview2b,store=exreview2b,objects=Moments,type=listing) 

data analysis3;
set analysis2b;
testsamp=(ranuni(&seed2.) < 0.01);
run;

/*Regression of interest*/
ods exclude all;
proc glm data=analysis3(where=(testsamp));
class industry empcat2 year;
model empgrowth wagegrowth = empcat2*industry*year/solution;
ods output parameterestimates=model3b;
output out=obsds3 r=inc;
run;
ods exclude none;

/* we parse the  Parameter variable to get back the characteristics we are after */
data model3b;
  set model3b;
  length industry empcat2 year sortorder 8 ;
  sortorder=_n_;
  industry=input(scan(Parameter,2," "),4.);
  empcat2=input(scan(Parameter,3," "),20.);
  year=input(scan(Parameter,4," "),4.);
run;

%output(parmc)
proc print data=model3b(obs=&printobs. drop=biased stderr tvalue);
run;

%endoutput(parmc)

%write(parmCparms,store=parmc,objects=print.print,type=listing) 

/*Effective sample size of each cell, meaning # included in each
  dummy variable combo, by establishment and observations*/
proc sql;
create table discreview3b as
select industry
      ,empcat2
      ,year
      ,count(distinct lbdnum)as nEstabs
      , count(*) as nObs
from analysis3(where=(testsamp))
where n(wagegrowth,empgrowth)=2
group by industry,empcat2,year
;quit;

/* we sort for merge */
proc sort data=model3b;
by industry empcat2 year;
run;
/* the Discreview dataset contains the COMPLETE set of statistics */
data discreview3b;
merge model3b discreview3b;
by industry empcat2 year;
run;


%output(exreview3b)
proc univariate data=discreview3b;
var nObs;
run;

%endoutput(exreview3b)

%write(exreview3b,store=exreview3b,objects=Moments Quantiles,type=listing) 

%output(exreview3c)
proc sort data=discreview3b;
by sortorder;
run;
proc print data=discreview3b(obs=&printobs.);
var Dependent industry empcat2 year Estimate nObs;
run;

%endoutput(exreview3c)

%write(exreview3c,store=exreview3c,objects=print.print,type=listing) 
