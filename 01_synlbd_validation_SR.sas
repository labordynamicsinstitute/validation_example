/*
 This file is auto-generated by the statrep package.
 Do not edit this file or your changes will be lost.
 Edit the LaTeX file instead.
 
 See the statrep package documentation and the file
 statrep.cfg for information on these settings.
 */
 
 
%include "./01_synlbd_validation_SR_preamble.sas" /nosource;
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
%let seed2=1234567;

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
    do year=1 to 3;
       emp= exp(ranuni(&seed2.)
         *(log(&maxemp.)-log(&minemp.))
         + log(&minemp.));
    payroll  = emp*30*ranuni(3153);
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

%write(univarA,store=univarA,objects=BasicMeasures,type=listing) 

%output(univarB)
ods graphics on;
proc univariate data=fakelbd;
var emp;
run;
ods graphics off;

%endoutput(univarB)

%write(univarB,store=univarB,objects=BasicMeasures,type=listing) 

%output(obsperind)
proc freq data=fakelbd;
table industry;
run;

%endoutput(obsperind)

%write(obsperind,store=obsperind,type=listing) 

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

proc sql;
create table discreview1 as
select industry,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds1
where inc ne .
group by industry
;quit;
data discreview1;
  merge discreview1(in=_a)
               param1(in=_b);
   by industry;
 run;

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
