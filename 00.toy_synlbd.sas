options nocenter;
libname home ".";

/* basic parameters */
%let indcnt=100;
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
  estabs= exp(ranuni(&seed1.)*(log(&maxestabcnt.)-log(&minestabcnt.)) + log(&minestabcnt.));
  output;
  end;
  run;

  proc univariate data=industries;
  var estabs;
  run;

/* now draw employment for each estab in each industry */

data fakelbd;
  set industries;
  by industry;
  drop i;
  do lbdnum=100000*industry+1 to 100000*industry+estabs; do year=1 to 3;
    emp= exp(ranuni(&seed2.)*(log(&maxemp.)-log(&minemp.)) + log(&minemp.));
    payroll  = emp*30*ranuni(3153);
    output;
  end; end;
run;

proc univariate data=fakelbd;
var emp;
run;

proc sql;
select industry into :minestabs
from industries
order by estabs asc
;
quit;

proc print data=fakelbd(where=(estabs=&minestabs));
title3 'Print of data for smallest industry';
run;


proc contents data=fakelbd;

proc means data=fakelbd;

proc freq data=fakelbd;
table industry;
title3 'Number of obs per industry';
run;


/*Project 1:  Analysis that meets validation requirements*/

/*Prepare data*/
data analysis1;
set fakelbd;
by industry lbdnum year;

wage = payroll/emp;

if first.lbdnum then do; lagE = .;         lagp = .;         lagw = .;         end;
else                 do; lagE = lag(emp); lagp=lag(payroll); lagw = lag(wage); end;

empgrowth = emp/lage;
wagegrowth= wage/lagw;
run;

/*Regression of interest*/
proc reg data=analysis1;
by industry;
where industry le 2;
model empgrowth = lagE lagw;
ods trace on/listing;
output out=obsds1 r=inc;
ods output parameterestimates=param1;
run;
ods trace off;

proc print data=param1;
title3 'Extract 1: Parameter Estimates';
run;

/*Disclsure avoidance review--effective sample size of each parameter in terms of establishments and total observations*/
proc sql;
create table discreview1 as
select industry,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds1
where inc ne .
group by industry
;quit;

/*Export validation table and sample size table*/
proc export data=param1 file="./validationtable1.csv" dbms=csv replace;
run;

proc export data=discreview1 file="./discreview1.csv" dbms=csv replace;
run;

/*Project 2:  Extract that fails validation requirements, but could be taken off of synthetic data server*/

/*Prepare data*/
data analysis2;
set fakelbd;
by industry lbdnum year;

wage = payroll/emp;

if first.lbdnum then do; lagE = .;         lagp = .;         lagw = .;         end;
else                 do; lagE = lag(emp); lagp=lag(payroll); lagw = lag(wage); end;

empgrowth = emp/lage;
wagegrowth= wage/lagw;
/*Specialized establishment size category*/
if 50 le emp le 54 then empcat = 1;
else if 55 le emp le 60 then empcat =2;
else empcat = .;
empcat2 = ceil(emp/5)*5;
testsamp = ranuni(32235) le 0.001;
run;

/*Tabulation of interest*/
proc means data=analysis2 n mean variance;
class industry empcat year;
where industry le 10 and year ge 2;
var empgrowth wagegrowth;
ods output summary=model2;
run;

proc print data=model2;
title3 'Extract 2: Mean and variance of empgrowth and wagegrowth by company has 55 or more employees';
run;

/*Disclosure avoidance review--effective sample size of each cell*/
proc sql;
create table discreview2 as
select "industry" as class,industry as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by industry
union
select "empcat" as class,empcat as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by empcat
union
select "year" as class,year as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by year
;

create table discreview2 as
select industry,empcat,year,count(distinct lbdnum)as nEstabs, count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by industry,empcat,year
;

quit;

/*Export table for export, disclosure avoidance table*/
proc export data=model2(drop=empgrowth_N wagegrowth_N) file="./validationtable2.csv" dbms=csv replace;
run;

proc export data=discreview2 file="./discreview2.csv" dbms=csv replace;
run;

/*Project 3a:  Tabular output with tiny empcats*/

/*Tabulation of interest*/
proc means data=analysis2 n mean variance;
class industry empcat2 year;
where industry le 10 and year ge 2;
var empgrowth wagegrowth;
ods output summary=model3a;
run;

proc print data=model3a;
title3 'Extract 2: Mean and variance of empgrowth and wagegrowth by company has 55 or more employees';
run;

/*Disclosure avoidance review--effective sample size of each cell*/
proc sql;
create table discreview3a as
select "industry" as class,industry as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by industry
union
select "empcat" as class,empcat2 as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by empcat2
union
select "year" as class,year as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by year
;

create table discreview3a as
select industry,empcat2,year,count(distinct lbdnum)as nEstabs, count(*) as nObs
from analysis2
where n(wagegrowth,empgrowth)=2
group by industry,empcat2,year
;quit;

/*Export table for export, disclosure avoidance table*/
proc export data=model3a(drop=empgrowth_N wagegrowth_N) file="./validationtable3a.csv" dbms=csv replace;
run;

proc export data=discreview3a file="./discreview3a.csv" dbms=csv replace;
run;

/*Project 3b:  Project three as a regression*/

proc glm data=analysis2(where=(testsamp));
class industry empcat2 year;
model empgrowth wagegrowth = empcat2*industry*year/solution;
ods output parameterestimates=model3b;
output out=obsds3 r=inc;
run;

/*Effective sample size of each cell, meaning # included in each dummy variable combo, by establishment and observations*/
proc sql;
create table discreview3b as
select "empcat*industry" as class,catx('*',empcat,industry) as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds3
where inc ne .
group by empcat,industry
union
select "year" as class,put(year,8.) as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds3
where inc ne .
group by year
;

create table discreview3b as
select industry,empcat2,year,count(distinct lbdnum)as nEstabs, count(*) as nObs
from analysis2(where=(testsamp))
where n(wagegrowth,empgrowth)=2
group by industry,empcat2,year
;quit;

proc print data=model3b;
title3 'Extract 3: Mean and variance of empgrowth and wagegrowth for companies with 1000+ employees';
run;

/*Export table and disclosure avoidance review*/
proc export data=model3b file="./validationtable3b.csv" dbms=csv replace;
run;

proc export data=discreview3b file="./discreview3b.csv" dbms=csv replace;
run;

%macro comment;

/*Project 3:  Output that could not be removed from the synthetic data server*/

/*Prepare data*/
data analysis3;
set fakelbd;
by industry lbdnum year;

wage = payroll/emp;

if first.lbdnum then do; lagE = .;         lagp = .;         lagw = .;         end;
else                 do; lagE = lag(emp); lagp=lag(payroll); lagw = lag(wage); end;

empgrowth = emp/lage;
wagegrowth= wage/lagw;

empcat = 500*floor(emp/500);
run;

/*Regression of interest*/
proc glm data=analysis3;
class empcat industry year;
model empgrowth = empcat industry year empcat*industry/solution;
ods output parameterestimates=model3;
output out=obsds3 r=inc;
run;

/*Effective sample size of each cell, meaning # included in each dummy variable combo, by establishment and observations*/
proc sql;
create table discreview3 as
select "empcat*industry" as class,catx('*',empcat,industry) as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds3
where inc ne .
group by empcat,industry
union
select "year" as class,put(year,8.) as cat,count(distinct lbdnum) as nEstabs,count(*) as nObs
from obsds3
where inc ne .
group by year
;quit;

proc print data=model3;
title3 'Extract 3: Mean and variance of empgrowth and wagegrowth for companies with 1000+ employees';
run;

/*Export table and disclosure avoidance review*/
proc export data=model3 file="./validationtable3.csv" dbms=csv replace;
run;

proc export data=discreview3 file="./discreview3.csv" dbms=csv replace;
run;

%mend comment;
