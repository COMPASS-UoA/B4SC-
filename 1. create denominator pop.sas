

%macro fyear(yr,st,en);

*FISCAL YEARS;


*BEFORE RUNNING CODE, MAKE SURE THE NEXT 4 LINES ARE CORRECT;
*YOU WILL NEED TO CHANGE ALL 4 LINES EVERY TIME YOU RUN A DIFFERENT YEAR;

%let refresh = 20200720; ** Specify IDI refresh to use for extractions;

%let year = &yr; ** Specify end of financial year of interest for the denominator population;
%let startyrsql = &st;  **Change this too**;
%let endyrsql = &en;  **Change this too- should be 5 years after date above and should align with end of fiscal year of interest**;


*END OF USER INPUT, DO NOT CHANGE ANYTHING IN THE REST OF THE CODE;


%let prevyear = %eval(&year. - 1);

libname central ODBC dsn=idi_clean_&refresh._srvprd schema=data;
libname moe ODBC dsn=idi_clean_&refresh._srvprd schema=moe_clean;
libname msd ODBC dsn=idi_clean_&refresh._srvprd schema=msd_clean;
libname moh ODBC dsn=idi_clean_&refresh._srvprd schema=moh_clean;
libname acc ODBC dsn=idi_clean_&refresh._srvprd schema=acc_clean;
libname dia ODBC dsn=idi_clean_&refresh._srvprd schema=dia_clean;
libname dlab "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage";
libname sec ODBC dsn=idi_clean_&refresh._srvprd schema=security;

*We are now using fiscal years so can just use respop table to get denominator population;
*It gives very slightly different numbers than in the original paper but is much easier;
data totalpop;
set central.snz_res_pop;
where srp_ref_date="30JUN&year"d;
keep snz_uid;
run;

*Add birth dates and spine ind;
proc sql;
create table finalpop2 as
select a.*, b.snz_birth_year_nbr, b.snz_birth_month_nbr, b.snz_sex_gender_code, b.snz_spine_ind
from totalpop as a left join central.personal_detail as b on a.snz_uid=b.snz_uid;
quit;


** Get a list of all deaths recorded in DIA data;
*Only extract deaths after 1990 as file size with all deaths was too large (we are looking at 4yo children so all deaths will be post 1990);
proc sql;
connect to odbc(dsn="idi_clean_&refresh._srvprd");
   create table dia_deaths as
   select * from connection to ODBC
   (select distinct snz_uid, dia_dth_death_year_nbr, dia_dth_death_month_nbr
   from dia_clean.deaths
   where (dia_dth_death_year_nbr <= &year. and dia_dth_death_year_nbr >= 1990) 
   order by snz_uid);
quit;

*Merge deaths with main file;
proc sql;
create table finalpop3 as
select *
from finalpop2 as a left join dia_deaths as b 
on a.snz_uid=b.snz_uid;
quit;

** Calculate age at 30 June;
*** remove people who died before or in the year of interest;
*** remove people who are not aged 4 in the year of interest;
data finalpop_age;
   set finalpop3;

   age1 = &year. - snz_birth_year_nbr;
   if snz_birth_month_nbr >= 7 then age=age1-1;
   else if snz_birth_month_nbr <=6 then age=age1;
  
   *We want people who were aged 4 in the relevant year;
   if age ne 4 then delete;

   *Remove people who died before start of relevant year;
   *Note 8 Dec 2020: this is only removing children who died in/before the first half of the fiscal year, need to adjust;
   *Error above only affects a really tiny number of children so will not make a substantive difference to results;
   if dia_dth_death_year_nbr < &year. and dia_dth_death_year_nbr ne . then delete;

   *Flag people who died during year but do not remove yet;
   if dia_dth_death_year_nbr=&year. then died_&year.=1;
   else died_&year.=0;

   *Create reference date for addresses, history variables etc;	
	*Reference date is the first day of the month of 4th birthday;
	ref_date_char=cat(snz_birth_year_nbr,'-',snz_birth_month_nbr,'-01');

	format ref_date_temp ddmmyy10.;
	ref_date_temp=input(ref_date_char, anydtdte10.);
	format ref_date ddmmyy10.;
	ref_date=intnx('year',ref_date_temp,4,'sameday');

   rename snz_sex_gender_code = sex;
   keep snz_uid snz_sex_gender_code age flag_birth flag_pharm flag_pho flag_hosp flag_nnpac flag_health snz_birth_year_nbr snz_birth_month_nbr died_&year. snz_spine_ind ref_date;

  run;


******************************************************************************
***   Remove individuals from the population if they are living overseas   ***
******************************************************************************;

  ** Calculate amount of time spent overseas in last 12 months;

proc sql;
   create table overseas_spells_1yr as
   select unique snz_uid , pos_applied_date, pos_ceased_date, pos_day_span_nbr
   from central.person_overseas_spell
   where pos_applied_date < "30JUN&year.:99:99:99.999"dt and pos_ceased_date >= "01JUL&prevyear.:00:00:00.000"dt
   order by snz_uid, pos_applied_date;
quit;

data overseas_time_1yr;
   set overseas_spells_1yr;
   if pos_ceased_date >= "30JUN&year.:99:99:99.999"dt and pos_applied_date < "01JUL&prevyear.:00:00:00.000"dt 
      then time_to_add = 365;

   else if pos_ceased_date >= "30JUN&year.:99:99:99.999"dt 
      then time_to_add = ("30JUN&year.:99:99:99.999"dt - pos_applied_date) / 86400;

   else if pos_ceased_date <= "30JUN&year.:99:99:99.999"dt and pos_applied_date >= "01JUL&prevyear.:00:00:00.000"dt 
      then time_to_add = pos_day_span_nbr;

   else if pos_ceased_date <= "30JUN&year.:99:99:99.999"dt and pos_applied_date < "01JUL&prevyear.:00:00:00.000"dt 
      then time_to_add = (pos_ceased_date - "01JUL&prevyear.:00:00:00.000"dt) / 86400;
run;

proc sql;
   create table time_overseas_1yr as select snz_uid, ROUND(SUM(time_to_add), 1) as days_overseas_last1
   from overseas_time_1yr
   group by snz_uid;
quit;

** Combine total population with time spent overseas;
data finalpop_res;
   merge finalpop_age (in=a) time_overseas_1yr (in=b);
   by snz_uid;
   if a;

   ** remove people who are overseas for more than 6 months out of the last 12;
   if days_overseas_last1 gt 183 then delete;
run;


*Match against pop cohort demographics to get resident status code;
proc sql;
create table denominator_pop as 
select a.*, b.moh_pop_nz_res_status_code
from finalpop_res as a left join moh.pop_cohort_demographics as b
on a.snz_uid=b.snz_uid;
quit;

*Output final file;
data dlab.denominator_pop_&year._junyr;
set denominator_pop;
run;


* End of code;

%mend;

%fyear (2011, '2006-07-01', '2011-06-30');
%fyear (2012, '2007-07-01', '2012-06-30');
%fyear (2013, '2008-07-01', '2013-06-30');
%fyear (2014, '2009-07-01', '2014-06-30');
%fyear (2015, '2010-07-01', '2015-06-30');
%fyear (2016, '2011-07-01', '2016-06-30');
%fyear (2017, '2012-07-01', '2017-06-30');
%fyear (2018, '2013-07-01', '2018-06-30');





