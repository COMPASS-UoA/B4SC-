
libname dlab "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage";

*Reading in data;
data b4sc;
set dlab.b4sc_allyr_final;
run;

* Create file with non-completion by dhb by module over whole time period;
*File is dhb_full_data;

	%macro dhb(var);

	proc summary nway data=b4sc;
	class dhb2015_name &var.;
	var vht_incomplete nurse_incomplete sdqt_incomplete;
	output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
	run;

	data &var.;
	set &var.;
	var="&var.";
	run;

	%mend;

	%dhb (n_mv_access);
	%dhb (low_bw);
	%dhb (sex);
	%dhb (maori_eth);
	%dhb (asian_eth);
	%dhb (pacific_eth);
	%dhb (euro_eth);
	%dhb (nzdep_quint);
	%dhb (sibling_count);
	%dhb (mother_highest_qual);
	%dhb (hh_benefit_inc);
	%dhb (mother_age_grp);
	%dhb (birth_father);
	%dhb (parent1_no_english);
	%dhb (urban);
	%dhb (hh_size_grp2);
	%dhb (hh_rent);
	%dhb (res_changes);
	%dhb (mother_smoke);
	%dhb (bw_grp);
	%dhb (gestation_grp);
	%dhb (disability_flag);
	%dhb (inpatient_visit_grp);
	%dhb (los_days_grp);
	%dhb (ed_visit_grp);
	%dhb (gp_visit_grp);

	data dhb_full_data;
	set mother_highest_qual
	n_mv_access
	low_bw
	sex
	maori_eth
	asian_eth
	pacific_eth
	euro_eth
	nzdep_quint
	sibling_count
	hh_benefit_inc
	mother_age_grp
	birth_father
	parent1_no_english
	urban
	hh_size_grp2
	hh_rent
	res_changes
	mother_smoke
	bw_grp
	gestation_grp
	disability_flag
	inpatient_visit_grp
	los_days_grp
	ed_visit_grp
	gp_visit_grp;
	run;

	*Output to datalab folder;
	PROC EXPORT DATA = dhb_full_data OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/dhb_full_data.csv"
	DBMS = CSV REPLACE; RUN;


* Create file with non-completion by dhb by module over whole time period;
*ungrouped, there will be a lot of suppression in the smaller TAs;
* file is taward_final; 

	%macro dhb(var);

	proc summary nway data=b4sc;
	class ta_ward_name &var.;
	var vht_incomplete nurse_incomplete sdqt_incomplete;
	output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
	run;

	data &var.;
	set &var.;
	var="&var.";
	run;

	%mend;

	%dhb (n_mv_access);
	%dhb (low_bw);
	%dhb (sex);
	%dhb (maori_eth);
	%dhb (asian_eth);
	%dhb (pacific_eth);
	%dhb (euro_eth);
	%dhb (nzdep_quint);
	%dhb (sibling_count);
	%dhb (mother_highest_qual);
	%dhb (hh_benefit_inc);
	%dhb (mother_age_grp);
	%dhb (birth_father);
	%dhb (urban);
	%dhb (hh_size_grp2);
	%dhb (hh_rent);
	%dhb (res_changes);
	%dhb (mother_smoke);
	%dhb (bw_grp);
	%dhb (inpatient_visit_grp);
	%dhb (ed_visit_grp);
	%dhb (gp_visit_grp);

	data taward_full_data;
	set mother_highest_qual
	n_mv_access
	low_bw
	sex
	maori_eth
	asian_eth
	pacific_eth
	euro_eth
	nzdep_quint
	sibling_count
	hh_benefit_inc
	mother_age_grp
	birth_father
	urban
	hh_size_grp2
	hh_rent
	res_changes
	mother_smoke
	bw_grp
	inpatient_visit_grp
	ed_visit_grp
	gp_visit_grp;
	run;

	*Need to add back in the zero rows so that they can be suppressed;
	*We didn't do this for DHB file because there aren't any blank rows there;
	*Get a complete list of all variables and levels;
	proc sql;
	create table varlevels as
	select distinct var, level
	from dhb_full_data
	order by var, level;
	quit;

	*Complete list of ta_wards;
	proc sql;
	create table tas as
	select distinct ta_ward_name
	from taward_full_data
	order by ta_ward_name;
	quit;

	*Merge so that every var level is recorded against every ta;
	proc sql;
	create table ta_varlist as
	select a.*, b.*
	from tas as a cross join varlevels as b;
	quit;

	proc sql;
	create table taward_varlist_full as
	select a.*, b.*
	from tas as a cross join varlevels as b;
	quit;

	*Remove gestation, mother english and disability as we are not using them for the ta/ward analysis;
	data taward_varlist;
	set taward_varlist_full;
	if var in('disability_flag' 'parent1_no_english' 'gestation_grp' 'father_highest_qual' 'los_days_grp') then delete;
	run;

	*Merge with original data files so that blanks are inserted for missing var levels;
	proc sql;
	create table taward_final as
	select a.ta_ward_name, a.var, a.level, b.total, b.vht_incomplete, b.nurse_incomplete, b.sdqt_incomplete
	from taward_varlist as a left join taward_full_data as b on a.ta_ward_name=b.ta_ward_name and a.var=b.var and a.level=b.level;
	quit;

	*Output to datalab folder;
	PROC EXPORT DATA = taward_final OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/taward_final.csv"
	DBMS = CSV REPLACE; RUN;




* National average coverage by year by module, VHT, Nurse, SDQT;
*File is year2011;

PROC SUMMARY NWAY DATA = dlab.b4sc_allyr_final;
CLASS year;
VAR vht_complete nurse_complete sdqt_complete;
OUTPUT OUT = year2011 (DROP=_type_ RENAME=(_FREQ_=total)) SUM= ;
RUN;

* National average coverage and by grouped TALB, by year by module (VHT, Nurse, SDQT);
*Files are 

%MACRO NA(year);

PROC SUMMARY NWAY DATA = dlab.b4sc_allyr_final (WHERE=(YEAR=&year.));
VAR vht_complete nurse_complete sdqt_complete;
OUTPUT OUT = year_&year. (DROP=_type_ RENAME=(_FREQ_=total)) SUM= ;
RUN;

DATA YEAR_&year.;
SET year_&year.;
YEAR = "&year.";
RUN;

%MEND;

%NA(2011);
%NA(2012);
%NA(2013);
%NA(2014);
%NA(2015);
%NA(2016);
%NA(2017);
%NA(2018);

DATA completion_all_year;
SET year_2011 year_2012 year_2013 year_2014 year_2015 year_2016 year_2017 year_2018;
RUN; 

* Task2: B4SC completion by year, by grouped TALB, by module;
PROC FREQ DATA = dlab.B4SC_ALLYR_FINAL; TABLE DHB2015_name grp_ta_ward_name child_nz_born; RUN; *71 grouped TA;

%MACRO TA(year);

PROC SUMMARY NWAY DATA = dlab.b4sc_allyr_final (WHERE=(YEAR=&year.));
CLASS grp_ta_ward_name;
VAR vht_complete nurse_complete sdqt_complete;
OUTPUT OUT = year_&year. (DROP=_type_ RENAME=(_FREQ_=total)) SUM= ;
RUN;

DATA YEAR_&year.;
SET year_&year.;
YEAR = "&year.";
RUN;

%MEND;

%TA(2011);
%TA(2012);
%TA(2013);
%TA(2014);
%TA(2015);
%TA(2016);
%TA(2017);
%TA(2018);

DATA completion_all_year_ta;
SET year_2011 year_2012 year_2013 year_2014 year_2015 year_2016 year_2017 year_2018;
RUN; 

* Task3: Non-completion for whole time period, by predictor, national and by grouped TALB and by DHB; 

%macro nation(var);

proc summary nway data=b4sc_allyr_final;
class &var.;
var vht_incomplete nurse_incomplete sdqt_incomplete;
output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
run;

data &var.;
set &var.;
var="&var.";
run;

%mend;

%nation (n_mv_access);
%nation (low_bw);
%nation (sex);
%nation (maori_eth);
%nation (asian_eth);
%nation (pacific_eth);
%nation (euro_eth);
%nation (nzdep_quint);
%nation (sibling_count);
%nation (mother_highest_qual);
%nation (hh_benefit_inc);
%nation (mother_age_grp);
%nation (birth_father);
%nation (parent1_no_english);
%nation (urban);
%nation (hh_size_grp);
%nation (hh_rent);
%nation (res_changes);
%nation (bw_grp);
%nation (gestation_grp);
%nation (disability_flag);
%nation (inpatient_visit_grp);
%nation (los_days_grp);
%nation (ed_visit_grp);
%nation (gp_visit_grp);
%nation (child_nz_born);
%nation (mother_no_english);
%nation (mother_nz_born);


data national_incomplete;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
parent1_no_english
urban
hh_size_grp
hh_rent
res_changes
bw_grp
gestation_grp
disability_flag
inpatient_visit_grp
los_days_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

* predictors - DHB level;

%macro dhb(var);

proc summary nway data=b4sc_allyr_final;
class dhb2015_name &var.;
var vht_incomplete nurse_incomplete sdqt_incomplete;
output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
run;

data &var.;
set &var.;
var="&var.";
run;

%mend;

%dhb (n_mv_access);
%dhb (low_bw);
%dhb (sex);
%dhb (maori_eth);
%dhb (asian_eth);
%dhb (pacific_eth);
%dhb (euro_eth);
%dhb (nzdep_quint);
%dhb (sibling_count);
%dhb (mother_highest_qual);
%dhb (hh_benefit_inc);
%dhb (mother_age_grp);
%dhb (birth_father);
%dhb (parent1_no_english);
%dhb (urban);
%dhb (hh_size_grp);
%dhb (hh_rent);
%dhb (res_changes);
%dhb (bw_grp);
%dhb (gestation_grp);
%dhb (disability_flag);
%dhb (inpatient_visit_grp);
%dhb (los_days_grp);
%dhb (ed_visit_grp);
%dhb (gp_visit_grp);
%dhb (child_nz_born);
%dhb (mother_no_english);
%dhb (mother_nz_born);

data dhb_full_data;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
parent1_no_english
urban
hh_size_grp
hh_rent
res_changes
bw_grp
gestation_grp
disability_flag
inpatient_visit_grp
los_days_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

* Predictor - TALB level;

*TA/ward file;
*grouped;

%macro TA(var);

proc summary nway data=b4sc_allyr_final;
class grp_ta_ward_name &var.;
var vht_incomplete nurse_incomplete sdqt_incomplete;
output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
run;

data &var.;
set &var.;
var="&var.";
run;

%mend;

%TA (n_mv_access);
%TA (low_bw);
%TA (sex);
%TA (maori_eth);
%TA (asian_eth);
%TA (pacific_eth);
%TA (euro_eth);
%TA (nzdep_quint);
%TA (sibling_count);
%TA (mother_highest_qual);
%TA (hh_benefit_inc);
%TA (mother_age_grp);
%TA (birth_father);
%TA (urban);
%TA (hh_size_grp);
%TA (hh_rent);
%TA (res_changes);
%TA (bw_grp);
%TA (inpatient_visit_grp);
%TA (ed_visit_grp);
%TA (gp_visit_grp);
%TA (child_nz_born);
%TA (parent1_no_english);
%TA (mother_no_english);
%TA (mother_nz_born);
%TA (disability_flag);


data taward_full_data;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
urban
hh_size_grp
hh_rent
res_changes
bw_grp
inpatient_visit_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

*Need to add back in the zero rows so that they can be suppressed;
*Get a complete list of all variables and levels;
proc sql;
create table varlevels as
select distinct var, level
from dhb_full_data
order by var, level;
quit;

*Complete list of ta_wards;
proc sql;
create table tas as
select distinct grp_ta_ward_name
from taward_full_data
order by grp_ta_ward_name;
quit;

*Merge so that every var level is recorded against every ta;
proc sql;
create table taward_varlist_full as
select a.*, b.*
from tas as a cross join varlevels as b;
quit;

*Remove gestation, mother english and disability as we are not using them for the ta/ward analysis;
data taward_varlist;
set taward_varlist_full;
if var in('disability_flag' 'parent1_no_english' 'gestation_grp' 'father_highest_qual' 'los_days_grp') then delete;
run;

*Merge with original data files so that blanks are inserted for missing var levels;
proc sql;
create table taward_final as
select a.grp_ta_ward_name, a.var, a.level, b.total, b.vht_incomplete, b.nurse_incomplete, b.sdqt_incomplete
from taward_varlist as a left join taward_full_data as b on a.grp_ta_ward_name=b.grp_ta_ward_name and a.var=b.var and a.level=b.level;
quit;

* Task 4: Missing counts for each predictor;
PROC FREQ DATA = B4SC_ALLYR_FINAL; 
TABLE mother_highest_qual / MISSING OUT = mother_highest_qual;
TABLE n_mv_access / MISSING OUT = n_mv_access;
TABLE low_bw / MISSING OUT = low_bw;
TABLE sex / MISSING OUT = sex;
TABLE maori_eth / MISSING OUT = maori_eth;
TABLE asian_eth / MISSING OUT = asian_eth;
TABLE pacific_eth / MISSING OUT = pacific_eth;
TABLE euro_eth / MISSING OUT = euro_eth;
TABLE nzdep_quint / MISSING OUT = nzdep_quint;
TABLE sibling_count / MISSING OUT = sibling_count;
TABLE hh_benefit_inc / MISSING OUT = hh_benefit_inc; 
TABLE mother_age_grp / MISSING OUT = mother_age_grp;
TABLE birth_father / MISSING OUT = birth_father;
TABLE urban / MISSING OUT = urban;
TABLE hh_size_grp / MISSING OUT = hh_size_grp;
TABLE hh_rent / MISSING OUT = hh_rent;
TABLE res_changes / MISSING OUT = res_changes;
TABLE bw_grp / MISSING OUT = bw_grp;
TABLE inpatient_visit_grp / MISSING OUT = inpatient_visit_grp;
TABLE ed_visit_grp / MISSING OUT = ed_visit_grp;
TABLE gp_visit_grp / MISSING OUT = gp_visit_grp;
TABLE child_nz_born / MISSING OUT = child_nz_born;
TABLE mother_no_english / MISSING OUT = mother_no_english;
TABLE mother_nz_born / MISSING OUT = mother_nz_born;
TABLE disability_flag / MISSING OUT = disability_flag;
RUN;

%MACRO MISSING(var);
DATA &var.;
SET &var.;
var = "&var.";
RENAME &var. = level;
RUN;
%MEND;

%MISSING (mother_highest_qual);
%MISSING (n_mv_access);
%MISSING (low_bw);
%MISSING (sex);
%MISSING (maori_eth);
%MISSING (asian_eth);
%MISSING (pacific_eth);
%MISSING (euro_eth);
%MISSING (nzdep_quint);
%MISSING (sibling_count);
%MISSING (hh_benefit_inc);
%MISSING (mother_age_grp);
%MISSING (birth_father);
%MISSING (urban);
%MISSING (hh_size_grp);
%MISSING (hh_rent);
%MISSING (res_changes);
%MISSING (bw_grp);
%MISSING (inpatient_visit_grp);
%MISSING (ed_visit_grp);
%MISSING (gp_visit_grp);
%MISSING (child_nz_born);
%MISSING (mother_no_english);
%MISSING (mother_nz_born);
%MISSING (disability_flag);


data missing_count;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
urban
hh_size_grp
hh_rent
res_changes
bw_grp
inpatient_visit_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

* Task 5: distribution of each predictor by DHB and by TALB;

%MACRO DHBpred(var);
PROC FREQ DATA = B4SC_ALLYR_FINAL;
TABLE DHB2015_name * &var./ NOPERCENT NOROW NOCOL MISSING OUT=&var. ;
RUN;

DATA &var.;
SET &var.;
DROP percent;
RENAME &var. = level;
var = "&var.";
RUN;
%MEND;

%DHBpred (mother_highest_qual);
%DHBpred (n_mv_access);
%DHBpred (low_bw);
%DHBpred (sex);
%DHBpred (maori_eth);
%DHBpred (asian_eth);
%DHBpred (pacific_eth);
%DHBpred (euro_eth);
%DHBpred (nzdep_quint);
%DHBpred (sibling_count);
%DHBpred (hh_benefit_inc);
%DHBpred (mother_age_grp);
%DHBpred (birth_father);
%DHBpred (urban);
%DHBpred (hh_size_grp);
%DHBpred (hh_rent);
%DHBpred (res_changes);
%DHBpred (bw_grp);
%DHBpred (inpatient_visit_grp);
%DHBpred (ed_visit_grp);
%DHBpred (gp_visit_grp);
%DHBpred (child_nz_born);
%DHBpred (mother_no_english);
%DHBpred (mother_nz_born);
%DHBpred (disability_flag);

data pred_dist_DHB;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
urban
hh_size_grp
hh_rent
res_changes
bw_grp
inpatient_visit_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

%MACRO TApred(var);
PROC FREQ DATA = B4SC_ALLYR_FINAL;
TABLE grp_ta_ward_name * &var./ NOPERCENT NOROW NOCOL MISSING OUT=&var. ;
RUN;

DATA &var.;
SET &var.;
DROP percent;
RENAME &var. = level;
var = "&var.";
RUN;
%MEND;

%TApred (mother_highest_qual);
%TApred (n_mv_access);
%TApred (low_bw);
%TApred (sex);
%TApred (maori_eth);
%TApred (asian_eth);
%TApred (pacific_eth);
%TApred (euro_eth);
%TApred (nzdep_quint);
%TApred (sibling_count);
%TApred (hh_benefit_inc);
%TApred (mother_age_grp);
%TApred (birth_father);
%TApred (urban);
%TApred (hh_size_grp);
%TApred (hh_rent);
%TApred (res_changes);
%TApred (bw_grp);
%TApred (inpatient_visit_grp);
%TApred (ed_visit_grp);
%TApred (gp_visit_grp);
%TApred (child_nz_born);
%TApred (mother_no_english);
%TApred (mother_nz_born);
%TApred (disability_flag);

data pred_dist_TA;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
urban
hh_size_grp
hh_rent
res_changes
bw_grp
inpatient_visit_grp
ed_visit_grp
gp_visit_grp
child_nz_born;
run;

* Export files;
PROC EXPORT DATA = completion_all_year OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/completion_all_year.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = completion_all_year_ta OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/completion_all_year_ta.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = national_incomplete OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/national_incomplete.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = dhb_full_data OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/dhb_full_data.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = taward_final OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/taward_final.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = missing_count OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/missing_count.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = pred_dist_DHB OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/pred_dist_DHB.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = pred_dist_TA OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/pred_dist_TA.csv"
DBMS = CSV REPLACE; RUN;

* Change WARD to LOCAL BOARD;
PROC IMPORT FILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/Annual Areas 2018.csv"
OUT = map DBMS = CSV REPLACE; RUN;

PROC SQL;
	CREATE TABLE dataset AS
	SELECT a.*, b.TA2018_name, b.CB2018_name
	FROM B4SC_ALLYR_FINAL AS a
	LEFT JOIN map AS b
	ON a.mb18_num = b.MB2018_code;
QUIT;

DATA dataset2;
SET dataset;
IF DHB2015_name = "Auckland" OR DHB2015_name = "Counties Manukau" OR DHB2015_name = "Waitemata" ;
RUN;

%MACRO LB(year);
PROC SUMMARY NWAY DATA = dataset2 (WHERE=(YEAR=&year.));
CLASS CB2018_name;
VAR vht_complete nurse_complete sdqt_complete;
OUTPUT OUT = year_&year. (DROP=_type_ RENAME=(_FREQ_=total)) SUM= ;
RUN;

DATA YEAR_&year.;
SET year_&year.;
YEAR = "&year.";
RUN;
%MEND;

%LB (2011);
%LB (2012);
%LB (2013);
%LB (2014);
%LB (2015);
%LB (2016);
%LB (2017);
%LB (2018);

DATA completion_all_year_LB;
SET year_2011 year_2012 year_2013 year_2014 year_2015 year_2016 year_2017 year_2018;
RUN; 



%macro LB_inc(var);

proc summary nway data=dataset2;
class CB2018_name &var.;
var vht_incomplete nurse_incomplete sdqt_incomplete;
output out=&var. (drop=_type_ rename=(_FREQ_=total &var=level)) sum= ;
run;

data &var.;
set &var.;
var="&var.";
run;

%mend;

%LB_inc (n_mv_access);
%LB_inc (low_bw);
%LB_inc (sex);
%LB_inc (maori_eth);
%LB_inc (asian_eth);
%LB_inc (pacific_eth);
%LB_inc (euro_eth);
%LB_inc (nzdep_quint);
%LB_inc (sibling_count);
%LB_inc (mother_highest_qual);
%LB_inc (hh_benefit_inc);
%LB_inc (mother_age_grp);
%LB_inc (birth_father);
%LB_inc (urban);
%LB_inc (hh_size_grp);
%LB_inc (hh_rent);
%LB_inc (res_changes);
%LB_inc (bw_grp);
%LB_inc (inpatient_visit_grp);
%LB_inc (ed_visit_grp);
%LB_inc (gp_visit_grp);
%LB_inc (child_nz_born);
%LB_inc (mother_no_english);
%LB_inc (mother_nz_born);
%LB_inc (disability_flag);


data lb_full_data0;
set mother_highest_qual
n_mv_access
low_bw
sex
maori_eth
asian_eth
pacific_eth
euro_eth
nzdep_quint
sibling_count
hh_benefit_inc
mother_age_grp
birth_father
urban
hh_size_grp
hh_rent
res_changes
bw_grp
inpatient_visit_grp
ed_visit_grp
gp_visit_grp
child_nz_born
disability_flag
mother_no_english
mother_nz_born;
run;

DATA lb_full_data;
SET lb_full_data0;
ARRAY CHANGE(4) total vht_incomplete nurse_incomplete sdqt_incomplete;
DO i=1 TO 4;
IF CHANGE(i)<6 THEN CHANGE(i)=.;
END;
RUN;

PROC EXPORT DATA = lb_full_data OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/lb_full_data.csv"
DBMS = CSV REPLACE; RUN;

PROC EXPORT DATA = completion_all_year_LB OUTFILE = "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/completion_all_year_LB.csv"
DBMS = CSV REPLACE; RUN;