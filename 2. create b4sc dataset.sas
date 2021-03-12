
*This program takes the denominator population file and adds on other variables;
*The macro runs through each fiscal year of denominator population separately and creates a separate output file for each year;
*Final files are eg dlab2.b4sc_cov_2013_junyr_full;
*variable called ref_date is the first day of the month of 4th birthday;


%macro year (year);

%let refresh = 20200720; ** Specify IDI refresh to use for extractions;

libname central ODBC dsn=idi_clean_&refresh._srvprd schema=data;
libname moe ODBC dsn=idi_clean_&refresh._srvprd schema=moe_clean;
libname msd ODBC dsn=idi_clean_&refresh._srvprd schema=msd_clean;
libname ird ODBC dsn=idi_sandpit_srvprd schema=clean_read_ir_restrict;
libname moh ODBC dsn=idi_clean_&refresh._srvprd schema=moh_clean;
libname acc ODBC dsn=idi_clean_&refresh._srvprd schema=acc_clean;
libname dia ODBC dsn=idi_clean_&refresh._srvprd schema=dia_clean;
libname sec ODBC dsn=idi_clean_&refresh._srvprd schema=security;
libname census ODBC dsn=idi_clean_&refresh._srvprd schema=cen_clean;
libname metadata ODBC dsn=IDI_Metadata_srvprd schema=clean_read_CLASSIFICATIONS;
libname dlab "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage";
libname dlab2 "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/LATEST CODE/FINAL FOR OUTPUT";
libname conc "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/Papers/B4SC denominator/Concordance files";
libname sandmoe ODBC dsn=idi_sandpit_srvprd schema=clean_read_moe;
libname soc ODBC dsn=idi_adhoc schema=clean_read_MOH_SOCRATES;

*Join denominator pop to B4SC data;
proc sql;
create table pop_b4sc as
select a.*, b.moh_bsc_general_date as general_date format ddmmyy10., b.moh_bsc_check_status_text, 
case 
	when moh_bsc_general_date=. then 0 else 1
end as general_complete,
b.moh_bsc_vision_date as vision_date format ddmmyy10., b.moh_bsc_vision_outcome_text, 
case 
	when moh_bsc_vision_date=. then 0 else 1
end as vision_complete,
b.moh_bsc_hearing_date as hearing_date format ddmmyy10., b.moh_bsc_hearing_outcome_text, 
case 
	when moh_bsc_hearing_date=. then 0 else 1
end as hearing_complete,
b.moh_bsc_growth_date as growth_date format ddmmyy10., b.moh_bsc_growth_outcome_text,
case 
	when moh_bsc_growth_date=. then 0 else 1
end as growth_complete, 
b.moh_bsc_dental_date as dental_date format ddmmyy10., b.moh_bsc_dental_outcome_text, 
case 
	when moh_bsc_dental_date=. then 0 else 1
end as dental_complete,
b.moh_bsc_imms_date as imms_date format ddmmyy10., b.moh_bsc_imms_outcome_text, 
case 
	when moh_bsc_imms_date=. then 0 else 1
end as imms_complete,
b.moh_bsc_peds_date as peds_date format ddmmyy10., b.moh_bsc_peds_outcome_text, 
case 
	when moh_bsc_peds_date=. then 0 else 1
end as peds_complete,
b.moh_bsc_sdqp_date as sdqp_date format ddmmyy10., b.moh_bsc_sdqp_outcome_text, 
case 
	when moh_bsc_sdqp_date=. then 0 else 1
end as sdqp_complete,
b.moh_bsc_sdqt_date as sdqt_date format ddmmyy10., b.moh_bsc_sdqt_outcome_text,
case 
	when moh_bsc_sdqt_date=. then 0 else 1
end as sdqt_complete
from dlab.denominator_pop_&year._junyr as a left join moh.b4sc as b
on a.snz_uid=b.snz_uid;
quit;

data b4sc_final;
set pop_b4sc;
n_checks_complete=sum(vision_complete, hearing_complete, growth_complete, dental_complete, imms_complete, peds_complete, sdqp_complete, sdqt_complete);

if hearing_complete=1 and vision_complete=1 then vht_complete=1;
else vht_complete=0;

if (growth_complete=1 and imms_complete=1 and dental_complete=1 and peds_complete=1 and sdqp_complete=1) then nurse_complete=1;
else nurse_complete=0;
run;


*Get ethnicity;
proc sql;
create table b4sc_eth as
select a.*, b.snz_ethnicity_grp1_nbr as euro_eth, b.snz_ethnicity_grp2_nbr as maori_eth,
b.snz_ethnicity_grp3_nbr as pacific_eth, b.snz_ethnicity_grp4_nbr as asian_eth, 
b.snz_ethnicity_grp5_nbr as melaa_eth, b.snz_ethnicity_grp6_nbr as other_eth
from b4sc_final as a left join central.personal_detail as b
on a.snz_uid=b.snz_uid;
quit;

*Get address;


	*Import address areas file for 2017;
	*Dropping mb2017 as meshblock splits create duplicates when using mb2016;
	proc import datafile = '/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/Annual Areas 2018.csv'
	dbms = DLM replace out = work.area_table_2018_orig ;
	delimiter = ",";
	getnames = yes;
	guessingrows = 500;
	run;

	*Remove duplicate rows;
	proc sql;
	create table area_table_2018 as
	select distinct *
	from area_table_2018_orig;
	quit;

	*Get latest address from address notification table;

	*Link to address table;
	proc sql;
		create table b4sc_address as
		select distinct a.*, b.ant_meshblock_code as mb18a, b.ant_ta_code as ant_taa, b.ant_notification_date as addr_date_updated format ddmmyy10.
		from b4sc_eth as a left join central.address_notification_full as b on a.snz_uid=b.snz_uid and b.ant_notification_date<ref_date
		order by snz_uid, addr_date_updated descending;
	quit;


	**select the most recently updated addresses**;
	data final_b4sc_address;
	   set b4sc_address;
	   by snz_uid;
	   if first.snz_uid;

	   *create a numeric version of mb18 for merging with concordance files;
	   mb18a_num=input(mb18a, 10.0);
	   keep snz_uid mb18a mb18a_num ant_taa;
	run;

	*Merge with original pop;
	proc sql;
	create table pop_addr as 
	select *
	from b4sc_eth as a left join final_b4sc_address as b
	on a.snz_uid=b.snz_uid;
	quit;

	*For people with no addresses prior to 4th birthday;
	*If there is an address update within 12 months after the 4th birthday, use that;
	
	data no_addr;
	set pop_addr;
	if mb18a='';
	keep snz_uid ref_date;
	run;
	
	*Link to address table;
	proc sql;
		create table b4sc_address2 as
		select distinct a.*, b.ant_meshblock_code as mb18b, b.ant_ta_code as ant_tab, b.ant_notification_date as addr_date_updated format ddmmyy10.
		from no_addr as a left join central.address_notification_full as b on a.snz_uid=b.snz_uid and (0<(b.ant_notification_date-ref_date)<=365)
		order by snz_uid, addr_date_updated;
	quit;

	
	**select the first address after the 4th birthday**;
	data final_after_address;
	   set b4sc_address2;
	   by snz_uid;
	   if first.snz_uid;

	   *create a numeric version of mb18 for merging with concordance files;
	   mb18b_num=input(mb18b, 10.0);
	   keep snz_uid mb18b mb18b_num ant_tab;
	run;

	*Merge with original pop;
	proc sql;
	create table final_b4sc_address as 
	select a.*, (CASE when a.mb18a='' then b.mb18b else a.mb18a end) as mb18, (CASE when a.mb18a_num=. then b.mb18b_num else a.mb18a_num end) as mb18_num,
		(CASE when a.ant_taa='' then b.ant_tab else a.ant_taa end) as ant_ta
	from pop_addr as a left join final_after_address as b
	on a.snz_uid=b.snz_uid;
	quit;


	*Merge with concordances to get DHB and NZDEP;
	proc sql;
	create table b4sc_urban as
	select a.*, b.dhb2015_code, b.dhb2015_name, b.ta2018_code as TA18, b.ta2018_name as ta_name, b.WARD2018_code as ward, b.WARD2018_name as ward_name,
	b.ua2017_code, b.ua2017_name, mb2013_code as mb13_num format z7.
	from final_b4sc_address as a left join work.area_table_2018 as b on a.mb18_num=b.mb2018_code;
	quit;

	*Convert mb13 to char with leading zeroes- wont work in sql;
	data b4sc_urban2;
	set b4sc_urban;
	mb13=strip(put(mb13_num,z7.));
	drop mb13_num;
	run;

	proc sql;
	create table addr_nzdep as 
	select a.*, b.depindex2013 as nzdep2013, b.depscore2013
	from b4sc_urban2 as a left join metadata.depindex2013 as b on a.mb13=b.meshblock2013;
	quit;


*Calculate number of address changes prior to 4th birthday;

	*Get all address updates prior to 4th bday;
	proc sql;
		create table address_changes as
		select distinct a.snz_uid, b.snz_idi_address_register_uid as address_uid, b.ant_meshblock_code as MB13, b.ant_ta_code as TA13, (case when b.snz_idi_address_register_uid=. then 0 else 1 end) as uid_count
		from addr_nzdep as a left join central.address_notification_full as b on a.snz_uid=b.snz_uid and b.ant_notification_date<ref_date 
		order by snz_uid;
	quit;

	*Calculate total number of different address_uids lived in from age 0 to 4;
	proc summary nway data=address_changes;
	class snz_uid;
	var uid_count;
	output out=uid_count (drop=_type_ _freq_) sum=;
	quit;

	*Calculate total number of different meshblocks lived in from age 0 to 4;
	proc sql;
	create table distinct_mbs as
	select distinct snz_uid, mb13, (case when mb13='' then 0 else 1 end) as mb_count
	from address_changes;
	quit; 

	proc summary nway data=distinct_mbs;
	class snz_uid;
	var mb_count;
	output out=mb_count (drop=_type_ _freq_) sum=;
	quit;

	*Calculate total number of different TAs prior to 4th birthday;
	proc sql;
	create table ta_changes as
	select distinct snz_uid, ta13, (case when ta13='' then 0 else 1 end) as ta_count
	from address_changes 
	order by snz_uid;
	quit;

	proc summary nway data=ta_changes;
	class snz_uid;
	var ta_count;
	output out=ta_count (drop=_type_ _freq_) sum=;
	quit;

	*Merge back into main file;
	proc sort data=mb_count; by snz_uid;
	proc sort data=ta_count; by snz_uid;
	proc sort data=uid_count; by snz_uid;
	proc sort data=addr_nzdep; by snz_uid;

	data b4sc_address_changes;
	merge addr_nzdep (in=a) mb_count ta_count uid_count;
	by snz_uid;
	if a;
	format birth_date ddmmyy10.;
	birth_date=input(cat('01-', snz_birth_month_nbr, '-', snz_birth_year_nbr),ddmmyy10.);
	run;

*Total number of hospital visits, total number of ED visits, and days in hospital before 4th birthday;
	proc sql;
	create table b4sc_hosp_all as
	select distinct a.snz_uid, (case when b.moh_evt_shrtsty_ed_flg_ind='Y' then 0 else b.moh_evt_los_nbr end) as los_days, b.moh_evt_evst_date, b.moh_evt_even_date, b.moh_evt_shrtsty_ed_flg_ind,
	(case when b.moh_evt_shrtsty_ed_flg_ind='Y' then 0 else 1 end) as inpatient_visits, (case when b.moh_evt_shrtsty_ed_flg_ind='Y' then 1 else 0 end) as ed_visits
	from b4sc_address_changes as a inner join moh.pub_fund_hosp_discharges_event as b
	on a.snz_uid=b.snz_uid and ref_date>=b.moh_evt_even_date;
	quit;

	*Merge back into main file;
	proc sql;
	create table b4sc_hosp_final as 
	select (case when b.ed_visits=. then 0 else b.ed_visits end) as ed_visits, (case when b.inpatient_visits=. then 0 else b.inpatient_visits end) as inpatient_visits,
	(case when b.los_days=. then 0 else b.los_days end) as los_days, b.moh_evt_even_date, b.moh_evt_evst_date, a.*
	from b4sc_address_changes as a left join b4sc_hosp_all as b on a.snz_uid=b.snz_uid;
	quit;

	*Remove first hospital visit in the month of birth (we are assuming this is the babys birth event);

		*Reverse sort by date of discharge;
		proc sort data=b4sc_hosp_final;
			by snz_uid moh_evt_evst_date;
		run;

		data b4sc_hosp_final2;
			set b4sc_hosp_final;

			format hosp_date ddmmyy10.;
			hosp_date=input(moh_evt_evst_date, anydtdte10.);
			
			*Flagging visits in the same month as the childs birth;
			if year(hosp_date)=snz_birth_year_nbr and month(hosp_date)=snz_birth_month_nbr then same_month=1;
		
			*Removing the first hospital visit if it is in the same month as the birth;
			last_uid=lag(snz_uid);
			if snz_uid ne last_uid and same_month=1 then delete;
		
		drop hosp_date same_month;
		run;

	*Count up total number of visits;
	proc summary nway data=b4sc_hosp_final2;
	class snz_uid;
	var los_days inpatient_visits ed_visits;
	output out=hosp_visits (drop=_type_ _freq_) sum=;
	quit;

	
	*Merge final counts back into main file;
	proc sql;
	create table b4sc_hosp_counts as 
	select (case when b.ed_visits=. then 0 else b.ed_visits end) as ed_visits, (case when b.inpatient_visits=. then 0 else b.inpatient_visits end) as inpatient_visits,
	(case when b.los_days=. then 0 else b.los_days end) as los_days, a.*
	from b4sc_address_changes as a left join hosp_visits as b on a.snz_uid=b.snz_uid;
	quit;


** Calculate days spent in NZ up to age 4;

proc sql;
   create table overseas_spells as
   select distinct a.snz_uid , datepart(b.pos_applied_date) as applied_date format ddmmyy10., datepart(b.pos_ceased_date) as ceased_date format ddmmyy10., 
   b.pos_day_span_nbr as days_overseas, a.ref_date, a.birth_date
   from b4sc_hosp_counts as a left join central.person_overseas_spell as b
   on a.snz_uid=b.snz_uid and datepart(b.pos_applied_date) < a.ref_date
   order by snz_uid, applied_date;
quit;


data overseas_time;
   set overseas_spells;
   	if applied_date < birth_date and ceased_date <= ref_date then total_days_overseas = ceased_date - birth_date;
	else if applied_date < birth_date and ceased_date > ref_date then total_days_overseas = ref_date - birth_date;
	else if applied_date >= birth_date and ceased_date <= ref_date then total_days_overseas = days_overseas;
	else if applied_date >= birth_date and ceased_date > ref_date then total_days_overseas = (ref_date-applied_date);
	if applied_date=. then total_days_overseas=0;
run;

proc summary nway data=overseas_time;
class snz_uid;
var total_days_overseas;
output out=total_overseas_time (drop=_freq_ _type_) sum=;
run;

** Combine total population with time spent overseas;
proc sql;
create table b4sc_overseas as
select 
(case 
	when b.total_days_overseas=. then .  
	when b.total_days_overseas<=182 then 0
	when b.total_days_overseas<=547 then 1  
	when b.total_days_overseas<=912 then 2 
	when b.total_days_overseas<=1277 then 3 
	else 4 
end) as years_overseas, b.total_days_overseas, a.*					
from b4sc_hosp_counts as a left join total_overseas_time as b
on a.snz_uid=b.snz_uid;
quit;


*GP attendance;
	*We only have one attendance date per quarter, so range of possible attendances per year is 0 to 4;
	PROC SQL;
	create table pho as 
	select distinct a.snz_uid, b.moh_pho_last_consul_date
	from b4sc_overseas as a left join moh.pho_enrolment as b on a.snz_uid=b.snz_uid and ref_date>=b.moh_pho_last_consul_date;
	quit;

	*Count number of attendances;
	proc summary data=pho (where=(moh_pho_last_consul_date ne .))nway;
	class snz_uid;
	output out=pho_summ (drop=_type_);
	run;

	*Add back on to main file;
	proc sql;
	create table b4sc_pho as 
	select a.*, case when b._freq_ is null then 0 else b._freq_ end as gp_visits
	from b4sc_overseas as a left join pho_summ as b on a.snz_uid=b.snz_uid;
	quit;


******************************;
***    Census predictors   ***;
******************************;

**
	NB

	FOR 2013 CENSUS THE HOUSEHOLD AND DWELLING INFO IS ONLY CORRECT FOR PEOPLE WHO WERE IN THEIR USUAL RESIDENCE ON CENSUS NIGHT (cen_ind_usu_res_code=1)
	SO FOR ALL OF OUR VARIABLES THAT COME FROM THE DWELLING OR HOUSEHOLD FORMS, WE WILL MAKE THEM BLANK FOR CHILDREN WHO WERE NOT AT THEIR USUAL RES ON CENSUS NIGHT
	FOR 2018 CENSUS IT IS DIFFERENT AND WE CAN USE HOUSEHOLD AND DWELLING INFORMATION FOR EVERYONE 

**;


    **********************************
	*Doing the 2013 census vars first;
    **********************************


	*Get usual resident code, birth country info (1201 is nz, 9999 is missing) and household and dwelling uids for each child;
	*Make the dwelling and household ids blank for those who were not in their usual residence on census night;
	proc sql;
	create table hh_uid_13 as
	select a.*, b.cen_ind_usu_res_code, b.cen_ind_birth_country_code as cen_ind_birth_country_code_13, 
    case when b.cen_ind_usu_res_code='1' then b.snz_cen_hhld_uid else . end as snz_cen_hhld_uid_13, 
    case when b.cen_ind_usu_res_code='1' then b.snz_cen_dwell_uid else . end as snz_cen_dwell_uid_13
	from b4sc_pho as a left join census.census_individual_2013 as b on a.snz_uid=b.snz_uid;
	quit;

	*Get information from household dataset;
	proc sql;
	create table with_cen_hh_13 as
	select a.*, b.cen_hhd_usu_res_cnt_code as hh_size_13, b.cen_hhd_tenure_code as hh_tenure_13, b.cen_hhd_inc_srce7_hhld_code as inc_s7_13, 
	b.cen_hhd_inc_srce8_hhld_code as inc_s8_13, b.cen_hhd_inc_srce9_hhld_code as inc_s9_13, b.cen_hhd_inc_srce10_hhld_code as inc_s10_13,
	b.cen_hhd_ttl_inc_hhld_code as hh_total_inc_13
	from hh_uid_13 as a left join census.census_household_2013 as b 
	on a.snz_cen_hhld_uid_13=b.snz_cen_hhld_uid;
	quit;

	*Get motor vehicle information from dwelling dataset;
	proc sql;
	create table with_cen_dwl_13 as
	select a.*, case when b.cen_dwl_motor_vehicle_cnt_code in('7' '9') then . else input(b.cen_dwl_motor_vehicle_cnt_code,1.0) end as n_mv_access_13
	from with_cen_hh_13 as a left join census.census_dwelling_2013 as b on a.snz_cen_dwell_uid_13=b.snz_cen_dwell_uid;
	quit;


	*Passthrough to subset relevant variables from births table;
	*We can no longer read the table from SAS because permissions are denied on a couple of birthplace variables;
	proc sql;
	connect to odbc(dsn="idi_clean_&refresh._srvprd");
	create table birth_subset as
	select * from connection to odbc
	   (select snz_uid
	      ,snz_dia_uid
	      ,dia_bir_sex_snz_code
	      ,dia_bir_still_birth_code
	      ,dia_bir_multiple_birth_code
	      ,dia_bir_birth_month_nbr
	      ,dia_bir_birth_year_nbr
	      ,parent1_snz_uid
	      ,parent1_snz_dia_uid
	      ,dia_bir_parent1_sex_snz_code
	      ,dia_bir_parent1_occupation_text
	      ,dia_bir_parent1_birth_month_nbr
	      ,dia_bir_parent1_birth_year_nbr
	      ,dia_bir_parent1_maori_ind
	      ,dia_bir_parent1_tribe_text
	      ,dia_bir_parent1_child_rel_text
	      ,parent2_snz_uid
	      ,parent2_snz_dia_uid
	      ,dia_bir_parent2_sex_snz_code
	      ,dia_bir_parent2_occupation_text
	      ,dia_bir_parent2_birth_month_nbr
	      ,dia_bir_parent2_birth_year_nbr
	      ,dia_bir_sibling1_sex_snz_code
	      ,dia_bir_sibling1_died_code
	      ,dia_bir_sibling2_sex_snz_code
	      ,dia_bir_sibling2_died_code
	      ,dia_bir_sibling3_sex_snz_code
	      ,dia_bir_sibling3_died_code
	      ,dia_bir_sibling4_sex_snz_code
	      ,dia_bir_sibling4_died_code
	      ,dia_bir_sibling5_sex_snz_code
	      ,dia_bir_sibling5_died_code
	      ,dia_bir_sibling6_sex_snz_code
	      ,dia_bir_sibling6_died_code
	      ,dia_bir_sibling7_sex_snz_code
	      ,dia_bir_sibling7_died_code
	      ,dia_bir_sibling8_sex_snz_code
	      ,dia_bir_sibling8_died_code
	      ,dia_bir_sibling9_sex_snz_code
	      ,dia_bir_sibling9_died_code
	      ,dia_bir_sibling10_sex_snz_code
	      ,dia_bir_sibling10_died_code
	      ,dia_bir_sibling11_sex_snz_code
	      ,dia_bir_sibling11_died_code
	      ,dia_bir_sibling12_sex_snz_code
	      ,dia_bir_sibling12_died_code
	      ,dia_bir_sibling13_sex_snz_code
	      ,dia_bir_sibling13_died_code
	      ,dia_bir_sibling14_sex_snz_code
	      ,dia_bir_sibling14_died_code
	      ,dia_bir_sibling15_sex_snz_code
	      ,dia_bir_sibling15_died_code
	      ,dia_bir_sibling16_sex_snz_code
	      ,dia_bir_sibling16_died_code
	      ,dia_bir_parents_rel_code
	      ,dia_bir_relationship_date
	      ,snz_dia_birth_reg_uid
	      ,dia_bir_birth_reg_month_nbr
	      ,dia_bir_birth_reg_year_nbr
	      ,snz_dia_death_reg_uid
	      ,dia_bir_birth_weight_nbr
	      ,dia_bir_birth_gestation_nbr
	from dia_clean.births);
	disconnect from odbc;
	quit;


	*Get snz_uid for parents;
		proc sql;
		create table parent_uid as 
		select a.*, b.parent1_snz_uid, b.parent2_snz_uid, b.dia_bir_parent1_sex_snz_code, b.dia_bir_parent2_sex_snz_code, 
		b.dia_bir_parent1_birth_year_nbr, b.dia_bir_parent2_birth_year_nbr, b.dia_bir_parent1_birth_month_nbr, b.dia_bir_parent2_birth_month_nbr,
		b.dia_bir_birth_weight_nbr, 
	case when b.dia_bir_birth_gestation_nbr is null then . else
		(case when b.dia_bir_birth_gestation_nbr<22 then . else	
			(case when b.dia_bir_birth_gestation_nbr<37 then 1 else
				(case when b.dia_bir_birth_gestation_nbr<=42 then 2 else
					(case when b.dia_bir_birth_gestation_nbr<=45 then 3 else . end) 
				end)
			end)
		end)
	end as gestation_grp,

	case when b.dia_bir_birth_weight_nbr is null then . else
		(case when b.dia_bir_birth_weight_nbr<500 then . else	
			(case when b.dia_bir_birth_weight_nbr<2500 then 1 else
				(case when b.dia_bir_birth_weight_nbr<=4000 then 2 else 3 end)
			end)
		end)
	end as bw_grp,

	case when snz_dia_uid is not null then (
	(case when (b.dia_bir_sibling1_sex_snz_code is not null and dia_bir_sibling1_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling2_sex_snz_code is not null and dia_bir_sibling2_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling3_sex_snz_code is not null and dia_bir_sibling3_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling4_sex_snz_code is not null and dia_bir_sibling4_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling5_sex_snz_code is not null and dia_bir_sibling5_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling6_sex_snz_code is not null and dia_bir_sibling6_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling7_sex_snz_code is not null and dia_bir_sibling7_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling8_sex_snz_code is not null and dia_bir_sibling8_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling9_sex_snz_code is not null and dia_bir_sibling9_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling10_sex_snz_code is not null and dia_bir_sibling10_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling11_sex_snz_code is not null and dia_bir_sibling11_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling12_sex_snz_code is not null and dia_bir_sibling12_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling13_sex_snz_code is not null and dia_bir_sibling13_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling14_sex_snz_code is not null and dia_bir_sibling14_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling15_sex_snz_code is not null and dia_bir_sibling15_died_code is null) then 1 else 0 end) +
	(case when (b.dia_bir_sibling16_sex_snz_code is not null and dia_bir_sibling16_died_code is null) then 1 else 0 end)
	) else . end as sibling_count,

	case when snz_dia_uid is not null then 1 else 0 end as child_birth_rec

		from with_cen_dwl_13 as a left join birth_subset as b
		on a.snz_uid=b.snz_uid;
		quit;

	*Get census information for mothers (parent1);
	proc sql;
	create table with_cen_parent1_13 as 
	select a.*, b.snz_cen_uid as parent1_cen_uid_13, b.cen_ind_smoking_stus_code as parent1_smoke_13, b.cen_ind_legl_marital_stus_code as parent1_legl_m_status_13,
	b.cen_ind_social_marital_stus_code as parent1_socl_m_status_13, b.cen_ind_wklfs_code as parent1_wklfs_13, b.cen_ind_official_language_code as parent1_lang_13, 
	b.cen_ind_highest_qual_code as parent1_highest_qual_13, b.cen_ind_std_highest_qual_code as parent1_std_highest_qual_13,
	b.cen_ind_birth_country_code as parent1_birth_country_code_13
	from parent_uid as a left join census.census_individual_2013 as b 
	on a.parent1_snz_uid=b.snz_uid;
	quit;
	
	*Get census information for fathers (parent2);
	proc sql;
	create table with_cen_parent2_13 as 
	select a.*, b.snz_cen_uid as parent2_cen_uid_13, b.cen_ind_smoking_stus_code as parent2_smoke_13, b.cen_ind_legl_marital_stus_code as parent2_legl_m_status_13,
	b.cen_ind_social_marital_stus_code as parent2_socl_m_status_13, b.cen_ind_wklfs_code as parent2_wklfs_13, b.cen_ind_official_language_code as parent2_lang_13, 
	b.cen_ind_highest_qual_code as parent2_highest_qual_13, b.cen_ind_std_highest_qual_code as parent2_std_highest_qual_13
	from with_cen_parent1_13 as a left join census.census_individual_2013 as b 
	on a.parent2_snz_uid=b.snz_uid;
	quit;


*Get the same information from 2018 census ;
*We will use this for later years and also to replace missing values from 2013 census;

	*Permisions are denied on one variable in Census individual 2018 table which means we cannot access the table from SAS;
	*Need to use passthrough to subset relevant variables from census 2018 table so we can then use the subsetted table;
	proc sql;
	connect to odbc(dsn="idi_clean_&refresh._srvprd");
	create table census18_subset as
	select * from connection to odbc
	   (select snz_uid
	      ,snz_cen_uid
	      ,cen_ind_record_type_code
	      ,cen_ind_conf_key
	      ,cn_snz_cen_dwell_uid
	      ,ur_snz_cen_dwell_uid
	      ,cen_ind_unit_impt_log
	      ,cen_ind_cn_occupant_ind
	      ,cen_ind_visitor_ind
	      ,cen_ind_age_code
	      ,cen_ind_age_impt_ind
	      ,cen_ind_sex_code
	      ,cen_ind_sex_impt_ind
	      ,cen_ind_birth_country_code
	      ,cen_ind_birth_country_impt_ind
	      ,cen_ind_language_code
	      ,cen_ind_languages_cnt_code
	      ,cen_ind_official_language_code
	      ,cen_ind_language_impt_ind
	      ,cen_ind_wklfs_code
	      ,cen_ind_wklfs_impt_ind
	      ,cen_ind_hst_qual_code
	      ,cen_ind_standard_hst_qual_code
	      ,cen_ind_scdry_scl_qual_code
	      ,cen_ind_scdry_sch_qual_imp_ind
	      ,cen_ind_post_scl_level_code
	      ,cen_ind_post_scl_level_imp_ind
	      ,cen_ind_smoking_stus_code
	      ,cen_ind_smoke_regular_ind_code
	      ,cen_ind_smoke_regular_impt_ind
	      ,cen_ind_smoke_ever_ind_code
	      ,cen_ind_smoke_ever_impt_ind
	from cen_clean.census_individual_2018);
	disconnect from odbc;
	quit;

	*Get birth country info (1201 is nz, 9999 is missing) and household and dwelling uids for each child;
	*Dont need to get usual resident codes as people in 2018 census have been repatriated to their usual residence (check this with Barry);
	proc sql;
	create table hh_uid_18 as
	select a.*, b.cen_ind_birth_country_code as cen_ind_birth_country_code_18, b.ur_snz_cen_dwell_uid as snz_cen_dwell_uid_18 
	from with_cen_parent2_13 as a left join census18_subset as b on a.snz_uid=b.snz_uid;
	quit;


	*Permisions are denied on some variables in Census dwelling 2018 table which means we cannot access the table from SAS;
	*Need to use passthrough to subset relevant variables from census 2018 table so we can then use the subsetted table;
	proc sql;
	connect to odbc(dsn="idi_clean_&refresh._srvprd");
	create table census18dwl_subset as
	select * from connection to odbc
	   (select snz_cen_dwell_uid
	 	,cen_dwl_motor_vehicle_cnt_code
		,cen_dwl_tenure_code
		,cen_dwl_nbr_occupants_code
	from cen_clean.census_dwelling_2018);
	disconnect from odbc;
	quit;


	*Get motor vehicle, occupant count and tenure information from dwelling dataset;
	proc sql;
	create table with_cen_dwl_18 as
	select a.*, case when b.cen_dwl_motor_vehicle_cnt_code in('77' '99') then . else input(b.cen_dwl_motor_vehicle_cnt_code,2.0) end as n_mv_access_18,
    b.cen_dwl_tenure_code as hh_tenure_18, b.cen_dwl_nbr_occupants_code as hh_size_18
	from hh_uid_18 as a left join census18dwl_subset as b on a.snz_cen_dwell_uid_18=b.snz_cen_dwell_uid;
	quit;

	*Get census information for mothers (parent1);
	proc sql;
	create table with_cen_parent1_18 as 
	select a.*, b.snz_cen_uid as parent1_cen_uid_18, b.cen_ind_smoking_stus_code as parent1_smoke_18, b.cen_ind_wklfs_code as parent1_wklfs_18, 
    b.cen_ind_official_language_code as parent1_lang_18, 
	b.cen_ind_hst_qual_code as parent1_highest_qual_18, b.cen_ind_standard_hst_qual_code as parent1_std_highest_qual_18,
	b.cen_ind_birth_country_code as parent1_birth_country_code_18
	from with_cen_dwl_18 as a left join census18_subset as b 
	on a.parent1_snz_uid=b.snz_uid;
	quit;
	
	*Get census information for fathers (parent2);
	proc sql;
	create table with_cen_parent2_18 as 
	select a.*, b.snz_cen_uid as parent2_cen_uid_18, b.cen_ind_smoking_stus_code as parent2_smoke_18, b.cen_ind_wklfs_code as parent2_wklfs_18, 
    b.cen_ind_official_language_code as parent2_lang_18, 
	b.cen_ind_hst_qual_code as parent2_highest_qual_18, b.cen_ind_standard_hst_qual_code as parent2_std_highest_qual_18
	from with_cen_parent1_18 as a left join census18_subset as b 
	on a.parent2_snz_uid=b.snz_uid;
	quit;


	**End of census variables;

	
*Flag for disability;

	*Add dates to SOCRATES referrals;
		proc sql;
		create table socrates_dates as
		select (input(b.referraldate,anydtdte13.)) as referraldate format ddmmyy10., a.*
		from soc.moh_disability as a left join soc.moh_referral as b on a.snz_moh_uid=b.snz_moh_uid
		order by snz_moh_soc_client_uid, referraldate;
		quit;

	*Add snz_uid;
		proc sql;
		create table socrates_dates_uid as
		select a.*, b.snz_uid
		from socrates_dates as a left join sec.concordance as b on a.snz_moh_uid=b.snz_moh_uid;
		quit;

		proc sql;
		create table b4sc_disability as
		select a.*, (case when b.code='' then 0 else 1 end) as disability_flag, b.referraldate as disability_date
		from with_cen_parent2_18 as a left join socrates_dates_uid as b
		on a.snz_uid=b.snz_uid and b.referraldate<a.ref_date
		order by snz_uid, referraldate;
		quit;

	*Remove duplicates (where child has >1 chronic condition, keep the earliest);
		data b4sc_disability_nodups;
		set b4sc_disability;
		last_uid=lag(snz_uid);
		if snz_uid=last_uid then delete;
		drop disability_date last_uid;
		run;


*We now have all data in the file;
*We need to do some recoding and reclassifying and create some new vars;

	data dlab2.b4sc_cov_&year._junyr_full;
	set b4sc_disability_nodups;
	

	*Replace TA with ward for Auckland;
	*WE NEED TO SWITCH WARD TO LOCAL BOARD;
	if mb18='' then ta_ward=.;
	else if ta18=76 then ta_ward=ward;
	else ta_ward=ta18;
	format ta_ward_name $25.;
	if mb18='' then ta_ward_name='';
	else if ta18=76 then ta_ward_name=ward_name;
	else ta_ward_name=ta_name;
	

	*NZDep quintiles;
	if nzdep2013=. then nzdep_quint=.;
	else if nzdep2013 le 2 then nzdep_quint=1;
	else if nzdep2013 le 4 then nzdep_quint=2;
	else if nzdep2013 le 6 then nzdep_quint=3;
	else if nzdep2013 le 8 then nzdep_quint=4;
	else if nzdep2013 ge 9 then nzdep_quint=5;

	if ua2017_code=. then urban=.;
	else if ua2017_code<=500 then urban=1;
	else urban=0;

	if hh_tenure in('1' '10' '11' '12' '3' '30' '31' '32') then hh_rent=0;
	else if hh_tenure in('2' '20' '21' '22') then hh_rent=1;
	else hh_rent=.;

	if parent1_highest_qual='00' then mother_highest_qual=0;
	else if parent1_highest_qual in('01' '02' '03' '04') then mother_highest_qual=1;
	else if parent1_highest_qual in('05' '06' '07' '08' '09' '10') then mother_highest_qual=2;
	else if parent1_highest_qual in('11' '12' '13' '14') then mother_highest_qual=3;
	else mother_highest_qual=.;

	if parent2_highest_qual='00' then father_highest_qual=0;
	else if parent2_highest_qual in('01' '02' '03' '04') then father_highest_qual=1;
	else if parent2_highest_qual in('05' '06' '07' '08' '09' '10') then father_highest_qual=2;
	else if parent2_highest_qual in('11' '12' '13' '14') then father_highest_qual=3;
	else father_highest_qual=.;

	mother_age=snz_birth_year_nbr-dia_bir_parent1_birth_year_nbr;
	father_age=snz_birth_year_nbr-dia_bir_parent2_birth_year_nbr;

	if parent1_smoke in('7' '9') then parent1_smoke='';
	if parent2_smoke in('7' '9') then parent2_smoke='';
	
	if parent1_snz_uid=. then birth_father=.;
	else if parent2_snz_uid=. then birth_father=0;
	else birth_father=1;

	if parent1_lang in('97' '98' '99') then parent1_lang='';
	if parent2_lang in('97' '98' '99') then parent2_lang='';

	if parent1_lang='' then parent1_no_english=.;
	else if parent1_lang in('11' '13' '22' '23' '26' '33' '51') then parent1_no_english=1;
	else parent1_no_english=0;

	if parent2_lang='' then parent2_no_english=.;
	else if parent2_lang in('11' '13' '22' '23' '26' '33' '51') then parent2_no_english=1;
	else parent2_no_english=0;

	if inc_s7='' then hh_benefit_inc=.;
	else if (inc_s7='07' or inc_s8='08' or inc_s9='09' or inc_s10='10') then hh_benefit_inc=1;
	else hh_benefit_inc=0;

	if hh_total_inc=99 then hh_total_inc='';

	if snz_cen_hhld_uid=. then census_flag=0;
	else census_flag=1;

	if parent1_snz_uid=. then parent1_flag=0;
	else parent1_flag=1;

	if parent2_snz_uid=. then parent2_flag=0;
	else parent2_flag=1;

	if parent1_cen_uid=. then parent1_census_flag=0;
	else parent1_census_flag=1;

	if parent2_cen_uid=. then parent2_census_flag=0;
	else parent2_census_flag=1;
	
	*Low birthweight;
	if dia_bir_birth_weight_nbr=. then low_bw=.;
	else if dia_bir_birth_weight_nbr<2500 then low_bw=1;
	else low_bw=0;

	if inc_s7='' then hh_unemp_benefit=.;
	else if inc_s7='07' then hh_unemp_benefit=1;
	else hh_unemp_benefit=0;

	if hh_size=. then hh_size_grp=.;
	else if hh_size<5 then hh_size_grp=1;
	else if hh_size<8 then hh_size_grp=2;
	else if hh_size ge 8 then hh_size_grp=3;
	hh_size_num=hh_size*1;

	if mother_age=. then mother_age_grp=.;
	else if mother_age<20 then mother_age_grp=1;
	else if mother_age<25 then mother_age_grp=2;
	else if mother_age<30 then mother_age_grp=3;
	else if mother_age<35 then mother_age_grp=4;
	else if mother_age ge 35 then mother_age_grp=5;

	if inpatient_visits=. then inpatient_visit_grp=.;
	else if inpatient_visits=0 then inpatient_visit_grp=0;
	else if inpatient_visits le 2 then inpatient_visit_grp=1;
	else if inpatient_visits ge 3 then  inpatient_visit_grp=2;

	if ed_visits=. then ed_visit_grp=.;
	else if ed_visits=0 then ed_visit_grp=0;
	else if ed_visits>=1 then ed_visit_grp=1;

	if uid_count=. then res_changes=.;
	else if uid_count<2 then res_changes=0;
	else res_changes=uid_count-1;
	if res_changes>4 then res_changes=4;

	if sibling_count>2 then sibling_count=2;

	year=&year.;

	run;

	%mend;

	%year(2011);
	%year(2012);
	%year(2013);
	%year(2014);
	%year(2015);
	%year(2016);
	%year(2017);
	%year(2018);
