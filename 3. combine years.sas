
*This program combines the files for the separate fiscal years into a single file;
*It also creates some new variables;



%let refresh = 20200720; ** Specify IDI refresh to use for extractions;

libname dlab "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage";
libname census ODBC dsn=idi_clean_&refresh._srvprd schema=cen_clean;
libname dlab2 "/nas/DataLab/MAA/MAA2016-12 Early Childhood Development Indicators/Users/Sheree/SIA B4SC coverage/LATEST CODE/FINAL FOR OUTPUT";


data b4sc_raw;
	set dlab2.b4sc_cov_2011_junyr_full dlab2.b4sc_cov_2012_junyr_full dlab2.b4sc_cov_2013_junyr_full dlab2.b4sc_cov_2014_junyr_full 
	dlab2.b4sc_cov_2015_junyr_full dlab2.b4sc_cov_2016_junyr_full dlab2.b4sc_cov_2017_junyr_full dlab2.b4sc_cov_2018_junyr_full;

	*Dropping old census vars as we need to recreate them below;
	*Ideally I would remove these from the previous program so they never get created in the first place;
	drop parent1_lang mother_no_english hh_tenure hh_benefit_inc parent1_highest_qual mother_highest_qual hh_size hh_sizhh_size n_mv_access_18
	los_days los_days_grp inpatient_visit_grp;

run;


*Combine mv access info from 2013 and 2018 censuses;
*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;


data dlab2.b4sc_allyr_FINAL;
	set b4sc_raw;
	*Categorising GP visits;
	if gp_visits<4 then gp_visit_grp=1;
	else if gp_visits<8 then gp_visit_grp=2;
	else if gp_visits<12 then gp_visit_grp=3;
	else gp_visit_grp=4;

	*converting sex to numeric;
	sex2=sex*1;
	drop sex;
	rename sex2=sex;

*SORTING OUT CENSUS VARS;

	*I can only find household income vars for 2013 census, so benefit income var uses 2013 info only;
		if inc_s7_13='' then hh_benefit_inc=.;
		else if (inc_s7_13='07' or inc_s8_13='08' or inc_s9_13='09' or inc_s10_13='10') then hh_benefit_inc=1;
		else hh_benefit_inc=0;

	*Combine rent information from 2013 and 2018 censuses;
	*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
	*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;

		if year le 2015 then do;
			if hh_tenure_13 ne . then hh_tenure=hh_tenure_13;
			else if hh_tenure_13=. then hh_tenure=hh_tenure_18;
		end;

		if  year ge 2016 then do;
			if hh_tenure_18 ne . then hh_tenure=hh_tenure_18;
			else if hh_tenure_18=. then hh_tenure=hh_tenure_13;
		end;

		if hh_tenure in('1' '10' '11' '12' '3' '30' '31' '32') then hh_rent=0;
		else if hh_tenure in('2' '20' '21' '22') then hh_rent=1;
		else hh_rent=.;

	*Combine mother highest qual info from 2013 and 2018 censuses;
	*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
	*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;

		if parent1_highest_qual_13 in('97' '99') then parent1_highest_qual_13='';
		if parent1_highest_qual_18 in('97' '99') then parent1_highest_qual_18='';

		if year le 2015 then do;
			if parent1_highest_qual_13 ne '' then parent1_highest_qual=parent1_highest_qual_13;
			else if parent1_highest_qual_13='' then parent1_highest_qual=parent1_highest_qual_18;
		end;

		if  year ge 2016 then do;
			if parent1_highest_qual_18 ne '' then parent1_highest_qual=parent1_highest_qual_18;
			else if parent1_highest_qual_18='' then parent1_highest_qual=parent1_highest_qual_13;
		end;

		if parent1_highest_qual='00' then mother_highest_qual=0;
		else if parent1_highest_qual in('01' '02' '03' '04') then mother_highest_qual=1;
		else if parent1_highest_qual in('05' '06' '07' '08' '09' '10') then mother_highest_qual=2;
		else if parent1_highest_qual in('11' '12' '13' '14') then mother_highest_qual=3;
		else mother_highest_qual=.;

	*Combine mother language spoken info from 2013 and 2018 censuses;
	*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
	*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;

		*convert to numeric because it's doing weird things with the length;
		parent1_lang_num_13=parent1_lang_13*1;
		parent1_lang_num_18=parent1_lang_18*1;

		if parent1_lang_num_13 ge 97 then parent1_lang_num_13=.;
		if parent1_lang_num_18 ge 97 then parent1_lang_num_18=.;

		if year le 2015 then do;
			if parent1_lang_num_13 ne . then parent1_lang=parent1_lang_num_13;
			else if parent1_lang_num_13=. then parent1_lang=parent1_lang_num_18;
		end;

		if  year ge 2016 then do;
			if parent1_lang_num_18 ne . then parent1_lang=parent1_lang_num_18;
			else if parent1_lang_num_18=. then parent1_lang=parent1_lang_num_13;
		end;

		if parent1_lang='' then mother_no_english=.;
		else if parent1_lang in(11, 13, 22, 23, 26, 33, 51) then mother_no_english=1;
		else mother_no_english=0;	


	*Combine household size info from 2013 and 2018 censuses;
	*THERE ARE SOME REALLY LARGE HOUSEHOLD SIZES IN 2018 CENSUS- MAYBE NPDS? - WE SHOULD CONSIDER MAKING THIS VARIABLE BLANK IF SOMEONE IS LIVING IN NPD;
	*FOR NOW I HAVE JUST BLANKED HH SIZES OVER 20 AS THEY SEEM IMPLAUSIBLE;

		*2013 and 2018 are recorded in different character formats so make them both numeric so that we can combine them;
		hh_size_num_13=hh_size_13*1;
		hh_size_num_18=hh_size_18*1;

		if hh_size_num_13 > 20 then hh_size_num_13=.;
		if hh_size_num_18 > 20 then hh_size_num_18=.;


		*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
		*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;

		if year le 2015 then do;
			if hh_size_num_13 ne . then hh_size=hh_size_num_13;
			else if hh_size_num_13=. then hh_size=hh_size_num_18;
		end;

		if  year ge 2016 then do;
			if hh_size_num_18 ne . then hh_size=hh_size_num_18;
			else if hh_size_num_18=. then hh_size=hh_size_num_13;
		end;

		if hh_size=. then hh_size_grp=.;
		else if hh_size<5 then hh_size_grp=1;
		else if hh_size<8 then hh_size_grp=2;
		else if hh_size ge 7 then hh_size_grp=3;

	*Combine motor vehicle access info from 2013 and 2018 censuses;
	*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
	*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;

		if year le 2015 then do;
			if n_mv_access_13 ne '' then n_mv_access=n_mv_access_13;
			else if n_mv_access_13='' then n_mv_access=n_mv_access_18;
		end;

		if  year ge 2016 then do;
			if n_mv_access_18 ne '' then n_mv_access=n_mv_access_18;
			else if n_mv_access_18='' then n_mv_access=n_mv_access_13;
		end;

		*Recategorise mv access as not enough children in top level;
		if n_mv_access ge 3 then n_mv_access=2;


	*Combine birth country (child) info from 2013 and 2018 censuses;
	*For years 2010 to 2015 incl we will use 2013 census info if available, 2018 if 2013 is not available;
	*For 2016 to 2018 incl we will use 2018 census info if available, 2013 if 2018 is not available;
	
		if cen_ind_birth_country_code_13='9999' then cen_ind_birth_country_code_13='';
		if cen_ind_birth_country_code_18='9999' then cen_ind_birth_country_code_18='';

			if year le 2015 then do;
			if cen_ind_birth_country_code_13 ne '' then cen_ind_birth_country_code=cen_ind_birth_country_code_13;
			else if cen_ind_birth_country_code_13='' then cen_ind_birth_country_code=cen_ind_birth_country_code_18;
		end;

		if  year ge 2016 then do;
			if cen_ind_birth_country_code_18 ne '' then cen_ind_birth_country_code=cen_ind_birth_country_code_18;
			else if cen_ind_birth_country_code_18='' then cen_ind_birth_country_code=cen_ind_birth_country_code_13;
		end;

		*If census indicates NZ born OR there is a birth record, count the child as born in NZ;
		if cen_ind_birth_country_code='1201' then child_nz_born=1;
		else if parent1_snz_uid ne . then child_nz_born=1;
		else child_nz_born=0;

		*Mother birth place- this is NZ if recorded as NZ, otherwise not NZ (no blanks here);
		IF year le 2015 THEN mother_birth_place = parent1_birth_country_code_13;
		ELSE IF year ge 2016 THEN mother_birth_place = parent1_birth_country_code_18;
		IF mother_birth_place = "1201" THEN mother_nz_born = 1; ELSE mother_nz_born = 0;

		drop mother_birth_place;


*Creating new (alternative) TA variable with some TAs combined as numbers are too small; 
	format grp_ta_ward_name $50.;
	if ta_ward_name="Waitomo District" then grp_ta_ward_name="Waitomo & Otorohanga";
	else if ta_ward_name="Otorohanga District" then grp_ta_ward_name="Waitomo & Otorohanga";
	else if ta_ward_name="Waimate District" then grp_ta_ward_name="Waimate & Mackenzie";
	else if ta_ward_name="Mackenzie District" then grp_ta_ward_name="Waimate & Mackenzie";
	else if ta_ward_name="Kaikoura District" then grp_ta_ward_name="Hurunui & Kaikoura";
	else if ta_ward_name="Hurunui District" then grp_ta_ward_name="Hurunui & Kaikoura";
	else if ta_ward_name="Westland District" then grp_ta_ward_name="Westland & Grey";
	else if ta_ward_name="Grey District" then grp_ta_ward_name="Westland & Grey";
	else if ta_ward_name="Carterton District" then grp_ta_ward_name="Carterton & Sth Wairarapa";
	else if ta_ward_name="South Wairarapa District" then grp_ta_ward_name="Carterton & Sth Wairarapa";
	else if ta_ward_name="Kawerau District" then grp_ta_ward_name="Kawerau & Whakatane";
	else if ta_ward_name="Whakatane District" then grp_ta_ward_name="Kawerau & Whakatane";
	else if ta_ward_name="Queenstown-Lakes District" then grp_ta_ward_name="Queenstown & Central Otago";
	else if ta_ward_name="Central Otago District" then grp_ta_ward_name="Queenstown & Central Otago"; 
	else if ta_ward_name="Gore District" then grp_ta_ward_name="Gore & Southland";
	else if ta_ward_name="Southland District" then grp_ta_ward_name="Gore & Southland";
	else grp_ta_ward_name=ta_ward_name;

*Create non-completion vars; 
  
	if vht_complete=. then vht_incomplete=.;
	else if vht_complete=0 then vht_incomplete=1;
	else if vht_complete=1 then vht_incomplete=0;

	if nurse_complete=. then nurse_incomplete=.;
	else if nurse_complete=0 then nurse_incomplete=1;
	else if nurse_complete=1 then nurse_incomplete=0;

	if sdqt_complete=. then sdqt_incomplete=.;
	else if sdqt_complete=0 then sdqt_incomplete=1;
	else if sdqt_complete=1 then sdqt_incomplete=0;

	dhb2015_name=compress(dhb2015_name, "'");
	ta_ward_name=compress(ta_ward_name, "'");
	grp_ta_ward_name=compress(grp_ta_ward_name, "'");

run;


