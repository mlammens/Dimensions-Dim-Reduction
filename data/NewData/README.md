Dimensions ZA - New data for dimensions-reduction paper
===============================

This folder includes the data for Protea that extends beyond the extent used in the American Naturalist paper, that is, beyond the extent of the fynbos layer used in Adam Wilson's interpolated climate dataset.

Protea_Site_Data.csv--includes site names, GPS, field-site data, and soil data where available for 2011-2014 (note that 2014 samples were used only for DNA collection, not trait or envi). Can be used to extract climate data from Adam and Schulze layers.

Protea_Trait_Data.csv--includes all trait observations from 2011-2013, raw and derived variables. Recommend merging site and trait data using "Site_name".

* indicates have not measured this trait on all individuals in populations, by design

Traits to use:
	d13C_12C *
	Percent_N *
	LMA
	#Stomatal_density_average *
	#Stomatal_pore_index_average *
	Wood_density
	area_lam
	lam_width

Climate variables to use:
	
	#pptcon
	#summer 
	ratio
	map
	mat
	tminave07
	tmaxave01
	P_Bray_II_mg_kg 
	K_Exchangeable_cations_cmol_kg
	Ca_Exchangeable_cations_cmol_kg
	C
	temp_win
	temp_spr
	temp_sum
	temp_aut
	rain_win
	rain_spr
	rain_sum
	rain_aut
	apan_win
	apan_spr
	apan_sum
	apan_aut
	altitude

	where summer = dec, jan, feb; autumn = march, april, may; winter = june, july, august; spring = september, october, november
	# = do not include anymore
To do: Find stomatal data for 2013 samples

	
	