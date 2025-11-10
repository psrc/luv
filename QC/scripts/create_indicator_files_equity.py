
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable


def get_indicators(cache_directory, run_description, years = [2023,2050], base_year=2023):

    source_data = SourceData(
        cache_directory = cache_directory,
        run_description = run_description,
        years = years,
        base_year = base_year,
        dataset_pool_configuration = DatasetPoolConfiguration(
            package_order=['psrc_parcel','urbansim_parcel','psrc', 'urbansim','opus_core'],
            package_order_exceptions={},
            ),       
    )

    indicators=[		

        DatasetTable(
            source_data = source_data,
            dataset_name = 'faz',
            name =  'persons_characteristics',
        attributes = [
            'pop_white = faz.aggregate(person.race_id == 1, intermediates=[household, building, parcel, zone])',
            'pop_black = faz.aggregate(person.race_id == 2, intermediates=[household, building, parcel, zone])',
            'pop_asian = faz.aggregate(person.race_id == 3, intermediates=[household, building, parcel, zone])', 
            'pop_other = faz.aggregate(person.race_id == 4, intermediates=[household, building, parcel, zone])',
            'pop_nhsp = faz.aggregate(person.race_id == 5, intermediates=[household, building, parcel, zone])',
            'pop_hsp = faz.aggregate(numpy.in1d(person.race_id, [6,7]), intermediates=[household, building, parcel, zone])',
            'pop_total = faz.aggregate(urbansim_parcel.building.population, intermediates=[parcel,zone])'
            ],
        ), 
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'faz',
            name =  'households_characteristics',
        attributes = [
            'mean_income = faz.aggregate(household.income * (household.income > 0), intermediates=[building, parcel, zone]) / faz.aggregate((household.household_id > 0) * (household.income > 0), intermediates=[building,parcel,zone])',
            'low_income = faz.aggregate(household.income < 56000, intermediates=[building, parcel, zone])',
            'high_income = faz.aggregate(household.income > 180000, intermediates=[building, parcel, zone])', 
            'hh_total =  faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel,zone])'
            ],        
        ),
        

# subreg by TOD types

#Table(
    #attribute = 'population_hct_1 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 1))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'population_hct_2 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 2))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'population_hct_3 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 3))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'population_hct_4 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 4))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'population_hct_5 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 5))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'population_hct_6 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 6))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'population_hct_7 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 7))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'population_hct_8 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 8))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'population_hct_9 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 9))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'population_hct_0 = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'households_hct_1 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 1))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'households_hct_2 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 2))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'households_hct_3 = city.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 3))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'households_hct_4 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 4))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'households_hct_5 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 5))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'households_hct_6 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 6))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'households_hct_7 = city.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 7))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'households_hct_8 = city.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 8))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'households_hct_9 = city.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 9))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'households_hct_0 = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'employment_hct_1 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 1))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'employment_hct_2 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 2))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'employment_hct_3 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 3))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'employment_hct_4 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 4))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'employment_hct_5 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 5))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

#Table(
    #attribute = 'employment_hct_6 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 6))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),	

## Table(
## attribute = 'employment_hct_7 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 7))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'employment_hct_8 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 8))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

## Table(
## attribute = 'employment_hct_9 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 9))',
        ## dataset_name = 'city',
        ## source_data = source_data,
        ## ),	

#Table(
    #attribute = 'employment_hct_0 = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),			

## # City by TOD (inside) - population, households, employment

#Table(
    #attribute = 'population_inside_hct = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id <> 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),

#Table(
       #attribute = 'households_inside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id <> 0))',
       #dataset_name = 'subreg',
       #source_data = source_data,
       #),

   #Table(
        #attribute = 'employment_inside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id <> 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),  

## # City by TOD (outside) - population, households, employment

#Table(
    #attribute = 'population_outside_hct = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #),

#Table(
       #attribute = 'households_outside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 0))',
       #dataset_name = 'subreg',
       #source_data = source_data,
       #),

   #Table(
        #attribute = 'employment_outside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 0))',
        #dataset_name = 'subreg',
        #source_data = source_data,
        #), 		


# # # Minority - population, households, employment, and activity units

# Table(
    # attribute = 'population = minority.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),

       # Table(
        # attribute = 'households = minority.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),

       # Table(
        # attribute = 'employment = minority.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),  

       # Table(
        # attribute = 'activity_units = minority.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + minority.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),

# ## Hex Level Indicators (minority_id = 2) -- for mapping

# Table(
    # attribute = 'minority_population = hex.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        # dataset_name = 'hex',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_households = hex.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        # dataset_name = 'hex',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # dataset_name = 'hex',
        # source_data = source_data,
        # ),  
        # Table(
    # attribute = 'minority_activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # dataset_name = 'hex',
        # source_data = source_data,
        # ),	

# # # # Minority Park/OS Buffer Indicators - #64

# Table(
    # attribute = 'population_park_buffer = minority.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'households_park_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'employment_park_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_park_buffer = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

# # Minority population inside urban growth boundary		

# Table(
    # attribute = 'minority_population_inside_ugb = minority.aggregate(urbansim_parcel.parcel.population * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),		


# # # # Minority Growth Amenities Buffer Indicators - #31

# Table(
    # attribute = 'population_growth_amenities = minority.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # Table(
   # attribute = 'households_growth_amenities = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),

       # Table(
    # attribute = 'employment_growth_amenities = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_growth_amenities = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),


# # # # Minority Transit Buffer level by Indicators - #28a

# Table(
    # attribute = 'population_transit_buffer = minority.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'households_transit_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'employment_transit_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_transit_buffer = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

# # # # Minority Census Tract Level Indicators - #79

# # # Non-minority = 1  
# Table(
   # attribute = 'non_minority_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'non_minority_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'non_minority_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
# # # Minority = 2
# Table(
   # attribute = 'minority_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'minority_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'minority_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),

# # # # Minority Subarea Level Indicators - #18

# # # Non-minority = 1
# Table(
    # attribute = 'non_minority_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_minority_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_minority_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
# # # Minority = 2
# Table(
    # attribute = 'minority_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),


# # # Minority TOD Level Indicators - #30

# # Non-Minority = 1
# Table(
    # attribute = 'non_minority_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_minority_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_minority_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),


# # Minority = 2
# Table(
    # attribute = 'minority_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'minority_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
  # attribute = 'developed_acres = minority.aggregate(numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft,intermediates=[parcel])/43560.0',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

        # DatasetTable(
        # source_data = source_data,
        # dataset_name = 'minority',
        # name =  'Acreage_by_built_res_density',
        # attributes = [
        # 'Nonres_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # ],
            # ),  



# # # Poverty - population, households, employment, and activity units

# Table(
    # attribute = 'population = poverty.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),

       # Table(
        # attribute = 'households = poverty.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),


       # Table(
        # attribute = 'employment = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),  


       # Table(
        # attribute = 'activity_units = poverty.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + poverty.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),

# ## Hex Level Indicators (poverty = 2) -- for mapping

# # Table(
    # # attribute = 'poverty_population = hex.aggregate(urbansim_parcel.parcel.population, * (parcel.poverty_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
        # # Table(
    # # attribute = 'poverty_households = hex.aggregate(urbansim_parcel.parcel.number_of_households, * (parcel.poverty_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
        # # Table(
    # # attribute = 'poverty_employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs, * (parcel.poverty_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),  
        # # Table(
    # # attribute = 'poverty_activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs, * (parcel.poverty_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),		   

# # # # Poverty Park/OS Buffer Indicators - #64

# Table(
    # attribute = 'population_park_buffer = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'households_park_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'employment_park_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_park_buffer = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

# # Poverty population inside urban growth boundary		

# Table(
    # attribute = 'poverty_population_inside_ugb = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),			

# # # # Poverty Growth Amenities Buffer Indicators - #31

# Table(
    # attribute = 'population_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

        # Table(
   # attribute = 'households_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),

       # Table(
    # attribute = 'employment_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_growth_amenities = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),


# # # # Poverty Transit Buffer level by Indicators - #28a

# Table(
    # attribute = 'population_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'households_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

        # Table(
    # attribute = 'employment_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),  

        # Table(
    # attribute = 'activity_units_transit_buffer = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

# # # # Poverty Census Tract Level Indicators - #79

# # # Non-poverty = 1  
# Table(
   # attribute = 'non_poverty_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'non_poverty_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'non_poverty_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
# # # # Poverty = 2
# Table(
   # attribute = 'poverty_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'poverty_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),
        # Table(
   # attribute = 'poverty_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        # dataset_name = 'census_tract',
        # source_data = source_data,
        # ),

# # # # Poverty Subarea Level Indicators - #18

# # # Non-poverty = 1
# Table(
    # attribute = 'non_poverty_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_poverty_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_poverty_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
# # # Poverty = 2
# Table(
    # attribute = 'poverty_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'poverty_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'poverty_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        # dataset_name = 'subarea',
        # source_data = source_data,
        # ),


# # # # Poverty TOD Level Indicators - #30

# # # Non-Poverty = 1
# Table(
    # attribute = 'non_poverty_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_poverty_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'non_poverty_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),


# # Poverty = 2
#Table(
        #attribute = 'poverty_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        #dataset_name = 'tod',
        #source_data = source_data,
        #),
    #Table(
        #attribute = 'poverty_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        #dataset_name = 'tod',
        #source_data = source_data,
        #),
    #Table(
        #attribute = 'poverty_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        #dataset_name = 'tod',
        #source_data = source_data,
        #),

    #Table(
      #attribute = 'developed_acres = poverty.aggregate(numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft,intermediates=[parcel])/43560.0',
        #dataset_name = 'poverty',
        #source_data = source_data,
        #),    

  #DatasetTable(
            #source_data = source_data,
        #dataset_name = 'poverty',
        #name =  'Acreage_by_built_res_density',
        #attributes = [
            #'Nonres_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Nonres_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Nonres_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Low_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Low_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Low_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Medium_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Medium_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'Medium_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'High_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'High_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #'High_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            #],
        #),
]
    return indicators



import os
from opus_core.indicator_framework.core.indicator_factory import IndicatorFactory

def write_info(directory, description, restrictions):
    # Store Description and Restrictions info
    path = os.path.join(directory, "indicators")
    descrfile = open(os.path.join(path, "Description.txt"), "w")
    descrfile.write(description)
    descrfile.close()
    restrfile = open(os.path.join(path, "Restrictions.txt"), "w")
    restrfile.write(restrictions)
    restrfile.close()


if __name__ == '__main__':
    ind_cache = os.path.join(os.environ['QC_BASE_DIRECTORY'], os.environ['QC_RUN1'])
    base_year = int(os.getenv('RUN1_BASE_YEAR', 2023))
    indicators = get_indicators(ind_cache, os.getenv('QC_RUN1_DESCR', ''),
                                years=[base_year, int(os.getenv('RUN1_END_YEAR', 2050))], 
                                base_year = base_year
                                )
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False,
        file_name_for_indicator_results = 'indicator_results_equity.html'
    )

    write_info(ind_cache, os.getenv('QC_RUN1_DESCR', ''), os.getenv('QC_RUN1_RESTR', ''))