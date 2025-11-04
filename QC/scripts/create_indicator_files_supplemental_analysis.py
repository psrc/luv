
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
# # subreg by TOD (inside) - population, households, employment


Table(
    attribute = 'population_inside_hct = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id <> 0))',
        dataset_name = 'subreg',
        source_data = source_data,
        ),


Table(
      attribute = 'households_inside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id <> 0))',
       dataset_name = 'subreg',
       source_data = source_data,
       ),

  Table(
       attribute = 'employment_inside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id <> 0))',
        dataset_name = 'subreg',
        source_data = source_data,
        ),  

# # subreg by TOD (outside) - population, households, employment

Table(
    attribute = 'population_outside_hct = subreg.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 0))',
        dataset_name = 'subreg',
        source_data = source_data,
        ),



Table(
      attribute = 'households_outside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.tod_id == 0))',
       dataset_name = 'subreg',
       source_data = source_data,
       ),



  Table(
       attribute = 'employment_outside_hct = subreg.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 0))',
        dataset_name = 'subreg',
        source_data = source_data,
        ),         

# Federal Way Greater Downtown	
# Table(
    # attribute = 'population = fedwaygrtdt.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        # dataset_name = 'fedwaygrtdt',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'households = fedwaygrtdt.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        # dataset_name = 'fedwaygrtdt',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'employment = fedwaygrtdt.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # dataset_name = 'fedwaygrtdt',
        # source_data = source_data,
        # ),  	

        # Table(
    # attribute = 'population = parcel.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        # dataset_name = 'parcel',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'households = parcel.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        # dataset_name = 'parcel',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'employment = parcel.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # dataset_name = 'parcel',
        # source_data = source_data,
        # ),  	
        # Table(
        # attribute = 'activity_units = parcel.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + parcel.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'parcel',
       # source_data = source_data,
       # ),		

# Measure 2: Average Annual Growth Rate by Alternative: RGSs and Centers & Station Areas - In Progress

# # Population 

        # # Growth Centers

        # Table(
        # attribute = 'population_tod_1_hct_block = growth_center.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 1) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'growth_center',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_2_hct_block = growth_center.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 2) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'growth_center',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_4_hct_block = growth_center.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 4) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'growth_center',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_5_hct_block = growth_center.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 5) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'growth_center',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_6_hct_block = growth_center.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 6) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'growth_center',
                # source_data = source_data,
                # ),


                # # City

        # Table(
        # attribute = 'population_tod_1_hct_block = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 1) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_2_hct_block = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 2) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_4_hct_block = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 4) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_5_hct_block = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 5) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                # Table(
        # attribute = 'population_tod_6_hct_block = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 6) * (parcel.hct_block_id <> 0))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),


                # # Employment - Does not need to be done since Measure #5 replicates the output


# Measure 3: Activity Unit per acre in Centers compared to 45/85 AU per acre - Done

# # Growth Centers

        # Table(
        # attribute = 'activity_units = growth_center.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + growth_center.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       # dataset_name = 'growth_center',
       # source_data = source_data,
       # ),

       # # TOD

        # Table(
    # attribute = 'kin_activity_units = tod.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.county_id == 33))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),	   

        # Table(
    # attribute = 'kit_activity_units = tod.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.county_id == 35))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),	   

        # Table(
    # attribute = 'pie_activity_units = tod.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.county_id == 53))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),	   

        # Table(
    # attribute = 'sno_activity_units = tod.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.county_id == 61))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),	   		


# Measure 5: Growth by Centers & Station Areas x Jurisdiction, by type/mode

# # Population

        # Table(
        # attribute = 'population_tod_1 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 1))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'population_tod_2 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 2))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'population_tod_4 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 4))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'population_tod_5 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 5))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'population_tod_6 = city.aggregate(urbansim_parcel.parcel.population * (parcel.tod_id == 6))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # # # Employment 

        # Table(
        # attribute = 'employment_tod_1 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 1))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'employment_tod_2 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 2))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),		 

                 # Table(
        # attribute = 'employment_tod_4 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 4))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'employment_tod_5 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 5))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'employment_tod_6 = city.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.tod_id == 6))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),		 


# Measure 6: Distribution of Centers and Station Areas geography by county - Done

# # Acres by TOD by City

# Table(
        # attribute = 'acres_tod_1 = city.aggregate(urbansim_parcel.parcel.gross_sqft/43560 * (parcel.tod_id == 1))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),		 

                 # Table(
        # attribute = 'acres_tod_2 = city.aggregate(urbansim_parcel.parcel.gross_sqft/43560 * (parcel.tod_id == 2))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'acres_tod_4 = city.aggregate(urbansim_parcel.parcel.gross_sqft/43560 * (parcel.tod_id == 4))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'acres_tod_5 = city.aggregate(urbansim_parcel.parcel.gross_sqft/43560 * (parcel.tod_id == 5))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),

                 # Table(
        # attribute = 'acres_tod_6 = city.aggregate(urbansim_parcel.parcel.gross_sqft/43560 * (parcel.tod_id == 6))',
                # dataset_name = 'city',
                # source_data = source_data,
                # ),		 



# # ## Hex Level Indicators (minority_id = 2) -- for mapping

# # Table(
    # # attribute = 'minority_population = hex.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
        # # Table(
    # # attribute = 'minority_households = hex.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
        # # Table(
    # # attribute = 'minority_employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),  
        # # Table(
    # # attribute = 'minority_activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),	

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
# # # # Minority = 2
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


# # # # Minority TOD Level Indicators - #30

# # # Non-Minority = 1
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

# # # Minority = 2
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

# # # Poverty = 2
# Table(
    # attribute = 'poverty_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'poverty_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
        # Table(
    # attribute = 'poverty_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),

]
    return indicators

def get_end_year_indicators(cache_directory, run_description, years = [2023,2050], base_year=2023):
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
             dataset_name = 'building',
             name =  'new_buildings',
             attributes = [
                 'building.building_type_id',
                 'building.parcel_id',
                 'urbansim_parcel.building.unit_price',
                 'urbansim_parcel.building.residential_units',
                 'urbansim_parcel.building.non_residential_sqft',
                 'urbansim_parcel.building.year_built',
                 'building.template_id',
                 'urbansim_parcel.building.building_sqft'
                 ],
             exclude_condition = 'building.year_built<2023',
             ),

         DatasetTable(
             source_data = source_data,
             dataset_name = 'parcel',
             name =  'households_jobs',
             attributes = [
                 'parcel.county_id',
                 'parcel.minority_id',
                 'parcel.poverty_id',
                                 'households = urbansim_parcel.parcel.number_of_households',
                                 'urbansim_parcel.parcel.population',
                 'urbansim_parcel.parcel.residential_units',
                 'urbansim_parcel.parcel.employment',
                 'non_home_based_employment = parcel.aggregate(psrc_parcel.building.number_of_non_home_based_jobs)',
                 'non_residential_sqft = parcel.aggregate(building.non_residential_sqft)',
                 'building_sqft = parcel.aggregate(urbansim_parcel.building.building_sqft)',
                 'psrc_parcel.parcel.job_capacity',
                 'parcel.plan_type_id',
                 'residential_units_base = parcel.aggregate(building.residential_units * (building.year_built < 2023))'
                 ],
             ),
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
    indicators = get_indicators(ind_cache, os.getenv('QC_RUN1_DESCR', '')#, years = [2014,2015,2020]
                                )
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False)

    # runs buildings indicator only for the simulation end year
    IndicatorFactory().create_indicators(
        indicators = get_end_year_indicators(ind_cache, os.getenv('QC_RUN1_DESCR', ''),years = [2050]),
        display_error_box = False,
        show_results = False)

    write_info(ind_cache, os.getenv('QC_RUN1_DESCR', ''), os.getenv('QC_RUN1_RESTR', ''))