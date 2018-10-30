
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable


# def jobs_by_sector(geo, package="psrc_parcel"):
    # return [
               # "Natural_resources = %s.%s.number_of_jobs_of_sector_1" % (package, geo),
               # "Construction = %s.%s.number_of_jobs_of_sector_2" % (package, geo),
               # "Manuf = %s.%s.number_of_jobs_of_sector_3" % (package, geo),
               # "WTU = %s.%s.number_of_jobs_of_sector_4" % (package, geo),
               # "Retail = %s.%s.number_of_jobs_of_sector_5" % (package, geo),
               # "Business_Services = %s.%s.number_of_jobs_of_sector_7" % (package, geo),
               # "Private_Ed = %s.%s.number_of_jobs_of_sector_8" % (package, geo),
               # "Healthcare = %s.%s.number_of_jobs_of_sector_9" % (package, geo),
               # "Food_Services = %s.%s.number_of_jobs_of_sector_10" % (package, geo),
               # "Personal_Services = %s.%s.number_of_jobs_of_sector_11" % (package, geo),
               # "government = %s.%s.number_of_jobs_of_sector_12" % (package, geo),
               # "edu = %s.%s.number_of_jobs_of_sector_13" % (package, geo)               
               # # Old Employment Sectors
               # # "construction_resources = %s.%s.number_of_jobs_of_sector_1 + %s.%s.number_of_jobs_of_sector_2" % (2*(package, geo)),
               # # "manuf_WTU = %s.%s.number_of_jobs_of_sector_3 + %s.%s.number_of_jobs_of_sector_4 + %s.%s.number_of_jobs_of_sector_5 + %s.%s.number_of_jobs_of_sector_6 + %s.%s.number_of_jobs_of_sector_8 + %s.%s.number_of_jobs_of_sector_9" % (6*(package, geo)),
               # # "retail_food_services = %s.%s.number_of_jobs_of_sector_7 + %s.%s.number_of_jobs_of_sector_14" % (2*(package, geo)),
               # # "FIRE_services = %s.%s.number_of_jobs_of_sector_12 + %s.%s.number_of_jobs_of_sector_10 + %s.%s.number_of_jobs_of_sector_11 + %s.%s.number_of_jobs_of_sector_13 + %s.%s.number_of_jobs_of_sector_15 + %s.%s.number_of_jobs_of_sector_16 + %s.%s.number_of_jobs_of_sector_17" % (7*(package, geo)),
               # # "government = %s.%s.number_of_jobs_of_sector_18" % (package, geo),
               # # "edu = %s.%s.number_of_jobs_of_sector_19" % (package, geo)
           # ]

#def get_indicators(cache_directory, run_description, years = [2014,2015,2017,2020,2025,2030,2035,2040,2045,2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2014], base_year=2014):
def get_indicators(cache_directory, run_description, years = [2014,2017,2050], base_year=2014):

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

# # Minority - population, households, employment, and activity units

    Table(
       attribute = 'population = minority.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
       dataset_name = 'minority',
       source_data = source_data,
       ),
    
	Table(
       attribute = 'households = minority.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
       dataset_name = 'minority',
       source_data = source_data,
       ),
    
	Table(
       attribute = 'employment = minority.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       dataset_name = 'minority',
       source_data = source_data,
       ),  
    
	Table(
       attribute = 'activity_units = minority.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + minority.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       dataset_name = 'minority',
       source_data = source_data,
       ),
  
# ## Hex Level Indicators (minority_id = 2) -- for mapping
  
    Table(
        attribute = 'minority_population = hex.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_households = hex.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),  
    Table(
        attribute = 'minority_activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),	
  
# # # Minority Park/OS Buffer Indicators - #64
  
    Table(
        attribute = 'population_park_buffer = minority.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

    # Table(
        # attribute = 'households_park_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

    Table(
        attribute = 'employment_park_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_park_buffer = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

	   
# # # Minority Growth Amenities Buffer Indicators - #31
  
    Table(
        attribute = 'population_growth_amenities = minority.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

   # Table(
       # attribute = 'households_growth_amenities = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'minority',
       # source_data = source_data,
       # ),

    Table(
        attribute = 'employment_growth_amenities = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_growth_amenities = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

	   
# # # Minority Transit Buffer level by Indicators - #28a

    Table(
        attribute = 'population_transit_buffer = minority.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

    # Table(
        # attribute = 'households_transit_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'minority',
        # source_data = source_data,
        # ),

    Table(
        attribute = 'employment_transit_buffer = minority.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_transit_buffer = minority.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        dataset_name = 'minority',
        source_data = source_data,
        ),

# # # Minority Census Tract Level Indicators - #79

# # Non-minority = 1  
   Table(
        attribute = 'non_minority_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_minority_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_minority_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
# # # Minority = 2
   Table(
        attribute = 'non_minority_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_minority_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_minority_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
		
# # # Minority Subarea Level Indicators - #18

# # Non-minority = 1
    Table(
        attribute = 'non_minority_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'non_minority_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'non_minority_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
# # Minority = 2
    Table(
        attribute = 'minority_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),

				
# # # Minority TOD Level Indicators - #30

# # Non-Minority = 1
    Table(
        attribute = 'non_minority_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 1))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    # Table(
        # attribute = 'non_minority_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
    Table(
        attribute = 'non_minority_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 1))',
        dataset_name = 'tod',
        source_data = source_data,
        ),

# # Minority = 2
    Table(
        attribute = 'minority_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.minority_id == 2))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.minority_id == 2))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    Table(
        attribute = 'minority_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.minority_id == 2))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
		
# # Poverty - population, households, employment, and activity units

    Table(
       attribute = 'population = poverty.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
       dataset_name = 'poverty',
       source_data = source_data,
       ),
    
	Table(
       attribute = 'households = poverty.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
       dataset_name = 'poverty',
       source_data = source_data,
       ),
    
	Table(
       attribute = 'employment = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       dataset_name = 'poverty',
       source_data = source_data,
       ),  
   
    
	Table(
       attribute = 'activity_units = poverty.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + poverty.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
       dataset_name = 'poverty',
       source_data = source_data,
       ),

## Hex Level Indicators (poverty = 2) -- for mapping
  
    Table(
        attribute = 'poverty_population = hex.aggregate(urbansim_parcel.parcel.population, * (parcel.poverty_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),
    Table(
        attribute = 'poverty_households = hex.aggregate(urbansim_parcel.parcel.number_of_households, * (parcel.poverty_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),
    Table(
        attribute = 'poverty_employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs, * (parcel.poverty_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),  
    Table(
        attribute = 'poverty_activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs, * (parcel.poverty_id == 2))',
        dataset_name = 'hex',
        source_data = source_data,
        ),		   
	   
# # # Poverty Park/OS Buffer Indicators - #64
  
    Table(
        attribute = 'population_park_buffer = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

    # Table(
        # attribute = 'households_park_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

    Table(
        attribute = 'employment_park_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_park_buffer = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

	   
# # # Poverty Growth Amenities Buffer Indicators - #31
  
    Table(
        attribute = 'population_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

   # Table(
       # attribute = 'households_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'poverty',
       # source_data = source_data,
       # ),

    Table(
        attribute = 'employment_growth_amenities = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_growth_amenities = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

	   
# # # Poverty Transit Buffer level by Indicators - #28a

    Table(
        attribute = 'population_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

    # Table(
        # attribute = 'households_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        # dataset_name = 'poverty',
        # source_data = source_data,
        # ),

    Table(
        attribute = 'employment_transit_buffer = poverty.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_transit_buffer = poverty.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        dataset_name = 'poverty',
        source_data = source_data,
        ),

# # # Poverty Census Tract Level Indicators - #79

# # Non-poverty = 1  
   Table(
        attribute = 'non_poverty_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_poverty_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_poverty_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
# # # Poverty = 2
   Table(
        attribute = 'non_poverty_population = census_tract.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_poverty_households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
   Table(
        attribute = 'non_poverty_employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        dataset_name = 'census_tract',
        source_data = source_data,
        ),
		
# # # Poverty Subarea Level Indicators - #18

# # Non-poverty = 1
    Table(
        attribute = 'non_poverty_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'non_poverty_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'non_poverty_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
# # Poverty = 2
    Table(
        attribute = 'poverty_population = subarea.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'poverty_households = subarea.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'poverty_employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        dataset_name = 'subarea',
        source_data = source_data,
        ),

		
# # # Poverty TOD Level Indicators - #30

# # Non-Poverty = 1
    Table(
        attribute = 'non_poverty_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 1))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    # Table(
        # attribute = 'non_poverty_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 1))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
    Table(
        attribute = 'non_poverty_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 1))',
        dataset_name = 'tod',
        source_data = source_data,
        ),

# # Poverty = 2
    Table(
        attribute = 'poverty_population = tod.aggregate(urbansim_parcel.parcel.population * (parcel.poverty_id == 2))',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    # Table(
        # attribute = 'poverty_households = tod.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.poverty_id == 2))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),
    Table(
        attribute = 'poverty_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.poverty_id == 2))',
        dataset_name = 'tod',
        source_data = source_data,
        ),

]
    return indicators

def get_end_year_indicators(cache_directory, run_description, years = [2017,2050], base_year=2014):
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
             exclude_condition = 'building.year_built<2015',
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
                 'residential_units_base = parcel.aggregate(building.residential_units * (building.year_built < 2015))'
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