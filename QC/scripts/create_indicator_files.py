
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
#def get_indicators(cache_directory, run_description, years = [2014,2015,2017,2020,2025], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2014,2015,2017,2020,2021,2022,2023,2024,2025,2030,2035,2040,2045,2046,2047,2048,2049,2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2014,2016,2017,2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2014,2015,2020,2025,2030,2035,2036], base_year=2014):
#def get_indicators(cache_directory, run_description, years = range(2014,2034), base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2050], base_year=2014):
def get_indicators(cache_directory, run_description, years = [2014,2017,2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = range(2014,2046), base_year=2014):
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
    
		   # DatasetTable(
       # source_data = source_data,
       # dataset_name = 'faz',
       # name =  'DU_and_HH_by_bld_type_by_faz_by_year',
       # attributes = [
           # 'DU_SF_19=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==19), intermediates=[parcel])',
           # 'DU_MF_12=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==12), intermediates=[parcel])',
           # 'DU_CO_4=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==4), intermediates=[parcel])',
           # 'DU_MH_11=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==11), intermediates=[parcel])',
           # 'DU_Total=faz.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           # 'HH_SF_19=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==19), intermediates=[parcel])',
           # 'HH_MF_12=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==12), intermediates=[parcel])',
           # 'HH_CO_4=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==4), intermediates=[parcel])',
           # 'HH_MH_11=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==11), intermediates=[parcel])',
           # 'HH_Total=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
       # ],
  # ),

# Park/OS Buffer Indicators
  
   # Table(
       # attribute = 'population_park_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   # Table(
       # attribute = 'households_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   # Table(
       # attribute = 'employment_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),  
  
   # Table(
       # attribute = 'activity_units_park_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

	   
# Growth Amenities Buffer Indicators
  
   # Table(
       # attribute = 'population_growth_amenities = county.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   Table(
       attribute = 'households_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       dataset_name = 'county',
       source_data = source_data,
       ),

   # Table(
       # attribute = 'employment_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),  
  
   # Table(
       # attribute = 'activity_units_growth_amenities = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

	   
# # Transit Buffer level by County Indicators

   # Table(
       # attribute = 'population_transit_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   # Table(
       # attribute = 'households_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   # Table(
       # attribute = 'employment_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),  
  
   # Table(
       # attribute = 'activity_units_transit_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

       
# # UGA Buffer Level by County Indicators  
   
   # Table(
       # attribute = 'population_uga_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.uga_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

   # Table(
       # attribute = 'households_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.uga_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),
   # Table(
       # attribute = 'employment_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.uga_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),  
  
   # Table(
       # attribute = 'activity_units_uga_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.uga_buffer_id == 1))',
       # dataset_name = 'county',
       # source_data = source_data,
       # ),

  
  # # Transit Buffer Level Indicators  
  
   # Table(
	   # attribute = 'population = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'transit_buffer',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'transit_buffer',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'transit_buffer',
	   # source_data = source_data,
	   # ),  
  
   # Table(
	   # attribute = 'activity_units = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'transit_buffer',
	   # source_data = source_data,
	   # ),

	   
# # UGA Buffer Level Indicators  
  
   # Table(
	   # attribute = 'population = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'uga_buffer',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'uga_buffer',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'uga_buffer',
	   # source_data = source_data,
	   # ),  
  
   # Table(
	   # attribute = 'activity_units = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'uga_buffer',
	   # source_data = source_data,
	   # ),

# # Grid Level Indicators  
  
   # Table(
	   # attribute = 'population = grid.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'grid',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = grid.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'grid',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = grid.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'grid',
	   # source_data = source_data,
	   # ),  
   # Table(
	   # attribute = 'activity_units = grid.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + grid.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'grid',
	   # source_data = source_data,
	   # ),
  
  # # Census Tract Level Indicators  
  
   # Table(
	   # attribute = 'population = census_tract.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'census_tract',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'census_tract',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'census_tract',
	   # source_data = source_data,
	   # ),
  
# # Subarea Level Indicators  
  
   # Table(
	   # attribute = 'population = subarea.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'subarea',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = subarea.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'subarea',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'subarea',
	   # source_data = source_data,
	   # ),
  
  
# # TOD Level Indicators  
  
   # Table(
	   # attribute = 'population = tod.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'tod',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = tod.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'tod',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'tod',
	   # source_data = source_data,
	   # ),
  
  
# # County Level Control indicators  - added 4.17.2018

   # Table(
	   # attribute = 'population = county.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
	   # dataset_name = 'county',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'households = county.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
	   # dataset_name = 'county',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'employment = county.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'county',
	   # source_data = source_data,
	   # ),
   # Table(
	   # attribute = 'activity_units = county.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + county.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
	   # dataset_name = 'county',
	   # source_data = source_data,
	   # ),		  

  
 # # County Regional Geography indicators  - added 1.23.2018

        # Table(
            # attribute = 'population = fips_rgs_proposed.aggregate(urbansim_parcel.parcel.population, intermediates=[city])',
            # dataset_name = 'fips_rgs_proposed',
            # source_data = source_data,
            # ),
	    # Table(
            # attribute = 'households = fips_rgs_proposed.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[city])',
            # dataset_name = 'fips_rgs_proposed',
            # source_data = source_data,
            # ),
        # Table(
            # attribute = 'employment = fips_rgs_proposed.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[city])',
            # dataset_name = 'fips_rgs_proposed',
            # source_data = source_data,
            # ),
        # Table(
            # attribute = 'activity_units = fips_rgs_proposed.aggregate(urbansim_parcel.parcel.population, intermediates=[city]) + fips_rgs_proposed.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[city])',
            # dataset_name = 'fips_rgs_proposed',
            # source_data = source_data,
            # ),		   
		   

  
# # County MHS vacancy indicators - added 11.1.2017
     # DatasetTable (
         # source_data = source_data,
         # dataset_name = 'county',
         # name = 'eoy_vacancy_by_building_type',
		 # output_type = 'csv',
         # attributes = [
			 # 'res_4_VR=numpy.true_divide(county.aggregate(urbansim_parcel.building.vacant_residential_units*(building.building_type_id==4)),county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==4)))',
			 # 'res_12_VR=numpy.true_divide(county.aggregate(urbansim_parcel.building.vacant_residential_units*(building.building_type_id==12)),county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==12)))',
			 # 'res_19_VR=numpy.true_divide(county.aggregate(urbansim_parcel.building.vacant_residential_units*(building.building_type_id==19)),county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==19)))',
			 # 'nonres_3_VR=numpy.true_divide(county.aggregate(psrc_parcel.building.vacant_non_home_based_job_space*(psrc_parcel.building.building_type_id==3)),county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==3)))',
			 # 'nonres_8_VR=numpy.true_divide(county.aggregate(psrc_parcel.building.vacant_non_home_based_job_space*(psrc_parcel.building.building_type_id==8)),county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==8)))',
			 # 'nonres_13_VR=numpy.true_divide(county.aggregate(psrc_parcel.building.vacant_non_home_based_job_space*(psrc_parcel.building.building_type_id==13)),county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==13)))',
			 # 'nonres_20_VR=numpy.true_divide(county.aggregate(psrc_parcel.building.vacant_non_home_based_job_space*(psrc_parcel.building.building_type_id==20)),county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==20)))',
			 # 'nonres_21_VR=numpy.true_divide(county.aggregate(psrc_parcel.building.vacant_non_home_based_job_space*(psrc_parcel.building.building_type_id==21)),county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==21)))',
		    # ],
         # ),

    # DatasetTable (
         # source_data = source_data,
         # dataset_name = 'county',
         # name = 'units_and_nonres_sqft_by_building_type',
		 # output_type = 'csv',
         # attributes = [
			 # 'res_4_units=county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==4))',
			 # 'res_12_units=county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==12))',
			 # 'res_19_units=county.aggregate(urbansim_parcel.building.residential_units*(building.building_type_id==19))',
			 # 'nonres_3_spaces=county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==3))',
			 # 'nonres_8_spaces=county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==8))',
			 # 'nonres_13_spaces=county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==13))',
			 # 'nonres_20_spaces=county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==20))',
			 # 'nonres_21_spaces=county.aggregate(psrc_parcel.building.total_non_home_based_job_space*(psrc_parcel.building.building_type_id==21))',
			 # 'nonres_3_sqft=county.aggregate(psrc_parcel.building.non_residential_sqft*(psrc_parcel.building.building_type_id==3))',
			 # 'nonres_8_sqft=county.aggregate(psrc_parcel.building.non_residential_sqft*(psrc_parcel.building.building_type_id==8))',
			 # 'nonres_13_sqft=county.aggregate(psrc_parcel.building.non_residential_sqft*(psrc_parcel.building.building_type_id==13))',
			 # 'nonres_20_sqft=county.aggregate(psrc_parcel.building.non_residential_sqft*(psrc_parcel.building.building_type_id==20))',
			 # 'nonres_21_sqft=county.aggregate(psrc_parcel.building.non_residential_sqft*(psrc_parcel.building.building_type_id==21))',
			 # ],
         # ),		 
  
    # # FAZ indicators 
    # # =====================
    
       # Table(
           # attribute = 'households=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel,zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'population=faz.aggregate(urbansim_parcel.building.population, intermediates=[parcel,zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'employment=faz.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel,zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'nonres_sqft=faz.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel,zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),	   
       # Table(
           # attribute = 'residential_units=faz.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel,zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'building_sqft=faz.aggregate(urbansim_parcel.parcel.building_sqft, intermediates=[zone])',
           # dataset_name = 'faz',
           # source_data = source_data,
           # ),	       
               
    # # TAZ indicators 
       # Table(
           # attribute = 'residential_units=zone.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'households=zone.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'population=zone.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'employment=zone.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'nonres_sqft=zone.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'building_sqft=zone.aggregate(urbansim_parcel.parcel.building_sqft)',
           # dataset_name = 'zone',
           # source_data = source_data,
           # ),       
       
       # DatasetTable(
                         # source_data = source_data,
                         # dataset_name = 'zone',
                         # name = 'employment_by_aggr_sector',
                         # attributes = jobs_by_sector("zone", "urbansim_parcel"),
                         # output_type = 'tab'
                             # ),
    
    # # # ## City indicators
    # # # ==================
    
       # Table(
           # attribute = 'households=city.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           # dataset_name = 'city',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'population=city.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           # dataset_name = 'city',
           # source_data = source_data,
            # ),
       # Table(
           # attribute = 'employment=city.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           # dataset_name = 'city',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'residential_units=city.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           # dataset_name = 'city',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'nonres_sqft=city.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           # dataset_name = 'city',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'building_sqft=city.aggregate(urbansim_parcel.parcel.building_sqft)',
           # dataset_name = 'city',
           # source_data = source_data,
           # ),           
       # Table(
          # attribute = 'acres=city.aggregate(parcel.parcel_sqft/43560.)',
          # dataset_name = 'city',
          # source_data = source_data,
          # ),
       # Table(
          # attribute =  'activity_units = city.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + city.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
          # dataset_name = 'city',
          # source_data = source_data,
          # ),
		  
       # DatasetTable(
                  # source_data = source_data,
                  # dataset_name = 'city',
                  # name = 'employment_by_aggr_sector',
                  # attributes = jobs_by_sector("city"),
                  # output_type = 'tab'
                      # ),         

    # # ## Tract-City indicators
    # # ==================
    
       # # # Table(
           # # # attribute = 'households=tractcity.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'population=tractcity.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
            # # # ),
       # # # Table(
           # # # attribute = 'employment=tractcity.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'residential_units=tractcity.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'nonres_sqft=tractcity.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'building_sqft=tractcity.aggregate(urbansim_parcel.parcel.building_sqft)',
           # # # dataset_name = 'tractcity',
           # # # source_data = source_data,
           # # # ),       
               
    # # ## Growth Centers Indicators
    # # ============================
      
       # Table(
           # attribute = 'residential_units=growth_center.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           # dataset_name = 'growth_center',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'households=growth_center.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           # dataset_name = 'growth_center',
           # source_data = source_data,
           # ),
      # Table(
          # attribute = 'population=growth_center.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
          # dataset_name = 'growth_center',
          # source_data = source_data,
           # ),
       # Table(
           # attribute = 'employment=growth_center.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           # dataset_name = 'growth_center',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'nonres_sqft=growth_center.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           # dataset_name = 'growth_center',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'building_sqft=growth_center.aggregate(urbansim_parcel.parcel.building_sqft)',
           # dataset_name = 'growth_center',
           # source_data = source_data,
           # ),          
       
       # Table(
          # attribute = 'acres=growth_center.aggregate(parcel.parcel_sqft/43560.)',
          # dataset_name = 'growth_center',
          # source_data = source_data,
          # ),       
    
    # # ## Large Area Indicators
    # # ============================
    
        # DatasetTable(
           # source_data = source_data,
           # dataset_name = 'large_area',
           # name = 'employment_by_aggr_sector',
           # attributes = ['large_area.county_id'] + jobs_by_sector("large_area"),
           # output_type = 'tab'
               # ),    

# ## #### ============  Miscellaneous tables
# ## #### ============  Checked pretty regularly 
# ## #### ============  Run for every year in the simulation

    # # ## Tract indicators
    # # ============================

# # # Table(
           # # # attribute = 'households=census_tract.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel,census_block_group])',
           # # # dataset_name = 'census_tract',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'population=census_tract.aggregate(urbansim_parcel.building.population, intermediates=[parcel,census_block_group])',
           # # # dataset_name = 'census_tract',
           # # # source_data = source_data,
           # # # ),
       # # # Table(
           # # # attribute = 'employment=census_tract.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel,census_block_group])',
           # # # dataset_name = 'census_tract',
           # # # source_data = source_data,
           # # # ),
       # # # # Table(
           # # # # attribute = 'nonres_sqft=fcensus_tract.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel,census_block_group])',
           # # # # dataset_name = 'census_tract',
           # # # # source_data = source_data,
           # # # # ),	   
       # # # Table(
           # # # attribute = 'residential_units=census_tract.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel,census_block_group])',
           # # # dataset_name = 'census_tract',
           # # # source_data = source_data,
           # # # ),
    
       # # # DatasetTable(
            # # # source_data = source_data,
            # # # dataset_name = 'census_tract',
            # # # name = 'employment_by_aggr_sector',
            # # # attributes = jobs_by_sector("census_tract"),
            # # # output_type = 'csv'
                # # # ),


# ## ##------Liming's Unplaced Households and Jobs in the Region-----------

    # Table(
           # attribute = 'num_unplaced_hhs=alldata.aggregate_all(household.building_id<=0)',
           # dataset_name = 'alldata',
           # source_data = source_data,
         # ),
    # Table(
           # attribute = 'num_unplaced_jobs=alldata.aggregate_all(job.building_id<=0)',
           # dataset_name = 'alldata',
           # source_data = source_data,
         # ),
    
    # #  Regional Total Tables	 
             
             
      # Table(
          # attribute = 'residential_units=alldata.aggregate_all(urbansim_parcel.building.residential_units)',
          # dataset_name = 'alldata',
          # source_data = source_data,
          # ),
      # Table(
          # attribute = 'non_residential_sqft=alldata.aggregate_all(urbansim_parcel.building.non_residential_sqft)',
          # dataset_name = 'alldata',
          # source_data = source_data,
          # ),
      # Table(
          # attribute = 'households=alldata.aggregate_all(urbansim_parcel.building.number_of_households)',
          # dataset_name = 'alldata',
          # source_data = source_data,
          # ),
      # Table(
          # attribute = 'employment=alldata.aggregate_all(urbansim_parcel.building.number_of_jobs)',
          # dataset_name = 'alldata',
          # source_data = source_data,
          # ),
      # Table(
          # attribute = 'population=alldata.aggregate_all(urbansim_parcel.building.population)',
          # dataset_name = 'alldata',
          # source_data = source_data,
          # ),
      
      # # Demographic indicators
      # DatasetTable(
             # source_data = source_data,
             # dataset_name = 'alldata',
             # name =  'pptyp',
             # output_type = 'csv',
             # attributes = [
                     # 'full_time_worker = alldata.aggregate_all(person.employment_status==1)',
                     # 'part_time_worker = alldata.aggregate_all(numpy.logical_and(person.employment_status==2,person.student==0))',
                     # 'non_working_adult_age_65_plus = alldata.aggregate_all(numpy.logical_and(person.employment_status<1,numpy.logical_and(person.student==0,person.age>64)))',
                     # 'non_working_adult_age_16_64 = alldata.aggregate_all(numpy.logical_and(person.employment_status<1,numpy.logical_and(person.student==0,numpy.logical_and(person.age<65,person.age>15))))',
                     # 'university_student = alldata.aggregate_all(numpy.logical_and(person.employment_status<>1,numpy.logical_and(person.student==1,person.age>18)))',
                     # 'hs_student_age_15_up = alldata.aggregate_all(numpy.logical_and(person.employment_status<>1,numpy.logical_and(person.student==1,numpy.logical_and(person.age>15,person.age<19))))',
                     # 'child_age_5_15 = alldata.aggregate_all(numpy.logical_and(person.age>4,person.age<16))',
                     # 'child_age_0_4 = alldata.aggregate_all(person.age<5)',
      # ##                'age_6_to_10 = alldata.aggregate_all(numpy.logical_and(person.age<11,person.age>=6))',
      # ##                'age_11_to_15 = alldata.aggregate_all(numpy.logical_and(person.age<16,person.age>=11))',
      # ##                'age_16_to_20 = alldata.aggregate_all(numpy.logical_and(person.age<21,person.age>=16))',
      # ##                'age_21_to_25 = alldata.aggregate_all(numpy.logical_and(person.age<26,person.age>=21))',
      # ##                'income = households.income',
                # ],
           # ), 
      # DatasetTable(
          # source_data = source_data,
              # dataset_name = 'alldata',
              # name =  'persons_by_age_groups_of_interest',
              # output_type = 'csv',
              # attributes = [
                  # 'Under5 = alldata.aggregate_all(person.age<5)',
                      # 'Five_18 = alldata.aggregate_all(numpy.logical_and(person.age<19,person.age>=5))',
                      # 'Nineteen_24 = alldata.aggregate_all(numpy.logical_and(person.age<25,person.age>=19))',
                      # 'Twentyfive_60 = alldata.aggregate_all(numpy.logical_and(person.age<61,person.age>=25))',
                      # 'Over_60 = alldata.aggregate_all(person.age>=61)',
                      # ],
              # ),
      
          # DatasetTable(
              # source_data = source_data,
              # dataset_name = 'alldata',
              # name =  'persons_by_5year_age_groups',
              # output_type = 'csv',
              # attributes = [
                  # 'age_0_to_5 = alldata.aggregate_all(person.age<6)',
                      # 'age_6_to_10 = alldata.aggregate_all(numpy.logical_and(person.age<11,person.age>=6))',
                      # 'age_11_to_15 = alldata.aggregate_all(numpy.logical_and(person.age<16,person.age>=11))',
                      # 'age_16_to_20 = alldata.aggregate_all(numpy.logical_and(person.age<21,person.age>=16))',
                      # 'age_21_to_25 = alldata.aggregate_all(numpy.logical_and(person.age<26,person.age>=21))',
                      # 'age_26_to_30 = alldata.aggregate_all(numpy.logical_and(person.age<31,person.age>=26))',
                      # 'age_31_to_35 = alldata.aggregate_all(numpy.logical_and(person.age<36,person.age>=31))',
                      # 'age_36_to_40 = alldata.aggregate_all(numpy.logical_and(person.age<41,person.age>=36))',
                      # 'age_41_to_45 = alldata.aggregate_all(numpy.logical_and(person.age<46,person.age>=41))',
                      # 'age_46_to_50 = alldata.aggregate_all(numpy.logical_and(person.age<51,person.age>=46))',
                      # 'age_51_to_55 = alldata.aggregate_all(numpy.logical_and(person.age<56,person.age>=51))',
                      # 'age_56_to_60 = alldata.aggregate_all(numpy.logical_and(person.age<61,person.age>=56))',
                      # 'age_61_to_65 = alldata.aggregate_all(numpy.logical_and(person.age<66,person.age>=61))',
                      # 'age_66_to_70 = alldata.aggregate_all(numpy.logical_and(person.age<71,person.age>=66))',
                      # 'age_71_to_75 = alldata.aggregate_all(numpy.logical_and(person.age<76,person.age>=71))',
                      # 'age_76_to_80 = alldata.aggregate_all(numpy.logical_and(person.age<81,person.age>=76))',
                      # 'age_81_to_85 = alldata.aggregate_all(numpy.logical_and(person.age<86,person.age>=81))',
                      # 'age_86_to_90 = alldata.aggregate_all(numpy.logical_and(person.age<91,person.age>=86))',
                      # 'age_91_to_95 = alldata.aggregate_all(numpy.logical_and(person.age<96,person.age>=91))',
                      # 'age_96_and_up = alldata.aggregate_all(person.age>=96)',
                      # ],
              # ),
      
          # DatasetTable(
              # source_data = source_data,
              # dataset_name = 'alldata',
              # name =  'regional_total_hhs_by_new_14incomegroups',
              # output_type = 'csv',
              # #output_type = 'sql',
              # #storage_location = database,
              # attributes = [
                  # 'Group1_Under50K = alldata.aggregate_all(household.income<50000)',
                      # 'Group2_50_75K = alldata.aggregate_all(numpy.logical_and(household.income<75001,household.income>=50000))',
                      # 'Group3_75_100K = alldata.aggregate_all(numpy.logical_and(household.income<100001,household.income>=75000))',
                      # 'Group4_Over100K = alldata.aggregate_all(household.income>=100001)',
                      # ],
              # ),
      
          # DatasetTable(
              # source_data = source_data,
              # dataset_name = 'alldata',
              # name =  'regional_total_hhs_by_30_60_90_in_14dollars_groups',
              # output_type = 'csv',
              # #output_type = 'sql',
              # #storage_location = database,
              # attributes = [
                  # 'Group1_Under36870K = alldata.aggregate_all(household.income<36870)',
                      # 'Group2_UpTo73700 = alldata.aggregate_all(numpy.logical_and(household.income<73700,household.income>=36870))',
                      # 'Group3_UpTo110600 = alldata.aggregate_all(numpy.logical_and(household.income<110600,household.income>=73700))',
                      # 'Group4_Over110600 = alldata.aggregate_all(household.income>=110600)',
                      # ],
              # ), 
              
          # DatasetTable(
              # source_data = source_data,
              # dataset_name = 'alldata',
              # name =  'pwtyp',
              # output_type = 'csv',
              # attributes = [
                        # 'full_time = alldata.aggregate_all((person.employment_status==1)*(urbansim_parcel.person.job_id > 0))',
                        # 'part_time = alldata.aggregate_all((person.employment_status==2)*(urbansim_parcel.person.job_id > 0))',
                        # 'workers_no_job = alldata.aggregate_all((person.employment_status >0)*(urbansim_parcel.person.job_id < 0))',
                        # 'non_workers_no_job = alldata.aggregate_all((person.employment_status <1)*(urbansim_parcel.person.job_id < 0))',
                      # ],
              # ),            
 
    ]
    return indicators

# def get_end_year_indicators(cache_directory, run_description, years = [2050], base_year=2014):
    # source_data = SourceData(
        # cache_directory = cache_directory,
        # run_description = run_description,
        # years = years,
        # base_year = base_year,
        # dataset_pool_configuration = DatasetPoolConfiguration(
            # package_order=['psrc_parcel','urbansim_parcel','psrc', 'urbansim','opus_core'],
            # package_order_exceptions={},
            # ),       
    # )
    
    # indicators=[
        # DatasetTable(
            # source_data = source_data,
            # dataset_name = 'building',
            # name =  'new_buildings',
            # attributes = [
                # 'building.building_type_id',
                # 'building.parcel_id',
                # 'urbansim_parcel.building.unit_price',
                # 'urbansim_parcel.building.residential_units',
                # 'urbansim_parcel.building.non_residential_sqft',
                # 'urbansim_parcel.building.year_built',
                # 'building.template_id',
                # 'urbansim_parcel.building.building_sqft'
            # ],
            # exclude_condition = 'building.year_built<2015',
            # ),
			
        # DatasetTable(
            # source_data = source_data,
            # dataset_name = 'parcel',
            # name =  'households_jobs',
            # attributes = [
                # 'parcel.county_id',
				# 'households = urbansim_parcel.parcel.number_of_households',
                # 'urbansim_parcel.parcel.population',
                # 'urbansim_parcel.parcel.residential_units',
                # 'urbansim_parcel.parcel.employment',
                # 'non_home_based_employment = parcel.aggregate(psrc_parcel.building.number_of_non_home_based_jobs)',
                # 'non_residential_sqft = parcel.aggregate(building.non_residential_sqft)',
                # 'building_sqft = parcel.aggregate(urbansim_parcel.building.building_sqft)',
                # 'psrc_parcel.parcel.job_capacity',
                # 'parcel.plan_type_id',
				# 'residential_units_base = parcel.aggregate(building.residential_units * (building.year_built < 2015))'
            # ],
            # ),
        # ]
    # return indicators


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
    
    