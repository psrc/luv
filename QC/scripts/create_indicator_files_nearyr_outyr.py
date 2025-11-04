
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable


def jobs_by_sector(geo, package="psrc_parcel"):
    return [
               "Natural_resources = %s.%s.number_of_jobs_of_sector_1" % (package, geo),
               "Construction = %s.%s.number_of_jobs_of_sector_2" % (package, geo),
               "Manuf = %s.%s.number_of_jobs_of_sector_3" % (package, geo),
               "WTU = %s.%s.number_of_jobs_of_sector_4" % (package, geo),
               "Retail = %s.%s.number_of_jobs_of_sector_5" % (package, geo),
               "Business_Services = %s.%s.number_of_jobs_of_sector_7" % (package, geo),
               "Private_Ed = %s.%s.number_of_jobs_of_sector_8" % (package, geo),
               "Healthcare = %s.%s.number_of_jobs_of_sector_9" % (package, geo),
               "Food_Services = %s.%s.number_of_jobs_of_sector_10" % (package, geo),
               "Personal_Services = %s.%s.number_of_jobs_of_sector_11" % (package, geo),
               "government = %s.%s.number_of_jobs_of_sector_12" % (package, geo),
               "edu = %s.%s.number_of_jobs_of_sector_13" % (package, geo)               
               # Old Employment Sectors
               # "construction_resources = %s.%s.number_of_jobs_of_sector_1 + %s.%s.number_of_jobs_of_sector_2" % (2*(package, geo)),
               # "manuf_WTU = %s.%s.number_of_jobs_of_sector_3 + %s.%s.number_of_jobs_of_sector_4 + %s.%s.number_of_jobs_of_sector_5 + %s.%s.number_of_jobs_of_sector_6 + %s.%s.number_of_jobs_of_sector_8 + %s.%s.number_of_jobs_of_sector_9" % (6*(package, geo)),
               # "retail_food_services = %s.%s.number_of_jobs_of_sector_7 + %s.%s.number_of_jobs_of_sector_14" % (2*(package, geo)),
               # "FIRE_services = %s.%s.number_of_jobs_of_sector_12 + %s.%s.number_of_jobs_of_sector_10 + %s.%s.number_of_jobs_of_sector_11 + %s.%s.number_of_jobs_of_sector_13 + %s.%s.number_of_jobs_of_sector_15 + %s.%s.number_of_jobs_of_sector_16 + %s.%s.number_of_jobs_of_sector_17" % (7*(package, geo)),
               # "government = %s.%s.number_of_jobs_of_sector_18" % (package, geo),
               # "edu = %s.%s.number_of_jobs_of_sector_19" % (package, geo)
           ]

#def get_indicators(cache_directory, run_description, years = [2014,2015,2017,2020,2025,2030,2035,2040,2045,2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2050], base_year=2014):
#def get_indicators(cache_directory, run_description, years = [2018,2050], base_year=2018):
def get_indicators(cache_directory, run_description, years = [2023,2050], base_year=2023):
#def get_indicators(cache_directory, run_description, years = [2014,2017,2050], base_year=2014):

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

# # ## Is Inside Urban Growth Boundary Indicators

# # # # County  
    # # Table(
        # # attribute = 'population_inside_ugb = county.aggregate(urbansim_parcel.parcel.population * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),
# # # # Regional
    # # Table(
        # # attribute = 'population_inside_ugb = alldata.aggregate_all(urbansim_parcel.parcel.population * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'alldata',
        # # source_data = source_data,
        # # ),
		
    
# # # ## Park/OS Buffer Indicators

    # # Table(
        # # attribute = 'population_park_buffer = alldata.aggregate_all(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'alldata',
        # # source_data = source_data,
        # # ),    

# # # Park/OS Buffer Indicators

    # # Table(
        # # attribute = 'population_park_buffer = city.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'city',
        # # source_data = source_data,
        # # ),    
		

# # ## Park/OS Buffer Indicators
  
    # # Table(
        # # attribute = 'population_park_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'households_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'employment_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),  
  
    # # Table(
        # # attribute = 'activity_units_park_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1) * (parcel.is_inside_urban_growth_boundary == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

	   
# # ## Growth Amenities Buffer Indicators
  
    # # Table(
        # # attribute = 'population_growth_amenities = county.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

   # # Table(
       # # attribute = 'households_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       # # dataset_name = 'county',
       # # source_data = source_data,
       # # ),

    # # Table(
        # # attribute = 'employment_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),  
  
    # # Table(
        # # attribute = 'activity_units_growth_amenities = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

	   
# # ## Transit Buffer level by County Indicators

    # # Table(
        # # attribute = 'population_transit_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'households_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'employment_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),  
  
    # # Table(
        # # attribute = 'activity_units_transit_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

       
# # ## UGA Buffer Level by County Indicators  
   
    # # Table(
        # # attribute = 'population_uga_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.uga_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'households_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.uga_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),
    # # Table(
        # # attribute = 'employment_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.uga_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),  
  
    # # Table(
        # # attribute = 'activity_units_uga_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.uga_buffer_id == 1))',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),

  
# # ## Transit Buffer Level Indicators  
  
    # # #Table(
    # # #    attribute = 'population = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
    # # #    dataset_name = 'transit_buffer',
    # # #    source_data = source_data,
    # # #    ),
    # # #Table(
    # # #    attribute = 'households = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
    # # #    dataset_name = 'transit_buffer',
    # # #    source_data = source_data,
    # # #    ),
    # # #Table(
    # # #    attribute = 'employment = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
    # # #    dataset_name = 'transit_buffer',
    # # #    source_data = source_data,
    # # #    ),  
    # # #
    # # #Table(
    # # #    attribute = 'activity_units = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
    # # #    dataset_name = 'transit_buffer',
    # # #    source_data = source_data,
    # # #    ),

# # ## UGA Buffer Level Indicators  
  
    # # #Table(
    # # #    attribute = 'population = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
    # # #    dataset_name = 'uga_buffer',
    # # #    source_data = source_data,
    # # #    ),
    # # #Table(
    # # #    attribute = 'households = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
    # # #    dataset_name = 'uga_buffer',
    # # #    source_data = source_data,
    # # #    ),
    # # #Table(
    # # #    attribute = 'employment = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
    # # #    dataset_name = 'uga_buffer',
    # # #    source_data = source_data,
    # # #    ),  
    # # #
    # # #Table(
    # # #    attribute = 'activity_units = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
    # # #    dataset_name = 'uga_buffer',
    # # #    source_data = source_data,
    # # #    ),

# # ## Hex Level Indicators -- for mapping
  
    # # Table(
        # # attribute = 'population = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
    # # Table(
        # # attribute = 'households = hex.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),
    # # Table(
        # # attribute = 'employment = hex.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),  
    # # Table(
        # # attribute = 'activity_units = hex.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + hex.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),

    # # Table(
        # # attribute = 'impervious_area = hex.aggregate(clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/3920400',
        # # dataset_name = 'hex',
        # # source_data = source_data,
        # # ),


# # ## Census Tract Level Indicators  
  
   # # Table(
        # # attribute = 'population = census_tract.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        # # dataset_name = 'census_tract',
        # # source_data = source_data,
        # # ),
   # # Table(
        # # attribute = 'households = census_tract.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        # # dataset_name = 'census_tract',
        # # source_data = source_data,
        # # ),
   # # Table(
        # # attribute = 'employment = census_tract.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # # dataset_name = 'census_tract',
        # # source_data = source_data,
        # # ),

# # ## Subarea Level Indicators  


# ## TOD Level Indicators  
  

    # # Table(
        # # attribute = 'population = subarea.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        # # dataset_name = 'subarea',
        # # source_data = source_data,
        # # ),
    # # Table(
        # # attribute = 'households = subarea.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        # # dataset_name = 'subarea',
        # # source_data = source_data,
        # # ),
    # # Table(
        # # attribute = 'employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        # # dataset_name = 'subarea',
        # # source_data = source_data,
        # # ),


# ## TOD Level Indicators  


    Table(
        attribute = 'population = tod.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'tod',
        source_data = source_data,
        ),
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


# ## Impervious Area Indicators at County total and Regional category breakdown

 	# Table(
        # attribute = 'kin_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.county_id == 33))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),		
	# Table(
        # attribute = 'kit_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.county_id == 35))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),		
	# Table(
        # attribute = 'pie_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.county_id == 53))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),		
	# Table(
        # attribute = 'sno_employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.county_id == 61))',
        # dataset_name = 'tod',
        # source_data = source_data,
        # ),						
		
# # ## Impervious Area Indicators at County total and Regional category breakdown



    # # Table(
        # # attribute = 'impervious_area = county.aggregate(clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
        # # dataset_name = 'county',
        # # source_data = source_data,
        # # ),
      
    # # DatasetTable(
        # # source_data = source_data,
        # # dataset_name = 'alldata',
        # # name =  'Impervious_acreage_by_dvpt_category_by_year',
        # # attributes = [
            # # 'Existing_older = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 1996) * clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
            # # 'Built_newer = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 1995) * (numpy.logical_or((parcel.baseyear_built > 1995),(parcel.baseyear_built < 1850))) * clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
            # # 'Redeveloped_older = alldata.aggregate_all((parcel.baseyear_built < parcel.aggregate(building.year_built, function=maximum)) * (numpy.logical_and((parcel.baseyear_built > 0),(parcel.baseyear_built < 1996))) * clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
        # # ],
    # # ),

# # ## Land Efficiency indicator
    

    # #    DatasetTable(
    # #    source_data = source_data,
    # #    dataset_name = 'alldata',
    # #    name =  'Acreage by development category',
    # #    attributes = [
    # #        'developed = alldata.aggregate_all(numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft,intermediates=[parcel])/43560.0',
    # #        'undeveloped = alldata.aggregate_all(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft,intermediates=[parcel])/43560.0',
    # #    ],
    # #),

    # #    DatasetTable(
    # #    source_data = source_data,
    # #    dataset_name = 'alldata',
    # #    name =  'Acreage by development category',
    # #    attributes = [
    # #        'built_by_2017_urban = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 2018) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # #        'built_after_2017_urban = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 2017) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # #        'undeveloped_urban = alldata.aggregate_all(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # #        'built_by_2017_rural = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 2018) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # #        'built_after_2017_rural = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 2017) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # #        'undeveloped_rural = alldata.aggregate_all(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # #    ],
    # #),


        # DatasetTable(
        # source_data = source_data,
        # dataset_name = 'alldata',
        # name =  'Acreage_by_built_res_density',
        # attributes = [
            # 'Nonres_existing = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_redev = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_newdev = alldata.aggregate_all((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_existing = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_redev = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_newdev = alldata.aggregate_all((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_existing = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_redev = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_newdev = alldata.aggregate_all((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_existing = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_redev = alldata.aggregate_all((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_newdev = alldata.aggregate_all((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # ],
        # ),

        # DatasetTable(
        # source_data = source_data,
        # dataset_name = 'county',
        # name =  'Acreage_by_built_res_density',
        # attributes = [
            # 'Nonres_existing = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_redev = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Nonres_newdev = county.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_existing = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_redev = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Low_newdev = county.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_existing = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_redev = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'Medium_newdev = county.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_existing = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_redev = county.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # 'High_newdev = county.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # ],
        # ),
        
               # # DatasetTable(
        # # source_data = source_data,
        # # dataset_name = 'minority',
        # # name =  'Acreage_by_built_res_density',
        # # attributes = [
            # # 'Nonres_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Nonres_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Nonres_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_existing = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_redev = minority.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_newdev = minority.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # ],
        # # ),


        # # DatasetTable(
        # # source_data = source_data,
        # # dataset_name = 'poverty',
        # # name =  'Acreage_by_built_res_density',
        # # attributes = [
            # # 'Nonres_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Nonres_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Nonres_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units == 0) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Low_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 3630) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'Medium_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 3630) * (urbansim_parcel.parcel.parcel_sqft_per_unit > 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_existing = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) < 2015) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_redev = poverty.aggregate((parcel.baseyear_built > 0) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # 'High_newdev = poverty.aggregate((parcel.baseyear_built < 1850) * (parcel.aggregate(building.year_built, function=maximum) > 2014) * (psrc_parcel.parcel.residential_units > 0) * (urbansim_parcel.parcel.parcel_sqft_per_unit <= 871) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft, intermediates=[parcel]) / 43560.0',
            # # ],
        # # ),
    # # #    DatasetTable(
    # # #    source_data = source_data,
    # # #    dataset_name = 'alldata',
    # # #    name =  'Acreage by development category',
    # # #    attributes = [
    # # #        'built_by_2017_urban = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 2018) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # # #        'built_after_2017_urban = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 2017) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # # #        'undeveloped_urban = alldata.aggregate_all(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==1),intermediates=[parcel])/43560.0',
    # # #        'built_by_2017_rural = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 2018) * numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # # #        'built_after_2017_rural = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 2017) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # # #        'undeveloped_rural = alldata.aggregate_all(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[parcel])/43560.0',
    # # #    ],
    # # #),

# # # # Mike's rural/city acreage indicators added 5.20.2019
    # # Table(
        # # attribute = 'impervious_area = fips_rgs_proposed.aggregate(clip_to_zero(((parcel.land_use_type_id==20) * 0.98 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.91 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.75 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 3)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[city])/43560.0',
        # # dataset_name = 'fips_rgs_proposed',
        # # source_data = source_data,
        # # ),

    # # Table( 
        # # attribute ='undeveloped_rural = fips_rgs_proposed.aggregate(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft * (parcel.is_inside_urban_growth_boundary==0),intermediates=[city])/43560.0',
        # # dataset_name = 'fips_rgs_proposed',
        # # source_data = source_data,
        # # ), 

    # # Table( 
        # # attribute ='undeveloped = fips_rgs_proposed.aggregate(numpy.logical_and((psrc_parcel.parcel.residential_units == 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary)==0)) * parcel.parcel_sqft,intermediates=[city])/43560.0',
        # # dataset_name = 'fips_rgs_proposed',
        # # source_data = source_data,
        # # ),  
	
	
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
                 'households = urbansim_parcel.parcel.number_of_households',
                 'urbansim_parcel.parcel.population',
                 'urbansim_parcel.parcel.residential_units',
                 'urbansim_parcel.parcel.employment',
                 'non_home_based_employment = parcel.aggregate(psrc_parcel.building.number_of_non_home_based_jobs)',
                 'non_residential_sqft = parcel.aggregate(building.non_residential_sqft)',
                 'building_sqft = parcel.aggregate(urbansim_parcel.building.building_sqft)',
                 'psrc_parcel.parcel.job_capacity',
                 'parcel.plan_type_id'
                 'developed_sqft = parcel.parcel_sqft * (numpy.logical_or((psrc_parcel.parcel.residential_units > 0),(parcel.aggregate(psrc_parcel.building.job_capacity_computed_if_necessary) > 0)))'
             ],
             ),
         ]
     return indicators

# def get_end_year_indicators(cache_directory, run_description, years = [2018,2050], base_year=2018):
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
                 # 'parcel.plan_type_id'
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