
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
#def get_indicators(cache_directory, run_description, years = [2050], base_year=2014):
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
    
## Park/OS Buffer Indicators
  
    Table(
        attribute = 'population_park_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.park_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

    Table(
        attribute = 'households_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.park_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

    Table(
        attribute = 'employment_park_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.park_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_park_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.park_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

	   
## Growth Amenities Buffer Indicators
  
    Table(
        attribute = 'population_growth_amenities = county.aggregate(urbansim_parcel.parcel.population * (parcel.growth_amenities_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

   Table(
       attribute = 'households_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.growth_amenities_id == 1))',
       dataset_name = 'county',
       source_data = source_data,
       ),

    Table(
        attribute = 'employment_growth_amenities = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.growth_amenities_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_growth_amenities = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.growth_amenities_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

	   
## Transit Buffer level by County Indicators

    Table(
        attribute = 'population_transit_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.transit_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

    Table(
        attribute = 'households_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.transit_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

    Table(
        attribute = 'employment_transit_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.transit_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_transit_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.transit_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

       
## UGA Buffer Level by County Indicators  
   
    Table(
        attribute = 'population_uga_buffer = county.aggregate(urbansim_parcel.parcel.population * (parcel.uga_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

    Table(
        attribute = 'households_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_households * (parcel.uga_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment_uga_buffer = county.aggregate(urbansim_parcel.parcel.number_of_jobs * (parcel.uga_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units_uga_buffer = county.aggregate((urbansim_parcel.parcel.population + urbansim_parcel.parcel.number_of_jobs) * (parcel.uga_buffer_id == 1))',
        dataset_name = 'county',
        source_data = source_data,
        ),

  
## Transit Buffer Level Indicators  
  
    Table(
        attribute = 'population = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'transit_buffer',
        source_data = source_data,
        ),
    Table(
        attribute = 'households = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        dataset_name = 'transit_buffer',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment = transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'transit_buffer',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units = transit_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + transit_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'transit_buffer',
        source_data = source_data,
        ),

## UGA Buffer Level Indicators  
  
    Table(
        attribute = 'population = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'uga_buffer',
        source_data = source_data,
        ),
    Table(
        attribute = 'households = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        dataset_name = 'uga_buffer',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment = uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'uga_buffer',
        source_data = source_data,
        ),  
  
    Table(
        attribute = 'activity_units = uga_buffer.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + uga_buffer.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'uga_buffer',
        source_data = source_data,
        ),

## Grid Level Indicators  
  
    Table(
        attribute = 'population = grid.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'grid',
        source_data = source_data,
        ),
    Table(
        attribute = 'households = grid.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        dataset_name = 'grid',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment = grid.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'grid',
        source_data = source_data,
        ),  
    Table(
        attribute = 'activity_units = grid.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel]) + grid.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'grid',
        source_data = source_data,
        ),
  
## Census Tract Level Indicators  
  
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

## Subarea Level Indicators  

    Table(
        attribute = 'population = subarea.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'households = subarea.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        dataset_name = 'subarea',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment = subarea.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'subarea',
        source_data = source_data,
        ),


## TOD Level Indicators  
  
    Table(
        attribute = 'population = tod.aggregate(urbansim_parcel.parcel.population, intermediates=[parcel])',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    Table(
        attribute = 'households = tod.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[parcel])',
        dataset_name = 'tod',
        source_data = source_data,
        ),
    Table(
        attribute = 'employment = tod.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[parcel])',
        dataset_name = 'tod',
        source_data = source_data,
        ),

## Impervious Area Indicators at County total and Regional category breakdown

     Table(
        attribute = 'impervious_area = county.aggregate(((parcel.land_use_type_id==20) * 0.95 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.8 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.65 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 1.1)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln_shifted(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft))),intermediates=[parcel])/43560.0',
        dataset_name = 'county',
        source_data = source_data,
        ),

    DatasetTable(
        source_data = source_data,
        dataset_name = 'alldata',
        name =  'Impervious_acreage_by_dvpt_category_by_year',
        attributes = [
            'Existing_older = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) < 1996) * (((parcel.land_use_type_id==20) * 0.95 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.8 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.65 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 1.1)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln_shifted(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
            'Built_newer = alldata.aggregate_all((parcel.aggregate(building.year_built, function=maximum) > 1995) * (numpy.logical_or((parcel.baseyear_built > 1995),(parcel.baseyear_built==-1))) * (((parcel.land_use_type_id==20) * 0.95 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.8 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.65 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 1.1)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln_shifted(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
            'Redeveloped_older = alldata.aggregate_all((parcel.baseyear_built < parcel.aggregate(building.year_built, function=maximum)) * (numpy.logical_and((parcel.baseyear_built<>-1),(parcel.baseyear_built < 1996))) * (((parcel.land_use_type_id==20) * 0.95 * parcel.parcel_sqft) + ((parcel.land_use_type_id==22) * 0.8 * parcel.parcel_sqft) + (numpy.in1d(parcel.land_use_type_id,[1,2,4,5,6,7,8,9,11,12,16,17,19,21,23]) * numpy.minimum(.65 * parcel.parcel_sqft, urbansim_parcel.parcel.building_sqft * 1.1)) + (parcel.aggregate(building.building_id >0,function=maximum) * ((numpy.in1d(parcel.land_use_type_id,[3,18,25,28,30]) * .85 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[10,14,15]) * .75 * parcel.parcel_sqft) +(numpy.in1d(parcel.land_use_type_id,[13,24]) * numpy.minimum((0.19 * ln_shifted(parcel.aggregate(building.residential_units) * 43560.0 / parcel.parcel_sqft) + 0.153),.8) * parcel.parcel_sqft)))),intermediates=[parcel])/43560.0',
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
    
    ## runs buildings indicator only for the simulation end year
    #IndicatorFactory().create_indicators(
    #    indicators = get_end_year_indicators(ind_cache, os.getenv('QC_RUN1_DESCR', ''),years = [2050]),
    #    display_error_box = False,
    #    show_results = False)

    write_info(ind_cache, os.getenv('QC_RUN1_DESCR', ''), os.getenv('QC_RUN1_RESTR', ''))