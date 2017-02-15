
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable


def jobs_by_sector(geo, package="psrc_parcel"):
    return [
               "construction_resources = %s.%s.number_of_jobs_of_sector_1 + %s.%s.number_of_jobs_of_sector_2" % (2*(package, geo)),
               "manuf_WTU = %s.%s.number_of_jobs_of_sector_3 + %s.%s.number_of_jobs_of_sector_4 + %s.%s.number_of_jobs_of_sector_5 + %s.%s.number_of_jobs_of_sector_6 + %s.%s.number_of_jobs_of_sector_8 + %s.%s.number_of_jobs_of_sector_9" % (6*(package, geo)),
               "retail_food_services = %s.%s.number_of_jobs_of_sector_7 + %s.%s.number_of_jobs_of_sector_14" % (2*(package, geo)),
               "FIRE_services = %s.%s.number_of_jobs_of_sector_12 + %s.%s.number_of_jobs_of_sector_10 + %s.%s.number_of_jobs_of_sector_11 + %s.%s.number_of_jobs_of_sector_13 + %s.%s.number_of_jobs_of_sector_15 + %s.%s.number_of_jobs_of_sector_16 + %s.%s.number_of_jobs_of_sector_17" % (7*(package, geo)),
               "government = %s.%s.number_of_jobs_of_sector_18" % (package, geo),
               "edu = %s.%s.number_of_jobs_of_sector_19" % (package, geo)
           ]

def get_indicators(cache_directory, run_description, years = [2014,2015,2020,2025,2030,2035,2040], base_year=2014):
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
    
    # FAZ indicators 
    # =====================
    
       Table(
           attribute = 'households=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'population=faz.aggregate(urbansim_parcel.building.population, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'employment=faz.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'nonres_sqft=faz.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),	   
       Table(
           attribute = 'residential_units=faz.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'building_sqft=faz.aggregate(urbansim_parcel.parcel.building_sqft, intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),	       
               
    # TAZ indicators 
       Table(
           attribute = 'residential_units=zone.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'households=zone.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'population=zone.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'employment=zone.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'nonres_sqft=zone.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'building_sqft=zone.aggregate(urbansim_parcel.parcel.building_sqft)',
           dataset_name = 'zone',
           source_data = source_data,
           ),       
       
       DatasetTable(
                         source_data = source_data,
                         dataset_name = 'zone',
                         name = 'employment_by_aggr_sector',
                         attributes = jobs_by_sector("zone", "urbansim_parcel"),
                         output_type = 'tab'
                             ),
    
    # ## City indicators
    # ==================
    
       Table(
           attribute = 'households=city.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),
       Table(
           attribute = 'population=city.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
            ),
       Table(
           attribute = 'employment=city.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),
       Table(
           attribute = 'residential_units=city.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),
       Table(
           attribute = 'nonres_sqft=city.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),
       Table(
           attribute = 'building_sqft=city.aggregate(urbansim_parcel.parcel.building_sqft)',
           dataset_name = 'city',
           source_data = source_data,
           ),       
       
       #Table(
       #    attribute = 'acres=city.aggregate(parcel.parcel_sqft/43560.)',
       #    dataset_name = 'city',
       #    source_data = source_data,
       #    ),
       DatasetTable(
                  source_data = source_data,
                  dataset_name = 'city',
                  name = 'employment_by_aggr_sector',
                  attributes = jobs_by_sector("city"),
                  output_type = 'tab'
                      ),         

    # ## Tract-City indicators
    # ==================
    
       Table(
           attribute = 'households=tractcity.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),
       Table(
           attribute = 'population=tractcity.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
            ),
       Table(
           attribute = 'employment=tractcity.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),
       Table(
           attribute = 'residential_units=tractcity.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),
       Table(
           attribute = 'nonres_sqft=tractcity.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),
       Table(
           attribute = 'building_sqft=tractcity.aggregate(urbansim_parcel.parcel.building_sqft)',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),       
               
    # ## Growth Centers Indicators
    # ============================
      
       Table(
           attribute = 'residential_units=growth_center.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
           dataset_name = 'growth_center',
           source_data = source_data,
           ),
       Table(
           attribute = 'households=growth_center.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'growth_center',
           source_data = source_data,
           ),
      Table(
          attribute = 'population=growth_center.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
          dataset_name = 'growth_center',
          source_data = source_data,
           ),
       Table(
           attribute = 'employment=growth_center.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'growth_center',
           source_data = source_data,
           ),
       Table(
           attribute = 'nonres_sqft=growth_center.aggregate(urbansim_parcel.building.non_residential_sqft, intermediates=[parcel])',
           dataset_name = 'growth_center',
           source_data = source_data,
           ),
       Table(
           attribute = 'building_sqft=growth_center.aggregate(urbansim_parcel.parcel.building_sqft)',
           dataset_name = 'growth_center',
           source_data = source_data,
           ),          
       
       #Table(
       #    attribute = 'acres=growth_center.aggregate(parcel.parcel_sqft/43560.)',
       #    dataset_name = 'growth_center',
       #    source_data = source_data,
       #    ),       
    
    # ## Large Area Indicators
    # ============================
    
        DatasetTable(
           source_data = source_data,
           dataset_name = 'large_area',
           name = 'employment_by_aggr_sector',
           attributes = ['large_area.county_id'] + jobs_by_sector("large_area"),
           output_type = 'tab'
               ),    

## #### ============  Miscellaneous tables
## #### ============  Checked pretty regularly 
## #### ============  Run for every year in the simulation


## ##------Liming's Unplaced Households and Jobs in the Region-----------

    Table(
           attribute = 'num_unplaced_hhs=alldata.aggregate_all(household.building_id<=0)',
           dataset_name = 'alldata',
           source_data = source_data,
         ),
    Table(
           attribute = 'num_unplaced_jobs=alldata.aggregate_all(job.building_id<=0)',
           dataset_name = 'alldata',
           source_data = source_data,
         ),
    
    #  Regional Total Tables	 
             
             
      Table(
          attribute = 'residential_units=alldata.aggregate_all(urbansim_parcel.building.residential_units)',
          dataset_name = 'alldata',
          source_data = source_data,
          ),
      Table(
          attribute = 'non_residential_sqft=alldata.aggregate_all(urbansim_parcel.building.non_residential_sqft)',
          dataset_name = 'alldata',
          source_data = source_data,
          ),
      Table(
          attribute = 'households=alldata.aggregate_all(urbansim_parcel.building.number_of_households)',
          dataset_name = 'alldata',
          source_data = source_data,
          ),
      Table(
          attribute = 'employment=alldata.aggregate_all(urbansim_parcel.building.number_of_jobs)',
          dataset_name = 'alldata',
          source_data = source_data,
          ),
      Table(
          attribute = 'population=alldata.aggregate_all(urbansim_parcel.building.population)',
          dataset_name = 'alldata',
          source_data = source_data,
          ),
    
    ]
    return indicators

import os
from opus_core.indicator_framework.core.indicator_factory import IndicatorFactory

if __name__ == '__main__':
    indicators = get_indicators(os.path.join(os.environ['QC_BASE_DIRECTORY'], os.environ['QC_RUN1']), 
                                os.getenv('QC_RUN1_DESCR', ''))
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False)
