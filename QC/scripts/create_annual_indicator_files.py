# This script should be used to create annual indicators used for intermediate years in R-reports.
# Note that it takes some time to run it.

from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable



def get_indicators(cache_directory, run_description, years = range(2014,2041), base_year=2014):
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
           attribute = 'householdsAn=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=faz.aggregate(urbansim_parcel.building.population, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'employmentAn=faz.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel,zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
               
    # TAZ indicators 
       Table(
           attribute = 'householdsAn=zone.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=zone.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'employmentAn=zone.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'zone',
           source_data = source_data,
           ),
    
    # ## City indicators
    # ==================
    
       Table(
           attribute = 'householdsAn=city.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=city.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=city.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'city',
           source_data = source_data,
           ),

    # ## Tract-City indicators
    # ==================
    
       Table(
           attribute = 'householdsAn=tractcity.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=tractcity.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'tractcity',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=tractcity.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'tractcity',
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
