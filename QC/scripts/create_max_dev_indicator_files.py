
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable



def get_indicators(cache_directory, run_description, years = [2015], base_year=2014):
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
    
    # ## City indicators
    # ==================
    
       #Table(
           #attribute = 'max_all_dev_residential_capacity=city.aggregate(psrc_parcel.parcel.max_developable_residential_capacity)',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_all_dev_nonresidential_capacity=city.aggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity)',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_all_dev_capacity=city.aggregate(psrc_parcel.parcel.max_developable_capacity)',
           #dataset_name = 'city',
           #source_data = source_data,
           #),
       #Table(
           #attribute = 'max_dev_residential_capacity=city.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_residential_capacity), function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_nonresidential_capacity=city.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity), function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_capacity=city.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_capacity), function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #),  
       
       # ## FAZ indicators
       # ==================
    
       Table(
           attribute = 'max_dev_residential_capacity=faz.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_residential_capacity), function=maximum), intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ), 
       Table(
           attribute = 'max_dev_nonresidential_capacity=faz.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity), function=maximum), intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ), 
       Table(
           attribute = 'max_dev_capacity=faz.aggregate(parcel.aggregate(development_project_proposal.disaggregate(psrc_parcel.parcel.max_developable_capacity), function=maximum), intermediates=[zone])',
           dataset_name = 'faz',
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
