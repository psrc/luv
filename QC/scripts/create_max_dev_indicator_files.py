
from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable

# When run for the first year after base year, the indicators based on project proposals
# capture the available development capacity at the base year.

def get_indicators(cache_directory, run_description, years = [2024], base_year=2023):
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
    
       Table(
           attribute = 'max_dev_residential_capacity=city.aggregate(psrc_parcel.parcel.max_developable_residential_capacity)',
           dataset_name = 'city',
           source_data = source_data,
           ), 
       Table(
           attribute = 'max_dev_nonresidential_capacity=city.aggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity)',
           dataset_name = 'city',
           source_data = source_data,
           ), 
       Table(
           attribute = 'max_dev_capacity=city.aggregate(psrc_parcel.parcel.max_developable_capacity)',
           dataset_name = 'city',
           source_data = source_data,
           ),
       #Table(
           #attribute = 'max_dev_residential_capacity=city.aggregate(parcel.aggregate(development_project_proposal.aggregate(urbansim_parcel.development_project_proposal_component.residential_units), function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_nonresidential_capacity=city.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft_non_residential, function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_capacity=city.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft, function=maximum))',
           #dataset_name = 'city',
           #source_data = source_data,
           #),    
       
       # ## FAZ indicators
       # ==================
       
       Table(
           attribute = 'max_dev_residential_capacity=faz.aggregate(psrc_parcel.parcel.max_developable_residential_capacity, intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'max_dev_nonresidential_capacity=faz.aggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity, intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),
       Table(
           attribute = 'max_dev_capacity=faz.aggregate(psrc_parcel.parcel.max_developable_capacity, intermediates=[zone])',
           dataset_name = 'faz',
           source_data = source_data,
           ),              
    
       #Table(
           #attribute = 'max_dev_residential_capacity=faz.aggregate(parcel.aggregate(development_project_proposal.aggregate(urbansim_parcel.development_project_proposal_component.residential_units), function=maximum), intermediates=[zone])',
           #dataset_name = 'faz',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_nonresidential_capacity=faz.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft_non_residential, function=maximum), intermediates=[zone])',
           #dataset_name = 'faz',
           #source_data = source_data,
           #), 
       #Table(
           #attribute = 'max_dev_capacity=faz.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft, function=maximum), intermediates=[zone])',
           #dataset_name = 'faz',
           #source_data = source_data,
           #),   
           
       # ## TAZ indicators                                                                                                                                     
       # ==================                                                                                                                                    

       #Table(
           #attribute = 'max_dev_residential_capacity=zone.aggregate(parcel.aggregate(development_project_proposal.aggregate(urbansim_parcel.development_project_proposal_component.residential_units), function=maximum))',
           #dataset_name = 'zone',
           #source_data = source_data,
           #),
       #Table(
           #attribute = 'max_dev_nonresidential_capacity=zone.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft_non_residential, function=maximum))',
           #dataset_name = 'zone',
           #source_data = source_data,
           #),
       #Table(
           #attribute = 'max_dev_capacity=zone.aggregate(parcel.aggregate(urbansim_parcel.development_project_proposal.building_sqft, function=maximum))',
           #dataset_name = 'zone',
           #source_data = source_data,
           #),
       Table(
           attribute = 'max_dev_residential_capacity=zone.aggregate(psrc_parcel.parcel.max_developable_residential_capacity)',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'max_dev_nonresidential_capacity=zone.aggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity)',
           dataset_name = 'zone',
           source_data = source_data,
           ),
       Table(
           attribute = 'max_dev_capacity=zone.aggregate(psrc_parcel.parcel.max_developable_capacity)',
           dataset_name = 'zone',
           source_data = source_data,
           ), 
       
       # ## Growth centers indicators
       # ============================
   
       Table(
              attribute = 'max_dev_residential_capacity=growth_center.aggregate(psrc_parcel.parcel.max_developable_residential_capacity)',
              dataset_name = 'growth_center',
              source_data = source_data,
              ), 
       Table(
              attribute = 'max_dev_nonresidential_capacity=growth_center.aggregate(psrc_parcel.parcel.max_developable_nonresidential_capacity)',
              dataset_name = 'growth_center',
              source_data = source_data,
              ), 
       Table(
              attribute = 'max_dev_capacity=growth_center.aggregate(psrc_parcel.parcel.max_developable_capacity)',
              dataset_name = 'growth_center',
              source_data = source_data,
              ),       
    
    ]
    return indicators

import os
from opus_core.indicator_framework.core.indicator_factory import IndicatorFactory

if __name__ == '__main__':
    base_year = int(os.getenv('RUN1_BASE_YEAR', 2023))
    indicators = get_indicators(os.path.join(os.environ['QC_BASE_DIRECTORY'], os.environ['QC_RUN1']), 
                                os.getenv('QC_RUN1_DESCR', ''),
                                years = [base_year+1], 
                                base_year = base_year)
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False,
		file_name_for_indicator_results = 'indicator_results_maxdev.html')
