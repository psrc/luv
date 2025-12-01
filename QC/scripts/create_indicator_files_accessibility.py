from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable

def get_indicators(cache_directory, run_description, years = [2023, 2050], base_year=2023,
                   output_subdir = "indicators"):
    
    years = [i for i in years if i >= base_year]
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
    storage_loc = os.path.join(cache_directory, output_subdir)
    
    indicators=[
        Table(
            attribute = 'lngcdacbd = ln(psrc.zone.generalized_cost_hbw_am_drive_alone_to_cbd)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),
        Table(
            attribute = 'lngcdaemp = ln(psrc.zone.generalized_cost_weighted_access_to_employment_hbw_am_drive_alone)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),
        Table(
            attribute = 'lngcdapop = ln(psrc.zone.generalized_cost_weighted_access_to_population_hbw_am_drive_alone)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),        
        Table(
            attribute = 'hbwavgtmda = psrc.zone.trip_weighted_average_time_hbw_from_home_am_drive_alone',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),        
        Table(
            attribute = 'lnemp30tw = ln(urbansim_parcel.zone.employment_within_30_minutes_travel_time_hbw_am_transit_walk)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),
        Table(
            attribute = 'lnemp30da = ln(urbansim_parcel.zone.employment_within_30_minutes_travel_time_hbw_am_drive_alone)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),         
        Table(
            attribute = 'lnemp20tw = ln(urbansim_parcel.zone.employment_within_20_minutes_travel_time_hbw_am_transit_walk)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),         
        Table(
            attribute = 'lnemp20da = ln(urbansim_parcel.zone.employment_within_20_minutes_travel_time_hbw_am_drive_alone)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),          
        Table(
            attribute = 'lnemp10wa = ln(urbansim_parcel.zone.employment_within_10_minutes_travel_time_hbw_am_walk)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),
        Table(
            attribute = 'lnemp10da = ln(urbansim_parcel.zone.employment_within_10_minutes_travel_time_hbw_am_drive_alone)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),
        Table(
            attribute = 'lnempden = ln(urbansim_parcel.zone.number_of_jobs_per_acre)',
            dataset_name = 'zone',
            source_data = source_data,
            storage_location = storage_loc
            ),             
    
    ]
    return indicators


import os
from opus_core.indicator_framework.core.indicator_factory import IndicatorFactory

if __name__ == '__main__':
    ind_cache = os.path.join(os.environ['QC_BASE_DIRECTORY'], os.environ['QC_RUN1'])
    indicators = get_indicators(ind_cache, os.getenv('QC_RUN1_DESCR', ''), 
                                base_year = int(os.getenv('RUN1_BASE_YEAR', 2023)),
                                output_subdir ="indicators_accessibility"
                                )
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False,
        file_name_for_indicator_results = 'indicator_results_accessibility.html'        
    )

