# This script should be used to create annual indicators used for intermediate years in R-reports.
# Note that it takes some time to run it.

from opus_core.configurations.dataset_pool_configuration import DatasetPoolConfiguration
from opus_core.indicator_framework.core.source_data import SourceData

from opus_core.indicator_framework.image_types.matplotlib_map import Map
from opus_core.indicator_framework.image_types.table import Table
from opus_core.indicator_framework.image_types.dataset_table import DatasetTable

def jobs_by_luv_sector(geo, package="psrc_parcel"):
    return [
         "Con_Res = %s.%s.number_of_jobs_of_sector_1 + %s.%s.number_of_jobs_of_sector_2" % (package, geo, package, geo),
                "Manuf_WTU = %s.%s.number_of_jobs_of_sector_3 + %s.%s.number_of_jobs_of_sector_4" % (package, geo, package, geo),
                "Retail = %s.%s.number_of_jobs_of_sector_5 + %s.%s.number_of_jobs_of_sector_10" % (package, geo, package, geo),
                "FIRES = %s.%s.number_of_jobs_of_sector_7 + %s.%s.number_of_jobs_of_sector_9 + %s.%s.number_of_jobs_of_sector_11" % (package, geo, package, geo, package, geo),
                "Gov = %s.%s.number_of_jobs_of_sector_12" % (package, geo),
                "Edu = %s.%s.number_of_jobs_of_sector_13 + %s.%s.number_of_jobs_of_sector_8" % (package, geo, package, geo)
     ]


def get_indicators(cache_directory, run_description, years = range(2023,2051), base_year=2023):
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
    
    indicators=[
    
    # # FAZ indicators 
    # # =====================
    
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
        Table(
            attribute = 'residential_unitsAn=faz.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
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
        Table(
            attribute = 'residential_unitsAn=zone.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'zone',
            source_data = source_data,
            ),		   
		   
    # ## Subregs indicators
    ##==================
    
       Table(
           attribute = 'householdsAn=subreg.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'subreg',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=subreg.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'subreg',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=subreg.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'subreg',
           source_data = source_data,
           ),
        Table(
            attribute = 'residential_unitsAn=subreg.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'subreg',
            source_data = source_data,
            ),		   
		   
		   
    # # ## City indicators
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
        Table(
            attribute = 'residential_unitsAn=city.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'city',
            source_data = source_data,
            ),	
		   
    # # ## target indicators
    # ==================
    
       Table(
           attribute = 'householdsAn=target.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'target',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=target.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'target',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=target.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'target',
           source_data = source_data,
           ),
        Table(
            attribute = 'residential_unitsAn=target.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'target',
            source_data = source_data,
            ),	
		   
    # # ## controls indicators
    # ==================
    
       Table(
           attribute = 'householdsAn=control.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'control',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=control.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'control',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=control.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'control',
           source_data = source_data,
           ),	
        Table(
            attribute = 'residential_unitsAn=control.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'control',
            source_data = source_data,
            ),	
		   
    # # ## control_hct indicators
    # ==================
    
       Table(
           attribute = 'householdsAn=control_hct.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           dataset_name = 'control_hct',
           source_data = source_data,
           ),
       Table(
           attribute = 'populationAn=control_hct.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           dataset_name = 'control_hct',
           source_data = source_data,
            ),
       Table(
           attribute = 'employmentAn=control_hct.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           dataset_name = 'control_hct',
           source_data = source_data,
           ),	
        Table(
            attribute = 'residential_unitsAn=control_hct.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
            dataset_name = 'control_hct',
            source_data = source_data,
            ),	
		   
    # # ## Tract-City indicators
    # # ==================
    
       # Table(
           # attribute = 'householdsAn=tractcity.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
           # dataset_name = 'tractcity',
           # source_data = source_data,
           # ),
       # Table(
           # attribute = 'populationAn=tractcity.aggregate(urbansim_parcel.building.population, intermediates=[parcel])',
           # dataset_name = 'tractcity',
           # source_data = source_data,
            # ),
       # Table(
           # attribute = 'employmentAn=tractcity.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel])',
           # dataset_name = 'tractcity',
           # source_data = source_data,
           # ),
       
       # # County Regional Geography indicators 
       
       # Table(
           # attribute = 'populationAn = fips_rgs.aggregate(urbansim_parcel.parcel.population, intermediates=[city])',
           # dataset_name = 'fips_rgs',
           # source_data = source_data,
            # ),
       # Table(
            # attribute = 'householdsAn = fips_rgs.aggregate(urbansim_parcel.parcel.number_of_households, intermediates=[city])',
            # dataset_name = 'fips_rgs',
            # source_data = source_data,
            # ),
        # Table(
            # attribute = 'employmentAn = fips_rgs.aggregate(urbansim_parcel.parcel.number_of_jobs, intermediates=[city])',
            # dataset_name = 'fips_rgs',
            # source_data = source_data,
            # ),
            
        DatasetTable(
             source_data = source_data,
            dataset_name = 'faz',
            name =  'DU_and_HH_by_bld_type_by_faz_by_year',
            attributes = [
                'DU_SF_19=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==19), intermediates=[parcel])',
                'DU_MF_12=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==12), intermediates=[parcel])',
                'DU_CO_4=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==4), intermediates=[parcel])',
                'DU_MH_11=faz.aggregate(urbansim_parcel.building.residential_units * (building.building_type_id==11), intermediates=[parcel])',
                'DU_Total=faz.aggregate(urbansim_parcel.building.residential_units, intermediates=[parcel])',
                'HH_SF_19=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==19), intermediates=[parcel])',
                'HH_MF_12=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==12), intermediates=[parcel])',
                'HH_CO_4=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==4), intermediates=[parcel])',
                'HH_MH_11=faz.aggregate(urbansim_parcel.building.number_of_households * (building.building_type_id==11), intermediates=[parcel])',
                'HH_Total=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel])',
                ],
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'county',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("county"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'subreg',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("subreg"),
            output_type = 'tab'
            ),	        
    
        DatasetTable(
            source_data = source_data,
            dataset_name = 'target',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("target"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'control',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("control"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'control_hct',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("control_hct"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'faz',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("faz"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'zone',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("zone", "urbansim_parcel"),
            output_type = 'tab'
            ),
        
        DatasetTable(
            source_data = source_data,
            dataset_name = 'city',
            name = 'employment_by_aggr_sector',
            attributes = jobs_by_luv_sector("city"),
            output_type = 'tab'
            ),
        
        DatasetTable(
               source_data = source_data,
               dataset_name = 'large_area',
               name = 'employment_by_aggr_sector',
               attributes = ['large_area.county_id'] + jobs_by_luv_sector("large_area"),
               output_type = 'tab'
               ),           
    ]
    return indicators

import os
from opus_core.indicator_framework.core.indicator_factory import IndicatorFactory

if __name__ == '__main__':
    base_year = int(os.getenv('RUN1_BASE_YEAR', 2023))
    indicators = get_indicators(os.path.join(os.environ['QC_BASE_DIRECTORY'], os.environ['QC_RUN1']), 
                                os.getenv('QC_RUN1_DESCR', ''),
                                years = range(base_year, int(os.getenv('RUN1_END_YEAR', 2050))+1),
                                base_year = base_year)
    IndicatorFactory().create_indicators(
        indicators = indicators,
        display_error_box = False, 
        show_results = False,
        file_name_for_indicator_results = 'indicator_results_annual.html')
