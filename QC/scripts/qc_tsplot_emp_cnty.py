

def get_base_dir():
    with open(os.getcwd()[:-7]+'inputs.txt', 'r') as f:
        line_data = f.readlines()
        
    for l in line_data:
        if l[0] =="#":
            pass
        
        if str(l[:l.find("=")]) == "QC_BASE_DIRECTORY":
            
            input_dir = str(l[l.find("=")+1:]).strip()
            input_dir_list = list(input_dir)
            del input_dir_list[input_dir.find("$")]
            base_dir = "".join(input_dir_list)
            
            

    return base_dir
    
    
def remove_duplicates(values):
    output = []
    seen = set()
    for value in values:
        # If value has not been encountered yet,
        # ... add it to both list and set.
        if value not in seen:
            output.append(value)
            seen.add(value)
    return output
    
def get_runid():
    #get the run_id from the folder names with try & except
    run_id = []
    for r in runs_folder:
        run_num = r
        for y in data_year:
            year = y
            try:
               
                df_name = pd.read_table(os.path.join(base_dir, run_num, 'indicators', 'large_area__dataset_table__employment_by_aggr_sector__'+year+".tab"))
                
            except:
                print "Alert: Aggregated Large Area Employment file for year "+ y + " is not available for the run  "+ str(run_num) 
                
            else:
                dot_loc = r.find(".")
                run_id.append(r[0:dot_loc])
                
    run_id = remove_duplicates(run_id)
    return run_id
    

def get_input_luv():
    with open(wrkdir +'inputs.txt', 'r') as f:
        line_data = f.readlines()
    run_grab = []
    for l in line_data:
        if l[0] =="#":
            pass
        if str(l[:6]) == "QC_RUN":
            
            run_grab.append(l[8:].strip())
            
    return run_grab
    
def get_run_name():
    with open(wrkdir +'inputs.txt', 'r') as f:
        line_data = f.readlines()

    for l in line_data:
        if l[0] =="#":
            pass
        if str(l[:l.find("=")]) == "QC_NAME":            
            run_name = str(l[l.find("=")+1:]).strip()
            
    return run_name
    
    
def creat_pklfiles():
    #Create a temp directory for the pkl files:
    
    if not os.path.exists(tmp_dir):
        os.makedirs(tmp_dir)
    
    # Create .pkl tables for all indicator files with try & except
    
    init_plk_list =[]
    geo = "large_area"
    if special:
        geo = "zone"
        
    for r in runs_folder:
        run_num = r
        for y in data_year:
            year = y
            try:
               
                df_name = pd.read_table(os.path.join(base_dir, run_num, 'indicators', '%s__dataset_table__employment_by_aggr_sector__%s.tab' % (geo, year)))
                
            except:
                print "Alert: Aggregated Employment file for "+geo+ " and year "+ y + " is not available for the run  "+ str(run_num) 
                
            else:
                year = y
                data_frame_name = "df_"+run_num+"_"+year+".pkl"
                init_plk_list.append(data_frame_name)
                df_name.reset_index(drop=True)
                df_name.to_pickle(os.path.join(tmp_dir, data_frame_name))
    
    return init_plk_list

def get_master_df():
    #read all the .pkl files in to one data frame
    df_main = pd.DataFrame()
    for df in init_plk_list:
        df_work = pd.read_pickle(os.path.join(tmp_dir, df))
        if special:
            df_work = pd.merge(df_work, place_table, on="zone_id")
            # aggregate from zones to special places 
            df_work = df_work.groupby(by=['place_id'], axis=0).sum().reset_index().astype('int').drop(['zone_id'], axis=1)
        else:
            df_work = df_work.groupby(by=['county_id'], axis=0).sum().reset_index().astype('int').drop(['large_area_id'], axis=1)
        df_work['year'] = df[-8:-4]
        dot_loc = df.find(".")
        run = df[3:dot_loc]
        col = df_work.columns.values.tolist()
        col_list=[]
        for i in col:
            col_name = i+"."+str(run)
            col_list.append(col_name)
        df_work.columns = col_list
        df_main = df_main.append(df_work)
        df_main = df_main.reset_index(drop = True)   
    return df_main
    
def get_regfile_run():
    #get regional file for each run
    df_main_region_list =[]
    id_name = 'county_id'
    if special:
        id_name = 'place_id'
    for i in run_id:
        if not df_main.columns.isin(["year."+i]).any():
            continue # data not available for this run
        data_frame_name = "df_region_"+i+".pkl"
        df_main_region_list.append(data_frame_name)
        df_main_test = df_main[df_main["year."+i].notnull()]
        df_main_test = df_main_test.dropna(axis = 1, how = 'any')   
        df_main_test = df_main_test.reset_index(drop=True)
        df_main_test = df_main_test.groupby(by=['year.'+i], axis=0).sum().reset_index()
        df_main_test["%s.%s" % (id_name, i)] = "region"
        df_main_test.to_pickle(os.path.join(tmp_dir, data_frame_name))
    
    
def get_cntyfile_run_year():
    #get file for each run by county by year
    id_name = 'county_id'
    if special:
        id_name = 'place_id'    
    df_main_list =[]
    for i in run_id:
        if not df_main.columns.isin(["%s.%s" % (id_name, i)]).any():
            continue # data not available for this run        
        data_frame_name = "df_"+i+".pkl"
        df_main_list.append(data_frame_name)
        df_main_test = df_main[df_main["%s.%s" % (id_name, i)].notnull()].dropna(axis = 1, how = 'any')
        df_main_test = df_main_test.reset_index(drop=True)
        df_main_test.to_pickle(os.path.join(tmp_dir, data_frame_name))
        

def _get_colors(col_id):
    # generate given number of colors
    num_colors = len(col_id)
    colors={}
    j = 0
    for i in np.arange(0., 360., 360. / num_colors):
        hue = i/360.
        lightness = (50 + np.random.rand() * 10)/100.
        saturation = (90 + np.random.rand() * 10)/100.
        colors[col_id[j]] = colorsys.hls_to_rgb(hue, lightness, saturation)
        j = j + 1
    return colors

def get_ts_html():
    line_color = _get_colors(run_id)
    scatter_list = []
    for index, geo in geo_ids.iterrows():
        cnty = geo['id']
        #line_color = ['rgb(49,163,84)', 'rgb(253,141,60)', 'rgb(44,127,184)', 'rgb(117,107,177)' ]
        #line_col_id = 0
        irun=0
        for run in run_id:
            irun = irun+1
            filename = os.path.join(tmp_dir, 'df_'+run+'.pkl')
            if not os.path.isfile(filename):
                continue
            df_test = pd.read_pickle(filename).astype('int')

            df_cnty = df_test[df_test["%s.%s" % (id_name, run)]==cnty].reset_index()
            col = df_cnty.columns.values.tolist()
            
            for c in col:
                if c == "year."+run or c == "%s.%s" % (id_name, run):
                    pass
                else:
                    line_col = "rgb(%s, %s, %s)" % (line_color[run][0], line_color[run][1],line_color[run][2])
                    #line_col_id = line_col_id +1  
                    cnty_scatter = go.Scatter(x=df_cnty["year."+run], 
                                                  y=df_cnty[c], 
                                                  mode = 'lines+markers',
                                                  marker = dict(color = line_col),
                                                  line= dict(color = line_col),
                                                  name= str(cnty)+'_'+(c)+':',
                                                  text = str(cnty)+"_"+c[:3]+"."+"_"+run+":",
                                                  hoverinfo = "x+text+y")
                    scatter_list.append(cnty_scatter)                    
                                      
        
        
    fig = tools.make_subplots(rows=len(geo_ids.index), cols=6, 
                            subplot_titles=("FIRE_services", "Construction resources", "Education", "Government", "Manuf WTU", "Retail & Food"), 
                            vertical_spacing = 0.03)
        
        
    row=0
    ytitle_col  = 1
    indnum = range(len(scatter_list))
    
    for index, geo in geo_ids.iterrows():
        row=row+1
        title_updated = False
        for i in indnum:            
            if scatter_list[i].name.split('_')[1].startswith('index'):
                continue            
            spl = scatter_list[i].name.split('_', 1)
            dot_loc = spl[1].find(".")
            sector_name = spl[1][:dot_loc]
            thisgeo = spl[0]
            loopgeo = str(geo['id'])
            if sector_name =="FIRE_services" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 1)
                if not title_updated:
                    fig['layout']['yaxis'+str(ytitle_col)].update(title=geo['name'])
                    ytitle_col = ytitle_col+6
                    title_updated=True
            if sector_name =="construction_resources" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 2)
            if sector_name =="edu" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 3)
            if sector_name =="government" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 4)
            if sector_name =="manuf_WTU" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 5)
            if sector_name == "retail_food_services" and thisgeo==loopgeo:
                fig.append_trace(scatter_list[i], row, 6)

        
    geo_title = 'County'
    if special:
        geo_title = 'Special Places'
    fig['layout'].update( title='Employment: ' + 'Aggregated Sectors by ' + geo_title +' ('+time.strftime("%m/%d/%Y")+")",  
                                                        titlefont = dict(color= "black", size = 25 ), 
                         showlegend = False,
                         height=2400, width=2350,
                         plot_bgcolor='rgba(245, 246, 249, 1)', 
                         paper_bgcolor =   'rgb(217,217,217)')
      
    #htmlfile = os.path.join(result_dir, "luv2_emp_run_"+("_").join(str(x) for x in run_id)+"_scatter"+'_'+time.strftime("%m%d%Y")+".html")
    htmlfile = "qc_ts_emp_cnty.html"
    if special:
        htmlfile = "qc_ts_emp_sp.html"
    plotly.offline.plot(fig, filename = os.path.join(result_dir, htmlfile), auto_open = plot_val)
    #plotly.offline.plot(fig, filename = "luv2_emp_run_"+("_").join(str(x) for x in run_id)+"_scatter"+'_'+time.strftime("%m%d%Y")+".Rmd")
    
    # write into Rmd
    if not special:
        rmdfile = open(os.path.join(result_dir, "emplots_by_sectors.Rmd"), "w")
        rmdfile.write("## Employment by Sector\n\n")
        rmdfile.write("* [Employment time series by county](%s)\n" % htmlfile)
    else:
        rmdfile = open(os.path.join(result_dir, "emplots_by_sectors.Rmd"), "a")
        rmdfile.write("* [Employment time series by special places](%s)\n" % htmlfile)
    rmdfile.close()
    
    
def remove_tmp_dir():
    shutil.rmtree(tmp_dir)
    
def get_geo_ids():
    # get unique geography identifiers
    idx = np.where(np.array(map(lambda x: x.startswith(id_name),  df_main.columns)))[0]
    if special:
        geos = pd.DataFrame({"id":np.unique(df_main[idx])})
        geos = pd.merge(geos, place_table.drop(['zone_id'], axis=1).drop_duplicates(), right_on="place_id", left_on='id')
    else:
        geos = pd.DataFrame({'id':[33,35,53,61], 'name':["King County", "Kitsap County", "Pierce County", "Snohomish County"]})
    return geos
    
if __name__ == "__main__":

    import sys
    import pandas as pd
    import numpy as np
    import os
    import time
    import colorsys
    import plotly
    import plotly.plotly as py
    from plotly.tools import FigureFactory as FF
    from plotly import tools
    import plotly.graph_objs as go
    import shutil
    from optparse import OptionParser
        
    # get command line arguments
    parser = OptionParser()
    parser.add_option("-s", "--special", action="store_true", dest="special",
                      help="special places geography") 
    parser.add_option("-m", "--make", action="store_true", dest="make",
                          help="running via a Makefile")
    (options, args) = parser.parse_args()
   
    #make = True # Should be set to True if run from Makefile
    make = not options.make is None 
    special = not options.special is None
    #special=True
    if make:
        base_dir = os.environ['QC_BASE_DIRECTORY']
        wrkdir = os.getcwd()
        runs_folder = [os.environ['QC_RUN1']] + [os.environ['QC_RUN2']]
        result_dir = os.environ['QC_RESULT_PATH']
        plot_val = False
    else:
        base_dir = get_base_dir()
        wrkdir = os.getcwd()[:-7]        
        runs_folder = get_input_luv()
        result_dir = os.path.join(wrkdir, "results", get_run_name())
        
    plot_val = not make
    data_dir = os.path.join(wrkdir, "data")
    run_dirs = []
    for folder in runs_folder:
        run_dirs = run_dirs + list(set(map(lambda x: x.lstrip().rstrip(), folder.split(',')))) # use set to make the list unique
    runs_folder = run_dirs
        
    data_year = ['2014', '2015','2020','2025','2030','2035','2040' ]
    id_name = 'county_id'
    if special:
        id_name = 'place_id'
        place_table = pd.read_table(os.path.join(data_dir, 'SpecialPlaces.csv'), sep=',').drop(['acres'], axis=1)
        
    #county_id = [33,35,53,61]
    print "base_dir: ", base_dir
    print "data points: ", data_year
    print "list of runs: ", runs_folder
    run_id = get_runid()
    print run_id
    print "Grabed the run numbers..."
        
    tmp_dir = os.path.join(result_dir, "tmp_tbls")
    init_plk_list = creat_pklfiles()
    print "got the temp files..."
    df_main = get_master_df()
    geo_ids = get_geo_ids()
    df_main_region_list = get_regfile_run()
    df_main_list = get_cntyfile_run_year()
    #print "got the data frames for the time series..."
    get_ts_html()
    if plot_val == True:
        print "scatterplots saved in the results directory as .html and deployed to the default browser..."
    if plot_val == False:
        print "scatterplots saved in the results directory as .html..."
    remove_tmp_dir()
    print "temporary files/folders removed... Finish!"
    
    
