



# def import_input_standalone():
    # from qc_scatterplot_emp_cnty_inputs import data_year, runs_folder, county_id 
    # return data_year
    # return runs_folder
    # return county_id


def import_packages():
    import pandas as pd
    import os
    import time
    import plotly
    import plotly.plotly as py
    from plotly.tools import FigureFactory as FF
    from plotly import tools
    import plotly.graph_objs as go


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
               
                df_name = pd.read_table('\\\\MODELSRV3\\e$\\opusgit\\urbansim_data\\data\\psrc_parcel\\runs\\'+run_num+'\\indicators'+'/'+'large_area__dataset_table__employment_by_aggr_sector__'+year+".tab")
                
            except:
                print "Alert: Aggregated Large Area Employment file for year "+ y + " is not available for the run  "+ str(run_num) 
                
            else:
                dot_loc = r.find(".")
                run_id.append(r[4:dot_loc])
                
    run_id = remove_duplicates(run_id)
    return run_id
    

def get_input_luv():
    with open(os.getcwd()[:-7]+'inputs.txt', 'r') as f:
        line_data = f.readlines()
    run_grab = []
    for l in line_data:
        if l[0] =="#":
            pass
        if str(l[:6]) == "QC_RUN":
            
            run_grab.append(l[8:].strip())
            

    return run_grab
    
    
    
    
def creat_pklfiles():
    #Create a temp directory for the pkl files:
    if not os.path.exists(os.getcwd()[:-7]+"tmp_tbls\\"):
        os.makedirs(os.getcwd()[:-7]+"tmp_tbls\\")
    
    
    # Create .pkl tables for all indicator files with try & except
    
    init_plk_list =[]
   
    for r in runs_folder:
        run_num = r
        for y in data_year:
            year = y
            try:
               
                df_name = pd.read_table('\\\\MODELSRV3\\e$\\opusgit\\urbansim_data\\data\\psrc_parcel\\runs\\'+run_num+'\\indicators'+'/'+'large_area__dataset_table__employment_by_aggr_sector__'+year+".tab")
                
            except:
                print "Alert: Aggregated Large Area Employment file for year "+ y + " is not available for the run  "+ str(run_num) 
                
            else:
                year = y
                data_frame_name = "df_"+run_num+"_"+year+".pkl"
                init_plk_list.append(data_frame_name)
                df_name.reset_index(drop=True)
                df_name.to_pickle(os.getcwd()[:-7]+"tmp_tbls\\"+data_frame_name)
    
    return init_plk_list

def get_master_df():
    #read all the .pkl files in to one data frame
    df_main = pd.DataFrame()
    for df in init_plk_list:
        df_work = pd.read_pickle(os.getcwd()[:-7]+"tmp_tbls\\"+df)
        df_work = df_work.groupby(by=['county_id'], axis=0).sum().reset_index().astype('int').drop(['large_area_id'], axis=1)
        df_work['year'] = df[-8:-4]
        dot_loc = df.find(".")
        run = df[7:dot_loc]
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
    for i in run_id:
        data_frame_name = "df_region_"+i+".pkl"
        df_main_region_list.append(data_frame_name)
        df_main_test = df_main[df_main["year."+i].notnull()]
        df_main_test = df_main_test.dropna(axis = 1, how = 'any')   
        df_main_test = df_main_test.reset_index(drop=True)
        df_main_test = df_main_test.groupby(by=['year.'+i], axis=0).sum().reset_index()
        df_main_test["county_id."+i] = "region"
        df_main_test.to_pickle(os.getcwd()[:-7]+"tmp_tbls\\"+data_frame_name)
    
    
def get_cntyfile_run_year():
    #get file for each run by county by year
    df_main_list =[]
    for i in run_id:
        data_frame_name = "df_"+i+".pkl"
        df_main_list.append(data_frame_name)
        df_main_test = df_main[df_main["county_id."+i].notnull()].dropna(axis = 1, how = 'any')
        df_main_test = df_main_test.reset_index(drop=True)
        df_main_test.to_pickle(os.getcwd()[:-7]+"tmp_tbls\\"+data_frame_name)
        

def get_scatter_html():        
    scatter_list = []
    for cnty in county_id:
        
        line_color = ['rgb(49,163,84)','rgb(253,141,60)',   'rgb(44,127,184)', 'rgb(117,107,177)' ]
        line_col_id = 0
        for run in run_id:
            line_col = line_color[line_col_id]
            line_col_id = line_col_id +1
                
            df_test = pd.read_pickle(os.getcwd()[:-7]+"tmp_tbls\\"+'df_'+run+'.pkl').astype('int')

            df_cnty = df_test[df_test["county_id."+run]==cnty].reset_index()
            col = df_cnty.columns.values.tolist()
            
            for c in col:
                if c == "year."+run or c =="county_id." +run:
                    pass
                else:
                    cnty_scatter = go.Scatter(x=df_cnty["year."+run], 
                                                  y=df_cnty[c], 
                                                  mode = 'lines+markers',
                                                  marker = dict(color = line_col),
                                                  name= str(cnty)+'_'+(c)+':',
                                                  text = str(cnty)+"_"+c[:3]+"."+"_"+run+":",
                                                  hoverinfo = "x+text+y")
                    scatter_list.append(cnty_scatter)
        
        
    #get the subplot titles 
    indnum = range(len(scatter_list))
    ind_lab = []
    for i in indnum:
        if scatter_list[i].name[3:-1] =="index":
            pass
        else:
            dot_loc = scatter_list[i].name.find(".")
            ind_lab.append(scatter_list[i].name[3:dot_loc])
                
    #subplot_titles = tuple(remove_duplicates(ind_lab))
    fig = tools.make_subplots(rows=6, cols=4, 
                                                      subplot_titles=("King County", "Kitsap County", "Pierce County", "Snohomish County"), 
                                                      
                                                      vertical_spacing = 0.03)
        
        
        
    for cnty in county_id:    
        indnum = range(len(scatter_list))
        ind_lab = []
        for i in indnum:
            dot_loc = scatter_list[i].name.find(".")
            if scatter_list[i].name[3:-1] =="index":
                pass
            if cnty ==33:
                 col_id = 1
            if cnty ==35:
                 col_id = 2
            if cnty ==53:
                col_id = 3
            if cnty ==61:
                col_id = 4
            ytitle_col  = 1
            if scatter_list[i].name[3:dot_loc] =="FIRE_services" and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 1,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass
            ytitle_col = ytitle_col+4; 
            if scatter_list[i].name[3:dot_loc] =="construction_resources"and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 2,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass
            ytitle_col = ytitle_col+4; 
            if scatter_list[i].name[3:dot_loc] =="edu"and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 3,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass
            ytitle_col = ytitle_col+4;
            if scatter_list[i].name[3:dot_loc] =="government"and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 4,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass
            ytitle_col = ytitle_col+4;
            if scatter_list[i].name[3:dot_loc] =="manuf_WTU"and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 5,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass
            ytitle_col = ytitle_col+4;
            if scatter_list[i].name[3:dot_loc] =="retail_food_services"and scatter_list[i].name[:2]==str(cnty):
                fig.append_trace(scatter_list[i], 6,col_id)
                if cnty==33: fig['layout']['yaxis'+str(ytitle_col)].update(title=scatter_list[i].name[3:dot_loc]); pass

        
        
    fig['layout'].update( title='LUV 2 Indicators: Employment: ' +
                                                      'Aggregate Sectors; Runs: '+ (", ").join(str(x) for x in run_id)+' ('+time.strftime("%m/%d/%Y")+")",  
                                                        titlefont = dict(color= "black", size = 25 ), 
                         showlegend = False,
                         height=2400, width=2350,
                         plot_bgcolor='rgba(245, 246, 249, 1)', 
                         paper_bgcolor =   'rgb(217,217,217)')
        
    #plotly.offline.plot(fig, filename = "qc_scatterplot_emp_cnty.html")
    plotly.offline.plot(fig, filename = os.getcwd()[:-7]+"results\\"+"luv2_emp_run_"+("_").join(str(x) for x in run_id)+"_scatter"+'_'+time.strftime("%m%d%Y")+".html")
    #plotly.offline.plot(fig, filename = "luv2_emp_run_"+("_").join(str(x) for x in run_id)+"_scatter"+'_'+time.strftime("%m%d%Y")+".Rmd")
    
def remove_tmp_dir():
    shutil.rmtree(os.getcwd()[:-7]+"tmp_tbls\\")
    
if __name__ == "__main__":
    # import_input_standalone()
    # from qc_scatterplot_emp_cnty_inputs import* 
    # import_packages()
    # import pandas as pd
    # import os
    # import time
    # import plotly
    # import plotly.plotly as py
    # from plotly.tools import FigureFactory as FF
    # from plotly import tools
    # import plotly.graph_objs as go
    # print "Import successful!"
    # print "data points: ", data_year
    # print "list of runs: ", runs_folder
    # print "list of county ids: ", county_id
    # run_id = get_runid()
    # print run_id
    # print "Grabed the run numbers..."
    # init_plk_list = creat_pklfiles()
    # print "got the temp files..."
    # df_main = get_master_df()
    # print "got the temp files..."
    # df_main_region_list = get_regfile_run()
    # df_main_list = get_cntyfile_run_year()
    # get_scatter_html()

    import pandas as pd
    import os
    import time
    import plotly
    import plotly.plotly as py
    from plotly.tools import FigureFactory as FF
    from plotly import tools
    import plotly.graph_objs as go
    import shutil
    print "Import successful!"
    
    
    
    runs_folder = get_input_luv()
    data_year = ['2014', '2015','2020','2025','2030','2035','2040' ]
    county_id = [33,35,53,61]
    print "data points: ", data_year
    print "list of runs: ", runs_folder
    print "list of county ids: ", county_id
    run_id = get_runid()
    print run_id
    print "Grabed the run numbers..."
    init_plk_list = creat_pklfiles()
    print "got the temp files..."
    df_main = get_master_df()
    print "got the temp files..."
    df_main_region_list = get_regfile_run()
    df_main_list = get_cntyfile_run_year()
    get_scatter_html()
    remove_tmp_dir()
    
    