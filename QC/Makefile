# read input file 
INPUT_FILE=inputs.txt
include $(INPUT_FILE)
# extract variable names while ignoring comments
# export into environment so that all scripts have access to it
export $(shell grep -v '^\#' $(INPUT_FILE) | cut -d= -f1)

SCRIPT_PATH=scripts
RESULT_BASE_PATH=results
RREPORT_PATH=../Rreports
# trim white spaces around 
RESULT_PATH=$(RESULT_BASE_PATH)/$(shell echo $(QC_NAME) | xargs)
RESULT_TEST_PATH=$(RESULT_BASE_PATH)/test

RTABLE_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/qc_rtable_*.R)
RPLOT_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/rplots_*.R)
RMAP_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/rmaps_*.R)

RTABLE_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/qc_rtable_%.R, $(SCRIPT_PATH)/qc_rtable_%.Rout, \
				$(RTABLE_SCRIPT_FILES))
RPLOT_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/rplots_%.R, $(SCRIPT_PATH)/rplots_%.Rout, \
                                $(RPLOT_SCRIPT_FILES))
RMAP_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/rmaps_%.R, $(SCRIPT_PATH)/rmaps_%.Rout, \
                                $(RMAP_SCRIPT_FILES))

# The following converts POSIX into Windows path, 
# which is needed when other processes (e.g. R) use it.  
WRKDIR=$(shell pwd  | sed 's|^/\([a-z]\)/|\1:/|')
export QC_SCRIPT_PATH=$(WRKDIR)/$(SCRIPT_PATH)
export QC_RESULT_PATH=$(WRKDIR)/$(RESULT_PATH)


.PHONY : test
test : $(RESULT_TEST_PATH)/testreport.txt

.PHONY : ind
ind : 
	python $(SCRIPT_PATH)/create_indicator_files_nearyr_outyr.py
	python $(SCRIPT_PATH)/create_indicator_files.py

	
.PHONY : ind-maxdev
ind-maxdev :
	python $(SCRIPT_PATH)/create_max_dev_indicator_files.py

.PHONY : ind-an
ind-an :
	python $(SCRIPT_PATH)/create_annual_indicator_files.py

.PHONY : all
all: rtables_raw rplots_raw rmaps_raw emplots_raw index

.PHONY : dash
dash: 
	R CMD BATCH $(SCRIPT_PATH)/run_dash.R $(SCRIPT_PATH)/run_dash.Rout

.PHONY : shiny
shiny:
	R CMD BATCH $(SCRIPT_PATH)/run_shiny.R $(SCRIPT_PATH)/run_shiny.Rout

.PHONY : rtables
rtables : rtables_raw index

.PHONY : rtables_raw
rtables_raw : $(RTABLE_SCRIPTOUT_FILES)

.PHONY : rplots
rplots : rplots_raw index

.PHONY : rplots_raw
rplots_raw : $(RPLOT_SCRIPTOUT_FILES)

.PHONY : rmaps
rmaps : rmaps_raw index

.PHONY : rmaps_raw
rmaps_raw : $(RMAP_SCRIPTOUT_FILES)

.PHONY : emplots
emplots : emplots_raw index

.PHONY : emplots_raw
emplots_raw : $(RESULT_PATH)/qc_ts_emp_cnty.html

$(RESULT_PATH)/qc_ts_emp_cnty.html : $(SCRIPT_PATH)/qc_tsplot_emp_cnty.py inputs.txt
	python $(SCRIPT_PATH)/qc_tsplot_emp_cnty.py -m
	python $(SCRIPT_PATH)/qc_tsplot_emp_cnty.py -m -s

.PHONY : rreport-faz
rreport-faz : $(RREPORT_PATH)/fazreportLUV.Rout

.PHONY : rreport-city
rreport-city : $(RREPORT_PATH)/cityreportLUV.Rout


index : $(SCRIPT_PATH)/create_index_file.R inputs.txt
	R CMD BATCH $(SCRIPT_PATH)/create_index_file.R  $(SCRIPT_PATH)/create_index_file.Rout

$(SCRIPT_PATH)/qc_rtable_%.Rout : $(SCRIPT_PATH)/qc_rtable_%.R inputs.txt
	R CMD BATCH $(SCRIPT_PATH)/qc_rtable_$*.R  $(SCRIPT_PATH)/qc_rtable_$*.Rout

$(SCRIPT_PATH)/rplots_%.Rout : $(SCRIPT_PATH)/rplots_%.R inputs.txt
	R CMD BATCH $(SCRIPT_PATH)/rplots_$*.R  $(SCRIPT_PATH)/rplots_$*.Rout

$(SCRIPT_PATH)/rmaps_%.Rout : $(SCRIPT_PATH)/rmaps_%.R inputs.txt
	R CMD BATCH $(SCRIPT_PATH)/rmaps_$*.R  $(SCRIPT_PATH)/rmaps_$*.Rout

$(RREPORT_PATH)/fazreportLUV.Rout : $(RREPORT_PATH)/fazreportLUV.R inputs.txt
	R CMD BATCH $(RREPORT_PATH)/fazreportLUV.R $(RREPORT_PATH)/fazreportLUV.Rout

$(RREPORT_PATH)/cityreportLUV.Rout : $(RREPORT_PATH)/cityreportLUV.R inputs.txt
	R CMD BATCH $(RREPORT_PATH)/cityreportLUV.R $(RREPORT_PATH)/cityreportLUV.Rout


.PHONY : clean
clean : 
	rm -f $(SCRIPT_PATH)/*.Rout
	rm -fr $(RESULT_PATH)/*
	rm -fr $(RESULT_TEST_PATH)

.PHONY : clean-index
clean-index :
	rm -f $(SCRIPT_PATH)/create_index_file.Rout

# Lines in the following recipe needs to be connected via ; and \ in order 
# for the R process to see the change in the env variable.
# Otherwise it would be one sub-process per line.
$(RESULT_TEST_PATH)/testreport.txt : $(SCRIPT_PATH)/create_testreport.R
	export QC_RESULT_PATH=$(WRKDIR)/$(RESULT_TEST_PATH); \
	R CMD BATCH $(SCRIPT_PATH)/create_testreport.R $(SCRIPT_PATH)/create_testreport.Rout

# placeholder (not implemented)
.PHONY : maps
maps : map.png

map.png : create_map.py
	python create_map.py

