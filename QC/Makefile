# read input file 
INPUT_FILE=inputs.txt
include $(INPUT_FILE)
# extract variable names while ignoring comments
# export into environment so that all scripts have access to it
export $(shell grep -v '^\#' $(INPUT_FILE) | cut -d= -f1)

SCRIPT_PATH=scripts
RESULT_PATH=results
RTABLE_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/qc_rtable_*.R)
RTABLE_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/qc_rtable_*.R, $(SCRIPT_PATH)/qc_rtable_%.Rout, $(RTABLE_SCRIPT_FILES))
RTABLE_RESULT_FILES=$(patsubst $(SCRIPT_PATH)/qc_rtable_%.R, $(RESULT_PATH)/qc_rtable_%.txt, $(RTABLE_SCRIPT_FILES))
export QC_SCRIPT_PATH=$(shell pwd)/$(SCRIPT_PATH)
export QC_RESULT_PATH=$(shell pwd)/$(RESULT_PATH)


.PHONY : test
test : testreport.txt

.PHONY : ind
ind : 
	python create_indicator_files.py

.PHONY : all
all: report maps rplots

.PHONY : rtables
rtables : $(RTABLE_RESULT_FILES)

$(RESULT_PATH)/qc_rtable_%.txt : $(SCRIPT_PATH)/qc_rtable_%.R
	R CMD BATCH $(SCRIPT_PATH)/qc_rtable_$*.R  $(SCRIPT_PATH)/qc_rtable_$*.Rout

.PHONY : maps
maps : map.png

.PHONY : rplots
rplots : rplots.png

.PHONY : clean
clean : 
	rm -f testreport.txt
	rm -f *.Rout
	rm -f $(SCRIPT_PATH)/*.Rout
	rm -f $(RESULT_PATH)/*.txt

testreport.txt : create_testreport.R
	R CMD BATCH create_testreport.R

map.png : create_map.py
	python create_map.py

rplots.png : create_rplots.R
	R CMD BATCH create_rplots.R

