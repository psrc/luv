# read input file 
INPUT_FILE=inputs.txt
include $(INPUT_FILE)
# extract variable names while ignoring comments
# export into environment so that all scripts have access to it
export $(shell grep -v '^\#' $(INPUT_FILE) | cut -d= -f1)

SCRIPT_PATH=scripts
RESULT_BASE_PATH=results
# trim white spaces around 
RESULT_PATH=$(RESULT_BASE_PATH)/$(shell echo $(QC_NAME) | xargs)
RESULT_TEST_PATH=$(RESULT_BASE_PATH)/test

RTABLE_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/qc_rtable_*.R)
RPLOT_SCRIPT_FILES=$(wildcard $(SCRIPT_PATH)/rplots_*.R)

RTABLE_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/qc_rtable_%.R, $(SCRIPT_PATH)/qc_rtable_%.Rout, \
				$(RTABLE_SCRIPT_FILES))
RPLOT_SCRIPTOUT_FILES=$(patsubst $(SCRIPT_PATH)/rplots_%.R, $(SCRIPT_PATH)/rplots_%.Rout, \
                                $(RPLOT_SCRIPT_FILES))

RTABLE_RESULT_FILES=$(patsubst $(SCRIPT_PATH)/qc_rtable_%.R, $(RESULT_PATH)/qc_rtable_%.txt, \
				$(RTABLE_SCRIPT_FILES))
RPLOT_RESULT_FILES=$(patsubst $(SCRIPT_PATH)/rplots_%.R, $(RESULT_PATH)/rplots_%.html, \
                                $(RPLOT_SCRIPT_FILES))

# The following converts POSIX into Windows path, 
# which is needed when other processes (e.g. R) use it.  
WRKDIR=$(shell pwd  | sed 's|^/\([a-z]\)/|\1:/|')
export QC_SCRIPT_PATH=$(WRKDIR)/$(SCRIPT_PATH)
export QC_RESULT_PATH=$(WRKDIR)/$(RESULT_PATH)


.PHONY : test
test : $(RESULT_TEST_PATH)/testreport.txt

.PHONY : ind
ind : 
	python $(SCRIPT_PATH)/create_indicator_files.py

.PHONY : all
all: report maps rplots

.PHONY : rtables
rtables : $(RTABLE_RESULT_FILES)

$(RESULT_PATH)/qc_rtable_%.txt : $(SCRIPT_PATH)/qc_rtable_%.R
	@rm -f $(QC_RESULT_PATH)/report.txt
	R CMD BATCH $(SCRIPT_PATH)/qc_rtable_$*.R  $(SCRIPT_PATH)/qc_rtable_$*.Rout

.PHONY : rplots
rplots : $(RPLOT_RESULT_FILES)

$(RESULT_PATH)/rplots_%.txt : $(SCRIPT_PATH)/rplots_%.R
	R CMD BATCH $(SCRIPT_PATH)/rplots_$*.R  $(SCRIPT_PATH)/rplots_$*.Rout

# placeholder
.PHONY : maps
maps : map.png

.PHONY : clean
clean : 
	rm -f $(SCRIPT_PATH)/*.Rout
	rm -f $(RESULT_PATH)/*.txt
	rm -fr $(RESULT_TEST_PATH)

# Lines in the following recipe needs to be connected via ; and \ in order 
# for the R process to see the change in the env variable.
# Otherwise it would be one sub-process per line.
$(RESULT_TEST_PATH)/testreport.txt : $(SCRIPT_PATH)/create_testreport.R
	export QC_RESULT_PATH=$(WRKDIR)/$(RESULT_TEST_PATH); \
	R CMD BATCH $(SCRIPT_PATH)/create_testreport.R $(SCRIPT_PATH)/create_testreport.Rout

# placeholder
map.png : create_map.py
	python create_map.py

