.PHONY : all
all: report maps rplots

.PHONY : report
report : report.txt 

.PHONY : maps
maps : map.png

.PHONY : rplots
rplots : rplots.png

.PHONY : clean
clean : 
	rm -f report.txt
	rm *.Rout

report.txt : create_report.R
	R CMD BATCH create_report.R

map.png : create_map.py
	python create_map.py

rplots.png : create_rplots.R
	R CMD BATCH create_rplots.R

