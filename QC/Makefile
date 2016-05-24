.PHONY : test
test : testreport

.PHONY : all
all: report maps rplots

.PHONY : testreport
testreport : testreport.txt 

.PHONY : maps
maps : map.png

.PHONY : rplots
rplots : rplots.png

.PHONY : clean
clean : 
	rm -f testreport.txt
	rm -f *.Rout

testreport.txt : create_testreport.R
	R CMD BATCH create_testreport.R

map.png : create_map.py
	python create_map.py

rplots.png : create_rplots.R
	R CMD BATCH create_rplots.R

