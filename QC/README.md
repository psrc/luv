# QCing Land Use Vision

This directory contains a set of scripts each of which performs a QC sub-task. The scripts are connected via a Makefile also found in this directory. In addition, one can generate Opus indicators or create R-reports on different geography levels.

## Installation
On Windows, check that [Git Bash](https://git-for-windows.github.io) is installed. Then open a Git Bash and type ``make --help``. If you get an error that make is not found, install "make" version 4.1 using the installer at ``X:\DSA\Software\Make Setup v4.1.0.exe``. If you still get an error, you might need to add the location of make.exe into your Path environment (The installer above sets it to ``C:\Make``). After you install, you'll need to close and re-open any Bash or command windows to use the new version.

<!--O from [SWCarpentryInstaller](https://github.com/swcarpentry/windows-installer/releases/latest)-->
As with any GitHub repository, clone the luv repository into your local computer: In a Git Bash navigate to a directory you want the local repository reside and type 

```
git clone https://github.com/psrc/luv.git luv
```

(The ``luv`` at the end is a name of your choice.) It should create a directory of the specified name, here ``luv``, that is identical to the GitHub repository.

Check if Python is working by typing 'python' into the bash terminal. Two packages (pandas and plotly) are required to run the python script. If you have the 'Anaconda' installation, you should already have 'pandas' isntalled. You can check the list of python packages installed by using the commands below into the bash terminal:

```
$ conda list
```

or

```
$ pip list
```

To install the required packages, just type into the bash terminal:

```
$ pip install pandas
```

and

```
$ pip install plotly
```

Make sure [R](https://www.r-project.org) is installed. Also, check that R is working from the command line by typing R into the bash terminal. If it is not found, add the location of R.exe into your Path environment (on modelsrv3 it's ``C:\Program Files\R\R-3.3.1\bin``).

While R is open, install the packages ``shinythemes``, ``htmltools``, ``flexdashboard``, ``shiny``, ``magrittr``, ``plotly``, ``data.table``, ``leaflet``, ``rgdal``, ``sp`` using

```
install.packages(c('shinythemes', 'htmltools', 'flexdashboard', 'shiny', 'magrittr', 'plotly', 'data.table', 'leaflet', 'rgdal', 'sp'), dependencies=TRUE)
```

If you plan to generate R-reports, you will also need ``ggplot2``, ``grid`` and ``gridExtra``. I believe they should be installed as dependencies in the previous step.


On Windows machines you will most likely need to install [pandoc](http://pandoc.org/installing.html) (click on "download page", scroll to the bottom of the page and select the windows installer).

For generating indicators (optional), [Opus needs to be installed](http://twiki/DSA/InstallingUrbanSim). 

Note that eveything should be installed and working on modelsrv3 in ``d://luvgit``.
 

## Running the Makefile

Navigate into the luv/QC directory. The Makefile here is a collection of scripts that do some part of the QC. To test the script, first locate where LUV results are located. Open the file input.txt and modify the 'QC\_BASE\_DIRECTORY' and 'QC\_RUN1' lines appropriately. (If your file system is connected to the E drive on modelsrv3, you might not need any changes). You can use comments to comment out unused rows. Note that the file is space sensitive, so make sure there are no spaces at the end of the lines.
Then run the following line from your bash terminal:

```
make test
```

It runs the code in scripts/create\_testreport.R which should create a file results/test/testreport.txt. If there is an error check the output file scripts/create\_testreport.Rout.

To delete the created files, type

```
make clean
```

After an UrbanSim run is finished, indicator files need to be created. Make sure the 'QC\_RUN1' entry in input.txt points to the right run. Optionally, put a description of the run, possibly restrictions, into 'QC\_RUN1\_DESCR' and 'QC\_RUN1\_RESTR'. Then type


```
make ind
```

Note that the above step need Opus to be installed.

For running the QC, some scripts perform comparisons of the current run (defined in  'QC\_RUN1') with other runs, which should be defined in 'QC\_RUN2' as comma-separated character string. Also in inputs.txt, the entry 'QC\_NAME' defines a directory where QC results are stored, and thus has the potential of overwriting existing results.

To run all the QCs, type

```
make all
```

For running only a subset of the scripts and other options, see next section. For an interactive visualization, see Section "Viewing Results".

IF:  you get an error message that says, 

```
'the requested operation requires elevation'
```
try running make in Git Bash (if you were using Console Z or some other program) or run the program as administrator.  Christy & Mark got that error message using Console Z on MODELSRV8 but not in Git Bash - web search indicated it has to do with Admin rights.  

## Extending the Makefile

The idea is that people write a QC script and connect it to the framework via the Makefile. We can create several grouppings for different types of outputs, each of them would correspond to a phonie. For example, there is a group called 'rtables'. The command 

```
make rtables
```
will run all R-scripts in the 'scripts' directory that are of the form 'qc\_rtable\_\*.R' where \* can be any character string. Such scripts should store their results in the 'results/{run\_name}' directory where {run\_name} corresponds to the QC\_NAME entry in the inputs.txt file.  The absolute path of this directory can be obtained from the environmental variable QC\_RESULT\_PATH. 

Scripts can operate either on the indicator files (as the testreport), or on outputs of other scripts. (That's where dependecies defined in a Makefile come very handy.)

Currently, the following phonies are implemented:

   * **test**: test the system
   * **clean**: delete test outputs and output files of the current run (defined by QC\_NAME in inputs.txt)
   * **ind**: run python indicator script (in scripts/create\_indicator\_files.py) on cache defined via QC\_BASE_DIRECTORY/QC\_RUN1 in inputs.txt (needs Opus installed)
   * **ind-maxdev**: run indicators for maximum developable capacity (in scripts/create\_max\_dev\_indicator\_files.py). Usually takes much longer than ordinary indicators.
   * **ind-an**: generate annual indicators implemented in scripts/create\_annual\_indicator\_files.py (useful for R-reports, see Section R-Reports). As above, Opus needs to be installed.
   * **rtables**: run R code in scripts/qc\_rtable\_\*.R
   * **rplots**: run R code in scripts/rplots\_\*.R
   * **rmaps**: run R code in scripts/rmaps\_\*.R
   * **emplots**: run python in scripts/qc\_tsplot\_emp\_cnty.py which generates time series plots of jobs by sector and county. 
   * **index**: run R code in scripts/create\_index\_file.R (see next section for more details)
   * **clean-index**: removes the index output file
   * **all**: run the phonies rtables, rplots, rmaps, emplots and index
   * **shiny**: create a shiny dashboard in a browser
   * **dash**: create a browser-based dashboard (obsolete; use shiny instead)
   * **rreport-city**: generate R-report on the city level (see Section R-Reports)
   * **rreport-faz**: generate R-report on the faz level (see Section R-Reports)

## Viewing Results

Each script should write its summary results into an Rmd file (in [R Markdown](http://rmarkdown.rstudio.com)), using a prefix that corresponds to its phonie (e.g. 'rplots\_', 'rtables\_'). The script 'scripts/create\_index\_file.R' (invoked by the index phonie) combines such files into one and translates it into index.html that can be viewed from a browser.  The file is located in the corresponding results directory.

Typing ''make shiny'' (preceeded by ''make all'') will launch a dashboard in your browser allowing you to view the results interactively. To exit the dashboard press Ctrl-C. Note that currently only one dashboard per computer can be launched. It is using results from a directory determined by  the QC\_NAME entry in inputs.txt

To view the Makefile results on the shiny server, copy the makefile results folder directly onto the server. In Git Bash, navigate to the location of the makefile results folder and type:

```
scp -r insert-makefile-results-folder-name shiny@dataweb:/home/shiny/apps/luv/QC/results
```

See Administrators for password.
 
## R-Reports

One can generate R-reports via the Makefile, using the phonies  

```
make rreport-city
``` 

and

```
make rreport-faz
```

The scripts uses the run from QC\_RUN1. In addition, comparison runs can be given in an entry of input.txt called RREPORT\_RUNS which should be given as a comma-separated character string. (Do not include the same value as QC\_RUN1 into RREPORT\_RUNS as QC\_RUN1 is automatically included.) The entry RREPORT\_ANNUAL should be either TRUE or FALSE and it determines, if intermediate years are shown in the plots. Note that usually we create 5-year indicators only. In order to get annual indicators if  RREPORT\_ANNUAL is set to TRUE (and if they haven't been created yet), run 

```
make ind-an
```

prior to generating the R-reports. It will create files in the QC\_RUN1 indicators directory which are called *An, e.g. faz\_\_table\_\_employmentAn.csv containing annual indicators. Note that this can take long time to process.

The resulting R-reports are stored as pdf files in its own directory which is ``../Rreports`` from the current directory, i.e. relative to the repository, [luv/Rreports](https://github.com/psrc/luv/tree/master/Rreports). 


## Synchronization

As this is under active development, don't forget to ``pull`` regularly to keep the repository version in sync with your local code. If you're writing a script, push it to the repository also on a regular basis so that everybody is informed what others are working on.

If you made local changes into inputs.txt, e.g. changes in directories, do not commit those, as everybody has different values in this file. To exclude input.txt from committing and pushing, do (from the QC directory)

```
git add -u
git reset -- inputs.txt
git commit -m 'describe your changes'
git push
```

If you have local changes you want to discard, e.g. in Makefile, do (from the QC directory)

```
git checkout Makefile
```
Repeat this for all files for which you want to discard your changes. Or, to discard all local changes, do

```
git checkout .
```

Then to synchronize with the GitHub repository, do 

```
git pull
```



