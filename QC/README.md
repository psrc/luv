# QCing Land Use Vision

This directory contains a set of scripts each of which performs a QC sub-task. The scripts are connected via a Makefile also found in this directory. 

## Installation
On Windows, check that [Git Bash](https://git-for-windows.github.io) is installed. Then open a Git Bash and type ``make --help``. If you get an error that make is not found, install make from [SWCarpentryInstaller](https://github.com/swcarpentry/windows-installer/releases/latest). If you still get an error, you might need to add the location of make.exe into your Path environment.

As with any GitHub repository, clone the luv repository into your local computer: In a Git Bash navigate to a directory you want the local repository reside and type 

```
git clone https://github.com/psrc/luv.git luv
```

(The ``luv`` at the end is a name of your choice.) It should create a directory of the specified name, here ``luv``, that is identical to the GitHub repository.

Also, check that R is working from the command line by typing R into the bash terminal. If it is not found using the above installer should fix it. Or, adding the location of R.exe into your Path environment.

On modelsrv3 the repository is installed in ``d://luvgit``.

## Running the Makefile

Navigate into the luv/QC directory. The Makefile here is a collection of scripts that do some part of the QC. It's currently under development, so most of them are just placeholders. To test the script, first locate where LUV results are located. Open the file input.txt and modify the 'directory' line appropriately. (If your file system is connected to the E drive on modelsrv3, you might not need any changes). You can use comments to comment out unused rows. 
Then run the following line from your bash terminal:

```
make test
```

It runs the code in scripts/create\_testreport.R which should create a file results/test/testreport.txt. If there is an error check the output file scripts/create\_testreport.Rout.

To delete the created files, type

```
make clean
```


## Extending the Makefile

The idea is that people write a QC script and connect it to the framework via the Makefile. We can create several grouppings for different types of outputs, each of them would correspond to a phonie. For example, there is a group called 'rtables'. The command 

```
make rtables
```
will run all R-scripts in the 'scripts' directory that are of the form 'qc\_rtable\_\*.R' where \* can be any character string. Such scripts should store their results in the 'results/{run\_name}' directory where {run\_name} corresponds to the QC\_NAME entry in the inputs.txt file.  The absolute path of this directory can be obtained from the environmental variable QC\_RESULT\_PATH. 

We can create other grouppings, e.g. for plots or maps. 

Scripts can operate either on the indicator files (as the testreport), or on outputs of other scripts. (That's where dependecies defined in a Makefile come very handy.)

Currently, the following phonies are implemented:

   * **test**: test the system
   * **clean**: delete test outputs and output files of the current run (defined by QC\_NAME in inputs.txt)
   * **ind**: run python indicator script (in scripts/create\_indicator\_files.py) on cache defined via QC\_BASE_DIRECTORY/QC\_RUN1 in inputs.txt
   * **rtables**: run R code in scripts/qc\_rtable\_\*.R
   * **rplots**: run R code in scripts/rplots\_\*.R


## Synchronization

As this is under active development, don't forget to ``pull`` regularly to keep the repository version in sync with your local code. If you're writing a script, push it to the repository also on a regular basis so that everybody is informed what others are working on.

If you have local changes you want to throw out, e.g. in Makefile, do (from the QC directory)

```
git checkout Makefile .
```
Repeat this for all files for which you want to discard your changes. Then do

```
git pull
```



