# QCing Land Use Vision

This directory contains a set of scripts each of which performs a QC sub-task. The scripts are connected via a Makefile also found in this directory. 

## Installation
On Windows, check that [Git Bash](https://git-for-windows.github.io) is installed. Then open a Git Bash and type ``make --help``. If you get an error that make is not found, install make from [SWCarpentryInstaller](https://github.com/swcarpentry/windows-installer/releases/latest). 

As with any GitHub repository, clone the luv repository into your local computer: In a Git Bash navigate to a directory you want the local repository reside and type 

```
git clone https://github.com/psrc/luv.git luv
```

(The ``luv`` at the end is a name of your choice.) It should create a directory of the specified name, here ``luv``, that is identical to the GitHub repository.

Also, check that R is working from the command line by typing R into the bash terminal. If it is not found using the above installer should fix it.

## Running the Makefile

The Makefile is a collection of scripts that do some part of the QC. It's currently under development, so most of them are just placeholders. To test the script, first locate where LUV results are located. Open the file input.txt and modify the 'directory' line appropriately. You can use comments to comment out unused rows. Make sure the fields are separated by tabs. 
Then run the following line from your bash terminal:

```
make test
```

It should create a file called testreport.txt. If there is an error look in the file create_testreport.Rout. Most likely it will be related to reading the inputs.txt file.

To delete the created files, type

```
make clean
```


## Extending the Makefile

The idea is that people write a script and connect it to the framework via the Makefile. We should create some rules for grouppings (e.g. plots, maps, reports) and establish naming conventions for the phonies. 

Scripts can operate either on the indicator files (as the testreport), or on outputs of other scripts. (That's where dependecies defined in a Makefile come very handy.)

More to come. 

## Synchronization

As this is under active development, don't forget to ``pull`` regularly to keep the repository version in sync with your local code. If you're writing a script, push it to the repository also on a regular basis so that everybody is informed what others are working on.



