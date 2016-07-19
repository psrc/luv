library(rmarkdown)

if(!interactive()) { # running using Makefile
	result.dir <- Sys.getenv('QC_RESULT_PATH')
	run.name <- Sys.getenv('QC_NAME')
} else { # running interactively
	run.name <- "run71"
	result.dir <- file.path("results", run.name)
}

create.chunk <- function(prefix) {
	rmd <- list.files(result.dir, pattern = paste0("^", prefix, ".*\\.Rmd"), recursive = TRUE, include.dirs = TRUE)
	if(length(rmd)==0)return()
	chunks <- paste0("```{r child = '", rmd, "'}\n```\n")
	cat(chunks, sep = '\n', append=TRUE, file=index.file)
}

source('templates/create_Rmd_blocks.R')
index.file <- file.path(result.dir, 'index.Rmd')
create.header(index.file, title=paste("LUV QC for ", run.name), date=date())
create.chunk('rtables_')
create.chunk('rplots_')
create.chunk('emplots_')


# convert index.Rmd into index.html
render(index.file)
