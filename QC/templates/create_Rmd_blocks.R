create.header <- function(file, title, author=NULL, date=NULL, output="html_document", theme="readable") {
	cat("---\n", file=file)
	cat("title:", title, "\n", file=file, append=TRUE)
	if (!is.null(author))
		cat("author:", title, "\n", file=file, append=TRUE)
	if (!is.null(date))
		cat("date:", date, "\n", file=file, append=TRUE)
	cat("output:\n", file=file, append=TRUE)
	cat(" ", output, ":\n", file=file, append=TRUE)
	cat("    toc: true\n", file=file, append=TRUE)
	cat("    theme:", theme, "\n", file=file, append=TRUE)
	cat("    pandoc_args: [\n", file=file, append=TRUE)
	cat("    	'--columns=500'\n", file=file, append=TRUE)
	cat("    			]\n", file=file, append=TRUE)
	cat("---\n", file=file, append=TRUE)
}

create.section <- function(file, title, text="") {
	cat("##", title, "\n\n", file=file, append=TRUE)
	cat(text, "\n", file=file, append=TRUE)
}

create.subsection <- function(file, title, text="") {
	cat("###", title, "\n\n", file=file, append=TRUE)
	cat(text, "\n", file=file, append=TRUE)
}

add.text <- function(file, text){
	cat(text, "\n", file=file, append=TRUE)
}

add.table <- function(file, df){
	#add.text(file, "```{r, eval=FALSE}")
	cat(paste(names(df), collapse = "|"), file=file, append=TRUE)
	cat("\n", file=file, append=TRUE)
	cat(paste(rep("-", ncol(df)), collapse = "|"), file=file, append=TRUE)
	cat("\n", file=file, append=TRUE)

	for(i in 1:nrow(df)){
		cat(paste(df[i,], collapse = "|"), file=file, append=TRUE)
		cat("\n", file=file, append=TRUE)
	}
	#add.text(file, "```")
	cat("\n", file=file, append=TRUE)
}

add.table.highlight <- function(file, df, highlight=c(), color='yellow'){
	cat(paste(names(df), collapse = "|"), file=file, append=TRUE)
	cat("\n", file=file, append=TRUE)
	cat(paste(rep("-", ncol(df)), collapse = "|"), file=file, append=TRUE)
	cat("\n", file=file, append=TRUE)
	for(i in 1:nrow(df)){
		if(i %in% highlight) {
			for(j in 1:ncol(df)) {
				cat(paste0('<span style="background:', color, '">'), file=file, append=TRUE)
				cat(df[i,j], file=file, append=TRUE)
				cat("</span>", file=file, append=TRUE)
				if(j < ncol(df))cat("|", file=file, append=TRUE)
			}
		} else 
			cat(paste(df[i,], collapse = "|"), file=file, append=TRUE)
		cat("\n", file=file, append=TRUE)
	}
	cat("\n", file=file, append=TRUE)
}

