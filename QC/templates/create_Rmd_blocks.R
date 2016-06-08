create.header <- function(file, title, author=NULL, date=NULL, output="html_document", theme="readable") {
	cat("---\n", file=file)
	cat("title:", title, "\n", file=file, append=TRUE)
	if (!is.null(author))
		cat("author:", title, "\n", file=file, append=TRUE)
	if (!is.null(date))
		cat("date:", date, "\n", file=file, append=TRUE)
	cat("output:\n", file=file, append=TRUE)
	cat(" ", output, ":\n", file=file, append=TRUE)
	cat("    theme:", theme, "\n", file=file, append=TRUE)
	cat("---\n", file=file, append=TRUE)
}

create.section <- function(file, title, text="") {
	cat("#", title, "\n\n", file=file, append=TRUE)
	cat(text, "\n", file=file, append=TRUE)
}

create.subsection <- function(file, title, text="") {
	cat("##", title, "\n\n", file=file, append=TRUE)
	cat(text, "\n", file=file, append=TRUE)
}

add.text <- function(file, text){
	cat(text, "\n", file=file, append=TRUE)
}
