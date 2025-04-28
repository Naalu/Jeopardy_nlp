pkgname <- "jeopardyNLP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('jeopardyNLP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("create_regular_episodes_df")
### * create_regular_episodes_df

flush(stderr()); flush(stdout())

### Name: create_regular_episodes_df
### Title: Create Processed Data Frame for Regular Episodes
### Aliases: create_regular_episodes_df

### ** Examples

# Assuming jeopardy_raw is your loaded raw data:
# regular_data <- create_regular_episodes_df(jeopardy_raw)
# head(regular_data)



cleanEx()
nameEx("read_jeopardy_tsv")
### * read_jeopardy_tsv

flush(stderr()); flush(stdout())

### Name: read_jeopardy_tsv
### Title: Read Jeopardy TSV Data [Deprecated]
### Aliases: read_jeopardy_tsv

### ** Examples

# Access internal data instead:
# library(jeopardyNLP)
# head(jeopardy_data)

# Or load explicitly:
# data(jeopardy_data)
# head(jeopardy_data)

# Only use for external files:
# external_file <- system.file("extdata", "master_season1-35.tsv", package = "jeopardyNLP")
# if (file.exists(external_file)) {
#   external_data <- read_jeopardy_tsv(external_file)
# }



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
