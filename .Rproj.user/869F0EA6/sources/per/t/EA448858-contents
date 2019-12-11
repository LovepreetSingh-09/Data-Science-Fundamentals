library(arules)
library(ggplot2)

bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",
                                 sep="\t",cols=c("userid", "title"),rm.duplicates=T, header = T)
summary(bookbaskets)


