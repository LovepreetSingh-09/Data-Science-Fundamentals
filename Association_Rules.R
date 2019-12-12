library(arules)
library(ggplot2)

bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",
                                 sep="\t",cols=c("userid", "title"),rm.duplicates=T, header = T)
summary(bookbaskets)

class(bookbaskets)
bookbaskets

colnames(bookbaskets)[1:10]
rownames(bookbaskets)[1:10]
