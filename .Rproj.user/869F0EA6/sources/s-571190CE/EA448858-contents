library(arules)
library(ggplot2)

bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",
                                 sep="\t",cols=c("userid", "title"),rm.duplicates=T, header = T)
summary(bookbaskets)

class(bookbaskets)
bookbaskets

colnames(bookbaskets)[1:10]
rownames(bookbaskets)[1:10]
bookbaskets[1]
inspect(bookbaskets[1])

size(bookbaskets[1:5])
booksize <- size(bookbaskets)
summary(booksize)
quantile(booksize,probs=seq(0,1,0.1))

ggplot(data.frame(count=booksize),aes(x=count))+
  geom_density(aes(binwidth=1))+
  scale_x_log10()

bookfreq <- itemFrequency(bookbaskets)
(bookfreq)[1]
summary(bookfreq)
length(bookfreq)
length(booksize)
sum(bookfreq)
sum(booksize)

# item Frequency can be find by dividing the values in a column by the total no. of rows
summary(bookfreq*sum(booksize)/sum(bookfreq))
# sum(booksize)/sum(bookfreq) = dim(bookbaskets)[1]
bookcount=bookfreq * dim(bookbaskets)[1]
summary(bookcount)
length(bookcount)
bookcount[1]
?itemFrequency
orderedBooks <- sort(bookCount, decreasing=T)
orderedBooks[1:10]
orderedBooks[1]/dim(bookbaskets)[1]

bookbaskets_use <- bookbaskets[booksize>1]
dim(bookbaskets_use)

rules <- apriori(bookbaskets_use,
                 parameter =list(support = 0.002, confidence=0.75))
summary(rules)
inspect(rules[1])

# Coverage is the support of the left side of the rule (X)
# Fisher’s exact test is a significance test for whether an observed pattern is real
#          or chance (the same thing lift measures; Fisher’s test is more formal).
measures <- interestMeasure(rules,method=c('coverage','fishersExactTest'),transactions = bookbaskets_use)
summary(measures)

inspect(head(sort(rules,by='confidence')),n=5)

brules <- apriori(bookbaskets_use,parameter =list(support = 0.001, confidence=0.6),
                  appearance=list(rhs=c("The Lovely Bones: A Novel"),default='lhs'))
summary(brules)
inspect(head(sort(brules,by='confidence')),n=5)

brulesSub <- subset(brules, subset=!(lhs %in% "Lucky : A Memoir"))
brulesConf <- sort(brulesSub, by="confidence")
inspect(head(lhs(brulesConf), n=5))

