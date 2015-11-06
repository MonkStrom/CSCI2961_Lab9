str(Titanic)
head(as.data.frame(Titanic))
titanic.raw <- NULL
for(i in 2:5){
  titanic.raw <- cbind(titanic.raw, rep(as.character(df[,i]), df$Freq))
}
titanic.raw <- as.data.frame(titanic.raw)
names(titanic.raw) <- names(df)[2:5]
dim(titanic.raw)
str(titanic.raw)
summary(titanic.raw)
rules.all <- apriori(titanic.raw)
inspect(rules.all)
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

rules <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st","Class=2nd","Class=3rd",
                                         "Age=Child","Age=Adult"),
                                   default="none"))
                control =list(verbose=F)
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
plot(rules.all)
plot(rules.all,method="grouped")
plot(rules.all,method="graph")
plot(rules.all,method="graph",control=list(type="items"))
plot(rules.all,method="paracoord",control=list(reorder=TRUE))
