#Code for paper "Comparing networks generated through...."
#Elspeth Ready, 2019/09/20


### set-up ###

library(statnet)
library(intergraph)
library(scales)
setwd("Dropbox/PR_Elspeth Ready/Formal-Informal Networks/Analysis")


### data prep and organization ###

#load ethnographic data
ethnodata <-read.csv("ethno_edges.csv", header=TRUE, stringsAsFactors=FALSE)
#make network
ethnog <- network(ethnodata[,1:2], matrix.type="edgelist", directed=FALSE)

#load network supplement data
nsdata <-read.csv("P2NS_edges.csv", header=TRUE, stringsAsFactors=FALSE)
#get all network members (need to add two isolate KRs)
P2allnamed <- sort(c(unique(c(nsdata$Sender_ID, nsdata$Receiver_ID)), "P1015", "P1063"))
nsg <- network.initialize(n=length(P2allnamed), directed=FALSE, multiple=FALSE)
nsg%v%"vertex.names" <- P2allnamed
senderindex <- match(nsdata$Sender_ID, P2allnamed)
receiverindex <- match(nsdata$Receiver_ID, P2allnamed)
add.edges(nsg, tail=senderindex, head=receiverindex)
#a hack to solve multiple edges problem
temp <- asIgraph(nsg)
library(igraph)
temp <- simplify(temp)
detach(package:igraph)
nsg <- asNetwork(temp)

#get interview data table to count respondents and get attributes
surveydat <- read.csv("PR_p1p2_combined.csv", header=TRUE, stringsAsFactors=FALSE)
SI <- paste0(surveydat$P2SIPrjName, surveydat$P2FIPrjName)
gettingRs <- cbind.data.frame(ID=surveydat$ProjectIDs, SI, KEY=surveydat$P2KEY, stringsAsFactors=FALSE)
allRs <- gettingRs[which(gettingRs$ID != "" & gettingRs$SI != "" & gettingRs$KEY != "" ),]
allRs <- allRs[which(!(allRs$ID %in% c("P1106", "P2041", "P1727"))),]
#do not include 1106 (no interview) and 1727 (NS refusal) who did not complete the P2 interviews, also 2041 who was a curiosity sample
#also excludes four individuals who completed some interviews but never were assigned a ProjectID

#organize location data for ethnographic network
sortedethnodat <- unique(data.frame(ID=c(ethnodata$Sender.ID, ethnodata$Receiver.ID),
                                    Nickname=c(ethnodata$Sender.Nickname, ethnodata$Receiver.Nickname),
                                    Location=c(ethnodata$Town), stringsAsFactors = FALSE))
sortedethnodat <- sortedethnodat[match(ethnog%v%"vertex.names", sortedethnodat$ID),]
sortedethnodat$Location <- factor(sortedethnodat$Location)
#flag individuals in ethno but not NS
sortedethnodat$Location[which(sortedethnodat$ID %in% c("9990", "9991", "9992", "9993"))] <- NA 
#sanity checks
sortedethnodat$ID == ethnog%v%"vertex.names" #all TRUE
duplicated(sortedethnodat$ID) #all FALSE

#sort data for NS net
nsgverts <- nsg%v%"vertex.names"
samplediff <- nsgverts %in% (ethnog%v%"vertex.names")
nsgattr <- data.frame(nsgverts, samplediff)
nsgattr <- merge(nsgattr, sortedethnodat, by.x="nsgverts", by.y="ID", all.x=TRUE, all.y=FALSE)

#create nets with INTERSECTING individuals
subsetethno <- which(!((ethnog%v%"vertex.names") %in% (nsg%v%"vertex.names")))
subethno <- ethnog
delete.vertices(subethno, subsetethno)
subethnolocs <- nsgattr$Location[match(subethno%v%"vertex.names",nsgattr$nsgverts)]

subsetnsg <- which(!((nsg%v%"vertex.names") %in% (ethnog%v%"vertex.names")))
subnsg <- nsg
delete.vertices(subnsg, subsetnsg)
subnsglocs <- nsgattr$Location[match(subnsg%v%"vertex.names",nsgattr$nsgverts)]

#get information on the key respondents
KRdats <- read.csv("PR_KRs.csv", header=TRUE)
KRs <- as.character(KRdats$ProjectIDs)
nsgKR <- nsg%v%"vertex.names" %in% KRs
ethnoKR <- ethnog%v%"vertex.names" %in% KRs
subnsgKR <- subnsg%v%"vertex.names" %in% KRs
subethnoKR <- subethno%v%"vertex.names" %in% KRs

#get injection frequency and gender
attributes <- surveydat[which(surveydat$ProjectIDs %in% (nsg%v%"vertex.names")),]
attributes <- attributes[order(attributes$ProjectIDs),]
rownames(attributes) <- 1:length(attributes$ProjectIDs)

#gender
attributes$P2SIGEN <- replace(attributes$P2SIGEN, which(attributes$P2SIGEN==9), NA)
### one line of code removed here for privacy reasons ###
gendata <- data.frame(FI=as.numeric(attributes$P2FIGEN), SI=as.numeric(attributes$P2SIGEN), OTHER=as.numeric(attributes$GEN))
GEN <- factor(apply(gendata, 1, max, na.rm=TRUE), levels=c(1,2)) #warnings OK.
for (i in 1:length(GEN)) {
  if (is.na(GEN[i])) {
    temp <- (nsdata$P2NS_1[which(nsdata$Receiver_ID == attributes$ProjectIDs[i])])
    GEN[i] <- temp[1]
  }
}
attributes$FINALGEN <- GEN

temp <- as.numeric(attributes$FINALGEN[match(ethnog%v%"vertex.names", attributes$ProjectIDs)])
temp[1:3] <- c(1,1,2) #coded from names in ethnographic data
ethnog%v%"GEN" <- temp
nsg%v%"GEN" <- as.numeric(attributes$FINALGEN[match(nsg%v%"vertex.names", attributes$ProjectIDs)])
subethno%v%"GEN" <- as.numeric(attributes$FINALGEN[match(subethno%v%"vertex.names", attributes$ProjectIDs)])
subnsg%v%"GEN"<- as.numeric(attributes$FINALGEN[match(subnsg%v%"vertex.names", attributes$ProjectIDs)])

#injection frequency
ij <- attributes$P2SIID2 #get from P2 second interview
for (i in 1:length(attributes$ProjectIDs)){
  if (is.na(ij[i])) { #if no value, look for P2 first interview
    ij[i] <- attributes$P2FIID2[i]
  }
  #tempij <- ij
  if (is.na(ij[i])) { #if still no value, get P1 data
    temp <- attributes$ID2[i]
    if (!is.na(temp)) {
      #need to recode to match across phases
      if (temp == 9) {temp <- 99}
      if (temp == 8) {temp <- 10}
      if (temp < 8) {temp <- temp+1}
      ij[i] <- temp
    }
  }
}
ij[ij==1] <- 2
ij[ij==10] <- 9
ij <- ij-1
attributes$FREQ <- 9-ij #reverse ordering (higher=more)

#tempij[tempij==10] <- 9
#attributes$IJtemp <- 9-tempij #reverse ordering (higher=more)

ethnog%v%"FREQ" <- as.numeric(as.character(attributes$FREQ[match(ethnog%v%"vertex.names", attributes$ProjectIDs)]))
nsg%v%"FREQ" <- as.numeric(as.character(attributes$FREQ[match(nsg%v%"vertex.names", attributes$ProjectIDs)]))
subethno%v%"FREQ" <- as.numeric(as.character(attributes$FREQ[match(subethno%v%"vertex.names", attributes$ProjectIDs)]))
subnsg%v%"FREQ"<- as.numeric(as.character(attributes$FREQ[match(subnsg%v%"vertex.names", attributes$ProjectIDs)]))


### data and methods section ###

#sample numbers in data and methods section
sum(table(nsdata$Sender_ID)==9) #13
length(allRs$ID) #116
sum(table(nsdata$Sender_ID)==9)/length(allRs$ID) #13/116

#calculate mean income
incs <- data.frame(ID=surveydat$ProjectIDs, FIinc=surveydat$P2FIDM9, FIben=surveydat$P2FIDM9P, SIinc=surveydat$P2SIDM9, SIben=surveydat$P2SIDM9P)
incs <- incs[!(incs$ID==""),] #this figure includes all survey respondents except those with no ID
FInc <- na.omit(incs$FIinc + incs$FIben)
SInc <- na.omit(incs$SIinc + incs$SIben)
mean(c(FInc, SInc))

#figure 1
ly1 <- network.layout.fruchtermanreingold(ethnog, layout.par=NULL)
ly2 <- network.layout.fruchtermanreingold(nsg, layout.par=NULL)
palette(c("black", "turquoise4", "coral", "white"))
pdf("ethno_nets_small.pdf", height=2, width=6)
par(mfrow=c(1,4), mar=c(1,0,2,1))
col1 <- as.numeric(sortedethnodat$Location)+1
col1[is.na(col1)] <- 4
plot.network(ethnog, vertex.cex=as.numeric(ethnoKR)*0.650+1.2, vertex.col=col1, label.cex=0.75, label="", 
             label.pos=2, main="(a) Ethnographic network", cex.main=0.75, coord=rbind(c(-25,-15), c(15,14), c(12,12), ly2[which(((nsg%v%"vertex.names") %in% (ethnog%v%"vertex.names"))),]), edge.col="grey50")
col2 <- as.numeric(nsgattr$Location)+1
col2[is.na(col2)] <- 4
plot.network(nsg, vertex.cex=as.numeric(nsgKR)*0.650+1.2, vertex.col=col2, label.cex=0.75, label="", 
             label.pos=2, main="(b) Sociometric network", cex.main=0.75, coord=ly2, edge.col="grey50")
plot.network(subethno, vertex.cex=as.numeric(subethnoKR)*0.650+1.2, label.cex=0.75, vertex.col=as.numeric(subethnolocs)+1,label="", 
             label.pos=2, main="(c) Subsetted ethno. network", cex.main=0.75, coord=ly2[which(((nsg%v%"vertex.names") %in% (subethno%v%"vertex.names"))),], edge.col="grey50")
plot.network(subnsg,vertex.cex=as.numeric(subnsgKR)*0.650+1.2, label.cex=0.75, vertex.col=as.numeric(subnsglocs)+1, label="", 
             label.pos=2, main="(d) Subsetted sociometric network", cex.main=0.75, coord=ly2[-which(!((nsg%v%"vertex.names") %in% (ethnog%v%"vertex.names"))),], edge.col="grey50")
legend("bottomright", c("Site 1", "Site 2"), pt.cex=1.1, pt.bg=c(2,3), pch=21, cex=0.8)
dev.off()

pdf("ethno_nets_inset.pdf", height=1, width=1)
par(mar=c(0,0,0,0))
plot.network(ethnog, vertex.cex=as.numeric(ethnoKR)*0.650+1.2, vertex.col=col1, label="", 
             label.pos=2, edge.col="grey50")
dev.off()

#### results section 1 (overlaps) ###

#number of nodes
ethnovsize <- network.size(ethnog)
nsvsize <- network.size(nsg)
subethnovsize <- network.size(subethno)
subnsgsize <- network.size(subnsg)

#number of overlapping nodes
ethnoinns <- sum((ethnog%v%"vertex.names") %in% (nsg%v%"vertex.names"))
nsinethno <- sum((nsg%v%"vertex.names") %in% (ethnog%v%"vertex.names"))
subethnoinns <- sum((subethno%v%"vertex.names") %in% (subnsg%v%"vertex.names"))
subnsinethno <- sum((subnsg%v%"vertex.names") %in% (subethno%v%"vertex.names"))

#combine in table with percent
netsummary <- data.frame(c(ethnovsize, nsvsize, subethnovsize, subnsgsize), c(ethnoinns/ethnovsize, nsinethno/nsvsize, subethnoinns/subethnovsize, subnsinethno/nsinethno), row.names=c("Ethno", "Formal", "Sub-ethno", "Sub-formal"))
colnames(netsummary) <- c("size", "nodeoverlap")

#number of edges
netsummary$edgecount <- c(network.edgecount(ethnog), network.edgecount(nsg), network.edgecount(subethno), network.edgecount(subnsg))

#number overlapping edges (more complicated, use igraph to make easier)
library(igraph)
ieth <- asIgraph(ethnog, vnames="vertex.names")
ethlist <- get.edgelist(ieth)
insg <- asIgraph(nsg, vnames="vertex.names")
nsglist <- get.edgelist(insg)
isubeth <- asIgraph(subethno, vnames="vertex.names")
subethlist <- get.edgelist(isubeth)
isubnsg <- asIgraph(subnsg, vnames="vertex.names")
subnsglist <- get.edgelist(isubnsg)

ethnew <- paste(ethlist[,1], ethlist[,2], sep="-")
ethnewdouble <- c(paste(ethlist[,1], ethlist[,2], sep="-"), paste(ethlist[,2], ethlist[,1], sep="-"))
nsgnew <- paste(nsglist[,1], nsglist[,2],sep="-")
nsgnewdouble <- c(paste(nsglist[,1], nsglist[,2], sep="-"), paste(nsglist[,2], nsglist[,1], sep="-"))

overlapedges <- numeric(length(ethnew))
for (i in 1:length(ethnew)){
  if (ethnew[i] %in% nsgnew) {
    overlapedges[i] <- 1
  }
  else {
    overlapedges[i] <- 0
  }
}
overlapfull <- ethnewdouble[which(overlapedges==1)]
length(overlapfull) #74 overlapping edges!

overlapfullmat <- matrix(unlist(strsplit(overlapfull, "-")), ncol=2, byrow=TRUE)
overlapnet1 <- graph.edgelist(overlapfullmat)
missvert <- V(overlapnet1)$name[!V(isubeth)$name %in% V(overlapnet1)$name]
overlapnet1 <- add.vertices(overlapnet1, 5, attr=list(name=missvert))
detach(package:igraph)

ol <- length(overlapfull)
netsummary$edgeoverlap <- c(ol/netsummary$edgecount)

netsummary #table 1

#number of key respondents
sum(nsgKR) #socio. network
sum(ethnoKR) #ethno. network

#QAP test
gcor(list(subethno, subnsg))
qt1 <- qaptest(list(subethno, subnsg), gcor, g1=1, g2=2, reps=10000)
qt1


### results section 2 (centrality) ###

#get degree dists
ethnodeg <- degree(ethnog, gmode="graph")
nsdeg <- degree(nsg, gmode="graph")
subethnodeg <- degree(subethno, gmode="graph")
subnsdeg <- degree(subnsg, gmode="graph")

#degree distribution plots
ethnodens <- density(log2(ethnodeg+1), bw=0.25)
nsdens <- density(log2(nsdeg+1), bw=0.25)
subethnodens <- density(log2(subethnodeg+1), bw=0.25)
subnsdens <- density(log2(subnsdeg+1), bw=0.25)
pdf("degree_dist_small.pdf", width=4, height=2, pointsize=8)
par(mar=c(4,4,3,0), mfrow=c(1,2))
hist(log2(ethnodeg+1), freq=FALSE, xlim=c(0,6), ylim=c(0,1), cex.axis=0.75,
     border="darkgreen", col=alpha("darkgreen", 0.3),
     main="Complete networks",
     xlab="log2(degree+1)", cex.main=0.9)
hist(log2(nsdeg+1), freq=FALSE, border="slateblue4", col=alpha("slateblue4", 0.4), add=TRUE)
lines(ethnodens, lwd=2, col="darkgreen")
lines(nsdens, lwd=2, col="slateblue4")
par(mar=c(4,2,3,2))
hist(log2(subethnodeg+1), freq=FALSE, xlim=c(0,6), ylim=c(0,1),
     border="darkgreen", col=alpha("darkgreen", 0.3),
     main="Subsetted networks", cex.axis=0.75,
     xlab="log2(degree+1)", ylab="", cex.main=0.9)
hist(log2(subnsdeg+1), freq=FALSE, border="slateblue4", col=alpha("slateblue4", 0.4), add=TRUE)
lines(subethnodens, lwd=2, col="darkgreen")
lines(subnsdens, lwd=2, col="slateblue4")
legend(3.5, 1, c("Ethno.", "Socio."), lwd=2, col=c("darkgreen", "slateblue4"), cex=0.7)
dev.off()

#correlation of degree
degdf1 <- data.frame(ID=ethnog%v%"vertex.names", ethnodeg)
degdf2 <- data.frame(ID=nsg%v%"vertex.names", nsdeg)
degdf <- merge(degdf1, degdf2, by="ID", all.x=TRUE, all.y=TRUE)
cor.test(degdf$ethnodeg, degdf$nsdeg)

#most central by degree
degdf <- degdf[order(degdf$nsdeg, degdf$ethnodeg, decreasing=TRUE),]
degdf[1:50,]
length(na.omit(degdf$ethnodeg[1:20]))
length(na.omit(degdf$ethnodeg[1:50]))

#degree stats
meandeg <- c(round(mean(ethnodeg), 2), round(mean(nsdeg), 2), 
             round(mean(subethnodeg), 2), round(mean(subnsdeg), 2))
sddeg <- c(round(sd(ethnodeg), 2), round(sd(nsdeg), 2), 
           round(sd(subethnodeg), 2), round(sd(subnsdeg), 2))
maxdeg <- c(max(ethnodeg), max(nsdeg), max(subethnodeg), max(subnsdeg))
tab2 <- data.frame(meandeg, sddeg, maxdeg)

#correlation tests for frequency and degree
ethf <- cor.test(ethnodeg, ethnog%v%"FREQ", method="spearman", na.rm=TRUE)
nsf <- cor.test(nsdeg, nsg%v%"FREQ", method="spearman", na.rm=TRUE)
sethf <- cor.test(subethnodeg, subethno%v%"FREQ", method="spearman")
snsf <- cor.test(subnsdeg, subnsg%v%"FREQ", method="spearman")
tab2$R <- c(round(ethf$estimate, 2), round(nsf$estimate, 2), round(sethf$estimate, 2), round(snsf$estimate, 2))
tab2$p <- c(round(ethf$p.value, 3), round(nsf$p.value, 3), round(sethf$p.value, 3), round(snsf$p.value, 3))

#transitivity
library(igraph) #again...
tab2$globtrans <- c(round(transitivity(ieth, type="global"), 2), 
                    round(transitivity(insg, type="global"), 2),
                    round(transitivity(isubeth, type="global"), 2),
                    round(transitivity(isubnsg, type="global"), 2))
detach(package:igraph)

tab2$tri <- c(summary(ethnog~triangle), summary(nsg~triangle), summary(subethno~triangle), summary(subnsg~triangle))

#table 2, voila...
tab2

#mean deg of different samples
mean(ethnodeg[which(ethnog%v%"vertex.names" %in% KRs)]) #4.1
mean(ethnodeg[-which(ethnog%v%"vertex.names" %in% KRs)]) #1.6

SRnoKR <- allRs$ID[-which(allRs$ID %in% KRs)]

mean(nsdeg[which(nsg%v%"vertex.names" %in% KRs)]) #6.3
mean(nsdeg[which(nsg%v%"vertex.names" %in% SRnoKR)]) #5.6
mean(nsdeg[-which(nsg%v%"vertex.names" %in% allRs$ID)]) #1.2

t.test(nsdeg[which(nsg%v%"vertex.names" %in% KRs)], nsdeg[which(nsg%v%"vertex.names" %in% SRnoKR)], alternative="greater")

#mostly due to indiv with highest degree...
mean(nsdeg[which(nsg%v%"vertex.names" %in% KRs & nsdeg!=max(nsdeg))])

#how many high deg indivs are KRs?
sum(degdf$ID[1:50] %in% KRs)

#betweenness
ebet <- data.frame(names=ethnog%v%"vertex.names",
                   ebet=betweenness(ethnog, gmode="graph", cmode="undirected"))
nbet <- data.frame(names=nsg%v%"vertex.names",
                   nsbet=betweenness(nsg, gmode="graph", cmode="undirected"))
allbets <- merge(ebet, nbet, all=TRUE)
allbets <- allbets[order(allbets$nsbet, allbets$ebet, decreasing=TRUE),]
cor.test(allbets$ebet, allbets$nsbet)

#betweenness within study locales
subnsg%v%"index" <- 1:78
edgess <- data.frame(as.edgelist(subnsg))
e1 <- (subnsg%v%"vertex.names")[edgess[,1]]
e2 <- (subnsg%v%"vertex.names")[edgess[,2]]
newes <- data.frame(e1, e2)
newes <- newes[-which(paste(edgess[,1], edgess[,2]) %in% c("19 24", "24 67", "67 76")),]
netnewes <- as.network(newes)
add.vertices(netnewes, 1)
tempnames <- (netnewes%v%"vertex.names")
tempnames[78] <- ((subnsg%v%"vertex.names")[which(!((subnsg%v%"vertex.names") %in% (netnewes%v%"vertex.names")))])
netnewes%v%"vertex.names" <- tempnames
esubbet <- data.frame(names=subethno%v%"vertex.names",
                      esubbet=betweenness(subethno, gmode="graph", cmode="undirected"))
withinbet <- data.frame(names=netnewes%v%"vertex.names",
                        withinbet=betweenness(netnewes, gmode="graph", cmode="undirected"))
allbets3 <- merge(esubbet, withinbet, all=TRUE)
cor.test(allbets3$esubbet, allbets3$withinbet)

#top ranks of betweenness
allbets[1:50,]
length(na.omit(allbets$ebet[1:20]))
length(na.omit(allbets$ebet[1:50]))

### results section 3 (ethno. inter) ###

#see table 2....

#men vs women
sum(ethnog%v%"GEN" == 2)
sum(nsg%v%"GEN" == 2)
sum(subethno%v%"GEN"== 2)
mean(ethnodeg[which(ethnog%v%"GEN"==2)])
mean(ethnodeg[which(ethnog%v%"GEN"==1)])
mean(nsdeg[which(nsg%v%"GEN"==2)])
mean(nsdeg[which(nsg%v%"GEN"==1)])

### results section 3 (transitivity) ###

#see table 2 again...

#triangles per person
count_triangles <- function(net) {
  egos <- ego.extract(nsg, neighborhood="combined")
  tris <- numeric(length=length(egos))
  i = 0
  for (dat in egos) {
    i <- i + 1
    tris[i] <- summary(network(dat, matrix.type="adjacency", loops=FALSE, directed=FALSE) ~ triangle)
  }
  return(tris)
}

iethtris <- count_triangles(ethnog)
insgtris <- count_triangles(nsg)
isubethtris <- count_triangles(subethno)
isubnsgtris <- count_triangles(subnsg)

#ids of survey respondents in the ethno net
survinethno <- allRs$ID[allRs$ID %in% (ethnog%v%"vertex.names")]
survnotinethno <- allRs$ID[-which(allRs$ID %in% (ethnog%v%"vertex.names"))]

#who has no triangles?
sum(insgtris[-which(nsg%v%"vertex.names" %in% allRs$ID)]==0) #74
sum(insgtris[which(nsg%v%"vertex.names" %in% allRs$ID)]==0) #34

#compare transitivity surrounding survey respondents in and out of ethnog
mean(insgtris[which(nsg%v%"vertex.names" %in% survinethno)]) 
mean(insgtris[which(nsg%v%"vertex.names" %in% survnotinethno)]) 

#key respondents vs non key-respondents
mean(insgtris[which(nsg%v%"vertex.names" %in% KRs)])
mean(insgtris[which(nsg%v%"vertex.names" %in% SRnoKR)]) 
t.test(insgtris[which(nsg%v%"vertex.names" %in% KRs)], insgtris[which(nsg%v%"vertex.names" %in% SRnoKR)], alternative="greater")
#without the central indiv
mean(insgtris[which(nsg%v%"vertex.names" %in% KRs & insgtris!=max(insgtris))])

     