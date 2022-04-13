install.packages("vegan")
library(vegan)
library("ggplot2")
install.packages("Hmisc")
library("Hmisc")

path_spiderdata <- "data/Data_Spider Community Analysis_Faizee Ali Khan - Spider Data.csv"

spiderdata <- read.csv(path_spiderdata, header = TRUE, sep = ",")

list(spiderdata$genus.or.species.code)
unique(spiderdata$genus.or.species.code)

columns = colnames(spiderdata)

spiderdata1 <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(spiderdata1)<-columns



for (i in spiderdata$s.no){
    if (spiderdata$family[i] != 'not identified')
       spiderdata1 <- rbind(spiderdata1,spiderdata[i,])
}
spiderdata1$s.no <- c(1:nrow(spiderdata1))
rownames(spiderdata1) <- c(1:nrow(spiderdata1))
spidergenuses <- unique(spiderdata1$genus.or.species.code)
columnnames <- c("plot", "habitat", spidergenuses)
spidertransposed <- data.frame(matrix(nrow = 0, ncol = length(spidergenuses)+2))
colnames(spidertransposed)<-columnnames


plots <- as.vector(unique(spiderdata1$plot))
j <- 1
for (i in plots) {
    spidertransposed[j,1] <- i
    j = j+1
}
    
spidertransposed[1:3,2] <- "scrubland"
spidertransposed[4:6,2] <- "disturbed"
spidertransposed[7:9,2] <- "lawn"



for (k in spiderdata1$s.no) {
    plotcode <- spiderdata1$plot[k]
    count <- spiderdata1$count[k]
    genus<-spiderdata1$genus.or.species.code[k]
    spidertransposed[which(spidertransposed$plot == plotcode),genus]<-count
    
    }

spidertransposed[is.na(spidertransposed)] <- 0

for(i in 3:ncol(spidertransposed)){
    
        spidertransposed[,i] <- as.numeric(spidertransposed[,i])
    
}
#spidertransposed$habitat <- as.factor(spidertransposed$habitat)



data1 <- spidertransposed[,3:ncol(spidertransposed)]
data2 <- spidertransposed[,1:2]

set.seed(2)
genus_NMDS <- metaMDS(data1, distance = "bray", k = 2)
plot(genus_NMDS)
#plotting
co=c("red","red","red","blue","blue","blue","green","green","green")
shape=c(18,18,18,16,16,16,14,14,14)


data.scores <- as.data.frame(scores(genus_NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- data2$plot  # create a column of site names, from the rownames of data.scores
data.scores$grp <- data2$habitat  #  add the grp variable created earlier
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(genus_NMDS, "species"))
species.scores$species <- rownames(species.scores)


grp.a <- data.scores[data.scores$grp == "scrubland", ][chull(data.scores[data.scores$grp == 
                                                                     "scrubland", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "disturbed", ][chull(data.scores[data.scores$grp == 
                                                                     "disturbed", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.c <- data.scores[data.scores$grp == "lawn", ][chull(data.scores[data.scores$grp == 
                                                                     "lawn", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b, grp.c)  #combine grp.a and grp.b
hull.data

ggplot() + 
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4) + # add the point markers
    scale_colour_manual(values=c("Scrubland" = "red", "Disturbed" = "blue", "Lawn" = "green")) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          axis.title.x = element_text(size=18), # remove x-axis labels
          axis.title.y = element_text(size=18), # remove y-axis labels
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank())

genus_NMDS

#Bootstrapping and testing for differences between the groups
fit <- adonis(data1 ~ habitat, data=data2, permutations=999, method="bray")
fit
#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(data1)
anova(betadisper(distances_data, data2$habitat))

#calculating spider species richness for each plot
k<-0
for (i in 1:9){
    for (j in 3:ncol(spidertransposed)){
        if (spidertransposed[i,j] != 0)
            k <- k+1
    }
    print(k)
    k<-0
}

# calculating spider density for each plot

k<-0
for (i in 1:9){
    for (j in 3:ncol(spidertransposed)){
        
            k <- k + spidertransposed[i,j]
    }
    print(k)
    k<-0
}

# calculating correlation matrix for different variables

path_variables <- "data/Data_Spider Community Analysis_Faizee Ali Khan - PC Sheet.csv"

variable_data <- read.csv(path_variables, header = TRUE, sep = ",")

corr_mat <- rcorr(as.matrix(variable_data[,2:(ncol(variable_data)-1)]), type = "pearson")

flattenCorr <- flattenCorrMatrix(corr_mat$r, corr_mat$P)


# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
    )
}

flattenCorr <- flattenCorrMatrix(corr_mat$r, corr_mat$P)
signi_corr_ind <- which(flattenCorr$p < 0.05 & abs(flattenCorr$cor) > 0.7)
flattenCorr[signi_corr_ind,]


signi_corr_ind <- which(flattenCorr$p < 0.05 & abs(flattenCorr$cor) > 0.7)
