TARGET='GENE' #specify gene target of perturbation screening
GENE='grna-gene' #specify gene name used to identify cells that received a perturbation

EXPRX = GetAssayData(object=data_sub_neg, assay="RNA",slot="data")[GENE,]
test=data.frame(positive= EXPRX>0)
data_sub_neg <- SetIdent(data_sub_neg, value = test$positive)
data1 <- subset(x = data_sub_neg, idents = "TRUE")

p1<- VlnPlot(data_sub_pos, pt.size=0, features=TARGET, cols = c('gray80')) + 
  geom_boxplot(color='black', fill='white',  width=0.1) + 
  theme(legend.position = 'none')+
  ggtitle(paste("All gRNA- \n for",TARGET, "expression")) +
  ylim(0,2)+
  xlab("")+
  theme(axis.ticks.x = element_blank())+
  scale_x_discrete(
    labels=c("gRNA -")
  )

gene_expression = GetAssayData(object = data_sub_pos, assay = "RNA", slot = "data")[TARGET,]
test=data.frame(positive= gene_expression>0)
data_sub_pos <- SetIdent(data_sub_pos, value = test$positive)
N <- subset(x = data_sub_pos, idents = "TRUE")
p3 <- VlnPlot(N, pt.size=0, features=TARGET, cols = c('lightpink2')) + 
  geom_boxplot(color='black', fill='white',  width=0.1) + 
  ylim(0,2)+
  theme(legend.position = 'none')+
  ggtitle(paste("Subset gRNA- \n for",TARGET, "expression")) +
  xlab("")+
  theme(axis.ticks.x = element_blank())+
  scale_x_discrete(
    labels=c("gRNA-")
  )

p2<- VlnPlot(data1, pt.size=0, features=TARGET, cols = c('maroon4')) + 
  geom_boxplot(color='black', fill='white',  width=0.1) + 
  theme(legend.position = 'none')+
  ggtitle(paste("All gRNA+ \n for",TARGET, "expression")) +
  ylim(0,2)+
  xlab("")+
  theme(axis.ticks.x = element_blank())+
  scale_x_discrete(
    labels=c("gRNA+")
  )

gene_expression = GetAssayData(object = data1, assay = "RNA", slot = "data")[TARGET,]
test=data.frame(positive= gene_expression>0)
data1 <- SetIdent(data1, value = test$positive)
M <- subset(x = data1, idents = "TRUE")
p4 <- VlnPlot(M, pt.size=0, features=TARGET, cols = c('#9a004d')) + 
  geom_boxplot(color='black', fill='white',  width=0.1) + 
  theme(legend.position = 'none')+
  ggtitle(paste("Subset gRNA+ \n for",TARGET, "expression")) +
  ylim(0,2)+
  xlab("")+
  theme(axis.ticks.x = element_blank())+
  scale_x_discrete(
    labels=c("gRNA+")
  )

library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol=2)
