####################################################################
#####                                                           ####
##### SCRIPT FOR DRAWING AND ANALYZING NETWORKS IN R            ####
#####                                                           ####       
####################################################################


##### Authors: Julian Olaya Restrepo & Marcos Barbeitos
##### E-mail: julianolaya80@yahoo.com 
##### Run in R 3.6.1 (2019-07-05)

rm(list=ls())

library("bipartite")
library("ggplot2")
library("network") 
library("png")
library("plyr")
library("reshape2")
library("rstudioapi")
library("permute")
library( "igraph" )
library( "RColorBrewer" )

#############################################

#set working directory to save plots
setwd("~/Banca_evaluadora_Lorena/R_mapas/R_analisis")

#############################################
##### 
##### MPA NETWORK
##### 
#############################################


library( tidyverse )

##### Read data
nodes.mpa <- read.csv("Nodos_MPAs.csv", header=T, as.is=T)
edges.mpa <- read.csv("EDGES_MPA_revLOrena.csv", header=T, as.is=T)

##### Build graph from dataframe
net_mpa <- graph_from_data_frame( d = edges.mpa, vertices = nodes.mpa, directed = FALSE )
summary(net_mpa)

#####  Eliminate nodes that neither answered the questionnare nor were mentioned by other MPAs
vert.isola <- which( degree(net_mpa) == 0)
netMPA <- delete.vertices( net_mpa, vert.isola )

#####  Simplify the graph and remove loops 
# edge are aggregated by: "sum" (The sum of the attributes is calculated)
#netEquipment <- simplify( netEquipment, remove.multiple = T, remove.loops = T)
 #                         edge.attr.comb=list(intencidade="sum", "ignore"))


# density as one of the proxys for connectivity
edge_density(netMPA,loops = FALSE)
# diameter
diameter(netMPA, directed = F, unconnected = TRUE)
# Weighted Degree 
V(netMPA)$weighted_degree = strength(netMPA, v=V(netMPA), mode=c('all'))

# Define colours for nodes based on gobernmental sphere
V(netMPA)$color=V(netMPA)$Country
V(netMPA)$color=gsub("Brazil","green",V(netMPA)$color)
V(netMPA)$color=gsub("Uruguay","deepskyblue",V(netMPA)$color)
V(netMPA)$color=gsub("Argentina","orange",V(netMPA)$color)

# Define colours for edges based on Red
E(netMPA)$color=E(netMPA)$Red
E(netMPA)$color=gsub("Biologico","red",E(netMPA)$color)
E(netMPA)$color=gsub("Gestao","#A6A6A6",E(netMPA)$color)

## Intencity (relevance) used as weight for edge size
w.netMPA <- E(netMPA)$weight / max(E(netMPA)$weight)

## STRENGTH CENTRALITY for node size
MPA.strength <- strength(netMPA,vids = V(netMPA),weights = w.netMPA)

l <- layout_with_fr(netMPA, weights=w.netMPA)

svg("Net_overall.svg")
par(mfrow=c(1,1),las=1,mai=c(0.5,1,.2,0.2))
plot(netMPA,
     vertex.size=((5*MPA.strength)/max(MPA.strength))+3,
     vertex.label.dist=1,
     vertex.label.color="black",
     vertex.label.sex=0.4, 
     edge.curve=0.3,
     edge.width=w.netMPA,
     edge.arrow.width=0.6,
     edge.arrow.size=0.1,
     vertex.label=NULL,
     layout = l
     )
# Close the graphics device
dev.off() 




#############################################
#############################################
### HACER GRAFICO POR TYPE BIOLOGICO Y GESTION
#############################################

#############################################
##### selecciono solo los edges del biologico
edges.bco <- edges.mpa[edges.mpa$Red == 'Biologico', ]

##### Build graph from dataframe
net_bco <- graph_from_data_frame( d = edges.bco, vertices = nodes.mpa, directed = FALSE )
summary(net_bco)

#####  Eliminate nodes that neither answered the questionnare nor were mentioned by other MPAs
vert.isola <- which( degree(net_bco) == 0)
netBCO <- delete.vertices( net_bco, vert.isola )

#####  Simplify the graph and remove loops 
# edge are aggregated by: "sum" (The sum of the attributes is calculated)
#netEquipment <- simplify( netEquipment, remove.multiple = T, remove.loops = T)
#                         edge.attr.comb=list(intencidade="sum", "ignore"))


# density as one of the proxys for connectivity
edge_density(netBCO,loops = FALSE)
# diameter
diameter(netBCO, directed = F, unconnected = TRUE)

# Define colours for nodes based on gobernmental sphere
V(netBCO)$color=V(netBCO)$Country
V(netBCO)$color=gsub("Brazil","green",V(netBCO)$color)
V(netBCO)$color=gsub("Uruguay","deepskyblue",V(netBCO)$color)
V(netBCO)$color=gsub("Argentina","orange",V(netBCO)$color)

## Intencity (relevance) used as weight
w.netBCO <- E(netBCO)$weight / max(E(netBCO)$weight)

## STRENGTH CENTRALITY
netBCO.strength <- strength(netBCO,vids = V(netBCO),weights = w.netBCO)

l.bco <- layout_with_fr(netBCO, weights=w.netBCO)
par(mfrow=c(1,1),las=1,mai=c(0.5,1,.2,0.2))
plot(netBCO,
     vertex.size=(5*netBCO.strength)/max(netBCO.strength)+3,
     vertex.label.color="black",
     vertex.label.sex=0.4, 
     edge.curve=0.3,
     vertex.label.cex=0.9,
     edge.width=w.netBCO,
     edge.color="black",
     edge.arrow.width=0.6,
     edge.arrow.size=0.1,
     vertex.label=NULL,
     layout = l.bco)

#############################################
##### selecciono solo los edges del biologico BALLENA
edges.Ea <- edges.bco[edges.bco$Specie == 'Ea', ]

##### Build graph from dataframe
net_Ea <- graph_from_data_frame( d = edges.Ea, vertices = nodes.mpa, directed = FALSE )
summary(net_Ea)

#####  Eliminate nodes that neither answered the questionnare nor were mentioned by other MPAs
vert.isola <- which( degree(net_Ea) == 0)
netEa <- delete.vertices( net_Ea, vert.isola )

#####  Simplify the graph and remove loops 
# edge are aggregated by: "sum" (The sum of the attributes is calculated)
#netEquipment <- simplify( netEquipment, remove.multiple = T, remove.loops = T)
#                         edge.attr.comb=list(intencidade="sum", "ignore"))


# density as one of the proxys for connectivity
edge_density(netEa,loops = FALSE)
# diameter
diameter(netEa, directed = F, unconnected = TRUE)

# Define colours for nodes based on gobernmental sphere
V(netEa)$color=V(netEa)$Country
V(netEa)$color=gsub("Brazil","green",V(netEa)$color)
V(netEa)$color=gsub("Uruguay","deepskyblue",V(netEa)$color)
V(netEa)$color=gsub("Argentina","orange",V(netEa)$color)

## Intencity (relevance) used as weight
w.netEa <- E(netEa)$weight / max(E(netEa)$weight)

## STRENGTH CENTRALITY
netEa.strength <- strength(netEa,vids = V(netEa),weights = w.netEa)

l.Ea <- layout_with_fr(netEa, weights=w.netEa)
svg("Net_Ea.svg")
par(mfrow=c(1,1),las=1,mai=c(0.5,1,.2,0.2))
plot(netEa,
     vertex.size=degree(netEa)+1,
     vertex.label.color="black",
     vertex.label.sex=0.4, 
     edge.curve=0.3,
     vertex.label.cex=0.9,
     edge.width=w.netEa,
     edge.color="black",
     edge.arrow.width=0.6,
     edge.arrow.size=0.1,
     vertex.label=NULL,
     layout = l.Ea)
dev.off()

#############################################
##### selecciono solo los edges del biologico LEON MARINO
edges.Of <- edges.bco[edges.bco$Specie == 'Of', ]

##### Build graph from dataframe
net_Of <- graph_from_data_frame( d = edges.Of, vertices = nodes.mpa, directed = FALSE )
summary(net_Of)

#####  Eliminate nodes that neither answered the questionnare nor were mentioned by other MPAs
vert.isola <- which( degree(net_Of) == 0)
netOf <- delete.vertices( net_Of, vert.isola )

#####  Simplify the graph and remove loops 
# edge are aggregated by: "sum" (The sum of the attributes is calculated)
#netEquipment <- simplify( netEquipment, remove.multiple = T, remove.loops = T)
#                         edge.attr.comb=list(intencidade="sum", "ignore"))


# density as one of the proxys for connectivity
edge_density(netOf,loops = FALSE)
# diameter
diameter(netOf, directed = F, unconnected = TRUE)

# Define colours for nodes based on gobernmental sphere
V(netOf)$color=V(netOf)$Country
V(netOf)$color=gsub("Brazil","green",V(netOf)$color)
V(netOf)$color=gsub("Uruguay","deepskyblue",V(netOf)$color)
V(netOf)$color=gsub("Argentina","orange",V(netOf)$color)

## Intencity (relevance) used as weight
w.netOf <- E(netOf)$weight / max(E(netOf)$weight)

## STRENGTH CENTRALITY
netOf.strength <- strength(netOf,vids = V(netOf),weights = w.netOf)

l.Of <- layout_with_fr(netOf, weights=w.netOf)
par(mfrow=c(1,1),las=1,mai=c(0.5,1,.2,0.2))
plot(netOf,
     vertex.size=degree(netOf)+2,
     vertex.label.color="black",
     vertex.label.sex=0.4, 
     edge.curve=0.3,
     vertex.label.cex=0.9,
     edge.width=w.netOf,
     edge.color="black",
     edge.arrow.width=0.6,
     edge.arrow.size=0.1,
     vertex.label=NULL,
     layout=l.Of)

#############################################
##### selecciono solo los edges del gestao
edges.gst <- edges.mpa[edges.mpa$Red == 'Gestao', ]

##### Build graph from dataframe
net_gst <- graph_from_data_frame( d = edges.gst, vertices = nodes.mpa, directed = FALSE )
summary(net_gst)

#####  Eliminate nodes that neither answered the questionnare nor were mentioned by other MPAs
vert.isola <- which( degree(net_gst) == 0)
netGST <- delete.vertices( net_gst, vert.isola )

#####  Simplify the graph and remove loops 
# edge are aggregated by: "sum" (The sum of the attributes is calculated)
#netEquipment <- simplify( netEquipment, remove.multiple = T, remove.loops = T)
#                         edge.attr.comb=list(intencidade="sum", "ignore"))


# density as one of the proxys for connectivity
edge_density(netGST,loops = FALSE)
# diameter
diameter(netGST, directed = F, unconnected = TRUE)

# Define colours for nodes based on gobernmental sphere
V(netGST)$color=V(netGST)$Country
V(netGST)$color=gsub("Brazil","green",V(netGST)$color)
V(netGST)$color=gsub("Uruguay","deepskyblue",V(netGST)$color)
V(netGST)$color=gsub("Argentina","orange",V(netGST)$color)

## Intencity (relevance) used as weight
w.netGST <- E(netGST)$weight / max(E(netGST)$weight)

## STRENGTH CENTRALITY
netGST.strength <- strength(netGST,vids = V(netGST),weights = w.netGST)

l.GST <- layout_with_fr(netGST, weights=w.netGST)
par(mfrow=c(1,1),las=1,mai=c(0.5,1,.2,0.2))
plot(netGST,
     vertex.size=degree(netGST)/max(degree(netGST))+6,
     vertex.label.color="black",
     vertex.label.sex=0.4, 
     edge.curve=0.3,
     vertex.label.cex=0.9,
     edge.width=w.netGST,
     edge.color="black",
     edge.arrow.width=0.6,
     edge.arrow.size=0.1,
     vertex.label=NULL,
     layout=l.GST)

