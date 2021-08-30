---
title: "Script2"
author: "Boris Mericskay et Florent Demoraes"
date: "27/07/2021"
output: html_document
---

# EXPLORATIONS CARTOGRAPHIQUES À L’ÉCHELLE RÉGIONALE 
---

Ce script a revient sur toutes étapes de manipulation des données DVF sous formes géographiques, les processus d'analyse spatiale et la cartographie.

```{r cars}
library(sf)
library(cartography)
```

### Définition de l'environnement de travail

```{r setup}

knitr::opts_knit$set(root.dir = 'D:/DVF/BZH')
```

### Importer le jeux de données (si nécessaire)

```{r cars}
DVFOK <- read.csv("Exports/DVFOK.csv", stringsAsFactors=FALSE)
```

---
## 1-Spatialisation des données DVF


### Transformer le csv en couche spatiale (objet sf)

```{r cars}
DVFgeo<- st_as_sf(DVFOK, coords=c("longitude","latitude"), crs=4326)
```

### Reprojeter la couche en SCR2154

```{r cars}
DVFgeo<- st_transform(DVFgeo, 2154)
st_crs(DVFgeo)
```

### Ecrire un shapefile (pour une utilisation dans SIG par exemple)

```{r cars}
st_write(DVFgeo, "Exports/MutationsBZH.shp", append=TRUE)
```

---
## 2-Ajout de couches géographique externes pour les analyses/cartes


### Importer la couche des communes (Admin Express IGN) et la reprojeter

```{r cars}
Communes <- st_read(dsn = "DATA/Communes.shp", stringsAsFactors = FALSE)
plot(Communes["INSEE_DEP"])
Communes<- st_transform(Communes, 2154)
st_crs(Communes)
```


### Fusion des communes pour recuperer les contours des departements

```{r cars}
depBZH <- Communes %>% group_by(INSEE_DEP) %>% summarize()
plot(depBZH["INSEE_DEP"])
```

---

## 3-Agrégations spatiales (en mode vectoriel)

### Compter le nombre de mutations par commune

```{r cars}
Communes <- Communes %>% mutate(NbMutations = lengths(st_intersects(Communes, DVFgeo)))
sum(Communes$NbMutations)
```

### Cartographie du nombre de mutations par commune

```{r cars}
par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbMutations", #appel de la variable à cartographier
                 col = "#F97B64", #couleur cercles
                 border = "#FFFFFF",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF par commune") 
```                 

### Compter le nombre de mutations de mutations de maisons et d'appartements

```{r cars}
Maisons <- DVFgeo %>% filter(type=='Maison')
Appartements <- DVFgeo %>% filter(type=="Appartement")

Communes <- Communes %>% mutate(NbMaisons = lengths(st_intersects(Communes, Maisons)))%>% mutate(NbAppart = lengths(st_intersects(Communes, Appartements)))
```


### Cartographie du nombre de ventes de maisons

```{r cars}
par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbMaisons", #appel de la variable à cartographier
                 col = "#2ECC40", #couleur cercles
                 border = "#000000",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF par commune - Maisons") 
```


### Cartographie du nombre de ventes d'appartements

```{r cars}
par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbAppart", #appel de la variable à cartographier
                 col = "#F012BE", #couleur cercles
                 border = "#000000",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF par commune -  Appartements") 

```

### Calculer le prix nominal moyen, le prix moyen au m2 et la surface moyenne par commune

```{r cars}
Communes2 <- Communes %>% st_join(DVFgeo) %>% group_by(INSEE_COM) %>% 
summarise(PrixMoyen = mean(prix), Prixm2Moyen = mean(prixm2), SurfaceMoyenne = mean(surface))
Communes2 <- as.data.frame(Communes2) %>% select(INSEE_COM, PrixMoyen, Prixm2Moyen, SurfaceMoyenne)

CommunesOK <- merge(Communes,Communes2, by="INSEE_COM")
```

### Ecrire un shapefile pour cartographier les données dans un logiciel tiers

```{r cars}
st_write(CommunesOK, "Exports/CommunesBZHDVF.shp", append=TRUE)
st_write(depBZH, "Exports/Departements.shp", append=TRUE)
```


### Cartographie du prix moyen au m2 par commune

```{r cars}
choroLayer(
  x = CommunesOK, 
  var = "Prixm2Moyen", 
  breaks = c(300, 1000, 1500, 2000, 2500, 5000),
  col = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61","#d7191c"),
  lwd = 0.1,
  legend.title.txt = "Prix moyen/m² (euros)")
title(main = "Prix moyen au m² par commune (2014-2019)")
```

### Cartographie du prix moyen par commune

```{r cars}
choroLayer(
  x = CommunesOK, 
  var = "PrixMoyen", 
  breaks = c(40000, 100000, 140000, 160000, 200000, 500000),
  col = c("#2166ac","#67a9cf", "#fddbc7","#f4a582","#ca0020"),
  lwd = 0.1,
  legend.title.txt = "Prix moyen/m² (euros)")
title(main = "Prix moyen des mutations par commune (2014-2019)")
```

---

### Créer un carroyage de 2km et calculer le prix moyen au m2

```{r cars}
grille <- st_make_grid(
  Communes,
  cellsize = 2000,
  crs = 2154,
  what = "polygons",
  square = TRUE)

grille <- st_sf(grille)
grille <- grille %>% mutate(id = row_number())
grille <- grille %>% mutate(IDOK = id)

grillem2 <- grille %>% st_join(DVFgeo) %>%  group_by(IDOK) %>% summarise( Nb_Mutation = n(), prixm2moyen = mean(prixm2))

choroLayer(
  x = grillem2,
  var = "prixm2moyen",
  breaks = c(300, 1000, 1500, 2000, 2500, 5000),
  col = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
  lwd = 0.1,
  legend.title.txt = "Prix moyen au m2 (euros)")
title(main = "Prix moyen au m2 par carreaux de 2km (2014-2019)")
```


### Ecrire un shapefile pour cartographier les données dans un logiciel tiers

```{r cars}
st_write(grillem2, "Exports/grillem2.shp", append=TRUE)

```

---


## 4-Lissage spatial

### Lissage spatial basé sur les communes


### première étape....
```{r cars}
```

### deuxième étape....
```{r cars}
```


### Lissage spatial basé sur les mutations



---

## 5-Classification des sous-marchés (CAH)

### Chargement des librairies

```{r cars}
library(cluster)
```


### Creer un dataframe qui recapitule les variables de la CAH pour chacune des communes

```{r cars}
CommunesDVFClassif1 <- as.data.frame(CommunesOK) %>% select(INSEE_COM, NbMutations, PrixMoyen, Prixm2Moyen, SurfaceMoyenne)
CommunesDVFClassif1 <- CommunesDVFClassif1 %>% na.omit()
CommunesDVFClassif <- CommunesDVFClassif1 %>% select(NbMutations, PrixMoyen, Prixm2Moyen, SurfaceMoyenne)
```

### Variables centrées-réduites

```{r cars}
CommunesDVFClassifscale <- scale(CommunesDVFClassif)
```

### Classification

```{r cars}
CAHCommunes <- agnes(CommunesDVFClassifscale,
                     metric = "euclidean",
                     method = "ward")
```

### Graphiques des gains d'inertie inter-classe

```{r cars}
sortedHeight<- sort(CAHCommunes$height,decreasing= TRUE)
relHeight<-sortedHeight/ sum(sortedHeight)*100
cumHeight<- cumsum(relHeight)

barplot(relHeight[1:30],names.arg=seq(1, 30, 1),col= "black",border= "white",xlab= "Noeuds",ylab= "Part de l'inertie totale (%)")
barplot(cumHeight[1:30],names.arg=seq(1, 30, 1),col= "black",border= "white",xlab= "Nombre de classes",ylab= "Part de l'inertie totale (%)")
```


### Dendogramme

```{r cars}
dendroCSP <- as.dendrogram(CAHCommunes)
plot(dendroCSP, leaflab = "none")

```

### Partition (en n classes)

```{r cars}
clusCSP <- cutree(CAHCommunes, k = 5)
CommunesCluster <- as.data.frame(CommunesDVFClassif1)
CommunesCluster$CLUSIMMO <- factor(clusCSP,
                                   levels = 1:5,
                                   labels = paste("CLUS", 1:5))
```

### Tableau reapitulatif des groupes

```{r cars}
RecapCAHRegion <- CommunesCluster %>% group_by(CLUSIMMO) %>% summarise(Nbcommunes= n(), NbMutationsmoyen = mean(NbMutations), Prixmoyen = mean(PrixMoyen), Prixm2moyen = mean(Prixm2Moyen), Surfacemoyenne=mean(SurfaceMoyenne))
```

### Graphique de variations à la moyenne

```{r cars}
SyntheseCAHRegion <- RecapCAHRegion %>% mutate(
  NbmutationBZH = mean(CommunesDVFClassif$NbMutations),
  SurfaceBZH = mean(CommunesDVFClassif$SurfaceMoyenne),
  PrixBZH = mean(CommunesDVFClassif$PrixMoyen),
  Prixm2BZH = mean(CommunesDVFClassif$Prixm2Moyen),
  NbMutation=(NbMutationsmoyen - NbmutationBZH)/NbmutationBZH*100,
  Prix=(Prixmoyen- PrixBZH)/PrixBZH*100,
  Prixm2=(Prixm2moyen- Prixm2BZH)/Prixm2BZH*100,
  Surface=(Surfacemoyenne- SurfaceBZH)/SurfaceBZH*100,
)

SyntheseCAHRegion <- SyntheseCAHRegion %>% select(CLUSIMMO, Surface, Prix, Prixm2)
gather <- SyntheseCAHRegion %>% gather(key=variable, value= "value", Surface:Prixm2)

ggplot(gather, aes(x=variable, y=value, fill=CLUSIMMO)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values=c("#416979","#39a699","#f9c155","#e45c3a","#7a0218","#7a0218")) +
  ylab("Variation a la moyenne (%)") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~CLUSIMMO, ncol = 1)
```

### Cartographie de la CAH 

```{r cars}
CommunesDVFCAH <- merge(Communes, CommunesCluster, by= "INSEE_COM")

par(mar=c(0,0,0.9,0))

typoLayer(
  x = CommunesDVFCAH,
  var="CLUSIMMO",
  col = c("#416979","#39a699","#f9c155","#e45c3a","#7a0218","#7a0218"),
  lwd = .7,
  legend.values.order = c("CLUS 1",
                          "CLUS 2",
                          "CLUS 3",
                          "CLUS 4",
                          "CLUS 5"),
  legend.pos = "bottomleft",
  legend.title.txt = "Sous-marchés immobiliers")
title(main = "Sous-marchés immobiliers (CAH)")
```

### Ecrire un shapefile de la CAH spatialisée pour cartographie dans un logiciel tiers

```{r cars}
st_write(CommunesDVFCAH, "Exports/CommunesBZHDVF.shp", append=TRUE)
```

---

## 6-Cartogramme lissé

### Chargement des librairies

```{r cars}
library(cluster)
```
