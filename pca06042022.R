# Utilized R code from the following link: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(ggplot2)
library(dplyr)
library(devtools)
library(ggbiplot)
library(caret)
library(ggpubr)
library(factoextra)
library(corrplot)

totaldata <- read.csv("/Users/andresbonilla/Desktop/PACS.June 1 adjusted.csv", row.names = NULL)
totaldata <- totaldata [-c(1,2,4)]
totaldata[,2:25][totaldata[,2:25] > 0] <- 1

commonsymptomsdata <- read.csv("/Users/andresbonilla/Desktop/PACS.June 1 adjusted 13 Sx.csv", row.names = NULL)
commonsymptomsdata <- commonsymptomsdata[-c(1,2,4)]
commonsymptomsdata[,2:14][commonsymptomsdata[,2:14] > 0] <- 1

 
maletotaldata <- filter(totaldata, Gender == "Male")
malecommonsymptoms <- filter(commonsymptomsdata, Gender == "Male")
femaletotaldata <- filter(totaldata, Gender == "Female")
femalecommonsymptoms <- filter(commonsymptomsdata, Gender == "Female")


gendersortedtotaldata <- totaldata[order(totaldata$Gender),]
gendersortedsymptoms <- commonsymptomsdata[order(commonsymptomsdata$Gender),]

maletotaldata.pca <- prcomp(maletotaldata[,c(2:25)], center = TRUE, scale. = TRUE)
fviz_eig(maletotaldata.pca)
fviz_pca_ind(maletotaldata.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(maletotaldata.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
var <- get_pca_var(maletotaldata.pca)
corrplot(var$cos2, is.corr=FALSE) 

get_eig(maletotaldata.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   4.6816858       19.5070243                    19.50702
# Dim.2   2.6106382       10.8776590                    30.38468
# Dim.3   1.8489646        7.7040190                    38.08870
# Dim.4   1.7059523        7.1081345                    45.19684
# Dim.5   1.6557244        6.8988518                    52.09569
# Dim.6   1.5605597        6.5023321                    58.59802
# Dim.7   1.2459963        5.1916513                    63.78967
# Dim.8   1.0155044        4.2312682                    68.02094
# Dim.9   0.9868231        4.1117631                    72.13270
# Dim.10  0.9582462        3.9926925                    76.12540
# Dim.11  0.7926623        3.3027595                    79.42816
# Dim.12  0.7256782        3.0236591                    82.45181
# Dim.13  0.6445961        2.6858170                    85.13763
# Dim.14  0.5427322        2.2613840                    87.39902
# Dim.15  0.5029698        2.0957074                    89.49472
# Dim.16  0.4750477        1.9793655                    91.47409
# Dim.17  0.3957926        1.6491360                    93.12322
# Dim.18  0.3307063        1.3779429                    94.50117
# Dim.19  0.3252104        1.3550434                    95.85621
# Dim.20  0.2855463        1.1897764                    97.04599
# Dim.21  0.2519503        1.0497928                    98.09578
# Dim.22  0.1982640        0.8261000                    98.92188
# Dim.23  0.1519837        0.6332656                    99.55515
# Dim.24  0.1067651        0.4448546                   100.00000

malecommonsymptoms.pca <- prcomp(malecommonsymptoms[,c(2:14)], center = TRUE, scale. = TRUE)
fviz_eig(malecommonsymptoms.pca)
fviz_pca_ind(malecommonsymptoms.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(malecommonsymptoms.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
var <- get_pca_var(malecommonsymptoms.pca)
corrplot(var$cos2, is.corr=FALSE) 

get_eig(malecommonsymptoms.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   2.5252552        19.425040                    19.42504
# Dim.2   1.9368078        14.898522                    34.32356
# Dim.3   1.3309877        10.238367                    44.56193
# Dim.4   1.2470133         9.592410                    54.15434
# Dim.5   1.1049621         8.499708                    62.65405
# Dim.6   0.9714113         7.472394                    70.12644
# Dim.7   0.8245888         6.342991                    76.46943
# Dim.8   0.7731608         5.947391                    82.41682
# Dim.9   0.6449272         4.960978                    87.37780
# Dim.10  0.5323284         4.094834                    91.47264
# Dim.11  0.4163114         3.202396                    94.67503
# Dim.12  0.3946604         3.035849                    97.71088
# Dim.13  0.2975855         2.289119                   100.00000

femaletotaldata.pca <- prcomp(femaletotaldata[,c(2:25)], center = TRUE, scale. = TRUE)
fviz_eig(femaletotaldata.pca)
fviz_pca_ind(femaletotaldata.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(femaletotaldata.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
var <- get_pca_var(femaletotaldata.pca)
corrplot(var$cos2, is.corr=FALSE) 

get_eig(femaletotaldata.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   4.6027055       19.1779394                    19.17794
# Dim.2   2.1036129        8.7650535                    27.94299
# Dim.3   1.8635209        7.7646706                    35.70766
# Dim.4   1.7877645        7.4490188                    43.15668
# Dim.5   1.5447364        6.4364017                    49.59308
# Dim.6   1.4111079        5.8796163                    55.47270
# Dim.7   1.2025466        5.0106109                    60.48331
# Dim.8   1.1501899        4.7924580                    65.27577
# Dim.9   1.0284390        4.2851624                    69.56093
# Dim.10  0.8784802        3.6603341                    73.22127
# Dim.11  0.8572869        3.5720288                    76.79329
# Dim.12  0.7293887        3.0391196                    79.83241
# Dim.13  0.6961430        2.9005958                    82.73301
# Dim.14  0.6264801        2.6103337                    85.34334
# Dim.15  0.5695331        2.3730547                    87.71640
# Dim.16  0.5158133        2.1492222                    89.86562
# Dim.17  0.4812888        2.0053699                    91.87099
# Dim.18  0.4271203        1.7796679                    93.65066
# Dim.19  0.3687381        1.5364086                    95.18707
# Dim.20  0.3227275        1.3446978                    96.53176
# Dim.21  0.2606012        1.0858382                    97.61760
# Dim.22  0.2169317        0.9038820                    98.52148
# Dim.23  0.2007397        0.8364156                    99.35790
# Dim.24  0.1541039        0.6420995                   100.00000

femalecommonsymptoms.pca <- prcomp(femalecommonsymptoms[,c(2:14)], center = TRUE, scale. = TRUE)
fviz_eig(femalecommonsymptoms.pca)
fviz_pca_ind(femalecommonsymptoms.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(femalecommonsymptoms.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
var <- get_pca_var(femalecommonsymptoms.pca)
corrplot(var$cos2, is.corr=FALSE) 

get_eig(femalecommonsymptoms.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   3.0909847        23.776805                    23.77681
# Dim.2   1.4767660        11.359738                    35.13654
# Dim.3   1.4386507        11.066544                    46.20309
# Dim.4   1.1553759         8.887507                    55.09059
# Dim.5   1.1118219         8.552476                    63.64307
# Dim.6   0.8773967         6.749205                    70.39228
# Dim.7   0.8257220         6.351707                    76.74398
# Dim.8   0.6925440         5.327262                    82.07124
# Dim.9   0.6552989         5.040761                    87.11201
# Dim.10  0.5378643         4.137418                    91.24942
# Dim.11  0.4838669         3.722053                    94.97148
# Dim.12  0.3463001         2.663847                    97.63532
# Dim.13  0.3074079         2.364676                   100.00000

gendersortedtotaldata.pca <- prcomp(gendersortedtotaldata[,c(2:25)], center = TRUE, scale. = TRUE)
fviz_eig(gendersortedtotaldata.pca)
fviz_pca_ind(gendersortedtotaldata.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(gendersortedtotaldata.pca, 
                col.ind = gendersortedsymptoms$Gender, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Gender") 
var <- get_pca_var(gendersortedtotaldata.pca)
corrplot(var$cos2, is.corr=FALSE) 

get_eig(gendersortedtotaldata.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   4.5117196        18.798832                    18.79883
# Dim.2   1.9336267         8.056778                    26.85561
# Dim.3   1.7564707         7.318628                    34.17424
# Dim.4   1.5111666         6.296527                    40.47076
# Dim.5   1.3829391         5.762246                    46.23301
# Dim.6   1.3097275         5.457198                    51.69021
# Dim.7   1.2209032         5.087097                    56.77731
# Dim.8   1.0875335         4.531390                    61.30870
# Dim.9   0.9354084         3.897535                    65.20623
# Dim.10  0.9308748         3.878645                    69.08488
# Dim.11  0.8412811         3.505338                    72.59021
# Dim.12  0.7737787         3.224078                    75.81429
# Dim.13  0.6977935         2.907473                    78.72176
# Dim.14  0.6236211         2.598421                    81.32019
# Dim.15  0.6128777         2.553657                    83.87384
# Dim.16  0.5789118         2.412132                    86.28597
# Dim.17  0.5438910         2.266213                    88.55219
# Dim.18  0.4824162         2.010067                    90.56225
# Dim.19  0.4756685         1.981952                    92.54421
# Dim.20  0.4210652         1.754438                    94.29865
# Dim.21  0.3919505         1.633127                    95.93177
# Dim.22  0.3766203         1.569251                    97.50102
# Dim.23  0.3485910         1.452463                    98.95349
# Dim.24  0.2511634         1.046514                   100.00000

gendersortedsymptoms.pca <- prcomp(gendersortedsymptoms[,c(2:14)], center = TRUE, scale. = TRUE)
fviz_eig(gendersortedsymptoms.pca)
fviz_pca_ind(gendersortedsymptoms.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
var <- get_pca_var(gendersortedsymptoms.pca)
corrplot(var$cos2, is.corr=FALSE) 
fviz_pca_biplot(gendersortedsymptoms.pca, 
                col.ind = gendersortedsymptoms$Gender, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Gender") 
get_eig(gendersortedsymptoms.pca)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1   2.8031718        21.562860                    21.56286
# Dim.2   1.3688489        10.529607                    32.09247
# Dim.3   1.2558216         9.660166                    41.75263
# Dim.4   1.1828262         9.098663                    50.85130
# Dim.5   1.0879381         8.368755                    59.22005
# Dim.6   0.9375612         7.212009                    66.43206
# Dim.7   0.8077090         6.213146                    72.64521
# Dim.8   0.7485203         5.757848                    78.40305
# Dim.9   0.6983289         5.371761                    83.77482
# Dim.10  0.6179187         4.753221                    88.52804
# Dim.11  0.5581960         4.293815                    92.82185
# Dim.12  0.4875972         3.750748                    96.57260
# Dim.13  0.4455622         3.427401                   100.00000


