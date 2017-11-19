#Subset Regression
#Load the packages 
library(glmnet) #Lasso and ridge regression 
library(ISLR) #Collection of Data Sets 
library(caret) #Classification and Regression Training
library(leaps) #

#Load the data
FStat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/FStat.csv')

colnames(FStat)
#[1] "Item"      "ID"        "Sex"       "Pop"       "PopSex"   
#[6] "birthyear" "FSTAT"     "claxln"    "scapht"    "scapbr"   
#[11] "humxln"    "radxln"    "ulnxln"    "sacaht"    "femxln"   
#[16] "fembln"    "tibxln"    "fibxln"   


WMStat<-FStat[which(FStat$PopSex== "WM"),]

#First we will perform subset selection

str(WMStat)
#'data.frame':	153 obs. of  18 variables:
# $ Item     : int  12 16 17 20 28 29 31 33 37 40 ...
# $ ID       : Factor w/ 424 levels "F1","F1001","F1002",..: 12 16 17 20 28 29 31 33 37 40 ...
# $ Sex      : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
# $ Pop      : Factor w/ 3 levels "B","H","W": 3 3 3 3 3 3 3 3 3 3 ...
# $ PopSex   : Factor w/ 5 levels "BF","BM","HM",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ birthyear: int  1904 1964 1963 1935 1923 1965 1955 1950 1908 1970 ...
# $ FSTAT    : int  180 155 186 170 182 178 165 183 183 198 ...
# $ claxln   : int  165 NA 168 149 153 159 155 165 NA 166 ...
# $ scapht   : int  164 NA 177 154 171 169 151 180 170 189 ...
# $ scapbr   : int  106 NA 116 108 114 110 100 115 113 114 ...
# $ humxln   : int  348 NA 358 340 362 331 340 369 NA 368 ...
# $ radxln   : int  264 NA 282 244 266 255 253 NA NA 283 ...
# $ ulnxln   : int  280 NA 302 261 285 278 266 NA 281 299 ...
# $ sacaht   : int  115 NA 107 110 127 109 93 NA NA 131 ...
# $ femxln   : int  485 478 502 455 500 478 453 511 490 516 ...
# $ fembln   : int  481 473 499 452 496 478 451 508 487 513 ...
# $ tibxln   : int  391 387 442 376 425 405 385 412 417 431 ...
# $ fibxln   : int  390 NA 428 376 414 406 382 408 417 426 ...

#Remove NA's 
WMStat <- na.omit(WMStat)

str(WMStat)
> str(WMStat)
#'data.frame':	51 obs. of  18 variables:
# $ Item     : int  12 17 20 28 29 31 40 42 47 64 ...
# $ ID       : Factor w/ 424 levels "F1","F1001","F1002",..: 12 17 20 28 29 31 40 42 47 64 ...
# $ Sex      : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
# $ Pop      : Factor w/ 3 levels "B","H","W": 3 3 3 3 3 3 3 3 3 3 ...
# $ PopSex   : Factor w/ 5 levels "BF","BM","HM",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ birthyear: int  1904 1963 1935 1923 1965 1955 1970 1905 1946 1969 ...
# $ FSTAT    : int  180 186 170 182 178 165 198 180 173 173 ...
# $ claxln   : int  165 168 149 153 159 155 166 159 151 146 ...
# $ scapht   : int  164 177 154 171 169 151 189 165 163 166 ...
# $ scapbr   : int  106 116 108 114 110 100 114 101 104 110 ...
# $ humxln   : int  348 358 340 362 331 340 368 356 327 334 ...
# $ radxln   : int  264 282 244 266 255 253 283 265 255 247 ...
# $ ulnxln   : int  280 302 261 285 278 266 299 282 274 270 ...
# $ sacaht   : int  115 107 110 127 109 93 131 111 111 119 ...
# $ femxln   : int  485 502 455 500 478 453 516 506 459 471 ...
# $ fembln   : int  481 499 452 496 478 451 513 502 455 471 ...
# $ tibxln   : int  391 442 376 425 405 385 431 399 378 374 ...
# $ fibxln   : int  390 428 376 414 406 382 426 384 384 369 ...
# - attr(*, "na.action")=Class 'omit'  Named int [1:102] 2 8 9 13 14 15 18 19 20 21 ...
#  .. ..- attr(*, "names")= chr [1:102] "16" "33" "37" "48" ...


reg.ss <- regsubsets(FSTAT ~ claxln+scapht+scapbr+humxln+radxln+ulnxln+sacaht+femxln+fembln+tibxln+fibxln, data = WMStat)
summary(reg.ss)
> summary(reg.ss)
#Subset selection object
#Call: regsubsets.formula(FSTAT ~ claxln + scapht + scapbr + humxln + 
#    radxln + ulnxln + sacaht + femxln + fembln + tibxln + fibxln, 
#    data = WMStat)
#11 Variables  (and intercept)
#       Forced in Forced out
#claxln     FALSE      FALSE
#scapht     FALSE      FALSE
#scapbr     FALSE      FALSE
#humxln     FALSE      FALSE
#radxln     FALSE      FALSE
#ulnxln     FALSE      FALSE
#sacaht     FALSE      FALSE
#femxln     FALSE      FALSE
#fembln     FALSE      FALSE
#tibxln     FALSE      FALSE
#fibxln     FALSE      FALSE
#1 subsets of each size up to 8
#Selection Algorithm: exhaustive
#         claxln scapht scapbr humxln radxln ulnxln sacaht femxln fembln
#1  ( 1 ) " "    " "    " "    " "    " "    " "    " "    "*"    " "   
#2  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    " "   
#3  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    " "   
#4  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    "*"   
#5  ( 1 ) "*"    "*"    " "    " "    " "    " "    " "    "*"    "*"   
#6  ( 1 ) "*"    "*"    "*"    " "    " "    " "    " "    "*"    "*"   
#7  ( 1 ) "*"    "*"    "*"    " "    " "    " "    "*"    "*"    "*"   
#8  ( 1 ) "*"    "*"    "*"    "*"    " "    " "    "*"    "*"    "*"   
#         tibxln fibxln
#1  ( 1 ) " "    " "   
#2  ( 1 ) " "    " "   
#3  ( 1 ) " "    "*"   
#4  ( 1 ) " "    "*"   
#5  ( 1 ) " "    "*"   
#6  ( 1 ) " "    "*"   
#7  ( 1 ) " "    "*"   
#8  ( 1 ) " "    "*"   



summary1 <- summary(reg.ss)

summary1$outmat

#         claxln scapht scapbr humxln radxln ulnxln sacaht femxln fembln
#1  ( 1 ) " "    " "    " "    " "    " "    " "    " "    "*"    " "   
#2  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    " "   
#3  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    " "   
#4  ( 1 ) "*"    " "    " "    " "    " "    " "    " "    "*"    "*"   
#5  ( 1 ) "*"    "*"    " "    " "    " "    " "    " "    "*"    "*"   
#6  ( 1 ) "*"    "*"    "*"    " "    " "    " "    " "    "*"    "*"   
#7  ( 1 ) "*"    "*"    "*"    " "    " "    " "    "*"    "*"    "*"   
#8  ( 1 ) "*"    "*"    "*"    "*"    " "    " "    "*"    "*"    "*"   
#         tibxln fibxln
#1  ( 1 ) " "    " "   
#2  ( 1 ) " "    " "   
#3  ( 1 ) " "    "*"   
#4  ( 1 ) " "    "*"   
#5  ( 1 ) " "    "*"   
#6  ( 1 ) " "    "*"   
#7  ( 1 ) " "    "*"   
#8  ( 1 ) " "    "*"   



summary1$rsq
#[1] 0.7151915 0.7465186 0.7685699 0.7713581 0.7744909 0.7769802 0.7807551
#[8] 0.7818809


#Let's plot some results 
#par(mfrow=c(2,2))
plot(summary1$rss,xlab=" Number of Variables ",ylab=" RSS", type = 'l')
#Output for this line of code can be viewed at : https://tinyurl.com/ycbyoy6g
plot(summary1$adjr2,xlab=" Number of Variables ", ylab=" Adjusted RSq",type = 'l')
#Output for this line of code can be viewed at : https://tinyurl.com/y8ps9nbv

bestset<-which.max(summary1$adjr2)
bestset
#[1] 3
points (bestset, summary1$adjr2[bestset], col = "red",cex= 1.5, pch= 20)
#Output for this line of code can be viewed at : https://tinyurl.com/yd4b3r67


plot(summary1$cp,xlab=" Number of Variables ", ylab= "Cp", type = 'l')
#Output for this line of code can be viewed at : https://tinyurl.com/ybt7pgno

cpmin<-which.min(summary1$cp)
cpmin
#[1] 3
points (cpmin, summary1$cp[cpmin], col ="red",cex= 1.5, pch= 20)
#Output for this line of code can be viewed at : https://tinyurl.com/yc7kwry4

bicmin<-which.min(summary1$bic)
bicmin
#[1] 3
plot(summary1$bic,xlab=" Number of Variables ", ylab= " BIC",type= 'l')
#Output for this line of code can be viewed at : https://tinyurl.com/y8z46qzn
points (bicmin, summary1$bic[3], col =" red", cex= 2, pch= 20)
par(mfrow=c(1,1))
#Output for this line of code can be found at : https://tinyurl.com/y7yphk4t
#We can also do other diagnostic graphs related to variable importance 

plot(reg.ss,scale="r2")
#Output for this line of code can be viewed at : https://tinyurl.com/yde5gzmf
plot(reg.ss,scale="adjr2")
#Output for this line of code can be viewed at : https://tinyurl.com/ycyjv2oe
plot(reg.ss,scale="Cp")
#Output for this line of code can be viewed at : https://tinyurl.com/ybklyvmt
plot(reg.ss,scale="bic")
#Output for this line of code can be viewed at : https://tinyurl.com/y8e5xg9j
coef(reg.ss, bicmin)
#(Intercept)      claxln      femxln      fibxln 
#38.68293384  0.15323151  0.16006472  0.09969595 

#We can also use forward stepwise selection of predictors using regsubsetsandforward
reg.fwd<-regsubsets(FSTAT ~., data= WMStat,nvmax= 18, method = "forward")
summary(reg.fwd)
> summary(reg.fwd)
#Subset selection object
#Call: regsubsets.formula(FSTAT ~ ., data = WMStat, nvmax = 18, method = "forward")
3443 Variables  (and intercept)
#          Forced in Forced out
#Item          FALSE      FALSE
#IDF107        FALSE      FALSE
#IDF1089       FALSE      FALSE
#IDF1091       FALSE      FALSE
#IDF1103       FALSE      FALSE
#IDF1104       FALSE      FALSE
#IDF1116       FALSE      FALSE
#IDF1140       FALSE      FALSE
#IDF1148       FALSE      FALSE
#IDF116        FALSE      FALSE
#IDF1203       FALSE      FALSE
#IDF1211       FALSE      FALSE
#IDF1257       FALSE      FALSE
#IDF1307       FALSE      FALSE
#IDF1329       FALSE      FALSE
#IDF1405       FALSE      FALSE
#IDF1416       FALSE      FALSE
#IDF1431       FALSE      FALSE
#IDF1478       FALSE      FALSE
#IDF1482       FALSE      FALSE
#IDF1483       FALSE      FALSE
#IDF1521       FALSE      FALSE
#IDF1531       FALSE      FALSE
#IDF1551       FALSE      FALSE
#IDF1577       FALSE      FALSE
#IDF1738       FALSE      FALSE
#IDF176        FALSE      FALSE
#IDF188        FALSE      FALSE
#IDF1882       FALSE      FALSE
#IDF1908       FALSE      FALSE
#IDF207        FALSE      FALSE
#IDF2289       FALSE      FALSE
#IDF271        FALSE      FALSE
#IDF273        FALSE      FALSE
#IDF285        FALSE      FALSE
#IDF292        FALSE      FALSE
#IDF293        FALSE      FALSE
#IDF370        FALSE      FALSE
#IDF381        FALSE      FALSE
#IDF401        FALSE      FALSE
#IDF402        FALSE      FALSE
#IDF406        FALSE      FALSE
#IDF423        FALSE      FALSE
#IDF446        FALSE      FALSE
#IDF544        FALSE      FALSE
#IDF656        FALSE      FALSE
#IDF760        FALSE      FALSE
#IDF761        FALSE      FALSE
#IDF827        FALSE      FALSE
#IDF83         FALSE      FALSE
#IDF1001       FALSE      FALSE
#IDF1002       FALSE      FALSE
#IDF1003       FALSE      FALSE
#IDF1005       FALSE      FALSE
#IDF1007       FALSE      FALSE
#IDF1008       FALSE      FALSE
#IDF1010       FALSE      FALSE
#IDF1011       FALSE      FALSE
#IDF1012       FALSE      FALSE
#IDF1067       FALSE      FALSE
#IDF1071       FALSE      FALSE
#IDF1077       FALSE      FALSE
#IDF1083       FALSE      FALSE
#IDF1086       FALSE      FALSE
#IDF109        FALSE      FALSE
#IDF1090       FALSE      FALSE
#IDF1092       FALSE      FALSE
#IDF1093       FALSE      FALSE
#IDF1095       FALSE      FALSE
#IDF1096       FALSE      FALSE
#IDF1097       FALSE      FALSE
#IDF1098       FALSE      FALSE
#IDF1100       FALSE      FALSE
#IDF1114       FALSE      FALSE
#IDF1118       FALSE      FALSE
#IDF1122       FALSE      FALSE
#IDF1123       FALSE      FALSE
#IDF1124       FALSE      FALSE
#IDF1126       FALSE      FALSE
#IDF1128       FALSE      FALSE
#IDF1129       FALSE      FALSE
#IDF1134       FALSE      FALSE
#IDF1142       FALSE      FALSE
#IDF1150       FALSE      FALSE
#IDF1151       FALSE      FALSE
#IDF1155       FALSE      FALSE
#IDF1157       FALSE      FALSE
#IDF1173       FALSE      FALSE
#IDF1176       FALSE      FALSE
#IDF1179       FALSE      FALSE
#IDF118        FALSE      FALSE
#IDF1180       FALSE      FALSE
#IDF1181       FALSE      FALSE
#IDF1182       FALSE      FALSE
#IDF1183       FALSE      FALSE
#IDF1184       FALSE      FALSE
#IDF1187       FALSE      FALSE
#IDF1191       FALSE      FALSE
#IDF1192       FALSE      FALSE
#IDF1196       FALSE      FALSE
#IDF1197       FALSE      FALSE
#IDF1200       FALSE      FALSE
#IDF1202       FALSE      FALSE
#IDF1209       FALSE      FALSE
#IDF1210       FALSE      FALSE
#IDF1212       FALSE      FALSE
#IDF1214       FALSE      FALSE
#IDF1218       FALSE      FALSE
#IDF1219       FALSE      FALSE
#IDF1220       FALSE      FALSE
#IDF1230       FALSE      FALSE
#IDF1231       FALSE      FALSE
#IDF1236       FALSE      FALSE
#IDF1238       FALSE      FALSE
#IDF1241       FALSE      FALSE
#IDF1242       FALSE      FALSE
#IDF1245       FALSE      FALSE
#IDF1260       FALSE      FALSE
#IDF1270       FALSE      FALSE
#IDF1273       FALSE      FALSE
#IDF1277       FALSE      FALSE
#IDF1287       FALSE      FALSE
#IDF1296       FALSE      FALSE
#IDF1301       FALSE      FALSE
#IDF1308       FALSE      FALSE
#IDF1318       FALSE      FALSE
#IDF1320       FALSE      FALSE
#IDF1321       FALSE      FALSE
#IDF1325       FALSE      FALSE
#IDF1328       FALSE      FALSE
#IDF1333       FALSE      FALSE
#IDF1335       FALSE      FALSE
#IDF1336       FALSE      FALSE
#IDF1337       FALSE      FALSE
#IDF1338       FALSE      FALSE
#IDF1339       FALSE      FALSE
#IDF1340       FALSE      FALSE
#IDF1341       FALSE      FALSE
#IDF1342       FALSE      FALSE
#IDF1343       FALSE      FALSE
#IDF1345       FALSE      FALSE
#IDF1346       FALSE      FALSE
#IDF1347       FALSE      FALSE
#IDF1348       FALSE      FALSE
#IDF1349       FALSE      FALSE
#IDF1350       FALSE      FALSE
#IDF1351       FALSE      FALSE
#IDF1353       FALSE      FALSE
#IDF1356       FALSE      FALSE
#IDF1358       FALSE      FALSE
#IDF1359       FALSE      FALSE
#IDF1362       FALSE      FALSE
#IDF1363       FALSE      FALSE
#IDF1374       FALSE      FALSE
#IDF1394       FALSE      FALSE
#IDF1398       FALSE      FALSE
#IDF1400       FALSE      FALSE
#IDF1402       FALSE      FALSE
#IDF1403       FALSE      FALSE
#IDF1404       FALSE      FALSE
#IDF1407       FALSE      FALSE
#IDF1411       FALSE      FALSE
#IDF1417       FALSE      FALSE
#IDF1420       FALSE      FALSE
#IDF1421       FALSE      FALSE
#IDF1422       FALSE      FALSE
#IDF1427       FALSE      FALSE
#IDF1430       FALSE      FALSE
#IDF1432       FALSE      FALSE
#IDF1435       FALSE      FALSE
#IDF1438       FALSE      FALSE
#IDF1446       FALSE      FALSE
#IDF1451       FALSE      FALSE
#IDF1452       FALSE      FALSE
#IDF1454       FALSE      FALSE
#IDF1462       FALSE      FALSE
#IDF1476       FALSE      FALSE
#IDF1479       FALSE      FALSE
#IDF1480       FALSE      FALSE
#IDF1495       FALSE      FALSE
#IDF1506       FALSE      FALSE
#IDF1508       FALSE      FALSE
#IDF1522       FALSE      FALSE
#IDF1523       FALSE      FALSE
#IDF1525       FALSE      FALSE
#IDF1526       FALSE      FALSE
#IDF1527       FALSE      FALSE
#IDF1539       FALSE      FALSE
#IDF1540       FALSE      FALSE
#IDF1548       FALSE      FALSE
#IDF1555       FALSE      FALSE
#IDF1570       FALSE      FALSE
#IDF1571       FALSE      FALSE
#IDF1575       FALSE      FALSE
#IDF1576       FALSE      FALSE
#IDF1581       FALSE      FALSE
#IDF1582       FALSE      FALSE
#IDF1586       FALSE      FALSE
#IDF1587       FALSE      FALSE
#IDF1590       FALSE      FALSE
#IDF1592       FALSE      FALSE
#IDF1619       FALSE      FALSE
#IDF1630       FALSE      FALSE
#IDF1644       FALSE      FALSE
#IDF1667       FALSE      FALSE
#IDF1671       FALSE      FALSE
#IDF1739       FALSE      FALSE
#IDF1740       FALSE      FALSE
#IDF1742       FALSE      FALSE
#IDF177        FALSE      FALSE
#IDF178        FALSE      FALSE
#IDF179        FALSE      FALSE
#IDF1838       FALSE      FALSE
#IDF1839       FALSE      FALSE
#IDF1844       FALSE      FALSE
#IDF1850       FALSE      FALSE
#IDF1857       FALSE      FALSE
#IDF1859       FALSE      FALSE
#IDF1860       FALSE      FALSE
#IDF1865       FALSE      FALSE
#IDF1867       FALSE      FALSE
#IDF1869       FALSE      FALSE
#IDF187        FALSE      FALSE
#IDF1870       FALSE      FALSE
#IDF1871       FALSE      FALSE
#IDF1872       FALSE      FALSE
#IDF1887       FALSE      FALSE
#IDF189        FALSE      FALSE
#IDF191        FALSE      FALSE
#IDF192        FALSE      FALSE
#IDF1924       FALSE      FALSE
#IDF1926       FALSE      FALSE
#IDF1953       FALSE      FALSE
#IDF1955       FALSE      FALSE
#IDF196        FALSE      FALSE
#IDF1960       FALSE      FALSE
#IDF1966       FALSE      FALSE
#IDF197        FALSE      FALSE
#IDF1972       FALSE      FALSE
#IDF198        FALSE      FALSE
#IDF199        FALSE      FALSE
#IDF200        FALSE      FALSE
#IDF204        FALSE      FALSE
#IDF2266       FALSE      FALSE
#IDF2267       FALSE      FALSE
#IDF2268       FALSE      FALSE
#IDF2269       FALSE      FALSE
#IDF227        FALSE      FALSE
#IDF2271       FALSE      FALSE
#IDF2273       FALSE      FALSE
#IDF2274       FALSE      FALSE
#IDF2276       FALSE      FALSE
#IDF2277       FALSE      FALSE
#IDF2278       FALSE      FALSE
#IDF228        FALSE      FALSE
#IDF2281       FALSE      FALSE
#IDF2285       FALSE      FALSE
#IDF2286       FALSE      FALSE
#IDF229        FALSE      FALSE
#IDF2300       FALSE      FALSE
#IDF2304       FALSE      FALSE
#IDF2306       FALSE      FALSE
#IDF2313       FALSE      FALSE
#IDF2331       FALSE      FALSE
#IDF2332       FALSE      FALSE
#IDF234        FALSE      FALSE
#IDF236        FALSE      FALSE
#IDF237        FALSE      FALSE
#IDF2373       FALSE      FALSE
#IDF2380       FALSE      FALSE
#IDF2381       FALSE      FALSE
#IDF2382       FALSE      FALSE
#IDF2383       FALSE      FALSE
#IDF2384       FALSE      FALSE
#IDF2385       FALSE      FALSE
#IDF2386       FALSE      FALSE
#IDF2387       FALSE      FALSE
#IDF2388       FALSE      FALSE
#IDF2389       FALSE      FALSE
#IDF2391       FALSE      FALSE
#IDF2392       FALSE      FALSE
#IDF2393       FALSE      FALSE
#IDF2395       FALSE      FALSE
#IDF2396       FALSE      FALSE
#IDF2399       FALSE      FALSE
#IDF2452       FALSE      FALSE
#IDF248        FALSE      FALSE
#IDF25         FALSE      FALSE
#IDF266        FALSE      FALSE
#IDF276        FALSE      FALSE
#IDF296        FALSE      FALSE
#IDF300        FALSE      FALSE
#IDF302        FALSE      FALSE
#IDF312        FALSE      FALSE
#IDF32         FALSE      FALSE
#IDF320        FALSE      FALSE
#IDF324        FALSE      FALSE
#IDF326        FALSE      FALSE
#IDF328        FALSE      FALSE
#IDF329        FALSE      FALSE
#IDF331        FALSE      FALSE
#IDF337        FALSE      FALSE
#IDF338        FALSE      FALSE
#IDF340        FALSE      FALSE
#IDF344        FALSE      FALSE
#IDF345        FALSE      FALSE
#IDF346        FALSE      FALSE
#IDF349        FALSE      FALSE
#IDF35         FALSE      FALSE
#IDF358        FALSE      FALSE
#IDF36         FALSE      FALSE
#IDF361        FALSE      FALSE
#IDF364        FALSE      FALSE
#IDF365        FALSE      FALSE
#IDF368        FALSE      FALSE
#IDF37         FALSE      FALSE
#IDF378        FALSE      FALSE
#IDF379        FALSE      FALSE
#IDF38         FALSE      FALSE
#IDF380        FALSE      FALSE
#IDF390        FALSE      FALSE
#IDF394        FALSE      FALSE
#IDF397        FALSE      FALSE
#IDF398        FALSE      FALSE
#IDF4          FALSE      FALSE
#IDF405        FALSE      FALSE
#IDF41         FALSE      FALSE
#IDF418        FALSE      FALSE
#IDF419        FALSE      FALSE
#IDF421        FALSE      FALSE
#IDF427        FALSE      FALSE
#IDF428        FALSE      FALSE
#IDF433        FALSE      FALSE
#IDF441        FALSE      FALSE
#IDF468        FALSE      FALSE
#IDF469        FALSE      FALSE
#IDF473        FALSE      FALSE
#IDF481        FALSE      FALSE
#IDF485        FALSE      FALSE
#IDF487        FALSE      FALSE
#IDF495        FALSE      FALSE
#IDF501        FALSE      FALSE
#IDF506        FALSE      FALSE
#IDF507        FALSE      FALSE
#IDF510        FALSE      FALSE
#IDF513        FALSE      FALSE
#IDF519        FALSE      FALSE
#IDF524        FALSE      FALSE
#IDF53         FALSE      FALSE
#IDF539        FALSE      FALSE
#IDF540        FALSE      FALSE
#IDF542        FALSE      FALSE
#IDF547        FALSE      FALSE
#IDF548        FALSE      FALSE
#IDF549        FALSE      FALSE
#IDF553        FALSE      FALSE
#IDF554        FALSE      FALSE
#IDF555        FALSE      FALSE
#IDF560        FALSE      FALSE
#IDF562        FALSE      FALSE
#IDF564        FALSE      FALSE
#IDF57         FALSE      FALSE
#IDF577        FALSE      FALSE
#IDF578        FALSE      FALSE
#IDF582        FALSE      FALSE
#IDF586        FALSE      FALSE
#IDF588        FALSE      FALSE
#IDF592        FALSE      FALSE
#IDF593        FALSE      FALSE
#IDF652        FALSE      FALSE
#IDF655        FALSE      FALSE
#IDF657        FALSE      FALSE
#IDF659        FALSE      FALSE
#IDF660        FALSE      FALSE
#IDF668        FALSE      FALSE
#IDF683        FALSE      FALSE
#IDF684        FALSE      FALSE
#IDF689        FALSE      FALSE
#IDF693        FALSE      FALSE
#IDF705        FALSE      FALSE
#IDF710        FALSE      FALSE
#IDF713        FALSE      FALSE
#IDF714        FALSE      FALSE
#IDF715        FALSE      FALSE
#IDF716        FALSE      FALSE
#IDF726        FALSE      FALSE
#IDF75         FALSE      FALSE
#IDF756        FALSE      FALSE
#IDF759        FALSE      FALSE
#IDF763        FALSE      FALSE
#IDF772        FALSE      FALSE
#IDF776        FALSE      FALSE
#IDF787        FALSE      FALSE
#IDF788        FALSE      FALSE
#IDF789        FALSE      FALSE
#IDF790        FALSE      FALSE
#IDF791        FALSE      FALSE
#IDF792        FALSE      FALSE
#IDF793        FALSE      FALSE
#IDF8          FALSE      FALSE
#IDF806        FALSE      FALSE
#IDF808        FALSE      FALSE
#IDF809        FALSE      FALSE
#IDF812        FALSE      FALSE
#IDF820        FALSE      FALSE
#IDF823        FALSE      FALSE
#IDF832        FALSE      FALSE
#IDF834        FALSE      FALSE
#IDF84         FALSE      FALSE
#IDF841        FALSE      FALSE
#IDF844        FALSE      FALSE
#IDF845        FALSE      FALSE
#IDF846        FALSE      FALSE
#IDF848        FALSE      FALSE
#IDF849        FALSE      FALSE
#IDF86         FALSE      FALSE
#IDF9          FALSE      FALSE
#IDF979        FALSE      FALSE
#IDF984        FALSE      FALSE
#IDF993        FALSE      FALSE
#IDF994        FALSE      FALSE
#IDF997        FALSE      FALSE
#IDF998        FALSE      FALSE
#IDF999        FALSE      FALSE
#SexM          FALSE      FALSE
#PopH          FALSE      FALSE
#PopW          FALSE      FALSE
#PopSexBM      FALSE      FALSE
#PopSexHM      FALSE      FALSE
#PopSexWF      FALSE      FALSE
#PopSexWM      FALSE      FALSE
#birthyear     FALSE      FALSE
#claxln        FALSE      FALSE
#scapht        FALSE      FALSE
#scapbr        FALSE      FALSE
#humxln        FALSE      FALSE
#radxln        FALSE      FALSE
#ulnxln        FALSE      FALSE
#sacaht        FALSE      FALSE
#femxln        FALSE      FALSE
#fembln        FALSE      FALSE
#tibxln        FALSE      FALSE
#fibxln        FALSE      FALSE
#1 subsets of each size up to 19
#Selection Algorithm: forward
#          Item IDF1001 IDF1002 IDF1003 IDF1005 IDF1007 IDF1008 IDF1010
#1  ( 1 )  " "  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "  " "     " "     " "     " "     " "     " "     " "    
#          IDF1011 IDF1012 IDF1067 IDF107 IDF1071 IDF1077 IDF1083 IDF1086
#1  ( 1 )  " "     " "     " "     " "    " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "    " "     " "     " "     " "    
#          IDF1089 IDF109 IDF1090 IDF1091 IDF1092 IDF1093 IDF1095 IDF1096
#1  ( 1 )  " "     " "    " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "    " "     " "     " "     " "     " "     " "    
#         IDF1097 IDF1098 IDF1100 IDF1103 IDF1104 IDF1114 IDF1116
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#         IDF1118 IDF1122 IDF1123 IDF1124 IDF1126 IDF1128 IDF1129
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1134 IDF1140 IDF1142 IDF1148 IDF1150 IDF1151 IDF1155
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1157 IDF116 IDF1173 IDF1176 IDF1179 IDF118 IDF1180 IDF1181
#1  ( 1 )  " "     " "    " "     " "     " "     " "    " "     " "    
#2  ( 1 )  " "     " "    " "     " "     " "     " "    " "     " "    
#          IDF1182 IDF1183 IDF1184 IDF1187 IDF1191 IDF1192 IDF1196
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1197 IDF1200 IDF1202 IDF1203 IDF1209 IDF1210 IDF1211
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1212 IDF1214 IDF1218 IDF1219 IDF1220 IDF1230 IDF1231
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1236 IDF1238 IDF1241 IDF1242 IDF1245 IDF1257 IDF1260
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1270 IDF1273 IDF1277 IDF1287 IDF1296 IDF1301 IDF1307
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1308 IDF1318 IDF1320 IDF1321 IDF1325 IDF1328 IDF1329
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1333 IDF1335 IDF1336 IDF1337 IDF1338 IDF1339 IDF1340
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1341 IDF1342 IDF1343 IDF1345 IDF1346 IDF1347 IDF1348
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1349 IDF1350 IDF1351 IDF1353 IDF1356 IDF1358 IDF1359
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1362 IDF1363 IDF1374 IDF1394 IDF1398 IDF1400 IDF1402
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1403 IDF1404 IDF1405 IDF1407 IDF1411 IDF1416 IDF1417
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1420 IDF1421 IDF1422 IDF1427 IDF1430 IDF1431 IDF1432
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1435 IDF1438 IDF1446 IDF1451 IDF1452 IDF1454 IDF1462
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1476 IDF1478 IDF1479 IDF1480 IDF1482 IDF1483 IDF1495
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1506 IDF1508 IDF1521 IDF1522 IDF1523 IDF1525 IDF1526
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1527 IDF1531 IDF1539 IDF1540 IDF1548 IDF1551 IDF1555
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1570 IDF1571 IDF1575 IDF1576 IDF1577 IDF1581 IDF1582
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     "*"     " "     " "    
#          IDF1586 IDF1587 IDF1590 IDF1592 IDF1619 IDF1630 IDF1644
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF1667 IDF1671 IDF1738 IDF1739 IDF1740 IDF1742 IDF176 IDF177
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    " "   
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    " "   
#          IDF178 IDF179 IDF1838 IDF1839 IDF1844 IDF1850 IDF1857 IDF1859
#1  ( 1 )  " "    " "    " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "    " "    " "     " "     " "     " "     " "     " "    
#          IDF1860 IDF1865 IDF1867 IDF1869 IDF187 IDF1870 IDF1871 IDF1872
#1  ( 1 )  " "     " "     " "     " "     " "    " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "    " "     " "     " "    
#          IDF188 IDF1882 IDF1887 IDF189 IDF1908 IDF191 IDF192 IDF1924
#1  ( 1 )  " "    " "     " "     " "    " "     " "    " "    " "    
#2  ( 1 )  " "    " "     " "     " "    " "     " "    " "    " "    
#          IDF1926 IDF1953 IDF1955 IDF196 IDF1960 IDF1966 IDF197 IDF1972
#1  ( 1 )  " "     " "     " "     " "    " "     " "     " "    " "    
#2  ( 1 )  " "     " "     " "     " "    " "     " "     " "    " "    
#          IDF198 IDF199 IDF200 IDF204 IDF207 IDF2266 IDF2267 IDF2268
#1  ( 1 )  " "    " "    " "    " "    " "    " "     " "     " "    
#2  ( 1 )  " "    " "    " "    " "    " "    " "     " "     " "    
#          IDF2269 IDF227 IDF2271 IDF2273 IDF2274 IDF2276 IDF2277 IDF2278
#1  ( 1 )  " "     " "    " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "    " "     " "     " "     " "     " "     " "    
#          IDF228 IDF2281 IDF2285 IDF2286 IDF2289 IDF229 IDF2300 IDF2304
#1  ( 1 )  " "    " "     " "     " "     " "     " "    " "     " "    
#2  ( 1 )  " "    " "     " "     " "     " "     " "    " "     " "    
#          IDF2306 IDF2313 IDF2331 IDF2332 IDF234 IDF236 IDF237 IDF2373
#1  ( 1 )  " "     " "     " "     " "     " "    " "    " "    " "    
#2  ( 1 )  " "     " "     " "     " "     " "    " "    " "    " "    
#          IDF2380 IDF2381 IDF2382 IDF2383 IDF2384 IDF2385 IDF2386
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF2387 IDF2388 IDF2389 IDF2391 IDF2392 IDF2393 IDF2395
#1  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#2  ( 1 )  " "     " "     " "     " "     " "     " "     " "    
#          IDF2396 IDF2399 IDF2452 IDF248 IDF25 IDF266 IDF271 IDF273
#1  ( 1 )  " "     " "     " "     " "    " "   " "    " "    " "   
#2  ( 1 )  " "     " "     " "     " "    " "   " "    " "    " "   
#          IDF276 IDF285 IDF292 IDF293 IDF296 IDF300 IDF302 IDF312 IDF32
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "  
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "  
#          IDF320 IDF324 IDF326 IDF328 IDF329 IDF331 IDF337 IDF338 IDF340
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF344 IDF345 IDF346 IDF349 IDF35 IDF358 IDF36 IDF361 IDF364
#1  ( 1 )  " "    " "    " "    " "    " "   " "    " "   " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "   " "    " "   " "    " "   
#          IDF365 IDF368 IDF37 IDF370 IDF378 IDF379 IDF38 IDF380 IDF381
#1  ( 1 )  " "    " "    " "   " "    " "    " "    " "   " "    " "   
#2  ( 1 )  " "    " "    " "   " "    " "    " "    " "   " "    " "   
#          IDF390 IDF394 IDF397 IDF398 IDF4 IDF401 IDF402 IDF405 IDF406
#1  ( 1 )  " "    " "    " "    " "    " "  " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "  " "    " "    " "    " "   
#          IDF41 IDF418 IDF419 IDF421 IDF423 IDF427 IDF428 IDF433 IDF441
#1  ( 1 )  " "   " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "   " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF446 IDF468 IDF469 IDF473 IDF481 IDF485 IDF487 IDF495 IDF501
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF506 IDF507 IDF510 IDF513 IDF519 IDF524 IDF53 IDF539 IDF540
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "   " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "   " "    " "   
#          IDF542 IDF544 IDF547 IDF548 IDF549 IDF553 IDF554 IDF555 IDF560
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF562 IDF564 IDF57 IDF577 IDF578 IDF582 IDF586 IDF588 IDF592
#1  ( 1 )  " "    " "    " "   " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "   " "    " "    " "    " "    " "    " "   
 #         IDF593 IDF652 IDF655 IDF656 IDF657 IDF659 IDF660 IDF668 IDF683
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF684 IDF689 IDF693 IDF705 IDF710 IDF713 IDF714 IDF715 IDF716
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "    " "   
#          IDF726 IDF75 IDF756 IDF759 IDF760 IDF761 IDF763 IDF772 IDF776
#1  ( 1 )  " "    " "   " "    " "    " "    " "    " "    " "    " "   
#2  ( 1 )  " "    " "   " "    " "    " "    " "    " "    " "    " "   
#          IDF787 IDF788 IDF789 IDF790 IDF791 IDF792 IDF793 IDF8 IDF806
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "  " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "  " "   
#          IDF808 IDF809 IDF812 IDF820 IDF823 IDF827 IDF83 IDF832 IDF834
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "   " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "   " "    " "   
#          IDF84 IDF841 IDF844 IDF845 IDF846 IDF848 IDF849 IDF86 IDF9
#1  ( 1 )  " "   " "    " "    " "    " "    " "    " "    " "   " " 
#2  ( 1 )  " "   " "    " "    " "    " "    " "    " "    " "   " " 
#          IDF979 IDF984 IDF993 IDF994 IDF997 IDF998 IDF999 SexM PopH
#1  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "  " " 
#2  ( 1 )  " "    " "    " "    " "    " "    " "    " "    " "  " " 
#          PopW PopSexBM PopSexHM PopSexWF PopSexWM birthyear claxln
#1  ( 1 )  " "  " "      " "      " "      " "      " "       " "   
#2  ( 1 )  " "  " "      " "      " "      " "      " "       " "   
#          scapht scapbr humxln radxln ulnxln sacaht femxln fembln tibxln
#1  ( 1 )  " "    " "    " "    " "    " "    " "    "*"    " "    " "   
#2  ( 1 )  " "    " "    " "    " "    " "    " "    "*"    " "    " "   
#          fibxln
#1  ( 1 )  " "   
#2  ( 1 )  " "   
# [ reached getOption("max.print") -- omitted 17 rows 
 




which.min(summary(reg.fwd)$bic)
#[1] 19
which.max(summary(reg.fwd)$adjr2)
#[1] 19
which.min(summary(reg.fwd)$cp)
#[1] 1

#Let's compare best-subsets and forward-stepwise models
coef(reg.ss, 3)	
#(Intercept)      claxln      femxln      fibxln 
#38.68293384  0.15323151  0.16006472  0.09969595 


coef(reg.fwd, 3)
#(Intercept)     IDF1435      IDF823      femxln 
#45.9609238   0.0000000   0.0000000   0.2770528 

coef(reg.ss, 4)
#(Intercept)      claxln      femxln      fembln      fibxln 
#39.9858392   0.1466959   0.3993270  -0.2477017   0.1074098 

coef(reg.fwd, 4)
#(Intercept)      IDF401     IDF1435      IDF823      femxln 
#48.3265243   3.7315945   0.0000000   0.0000000   0.2719019 

#Hence, we have successfully performed Subset Selection Regression. To view all output at once, visit 
#the issue page for this repository
