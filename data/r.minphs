#
# Minimum phase weights for lowpassing strain data
# (i.e. Agnew and Hodgkinson XXXX)
#
#
# 5X lowpass-decimation
#
by5 <- "wgts
0.0137003
0.0323895
0.0627611
0.1018581
0.1454735
0.1742102
0.1888950
0.1786665
0.1440888
0.0931153
0.0316546
-0.0217322
-0.0580216
-0.0707892
-0.0601628
-0.0350987
-0.0047034
0.0202246
0.0332569
0.0322796
0.0222429
0.0075065
-0.0051609
-0.0117785
-0.0142579
-0.0085583
-0.0023946
0.0030231
0.0062511
0.0017000
0.0019444
0.0000312
-0.0014347
-0.0030002
0.0018199
"
minphs.by5 <- read.table(textConnection(by5), header=TRUE)
#
# 3X lpd
#
by3 <- "wgts
0.0373766
0.1165151
0.2385729
0.3083302
0.2887327
0.1597948
0.0058244
-0.0973639
-0.1051034
-0.0358455
0.0359044
0.0632477
0.0302351
-0.0168856
-0.0356758
-0.0190635
0.0126188
0.0159705
0.0082144
-0.0087978
-0.0037289
-0.0017068
0.0028335
"
minphs.by3 <- read.table(textConnection(by3), header=TRUE)
#
# 2X lpd
#
by2 <- "wgts
0.0983262
0.2977611
0.4086973
0.3138961
0.0494246
-0.1507778
-0.1123764
0.0376576
0.0996838
0.0154992
-0.0666489
-0.0346632
0.0322767
0.0399294
-0.0097461
-0.0341585
-0.0039241
0.0246776
0.0099725
-0.0157879
-0.0099098
0.0078510
0.0081126
-0.0026986
-0.0061424
0.0007108
0.0039659
-0.0006209
-0.0017117
0.0007240
"
minphs.by2 <- read.table(textConnection(by2), header=TRUE)
#
#minphs.by5 <- read.table("dec5", header=FALSE, col.names="wgts")
#minphs.by3 <- read.table("dec3", header=FALSE, col.names="wgts")
#minphs.by2 <- read.table("dec2", header=FALSE, col.names="wgts")
#
minphs <- list(
    by5=minphs.by5$wgts,
    by3=minphs.by3$wgts,
    by2=minphs.by2$wgts
)
# version (1.0 is from orig. paper)
attr(minphs, "version") <- 1.1
attr(minphs, "reference") <- c("D.C. Agnew and K. Hodgkinson",2007,
"Designing compact causal digital filters for low-frequency strainmeter data",
"Bulletin of the Seismological Society of America",
"vol 97","1B","91-99","doi: 10.1785/0120060088")

save(minphs, file="minphs.rda")
