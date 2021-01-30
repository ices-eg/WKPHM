WKPHM Advice Template
================
WKPHM
January 30, 2021

## Purpose

The objective of this piece of code was to develop a relatively simple
model for a species of coral that could be used to demonstrate the
pieces of the proposed ICES PHM advice template. The species chosen was
*Acanthogorgia armata*. It was chosen simply because it had a fairly
large number of observations in the ICES VME database. This is not meant
to be a realistic model of the distribution of *Acanthogorgia armata*,
but is instead used here to generate the components of an PHM (data,
model, residuals) that can be used to evaluate its predictions and
utility.

## Dependent data

The dependent data for *Acanthogorgia armata* were observations of
presence or absence in the open ICES VME database. In total there were
901 observations of presence in the database. In the code below the
counts for the species are also included in the data that is generated,
but these were not used in the modeling at this time. Absences were
inferred from sample locations where observations for other species were
made, but *Acanthogorgia armata* were not recorded. In total there were
\> 30,000 absences in the ICES VME database.

The code below does the following

1.  Import the dependent data from the open ICES data base
2.  Calculates a mean depth for each observation where an upper and
    lower depth were recorded
3.  Compiles a data frame of the unique observation locations (by their
    recorded middle latitude and longitude)
4.  Subsets all the records where *Acanthogorgia armata* was observed
    and assigns them a presence (0) and sums any counts that were
    recorded at the location (or provides count = 1 where no count was
    provided, but presence was noted)
5.  Merges those presence records with the entire database and assigns
    presence and counts = 0 to the records with no presence observation
6.  There was a single duplicate record for this species where one entry
    included a count = 1 and one a count = 3. The counts were summed to
    4 and the duplicate line removed.

<!-- end list -->

``` r
VME_data<-read.csv("vme_open_extraction11012021.csv",header=TRUE,stringsAsFactors=FALSE) #Import the entire database
VME_data$Depth<-abs((as.numeric(VME_data$DepthLower)+as.numeric(VME_data$DepthUpper))/2) #Generate a mean depth for each point (where depths were collected) based on the upper and lower recorded depths
All_depths<-aggregate(Depth~MiddleLatitude+MiddleLongitude,data=VME_data,FUN="mean",na.rm=TRUE) #Aggregate these depths to each position in the database
dim(VME_data)
dim(All_depths)

rev(sort(table(VME_data$Species)))
All_points<-unique(data.frame(Longitude=VME_data$MiddleLongitude,Latitude=VME_data$MiddleLatitude)) #Generate the unique positions of all the observations in the database
All_points<-All_points[All_points$Latitude!=0,] # Remove a record with Latitude and Longitude = 0. This is likely an error
dim(All_points)

All_points<-merge(All_points,All_depths,by.x=c("Longitude","Latitude"),by.y=c("MiddleLongitude","MiddleLatitude"),all.x=TRUE) #Add the depth values to the unique position set

Acanth_arm<-subset(VME_data,VME_data$Species=="Acanthogorgia armata") #Subset the data for a species of interest
Acanth_arm<-data.frame(ICES_ID=Acanth_arm$ï..ICES_ID,Species=Acanth_arm$Species,Longitude=Acanth_arm$MiddleLongitude,Latitude=Acanth_arm$MiddleLatitude,Presence=1,Number=as.numeric(Acanth_arm$Number)) #Create a data frame of the minimal data for presence of the species, positions and numbers observed
Acanth_arm<-merge(All_points,Acanth_arm,by=c("Longitude","Latitude"),all.x=TRUE) #Merge the species data with the full set of observation points
Acanth_arm$Species<-"Acanthogorgia armata" # Fill the Species column
Acanth_arm$Presence[is.na(Acanth_arm$Presence)]<-0 #Add zeros for absence at those observation points where the species of interest was not recorded (these are now absences)
Acanth_arm$Number[is.na(Acanth_arm$Number)]<-0 # Add zeros for number column at those observation points where the species of interst was not recorded
dim(Acanth_arm)
dim(All_points) # These two data frames should have the same number of rows. For this species, the numbers don't match, which means there is a duplicate record at a Lat-long pair for the species

which(duplicated(cbind(Acanth_arm$Latitude,Acanth_arm$Longitude))) # find which record is duplicated
which(duplicated(cbind(Acanth_arm$Latitude,Acanth_arm$Longitude),fromLast=TRUE)) #find the other record that is duplicated
Acanth_arm[13705:13706,] # Look to see whats happening. It looks like there is one record with number = 1 and one with number = 3,
Acanth_arm[13705,"Number"]<-4 # Change 1 record to reflect the total number observed at that position
Acanth_arm<-Acanth_arm[-13706,] # Delete the remaining record
dim(Acanth_arm)

#table(Acanth_arm$Presence) # See how many presences (1) and absences (0) there are in the data
```

Next the entire set of data points were mapped to show the distribution
of presences and absences. (Note that I split the code to insert the
figures throughout the document, mostly so it would produce hard copies
of the figures themselves as .png files.)

``` r
#IMPORT A BASEMAP AND TRANSFORM TO A NICER PROJECTION FOR THE NORTH ATLANTIC
bg = ne_countries(scale = "medium",  returnclass = "sf")
p<-ggplot()+
  #basemap
  geom_sf(data = bg)+
  coord_sf(xlim = range(Acanth_arm$Longitude, na.rm = TRUE), 
           ylim = range(Acanth_arm$Latitude, na.rm = TRUE), 
           expand = TRUE)+
  
  # add points
  geom_point(data = Acanth_arm, 
             aes(x=Longitude,y=Latitude,group=as.factor(Presence),color=as.factor(Presence)),
             alpha = 0.7, shape=21, size = 2)+
  
  # formatting
  scale_fill_viridis_d(option = "inferno")+
  scale_color_viridis_d(option = "inferno")+
  labs(x=NULL, y=NULL, 
       fill = 'Presence', 
       color = 'Presence')+
  theme_dark()+
  theme(panel.grid = element_blank())

png("./Figures/Figure1.png",height=6,width=6,unit="in",res=300)
print(p)
dev.off()
```

<div class="figure">

<img src="C:/Users/rooperc/Desktop/ICES_WKPHM/./Figures/Figure1.png" alt="Figure 1. Locations of presence and absence observations for *Acanthogorgia armata* from the ICES database" width="1800" />

<p class="caption">

Figure 1. Locations of presence and absence observations for
*Acanthogorgia armata* from the ICES database

</p>

</div>

## Explanatory variables

Based on the distribution of presences and absences in Figure 1 (and the
desire for the code to run fairly fast), a subset of the ICES subareas
(SubAreas 6,7,8,9,10 and 12) were chosen for the modeling. All
subsequent data layers were trimmed to include only observations and
explanatory variables from this region.

The explanatory variables used for this exercise were bathymetry and two
derivitives of bathymetry and one environmental variable. The bathymetry
used here was downloaded from the GEBCO website (GEBCO\_2020 grid;
www.gebco.net/data\_and\_products/gridded\_bathymetry\_data). It
consists of gridded bathymetry from a wide varieity of sources on a 15
arc-second grid for the globe. The details of the data sources can be
found on the website (GEBCO Compilation Group (2020) GEBCO 2020 Grid
(<doi:10.5285/a29c5465-b138-234d-e053-6c86abc040b9>)).

Oxygen data were downloaded from the World Ocean Atlas 2018 database
(<https://www.nodc.noaa.gov/OC5/woa18/>). This data is a compilation of
oxygen measurements averaged over time for as long as there are
measurments at standardized depth intervals and on a standard 0.5 degree
longitude and latitude grid. These data were clipped to the area of
interest and interpolated to the 15 arc-second grid used by the
bathymetry.

Both the bathymetry and oxygen layers were aggregated to a 30 arc-second
grid in order to save processing time.

NOTE: This section of code is for reference only and describes how the
raw data was processed. I have not provided the raw data from GEBCO or
World Ocean Atlas since the files are so large and take so much time to
process. Neither these or the aggregated rasters were small enough to
upload to the github site, so they are located in a zip file on my
google drive. I can provide the raw files on request (Chris Rooper), the
code below downloads the aggregated layers from the google drive.

``` r
# bathy<-raster("GEBCO_bathy.tif")
# newproj<-crs(bathy)
# ICES_regions<-readOGR("Shapefiles","ICES_Areas_20160601_cut_dense_3857")
# ICES_regions<-spTransform(ICES_regions,newproj)
# ICES_regions3<-subset(ICES_regions,as.numeric(as.character(ICES_regions$SubArea))>5&as.numeric(as.character(ICES_regions$SubArea))<13)
# plot(bathy)
# plot(ICES_regions3,add=TRUE,border="red")
# points(cbind(Acanth_arm$Longitude[Acanth_arm$Presence==1],Acanth_arm$Latitude[Acanth_arm$Presence==1]),pch=20,col="purple")
# 
# ICES_bathy<-crop(bathy,ICES_regions3,progress="text",overwrite=TRUE,filename="ICES_bathy3")
# ICES_bathy[ICES_bathy>0]<-NA
# ICES_bathy<-ICES_bathy*-1
# writeRaster(ICES_bathy,"ICES_bathy3",overwrite=TRUE)
# plot(ICES_bathy)
# 
# file1<-nc_open("woa18_all_o00_01.nc")
# O2<-ncvar_get(file1,varid="o_an",collapse_degen = FALSE)
# Lat<-as.vector(ncvar_get(file1,varid="lat"))
# Lon<-as.vector(ncvar_get(file1,varid="lon"))
# Depth<-as.vector(ncvar_get(file1,varid="depth"))
# Lon1<-rep(Lon,length(Lat)*length(Depth))
# Lat1<-rep(rep(Lat,each=length(Lon)),length(Depth))
# Depth1<-rep(rep(Depth,each=length(Lon)),each=length(Lat))
# O2<-as.vector(ncvar_get(file1,varid="o_an"))
# O2_data<-data.frame(Longitude=Lon1,Latitude=Lat1,Depth=Depth1)
# t1<-which(O2_data$Latitude>=30&O2_data$Latitude<=87&O2_data$Longitude>=-80&O2_data$Longitude<=60)
# O2_data<-data.frame(Longitude=Lon1[t1],Latitude=Lat1[t1],Depth=Depth1[t1],O2=O2[t1])
# O2_data<-subset(O2_data,O2_data$O2>=0)
# O2_depth<-aggregate(Depth~Longitude+Latitude,data=O2_data,FUN="min")
# O2_data<-merge(O2_depth,O2_data,by=c("Longitude","Latitude","Depth"))
# nc_close(file1)
# 
# O2.project<-SpatialPointsDataFrame(coords=c(O2_data["Longitude"],O2_data["Latitude"]),data=O2_data["O2"], proj4string=newproj) 
# t1<-which(raster::extract(ICES_bathy,O2.project)>0)
# O2.project<-O2.project[t1,]
# 
# #             Interpolate to raster
# O2.idw<-gstat(id = "O2", formula = O2~1, data=O2.project, nmax=8, set=list(idp = 2.5))
# O2.raster<-interpolate(ICES_bathy,O2.idw,overwrite=TRUE,xyOnly=TRUE,filename="O2raster", progress="text")
# 
# O2.raster<-mask(O2.raster,ICES_bathy,overwrite=TRUE, filename="O2raster2")
# plot(O2.raster)
# 
# O2.raster<-aggregate(O2.raster,fact=2,filename="O2",FUN="mean",progress="text",overwrite=TRUE)
# bathy<-aggregate(ICES_bathy,fact=2,filename="bathy",FUN="mean",progress="text",overwrite=TRUE)
```

From the aggregated bathymetry two derived variables (slope and
topographic position index) were calculated using the raster package
(Hijmans 2019). The four explanatory variables are shown in Figure 2.

``` r
#Download the depth, oxygen and ICES area rasters and polygon from the google drive
temp <- tempfile(fileext = ".zip")
drive_deauth()
drive_download(as_id("1k0j9yTDFAme0zwxRLOJtrip3TXjKI9L6"), path = temp, overwrite = TRUE)
unzip(temp,overwrite=TRUE)

#Read in the raster layers and derive slope and TPI from the bathymerty
ICES_bathy<-raster("./ICES_variables/bathy")
ICES_slope<-terrain(ICES_bathy,opt="slope",progress="text",overwrite=TRUE)
ICES_TPI<-terrain(ICES_bathy,opt="tpi",progress="text",overwrite=TRUE)
O2.raster<-raster("./ICES_variables/O2")
raster.stack<-stack(ICES_bathy,ICES_slope,ICES_TPI,O2.raster)
names(raster.stack)<-c("bathy","slope","TPI","O2")
#Extract the explanatory variables to the VME data locations
variables<-data.frame(raster::extract(raster.stack,cbind(Acanth_arm$Longitude,Acanth_arm$Latitude)))
Acanth_arm<-cbind(Acanth_arm,variables)
variables<-subset(variables,variables$bathy>0)
Acanth_arm<-subset(Acanth_arm,Acanth_arm$bathy>0)

#Import the ICES region shapefile and subset to include only SubAreas 6-12
newproj<-crs(ICES_bathy)
ICES_regions<-readOGR("./ICES_variables/Shapefiles","ICES_Areas_20160601_cut_dense_3857")
ICES_regions<-spTransform(ICES_regions,newproj)
ICES_regions3<-subset(ICES_regions,as.numeric(as.character(ICES_regions$SubArea))>5&as.numeric(as.character(ICES_regions$SubArea))<13)

png("./Figures/Figure2.png",height=6,width=6,units="in",res=300)
plot(raster.stack)
dev.off()
```

<div class="figure">

<img src="C:/Users/rooperc/Desktop/ICES_WKPHM/./Figures/Figure2.png" alt="Figure 2. Map of bathymetry, slope, TPI and Oxygen used as explanatory variables in this analysis of ICES VME data" width="1800" />

<p class="caption">

Figure 2. Map of bathymetry, slope, TPI and Oxygen used as explanatory
variables in this analysis of ICES VME data

</p>

</div>

### Collinearity

The four explanatory variables were examined for collinearity using a
pearson correlations (Figure 3). Variance inflation inflation factors
(Zuur et al. 2002) were also examined. In both cases the values were
low, suggesting that the variables were fairly independent of each
other.

``` r
cormat1<-cor(variables,use="complete.obs")
png("./Figures/Figure3.png",width=6,height=6,units="in",res=300)
corrplot(cormat1,method="number",type="lower")
dev.off()
```

    ## 
    ## -------------------
    ##   &nbsp;     GVIF  
    ## ----------- -------
    ##  **bathy**   1.235 
    ## 
    ##  **slope**   1.189 
    ## 
    ##   **TPI**    1.004 
    ## 
    ##   **O2**     1.112 
    ## -------------------

<div class="figure">

<img src="C:/Users/rooperc/Desktop/ICES_WKPHM/./Figures/Figure3.png" alt="Figure 3. Correlation among independent variables used in modeling." width="1800" />

<p class="caption">

Figure 3. Correlation among independent variables used in modeling.

</p>

</div>

## Model building

To build the model of *Acanthogorgia armata* a generalized linear model
was constructed that contained four explanatory variables (depth, slope,
topographic position index and oxygen). Up to second order polynomials
were included and the dependent data was presence or absence of
*Acanthogorgia armata*. The full model was

\[y = \alpha+\beta_{1}depth+\beta_{2}slope+\beta_{3}TPI+\beta_{4}O_{2}+\beta_{5}depth^2+\beta_{6}slope^2+\beta_{7}TPI^2+\beta_{8}O_{2}^2+\sigma\]

### Model fitting

A binomial error distribution (\(\sigma\)) was used for the model
fitting. A full model was fit initially containing all the variables and
polynomials. This model was reduced sequentially by removing the least
significant term and comparing the AIC for the resulting reduced model.
This was repeated until there was no reduction in AIC when removing a
variable and all variables remaining in the model were significant.

The results of the sequential variable reduction was the removal of the
depth variable. Slope, TPI and Oxygen (and the polynomials for these
variables) were all signficant.

``` r
TableData<-data.frame(Fold=character(),AIC=numeric(),threshold=numeric(),AUC_training=numeric(),AUC_testing=numeric(),TSS_training=numeric(),TSS_testing=numeric(),Cor_training=numeric(),Cor_testing=numeric(),RMSE_training=numeric(),RMSE_testing=numeric(),stringsAsFactors=FALSE)

Acanth.GLM.pa<-glm(Presence~bathy+slope+TPI+O2+I(bathy^2)+I(slope^2)+I(TPI^2)+I(O2^2),data=Acanth_arm,family=binomial)
summary(Acanth.GLM.pa)
```

    ## 
    ## Call:
    ## glm(formula = Presence ~ bathy + slope + TPI + O2 + I(bathy^2) + 
    ##     I(slope^2) + I(TPI^2) + I(O2^2), family = binomial, data = Acanth_arm)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8506  -0.0609  -0.0391  -0.0085   3.5760  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -6.008e+02  1.485e+02  -4.046 5.21e-05 ***
    ## bathy       -1.051e-04  8.873e-04  -0.118 0.905718    
    ## slope        7.828e+01  2.131e+01   3.673 0.000240 ***
    ## TPI          6.872e-02  3.265e-02   2.104 0.035344 *  
    ## O2           4.754e+00  1.174e+00   4.051 5.10e-05 ***
    ## I(bathy^2)  -5.258e-08  2.295e-07  -0.229 0.818759    
    ## I(slope^2)  -4.595e+02  1.343e+02  -3.422 0.000621 ***
    ## I(TPI^2)    -2.806e-03  1.181e-03  -2.376 0.017503 *  
    ## I(O2^2)     -9.467e-03  2.313e-03  -4.092 4.27e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 249.07  on 3733  degrees of freedom
    ## Residual deviance: 171.70  on 3725  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 189.7
    ## 
    ## Number of Fisher Scoring iterations: 12

``` r
#Stepwise reduction based on AIC
Acanth.GLM.pa<-glm(Presence~slope+TPI+O2+I(slope^2)+I(TPI^2)+I(O2^2),data=Acanth_arm,family=binomial)
summary(Acanth.GLM.pa)
```

    ## 
    ## Call:
    ## glm(formula = Presence ~ slope + TPI + O2 + I(slope^2) + I(TPI^2) + 
    ##     I(O2^2), family = binomial, data = Acanth_arm)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7619  -0.0595  -0.0392  -0.0089   3.6151  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.603e+02  1.400e+02  -4.003 6.26e-05 ***
    ## slope        7.706e+01  1.980e+01   3.893 9.92e-05 ***
    ## TPI          6.863e-02  3.257e-02   2.107 0.035107 *  
    ## O2           4.428e+00  1.105e+00   4.008 6.12e-05 ***
    ## I(slope^2)  -4.568e+02  1.320e+02  -3.461 0.000537 ***
    ## I(TPI^2)    -2.718e-03  1.161e-03  -2.342 0.019201 *  
    ## I(O2^2)     -8.816e-03  2.176e-03  -4.051 5.10e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 249.07  on 3733  degrees of freedom
    ## Residual deviance: 173.02  on 3727  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 187.02
    ## 
    ## Number of Fisher Scoring iterations: 12

``` r
######TEST THE PREDICTIONS AGAINST THE TRAINING DATA############################
TableData[1,1]<-"Full model"
train.auc_data<-data.frame(cbind(seq(1,length(Acanth.GLM.pa$y),1),Acanth.GLM.pa$y,Acanth.GLM.pa$fitted.values))
##Calculate the AUC
TableData[1,4]<-round(auc(train.auc_data,na.rm=TRUE)[1],3)
#Calculate the RMSE
TableData[1,10]<-round(sqrt(mean((train.auc_data$X2-train.auc_data$X3)^2)),3)
#Calculate the TSS
train.threshold<-optimal.thresholds(train.auc_data,opt.methods=3)[1,2]
TableData[1,3]<-train.threshold
TableData[1,6]<-round(sensitivity(cmx(train.auc_data,threshold=train.threshold))+specificity(cmx(train.auc_data,threshold=train.threshold))-1,3)[1]
#Calculate the Spearmans Rank correlation
TableData[1,8]<-round(cor.test(train.auc_data[,2],train.auc_data[,3],method="spearman")$estimate,3)
#AIC
TableData[1,2]<-Acanth.GLM.pa$aic

print(cmx(train.auc_data,threshold=train.threshold))
```

    ##          observed
    ## predicted    1    0
    ##         1   17  346
    ##         0    3 3368

``` r
#RESPONSE CURVE PLOTS
## generate prediction frame
TPIframe <- data.frame(TPI=seq(min(Acanth_arm$TPI,na.rm=TRUE),max(Acanth_arm$TPI,na.rm=TRUE),length=51),O2=median(Acanth_arm$O2,na.rm=TRUE),slope=median(Acanth_arm$slope,na.rm=TRUE))
TPIframe$predicted <- predict(Acanth.GLM.pa,newdata=TPIframe,type="response",se.fit=TRUE)$fit
TPIframe$se.fit <- predict(Acanth.GLM.pa,newdata=TPIframe,type="response",se.fit=TRUE)$se.fit

p3<-ggplot(TPIframe)+geom_ribbon(aes(x=TPI,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=TPI,y=predicted))+xlab("Topographic position index")+ylab("Probability of presence")


O2frame <- data.frame(O2=seq(min(Acanth_arm$O2,na.rm=TRUE),max(Acanth_arm$O2,na.rm=TRUE),length=51),slope=median(Acanth_arm$slope,na.rm=TRUE),TPI=median(Acanth_arm$TPI,na.rm=TRUE))
O2frame$predicted <- predict(Acanth.GLM.pa,newdata=O2frame,type="response",se.fit=TRUE)$fit
O2frame$se.fit <- predict(Acanth.GLM.pa,newdata=O2frame,type="response",se.fit=TRUE)$se.fit

p1<-ggplot(O2frame)+geom_ribbon(aes(x=O2,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=O2,y=predicted))+xlab("Oxygen")+ylab("Probability of presence")

slopeframe <- data.frame(slope=seq(min(Acanth_arm$slope,na.rm=TRUE),max(Acanth_arm$slope,na.rm=TRUE),length=51),O2=median(Acanth_arm$O2,na.rm=TRUE),TPI=median(Acanth_arm$TPI,na.rm=TRUE))
slopeframe$predicted <- predict(Acanth.GLM.pa,newdata=slopeframe,type="response",se.fit=TRUE)$fit
slopeframe$se.fit <- predict(Acanth.GLM.pa,newdata=slopeframe,type="response",se.fit=TRUE)$se.fit

p2<-ggplot(slopeframe)+geom_ribbon(aes(x=slope,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=slope,y=predicted))+xlab("Slope")+ylab("Probability of presence")

#plot it
png("./Figures/response_curvesFull.png",width=8,height=6,units="in",res=300)
grid.arrange(p1,p2,p3,ncol=1)
dev.off()
```

    ## png 
    ##   2

``` r
####FIGURE 3 - MAXENT DIAGNOSTICS PLOTS
#Plots for data
png(filename="./Figures/GLMdiagnosticsFull.png",width=6,height=6,res=300,units="in")
par(mfcol=c(2,2),family="sans",mar=c(4,4,1,.01))
auc.roc.plot(train.auc_data,opt.methods=2,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
calibration.plot(train.auc_data,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
```

    ##    BinCenter NBin      BinObs     BinPred BinObsCIlower BinObsCIupper
    ## 1       0.05 3693 0.004332521 0.003814336   0.002478376   0.007026232
    ## 2       0.15   36 0.083333333 0.131696602   0.017526496   0.224689761
    ## 3       0.25    5 0.200000000 0.234516165   0.005050763   0.716417936
    ## 4       0.35    0          NA          NA            NA            NA
    ## 5       0.45    0          NA          NA            NA            NA
    ## 6       0.55    0          NA          NA            NA            NA
    ## 7       0.65    0          NA          NA            NA            NA
    ## 8       0.75    0          NA          NA            NA            NA
    ## 9       0.85    0          NA          NA            NA            NA
    ## 10      0.95    0          NA          NA            NA            NA

``` r
presence.absence.hist(train.auc_data,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")
```

    ## [1] "height of tallest bar truncated to fit on plot"

``` r
dev.off()
```

    ## png 
    ##   2

### Model predictions

``` r
#######MAKE THE PREDICTION RASTER ##############################################
#Predict the model to a raster to see where suitable habitat is predicted
Acanth.GLM.pa.raster<-predict(raster.stack, Acanth.GLM.pa,filename="./Predictions/GLMFull",fun=predict, na.rm=TRUE,overwrite=TRUE,progress="text",type="response",newdata.guaranteed=TRUE)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

``` r
png(filename="./Figures/ProbabilityMapFull.png",width=6,height=6.5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(Acanth.GLM.pa.raster, main = "", box=F,col = viridis(255),ext=ICES_regions3,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of presence",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
dev.off()
```

    ## png 
    ##   2

## Model validation

``` r
##Sample training and testing data sets####
Acanth_arm$Group<-kfold(Acanth_arm,k=5)

foldlist<-vector(mode="list",length=5)

###LOOP THROUGH THE FOLDS####
for(i in 1:5){
training.data<-subset(Acanth_arm,Acanth_arm$Group!=i)
test.data<-subset(Acanth_arm,Acanth_arm$Group==i)

#######MAKE THE MODELS#############
Acanth.GLM.paf<-glm(Presence~slope+O2+TPI+I(slope^2)+I(O2^2)+I(TPI^2),data=training.data,family=binomial)
summary(Acanth.GLM.pa)
foldlist[[i]]<-Acanth.GLM.paf
names(foldlist[i])<-paste0("AcanthGLMFold",i)

######TEST THE PREDICTIONS AGAINST THE TRAINING DATA############################
TableData[i+1,1]<-paste0("GLMFold_",i)
train.auc_dataf<-data.frame(cbind(seq(1,length(Acanth.GLM.paf$y),1),Acanth.GLM.paf$y,Acanth.GLM.paf$fitted.values))
##Calculate the AUC
TableData[i+1,4]<-round(auc(train.auc_dataf,na.rm=TRUE)[1],3)
#Calculate the RMSE
TableData[i+1,10]<-round(sqrt(mean((train.auc_dataf$X2-train.auc_dataf$X3)^2)),3)
#Calculate the TSS
train.thresholdf<-optimal.thresholds(train.auc_dataf,opt.methods=3)[1,2]
TableData[i+1,3]<-train.thresholdf
TableData[i+1,6]<-round(sensitivity(cmx(train.auc_dataf,threshold=train.thresholdf))+specificity(cmx(train.auc_dataf,threshold=train.thresholdf))-1,3)[1]
#Calculate the Spearmans Rank correlation
TableData[i+1,8]<-round(cor.test(train.auc_dataf[,2],train.auc_dataf[,3],method="spearman")$estimate,3)
#AIC
TableData[i+1,2]<-Acanth.GLM.paf$aic

print(cmx(train.auc_dataf,threshold=train.thresholdf))

######TEST THE PREDICTIONS AGAINST THE TEST DATA############################
pred<-predict(Acanth.GLM.paf,newdata=test.data,type="response")
test.auc_dataf<-data.frame(cbind(seq(1,length(test.data$Presence),1),test.data$Presence,pred))
test.auc_dataf<-subset(test.auc_dataf,test.auc_dataf$pred>=0)
##Calculate the AUC
TableData[i+1,5]<-round(auc(test.auc_dataf,na.rm=TRUE)[1],3)
#Calculate the RMSE
TableData[i+1,11]<-round(sqrt(mean((test.auc_dataf$V2-test.auc_dataf$pred)^2)),3)
#Calculate the TSS
TableData[i+1,7]<-round(sensitivity(cmx(test.auc_dataf,threshold=train.thresholdf))+specificity(cmx(test.auc_dataf,threshold=train.thresholdf))-1,3)[1]
#Calculate the Spearmans Rank correlation
TableData[i+1,9]<-round(cor.test(test.auc_dataf[,2],test.auc_dataf[,3],method="spearman")$estimate,3)

print(cmx(test.auc_dataf,threshold=train.thresholdf))

#RESPONSE CURVE PLOTS
## generate prediction frame
TPIframe <- data.frame(TPI=seq(min(Acanth_arm$TPI,na.rm=TRUE),max(Acanth_arm$TPI,na.rm=TRUE),length=51),O2=median(Acanth_arm$O2,na.rm=TRUE),slope=median(Acanth_arm$slope,na.rm=TRUE))
TPIframe$predicted <- predict(Acanth.GLM.paf,newdata=TPIframe,type="response",se.fit=TRUE)$fit
TPIframe$se.fit <- predict(Acanth.GLM.paf,newdata=TPIframe,type="response",se.fit=TRUE)$se.fit

p3<-ggplot(TPIframe)+geom_ribbon(aes(x=TPI,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=TPI,y=predicted))+xlab("Topographic position index")+ylab("Probability of presence")

O2frame <- data.frame(O2=seq(min(Acanth_arm$O2,na.rm=TRUE),max(Acanth_arm$O2,na.rm=TRUE),length=51),slope=median(Acanth_arm$slope,na.rm=TRUE),TPI=median(Acanth_arm$TPI,na.rm=TRUE))
O2frame$predicted <- predict(Acanth.GLM.paf,newdata=O2frame,type="response",se.fit=TRUE)$fit
O2frame$se.fit <- predict(Acanth.GLM.paf,newdata=O2frame,type="response",se.fit=TRUE)$se.fit

p1<-ggplot(O2frame)+geom_ribbon(aes(x=O2,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=O2,y=predicted))+xlab("Oxygen")+ylab("Probability of presence")

slopeframe <- data.frame(slope=seq(min(Acanth_arm$slope,na.rm=TRUE),max(Acanth_arm$slope,na.rm=TRUE),length=51),O2=median(Acanth_arm$O2,na.rm=TRUE),TPI=median(Acanth_arm$TPI,na.rm=TRUE))
slopeframe$predicted <- predict(Acanth.GLM.paf,newdata=slopeframe,type="response",se.fit=TRUE)$fit
slopeframe$se.fit <- predict(Acanth.GLM.paf,newdata=slopeframe,type="response",se.fit=TRUE)$se.fit

p2<-ggplot(slopeframe)+geom_ribbon(aes(x=slope,ymin=predicted-se.fit,ymax=predicted+se.fit),fill="grey70") + geom_line(aes(x=slope,y=predicted))+xlab("Slope")+ylab("Probability of presence")

#plot it
png(paste0("./Figures/response_curvesfold",i,".png"),width=8,height=6,units="in",res=300)
grid.arrange(p1,p2,p3,ncol=1)
dev.off()

####FIGURE 3 - MAXENT DIAGNOSTICS PLOTS
#Plots for data
png(filename=paste0("./Figures/GLMdiagnosticsfold",i,".png"),width=6,height=6,res=300,units="in")
par(mfcol=c(2,2),family="sans",mar=c(4,4,1,.01))
auc.roc.plot(train.auc_data,opt.methods=2,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
calibration.plot(train.auc_data,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
presence.absence.hist(train.auc_data,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")
dev.off()

Acanth.GLM.pa.rasterf<-predict(raster.stack, Acanth.GLM.paf,filename=paste("./Predictions/GLMFold",i,sep=""),fun=predict, na.rm=TRUE,overwrite=TRUE,progress="text",type="response",newdata.guaranteed=TRUE)


png(filename=paste0("./Figures/ProbabilityMapFold",i,".png"),width=6,height=6.5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(Acanth.GLM.pa.rasterf, main = "", box=F,col = viridis(255),ext=ICES_regions3,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of presence",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
dev.off()
}
```

    ##          observed
    ## predicted    1    0
    ##         1   10  253
    ##         0    4 2721
    ##          observed
    ## predicted   1   0
    ##         1   4  65
    ##         0   2 675

    ## [1] "height of tallest bar truncated to fit on plot"

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

    ##          observed
    ## predicted    1    0
    ##         1   16  208
    ##         0    3 2761
    ##          observed
    ## predicted   1   0
    ##         1   0  46
    ##         0   1 699

    ## [1] "height of tallest bar truncated to fit on plot"

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

    ##          observed
    ## predicted    1    0
    ##         1   14  309
    ##         0    4 2659
    ##          observed
    ## predicted   1   0
    ##         1   2  86
    ##         0   0 660

    ## [1] "height of tallest bar truncated to fit on plot"

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

    ##          observed
    ## predicted    1    0
    ##         1   11  270
    ##         0    3 2703
    ##          observed
    ## predicted   1   0
    ##         1   5  53
    ##         0   1 688

    ## [1] "height of tallest bar truncated to fit on plot"

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

    ##          observed
    ## predicted    1    0
    ##         1   13  236
    ##         0    2 2736
    ##          observed
    ## predicted   1   0
    ##         1   3  67
    ##         0   2 675

    ## [1] "height of tallest bar truncated to fit on plot"

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
    ## 

## Model uncertainty

### Spatial patterns in residuals

``` r
##################################################################

Acanth.GLM.padata<-subset(Acanth.GLM.pa$data,Acanth.GLM.pa$data$slope>=0)
Acanth.GLM.padata$residuals<-Acanth.GLM.pa$residuals

p<-ggplot()+
  #basemap
  geom_sf(data = bg)+
  coord_sf(xlim = range(Acanth.GLM.padata$Longitude, na.rm = TRUE), 
           ylim = range(Acanth.GLM.padata$Latitude, na.rm = TRUE), 
           expand = TRUE)+
  
  # add points
  geom_point(data = Acanth.GLM.padata, 
             aes(x=Longitude,y=Latitude,fill=residuals),
             alpha = 0.7, shape=21, size = 2)+
  
  # formatting
  scale_fill_viridis_c(option = "magma")+
  scale_color_viridis_c(option = "magma")+
  theme_dark()+
  theme(panel.grid = element_blank())

png(filename="./Figures/Residuals.png",width=6,height=7,res=300,units="in")
print(p)
dev.off()
```

    ## png 
    ##   2

``` r
#Calculate prediction error and SE

F1<-raster("./Predictions/GLMFold1")
F2<-raster("./Predictions/GLMFold2")
F3<-raster("./Predictions/GLMFold3")
F4<-raster("./Predictions/GLMFold4")
F5<-raster("./Predictions/GLMFold5")
Error.stack<-stack(F1,F2,F3,F4,F5)
#plot(Error.stack)
Prediction.se<-calc(Error.stack,fun=sd)

png(filename="./Figures/Fullmapse.png",width=6,height=7,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(Prediction.se, main = "", box=F,col = colorRampPalette(c("darkgreen","coral"))(30),ext=ICES_regions3,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Prediction standard error",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
dev.off()
```

    ## png 
    ##   2
