install.packages("paleobioDB")
install.packages("ggplot2")
install.packages("CoordinateCleaner")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("divDyn")
install.packages("divvy")
install.packages("readxl")
install.packages("velociraptr")
library("paleobioDB")
library("ggplot2")
library("pacman")
library("CoordinateCleaner")
library("velociraptr")
library("dplyr")
library("divDyn")
library("palaeoverse")
library("tidyverse")
library("divvy")
library("readxl")

setwd("D:/USGS Internship Projects/R and Jupyter Files")

# Load in PBDB that has lat and lng data

DataPBDB <-downloadPBDB(Taxa="Brachiopoda",StartInterval="Cambrian",StopInterval = "Devonian")
DataPBDB4 <- downloadPBDB(Taxa="Brachiopoda", StartInterval = "Carboniferous", StopInterval = "Triassic")
DataPBDB2 <- downloadPBDB(Taxa="Brachiopoda", StartInterval = "Jurassic", StopInterval = "Miocene")
DataPBDB3 <- downloadPBDB(Taxa="Brachiopoda", StartInterval = "Pliocene", StopInterval = "Holocene")

DataPBDB_Cni <-downloadPBDB(Taxa="Cnidaria",StartInterval="Cambrian",StopInterval = "Devonian")
DataPBDB4_Cni <- downloadPBDB(Taxa="Cnidaria", StartInterval = "Carboniferous", StopInterval = "Triassic")
DataPBDB2_Cni <- downloadPBDB(Taxa="Cnidaria", StartInterval = "Jurassic", StopInterval = "Miocene")
DataPBDB3_Cni <- downloadPBDB(Taxa="Cnidaria", StartInterval = "Pliocene", StopInterval = "Holocene")
## ^ We have downloaded all the data to the genus level and then bound it all together
# to be included into one dataframe from the Cambrain to the Holocene 

#Binding of the data
Cam_Hol_Brac<- rbind(DataPBDB,DataPBDB2,DataPBDB3,DataPBDB4)
Cam_Hol_Cni<-rbind(DataPBDB_Cni,DataPBDB2_Cni,DataPBDB3_Cni,DataPBDB4_Cni)
#Creating bins for spatial correlation
bins<-lat_bins(size=10, max=90,min=-90,fit=TRUE)
#creating dataframe that includes bins 
bin_lat_df<-bin_lat(occdf=Cam_Hol_Brac,bins=bins,lat="lat", boundary=FALSE)

# Coordinate Cleaning for dataframe

#validates all data has both lat and long
cl <- cc_val(bin_lat_df,lat= "lat", lon= "lng")
cl <- cc_equ(cl, lat = "lat", lon = "lng")
#Removes data at centroids of political zones
cl <- cc_cen(cl, lat = "lat", lon = "lng")
#Removes data asociated with biodiversity institutions
cl <- cc_inst(cl, lat = "lat", lon = "lng")
#removing data from GBIF headquarters
cl <- cc_gbif(cl, lat = "lat", lon = "lng")
#removing data at 0,0 lat and long
cl <- cc_zero(cl, lat = "lat", lon = "lng")
#removing data with an age range greater than 35 Ma on the dataset and taxa level.
cl <- cf_range(cl, taxon = "", min_age = "min_ma", max_age = "max_ma",lat="lat",lon = "lng")
cl <- cf_range(cl, taxon = "accepted_name", min_age = "min_ma", max_age = "max_ma",lon= "lng", lat= "lat")
#Removing outlieres on the genus level
cl <- cf_outl(cl, taxon = "accepted_name", lat = "lat", lon = "lng",
              min_age = "min_ma", max_age = "max_ma")
nrow(Cam_Hol_Brac) - nrow(cl)


n<-45
set.seed(1)
bandit_df<-bandit(dat = cl ,xy=c("lng","lat"),nSite= n,bin = 36, iter = 5, output = "full")
bandit1<-bandit_df[["[-90,-54)"]]
nrow(bandit1)
bandit2<-bandit_df[["[-54,-18)"]]
nrow(bandit2)
bandit3<-bandit_df[["[-18,18)"]]
nrow(bandit3)
bandit4<-bandit_df[["[18,54)"]]
nrow(bandit4)
bandit5<-bandit_df[["[54,90)"]]
nrow(bandit5)
all_bandit<-rbind(bandit1,bandit2,bandit3,bandit4,bandit5)
bandit_1_5<-rbind(bandit1,bandit5)

interval.ma    <- all_bandit %>%  #this will properly resolve your data to stage level
  group_by(early_interval) %>% 
  summarise(min_ma = min(min_ma))
names(interval.ma) <-c("early_interval", "interval.ma")
all_bandit <- merge(all_bandit, interval.ma, by=c("early_interval"))

# Find first and last occurrences and merge back into data frame, using min_ma column
fadlad <-  all_bandit %>%
  group_by(accepted_name) %>%
  summarise(
    fad = max(interval.ma),
    lad = min(interval.ma)
  )

# Merge fad and lad information into data frame
pbdb_brac <- merge(all_bandit, fadlad, by=c("accepted_name")) #accepted_name is the corrected version of genus names

fad_hist <- hist(pbdb_brac$fad, breaks = 27, plot = FALSE)
lad_hist <- hist(pbdb_brac$lad, breaks = 27, plot = FALSE)

# Set up the plotting area
plot(fad_hist$mids, fad_hist$counts, type = "n", 
     xlab = "Age", ylab = "Frequency", main = "Histogram of Brachiopod Genus Data by FAD and LAD",
     xlim = rev(range(fad_hist$breaks)), ylim = c(0, max(fad_hist$counts, lad_hist$counts)),
     xaxt= "n")

# Add the FAD histogram
rect(fad_hist$breaks[-length(fad_hist$breaks)], 0, fad_hist$breaks[-1], fad_hist$counts, 
     col = rgb(30/255, 144/255, 255/255, 0.5), border = NA)

# Add the LAD histogram
rect(lad_hist$breaks[-length(lad_hist$breaks)], 0, lad_hist$breaks[-1], lad_hist$counts, 
     col = rgb(255/255, 105/255, 180/255, 0.5), border = NA)

# Adding a legend
legend("topright", legend = c("First Appearance Datum", "Last Appearance Datum"), 
       fill = c(rgb(30/255, 144/255, 255/255, 0.5), rgb(255/255, 105/255, 180/255, 0.5)),
       border = NA)
#adding axis_geo
box()
axis_geo(side=1, intervals="periods",height=0.13)

#Brachiopod High latitude#############################
interval.ma_15    <- bandit_1_5 %>%  #this will properly resolve your data to stage level
  group_by(early_interval) %>% 
  summarise(min_ma = min(min_ma))
names(interval.ma_15) <-c("early_interval", "interval.ma")
bandit_1_5<- merge(bandit_1_5, interval.ma_15, by=c("early_interval"))

# Find first and last occurrences and merge back into data frame, using min_ma column
fadlad_15 <-  bandit_1_5 %>%
  group_by(accepted_name) %>%
  summarise(
    fad_15 = max(interval.ma),
    lad_15 = min(interval.ma)
  )

# Merge fad and lad information into data frame
pbdb_brac_15 <- merge(bandit_1_5, fadlad_15, by=c("accepted_name")) #accepted_name is the corrected version of genus names

fad_hist_15 <- hist(pbdb_brac_15$fad, breaks = 27, plot = FALSE)
lad_hist_15 <- hist(pbdb_brac_15$lad, breaks = 27, plot = FALSE)

# Set up the plotting area
plot(fad_hist_15$mids, fad_hist_15$counts, type = "n", 
     xlab = "Age", ylab = "Frequency", main = "Histogram of Brachiopod Genus Data by FAD and LAD",
     xlim = rev(range(fad_hist_15$breaks)), ylim = c(0, max(fad_hist_15$counts, lad_hist_15$counts)),
     xaxt= "n")

# Add the FAD histogram
rect(fad_hist_15$breaks[-length(fad_hist_15$breaks)], 0, fad_hist_15$breaks[-1], fad_hist_15$counts, 
     col = rgb(30/255, 144/255, 255/255, 0.5), border = NA)

# Add the LAD histogram
rect(lad_hist$breaks[-length(lad_hist$breaks)], 0, lad_hist$breaks[-1], lad_hist$counts, 
     col = rgb(255/255, 105/255, 180/255, 0.5), border = NA)

# Adding a legend
legend("topright", legend = c("First Appearance Datum", "Last Appearance Datum"), 
       fill = c(rgb(30/255, 144/255, 255/255, 0.5), rgb(255/255, 105/255, 180/255, 0.5)),
       border = NA)
#adding axis_geo
box()
axis_geo(side=1, intervals="periods",height=0.13)

#Cnidarian Data 

set.seed(2)
bin_lat_df_cni<-bin_lat(occdf=Cam_Hol_Cni,bins=bins,lat="lat", boundary=FALSE)

#Coordinate Cleaner Checks for Cnidaraia Data
rm("cl_c_C")
#validates all data has both lat and long
cl_c <- cc_val(bin_lat_df_cni,lat= "lat", lon= "lng")
cl_c <- cc_equ(cl_c, lat = "lat", lon = "lng")
#Removes data at centroids of political zones
cl_c <- cc_cen(cl_c, lat = "lat", lon = "lng")
#Removes data asociated with biodiversity institutions
cl_c <- cc_inst(cl_c, lat = "lat", lon = "lng")
#removing data from GBIF headquarters
cl_c <- cc_gbif(cl_c, lat = "lat", lon = "lng")
#removing data at 0,0 lat and long
cl_c <- cc_zero(cl_c, lat = "lat", lon = "lng")
#removing data with an age range greater than 35 Ma on the dataset and taxa level.
cl_c <- cf_range(cl_c, taxon = "", min_age = "min_ma", max_age = "max_ma",lat="lat",lon = "lng")
cl_c <- cf_range(cl_c, taxon = "accepted_name", min_age = "min_ma", max_age = "max_ma",lon= "lng", lat= "lat")
#Removing outlieres on the genus level
cl_c <- cf_outl(cl_c, taxon = "accepted_name", lat = "lat", lon = "lng",
                min_age = "min_ma", max_age = "max_ma")
nrow(Cam_Hol_Cni) - nrow(cl_c)


n_cni<-25
set.seed(3)
bandit_df_cni<-bandit(dat = cl_c ,xy=c("lng","lat"),nSite= n_cni,bin = 36, iter = 5, output = "full")
bandit1_cni<-bandit_df_cni[["[-90,-54)"]]
nrow(bandit1_cni)
bandit2_cni<-bandit_df_cni[["[-54,-18)"]]
nrow(bandit2_cni)
bandit3_cni<-bandit_df_cni[["[-18,18)"]]
nrow(bandit3_cni)
bandit4_cni<-bandit_df_cni[["[18,54)"]]
nrow(bandit4_cni)
bandit5_cni<-bandit_df_cni[["[54,90)"]]
nrow(bandit5_cni)
all_bandit_cni<-rbind(bandit1_cni,bandit2_cni,bandit3_cni,bandit4_cni,bandit5_cni)
bandit_1_5_cni<-rbind(bandit1_cni,bandit5_cni)
# Adjust radiometric ages
interval.ma.cni    <- all_bandit_cni %>%  #this will properly resolve your data to stage level
  group_by(early_interval) %>% 
  summarise(min_ma = min(min_ma))
names(interval.ma.cni) <-c("early_interval", "interval.ma")
all_bandit_cni <- merge(all_bandit_cni, interval.ma.cni, by=c("early_interval"))


fadlad_CniA <- all_bandit_cni %>%
  group_by(accepted_name) %>%
  summarise(
    fad_CniA = max(interval.ma),
    lad_CniA = min(interval.ma)
  )
# Merge fad and lad information into data frame
pbdb_CniA <- merge(all_bandit_cni, fadlad_CniA, by=c("accepted_name")) #accepted_name is the corrected version of genus names

#Base R plotting for Cnidaria
fad_hist_cni <- hist(pbdb_CniA$fad, breaks = 27, plot = FALSE)
lad_hist_cni <- hist(pbdb_CniA$lad, breaks = 27, plot = FALSE)

# Set up the plotting area
plot(fad_hist_cni$mids, fad_hist_cni$counts, type = "n", 
     xlab = "", ylab = "Frequency", main = "",
     xlim = rev(range(fad_hist_cni$breaks)), ylim = c(0, max(fad_hist_cni$counts, lad_hist_cni$counts)),
     xaxt= "n")

# Add the FAD histogram
rect(fad_hist_cni$breaks[-length(fad_hist_cni$breaks)], 0, fad_hist_cni$breaks[-1], fad_hist_cni$counts, 
     col = rgb(30/255, 144/255, 255/255, 0.5), border = NA)

# Add the LAD histogram
rect(lad_hist_cni$breaks[-length(lad_hist_cni$breaks)], 0, lad_hist_cni$breaks[-1], lad_hist_cni$counts, 
     col = rgb(255/255, 105/255, 180/255, 0.5), border = NA)

# Adding a legend
legend("topleft", legend = c("First Appearance Datum", "Last Appearance Datum"), 
       fill = c(rgb(30/255, 144/255, 255/255, 0.5), rgb(255/255, 105/255, 180/255, 0.5)),
       border = NA,)

#adding axis_geo
box()
axis_geo(side=1, intervals="periods",height=0.13)

#High latitude only Cnidarian

interval.ma.cni.15    <- bandit_1_5_cni %>%  #this will properly resolve your data to stage level
  group_by(early_interval) %>% 
  summarise(min_ma = min(min_ma))
names(interval.ma.cni.15) <-c("early_interval", "interval.ma")
bandit_1_5_cni <- merge(bandit_1_5_cni, interval.ma.cni.15, by=c("early_interval"))


fadlad_CniA_15 <- bandit_1_5_cni %>%
  group_by(accepted_name) %>%
  summarise(
    fad_CniA_15 = max(interval.ma),
    lad_CniA_15 = min(interval.ma)
  )
# Merge fad and lad information into data frame
pbdb_CniA.15 <- merge(bandit_1_5_cni, fadlad_CniA_15, by=c("accepted_name")) #accepted_name is the corrected version of genus names

#Base R plotting for Cnidaria
fad_hist_cni_15 <- hist(pbdb_CniA.15$fad, breaks = 27, plot = FALSE)
lad_hist_cni_15 <- hist(pbdb_CniA.15$lad, breaks = 27, plot = FALSE)

# Set up the plotting area
plot(fad_hist_cni_15$mids, fad_hist_cni_15$counts, type = "n", 
     xlab = "", ylab = "Frequency", main = "",
     xlim = rev(range(fad_hist_cni_15$breaks)), ylim = c(0, max(fad_hist_cni_15$counts, lad_hist_cni$counts)),
     xaxt= "n")

# Add the FAD histogram
rect(fad_hist_cni_15$breaks[-length(fad_hist_cni_15$breaks)], 0, fad_hist_cni_15$breaks[-1], fad_hist_cni_15$counts, 
     col = rgb(30/255, 144/255, 255/255, 0.5), border = NA)

# Add the LAD histogram
rect(lad_hist_cni_15$breaks[-length(lad_hist_cni_15$breaks)], 0, lad_hist_cni_15$breaks[-1], lad_hist_cni_15$counts, 
     col = rgb(255/255, 105/255, 180/255, 0.5), border = NA)

# Adding a legend
legend("topleft", legend = c("First Appearance Datum", "Last Appearance Datum"), 
       fill = c(rgb(30/255, 144/255, 255/255, 0.5), rgb(255/255, 105/255, 180/255, 0.5)),
       border = NA,)

#adding axis_geo
box()
axis_geo(side=1, intervals="periods",height=0.13)

#Circular Subampling

circular_brac<-cookies(dat= cl,xy=c("lat","lng"),iter = 5,nSite = n,r=50,weight=FALSE,output='full')
warnings()
circ_brac_1<-circular_brac[[1]]
circ_brac_2<-circular_brac[[2]]
circ_brac_3<-circular_brac[[3]]
circ_brac_4<-circular_brac[[4]]
circ_brac_5<-circular_brac[[5]]
circ_brac_all<- rbind(circ_brac_1,circ_brac_2,circ_brac_3,circ_brac_4,circ_brac_5)

interval.ma.circbrac    <- circ_brac_all %>%  #this will properly resolve your data to stage level
  group_by(early_interval) %>% 
  summarise(min_ma = min(min_ma))
names(interval.ma.circbrac) <-c("early_interval", "interval.ma")
circ_brac_all <- merge(circ_brac_all, interval.ma.circbrac, by=c("early_interval"))


fadlad_circbrac <- circ_brac_all %>%
  group_by(accepted_name) %>%
  summarise(
    fad_circbrac = max(interval.ma),
    lad_circbrac = min(interval.ma)
  )
# Merge fad and lad information into data frame
pbdb_circbrac <- merge(circ_brac_all, fadlad_circbrac, by=c("accepted_name")) #accepted_name is the corrected version of genus names

#Base R plotting for Cnidaria
fad_hist_circbrac <- hist(pbdb_circbrac$fad, breaks = 27, plot = FALSE)
lad_hist_circbrac <- hist(pbdb_circbrac$lad, breaks = 27, plot = FALSE)

# Set up the plotting area
plot(fad_hist_circbrac$mids, fad_hist_circbrac$counts, type = "n", 
     xlab = "", ylab = "Frequency", main = "",
     xlim = rev(range(fad_hist_circbrac$breaks)), ylim = c(0, max(fad_hist_circbrac$counts, lad_hist_circbrac$counts)),
     xaxt= "n")

# Add the FAD histogram
rect(fad_hist_circbrac$breaks[-length(fad_hist_circbrac$breaks)], 0, fad_hist_circbrac$breaks[-1], fad_hist_circbrac$counts, 
     col = rgb(30/255, 144/255, 255/255, 0.5), border = NA)

# Add the LAD histogram
rect(lad_hist_circbrac$breaks[-length(lad_hist_circbrac$breaks)], 0, lad_hist_circbrac$breaks[-1], lad_hist_circbrac$counts, 
     col = rgb(255/255, 105/255, 180/255, 0.5), border = NA)

# Adding a legend
legend("topleft", legend = c("First Appearance Datum", "Last Appearance Datum"), 
       fill = c(rgb(30/255, 144/255, 255/255, 0.5), rgb(255/255, 105/255, 180/255, 0.5)),
       border = NA,)

#adding axis_geo
box()
axis_geo(side=1, intervals="periods",height=0.13)

