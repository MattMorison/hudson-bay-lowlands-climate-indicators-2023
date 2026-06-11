#Code adapted from: https://cran.r-project.org/web/packages/futureheatwaves/vignettes/starting_from_netcdf.html
#Notepad++ Alt+Shift+arrows - select mult

library(ncdf4)
library(ncdf4.helpers)
library(PCICt)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(CRHMr)
library(ggfan)

####The following sections of code (up to line 1490) read netCDF files and create a dataframe for each of four calendars (e.g. leap, no leap, 360-day, gregorian)
####Update working dir for each variable to the folder containing the relevant variable, then should be good to run these

##############################################
############MAXIMUM TEMPERATURE###############
##############################################


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./HBL_SmBoundBox_MaximumTemperature")

bcccsm11_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bnuesm_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bnuesm_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bnuesm_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
canesm2_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
canesm2_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
canesm2_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ccsm4_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ccsm4_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ccsm4_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
csiromk360_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
csiromk360_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
csiromk360_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
miroc5_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
miroc5_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
miroc5_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesm_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesm_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesm_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1m_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1m_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
noresm1m_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1me_26_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_45_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_85_tmax <- nc_open("tasmax_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")


#Identify type of calendar used by each model
bcccsm11_26_tmax$dim$time$calendar
bcccsm11_45_tmax$dim$time$calendar
bcccsm11_85_tmax$dim$time$calendar
bcccsm11m_26_tmax$dim$time$calendar
bcccsm11m_45_tmax$dim$time$calendar
bcccsm11m_85_tmax$dim$time$calendar
bnuesm_26_tmax$dim$time$calendar
bnuesm_45_tmax$dim$time$calendar
bnuesm_85_tmax$dim$time$calendar
canesm2_26_tmax$dim$time$calendar
canesm2_45_tmax$dim$time$calendar
canesm2_85_tmax$dim$time$calendar
ccsm4_26_tmax$dim$time$calendar
ccsm4_45_tmax$dim$time$calendar
ccsm4_85_tmax$dim$time$calendar
cesm1cam5_26_tmax$dim$time$calendar
cesm1cam5_45_tmax$dim$time$calendar
cesm1cam5_85_tmax$dim$time$calendar
cnrmcm5_26_tmax$dim$time$calendar
cnrmcm5_45_tmax$dim$time$calendar
cnrmcm5_85_tmax$dim$time$calendar
csiromk360_26_tmax$dim$time$calendar
csiromk360_45_tmax$dim$time$calendar
csiromk360_85_tmax$dim$time$calendar
fgoalsg2_26_tmax$dim$time$calendar 
fgoalsg2_45_tmax$dim$time$calendar 
fgoalsg2_85_tmax$dim$time$calendar 
gfdlcm3_26_tmax$dim$time$calendar
gfdlcm3_45_tmax$dim$time$calendar
gfdlcm3_85_tmax$dim$time$calendar
gfdlesm2g_26_tmax$dim$time$calendar
gfdlesm2g_45_tmax$dim$time$calendar
gfdlesm2g_85_tmax$dim$time$calendar
gfdlesm2m_26_tmax$dim$time$calendar
gfdlesm2m_45_tmax$dim$time$calendar
gfdlesm2m_85_tmax$dim$time$calendar
hadgem2ao_26_tmax$dim$time$calendar
hadgem2ao_45_tmax$dim$time$calendar
hadgem2ao_85_tmax$dim$time$calendar
hadgem2es_26_tmax$dim$time$calendar
hadgem2es_45_tmax$dim$time$calendar
hadgem2es_85_tmax$dim$time$calendar
ipslcm5alr_26_tmax$dim$time$calendar
ipslcm5alr_45_tmax$dim$time$calendar
ipslcm5alr_85_tmax$dim$time$calendar
ipslcm5amr_26_tmax$dim$time$calendar
ipslcm5amr_45_tmax$dim$time$calendar
ipslcm5amr_85_tmax$dim$time$calendar
miroc5_26_tmax$dim$time$calendar
miroc5_45_tmax$dim$time$calendar
miroc5_85_tmax$dim$time$calendar
mirocesm_26_tmax$dim$time$calendar 
mirocesm_45_tmax$dim$time$calendar 
mirocesm_85_tmax$dim$time$calendar 
mirocesmchem_26_tmax$dim$time$calendar
mirocesmchem_45_tmax$dim$time$calendar
mirocesmchem_85_tmax$dim$time$calendar
mpiesmlr_26_tmax$dim$time$calendar 
mpiesmlr_45_tmax$dim$time$calendar 
mpiesmlr_85_tmax$dim$time$calendar 
mpiesmmr_26_tmax$dim$time$calendar 
mpiesmmr_45_tmax$dim$time$calendar 
mpiesmmr_85_tmax$dim$time$calendar 
mricgcm3_26_tmax$dim$time$calendar 
mricgcm3_45_tmax$dim$time$calendar 
mricgcm3_85_tmax$dim$time$calendar 
noresm1m_26_tmax$dim$time$calendar 
noresm1m_45_tmax$dim$time$calendar 
noresm1m_85_tmax$dim$time$calendar 
noresm1me_26_tmax$dim$time$calendar
noresm1me_45_tmax$dim$time$calendar
noresm1me_85_tmax$dim$time$calendar

#bcccsm11 365-day
#bcccsm11m 365-day
#bnuesm 365-day
#canesm2 365-day
#ccsm4 365-day
#cesm1cam5 365-day
#cnrmcm5 standard
#csiromk360 365-day
#fgoalsg2 365-day
#gfdlcm3 365-day
#gfdlesm2g 365-day
#gfdlesm2m 365-day
#hadgem2ao 360-day
#hadgem2es 360-day
#ipslcm5alr 365-day
#ipslcm5amr 365-day
#miroc5 365-day
#mirocesm standard
#mirocesmchem standard
#mpiesmlr proleptic gregorian
#mpiesmmr proleptic greogorian
#mricgcm3 standard
#noresm1m 365-day
#noresm1me 365-day

#On downloading daily data in csv format you will receive a zip file which contains four csv files, as follows:
#1. *_360_day.csv - this file contains those models having a 360-day year, with 12 months each containing 30 days.
#2. *_365_day.csv - this file contains those models having a 365-day year, and no leap years.
#3. *_standard.csv - this file contains those models which have 365-day years and every fourth year is a leap year containing 366 days.
#4. *_propleptic_gregorian.csv - this file contains those models which have 365-day years and every fourth year is a leap year containing 366 days.

threesixtytime <- nc.get.time.series(hadgem2ao_85_tmax, v = "tasmax",
                               time.dim.name = "time")
threesixtytime <- as.Date(format(threesixtytime,"%Y-%m-%d"))


threesixtyfivetime <- nc.get.time.series(bcccsm11_26_tmax, v = "tasmax",
                               time.dim.name = "time")
threesixtyfivetime <- as.Date(format(threesixtyfivetime,"%Y-%m-%d"))

standardtime <- nc.get.time.series(mirocesm_85_tmax, v = "tasmax",
                               time.dim.name = "time")
standardtime <- as.Date(format(standardtime,"%Y-%m-%d"))

prolepticgregtime <- nc.get.time.series(mpiesmlr_85_tmax, v = "tasmax",
                                   time.dim.name = "time")
prolepticgregtime <- as.Date(format(prolepticgregtime,"%Y-%m-%d"))


#Get tasmax for each file
bcccsm11_26_tmax<-ncvar_get(bcccsm11_26_tmax,"tasmax")
bcccsm11_45_tmax<-ncvar_get(bcccsm11_45_tmax,"tasmax")
bcccsm11_85_tmax<-ncvar_get(bcccsm11_85_tmax,"tasmax")
bcccsm11m_26_tmax<-ncvar_get(bcccsm11m_26_tmax,"tasmax")
bcccsm11m_45_tmax<-ncvar_get(bcccsm11m_45_tmax,"tasmax")
bcccsm11m_85_tmax<-ncvar_get(bcccsm11m_85_tmax,"tasmax")
bnuesm_26_tmax<-ncvar_get(bnuesm_26_tmax,"tasmax")
bnuesm_45_tmax<-ncvar_get(bnuesm_45_tmax,"tasmax")
bnuesm_85_tmax<-ncvar_get(bnuesm_85_tmax,"tasmax")
canesm2_26_tmax<-ncvar_get(canesm2_26_tmax,"tasmax")
canesm2_45_tmax<-ncvar_get(canesm2_45_tmax,"tasmax")
canesm2_85_tmax<-ncvar_get(canesm2_85_tmax,"tasmax")
ccsm4_26_tmax<-ncvar_get(ccsm4_26_tmax,"tasmax")
ccsm4_45_tmax<-ncvar_get(ccsm4_45_tmax,"tasmax")
ccsm4_85_tmax<-ncvar_get(ccsm4_85_tmax,"tasmax")
cesm1cam5_26_tmax<-ncvar_get(cesm1cam5_26_tmax,"tasmax")
cesm1cam5_45_tmax<-ncvar_get(cesm1cam5_45_tmax,"tasmax")
cesm1cam5_85_tmax<-ncvar_get(cesm1cam5_85_tmax,"tasmax")
cnrmcm5_26_tmax<-ncvar_get(cnrmcm5_26_tmax,"tasmax")
cnrmcm5_45_tmax<-ncvar_get(cnrmcm5_45_tmax,"tasmax")
cnrmcm5_85_tmax<-ncvar_get(cnrmcm5_85_tmax,"tasmax")
csiromk360_26_tmax<-ncvar_get(csiromk360_26_tmax,"tasmax")
csiromk360_45_tmax<-ncvar_get(csiromk360_45_tmax,"tasmax")
csiromk360_85_tmax<-ncvar_get(csiromk360_85_tmax,"tasmax")
fgoalsg2_26_tmax<-ncvar_get(fgoalsg2_26_tmax,"tasmax")
fgoalsg2_45_tmax<-ncvar_get(fgoalsg2_45_tmax,"tasmax")
fgoalsg2_85_tmax<-ncvar_get(fgoalsg2_85_tmax,"tasmax")
gfdlcm3_26_tmax<-ncvar_get(gfdlcm3_26_tmax,"tasmax")
gfdlcm3_45_tmax<-ncvar_get(gfdlcm3_45_tmax,"tasmax")
gfdlcm3_85_tmax<-ncvar_get(gfdlcm3_85_tmax,"tasmax")
gfdlesm2g_26_tmax<-ncvar_get(gfdlesm2g_26_tmax,"tasmax")
gfdlesm2g_45_tmax<-ncvar_get(gfdlesm2g_45_tmax,"tasmax")
gfdlesm2g_85_tmax<-ncvar_get(gfdlesm2g_85_tmax,"tasmax")
gfdlesm2m_26_tmax<-ncvar_get(gfdlesm2m_26_tmax,"tasmax")
gfdlesm2m_45_tmax<-ncvar_get(gfdlesm2m_45_tmax,"tasmax")
gfdlesm2m_85_tmax<-ncvar_get(gfdlesm2m_85_tmax,"tasmax")
hadgem2ao_26_tmax<-ncvar_get(hadgem2ao_26_tmax,"tasmax")
hadgem2ao_45_tmax<-ncvar_get(hadgem2ao_45_tmax,"tasmax")
hadgem2ao_85_tmax<-ncvar_get(hadgem2ao_85_tmax,"tasmax")
hadgem2es_26_tmax<-ncvar_get(hadgem2es_26_tmax,"tasmax")
hadgem2es_45_tmax<-ncvar_get(hadgem2es_45_tmax,"tasmax")
hadgem2es_85_tmax<-ncvar_get(hadgem2es_85_tmax,"tasmax")
ipslcm5alr_26_tmax<-ncvar_get(ipslcm5alr_26_tmax,"tasmax")
ipslcm5alr_45_tmax<-ncvar_get(ipslcm5alr_45_tmax,"tasmax")
ipslcm5alr_85_tmax<-ncvar_get(ipslcm5alr_85_tmax,"tasmax")
ipslcm5amr_26_tmax<-ncvar_get(ipslcm5amr_26_tmax,"tasmax")
ipslcm5amr_45_tmax<-ncvar_get(ipslcm5amr_45_tmax,"tasmax")
ipslcm5amr_85_tmax<-ncvar_get(ipslcm5amr_85_tmax,"tasmax")
miroc5_26_tmax<-ncvar_get(miroc5_26_tmax,"tasmax")
miroc5_45_tmax<-ncvar_get(miroc5_45_tmax,"tasmax")
miroc5_85_tmax<-ncvar_get(miroc5_85_tmax,"tasmax")
mirocesm_26_tmax<-ncvar_get(mirocesm_26_tmax,"tasmax")
mirocesm_45_tmax<-ncvar_get(mirocesm_45_tmax,"tasmax")
mirocesm_85_tmax<-ncvar_get(mirocesm_85_tmax,"tasmax")
mirocesmchem_26_tmax<-ncvar_get(mirocesmchem_26_tmax,"tasmax")
mirocesmchem_45_tmax<-ncvar_get(mirocesmchem_45_tmax,"tasmax")
mirocesmchem_85_tmax<-ncvar_get(mirocesmchem_85_tmax,"tasmax")
mpiesmlr_26_tmax<-ncvar_get(mpiesmlr_26_tmax,"tasmax")
mpiesmlr_45_tmax<-ncvar_get(mpiesmlr_45_tmax,"tasmax")
mpiesmlr_85_tmax<-ncvar_get(mpiesmlr_85_tmax,"tasmax")
mpiesmmr_26_tmax<-ncvar_get(mpiesmmr_26_tmax,"tasmax")
mpiesmmr_45_tmax<-ncvar_get(mpiesmmr_45_tmax,"tasmax")
mpiesmmr_85_tmax<-ncvar_get(mpiesmmr_85_tmax,"tasmax")
mricgcm3_26_tmax<-ncvar_get(mricgcm3_26_tmax,"tasmax")
mricgcm3_45_tmax<-ncvar_get(mricgcm3_45_tmax,"tasmax")
mricgcm3_85_tmax<-ncvar_get(mricgcm3_85_tmax,"tasmax")
noresm1m_26_tmax<-ncvar_get(noresm1m_26_tmax,"tasmax")
noresm1m_45_tmax<-ncvar_get(noresm1m_45_tmax,"tasmax")
noresm1m_85_tmax<-ncvar_get(noresm1m_85_tmax,"tasmax")
noresm1me_26_tmax<-ncvar_get(noresm1me_26_tmax,"tasmax")
noresm1me_45_tmax<-ncvar_get(noresm1me_45_tmax,"tasmax")
noresm1me_85_tmax<-ncvar_get(noresm1me_85_tmax,"tasmax")


#Spatially average across 29 CD.ca grid cells
bcccsm11_26_tmax<-as.data.frame(rowMeans(bcccsm11_26_tmax))
bcccsm11_45_tmax<-as.data.frame(rowMeans(bcccsm11_45_tmax))
bcccsm11_85_tmax<-as.data.frame(rowMeans(bcccsm11_85_tmax))
bcccsm11m_26_tmax<-as.data.frame(rowMeans(bcccsm11m_26_tmax))
bcccsm11m_45_tmax<-as.data.frame(rowMeans(bcccsm11m_45_tmax))
bcccsm11m_85_tmax<-as.data.frame(rowMeans(bcccsm11m_85_tmax))
bnuesm_26_tmax<-as.data.frame(rowMeans(bnuesm_26_tmax))
bnuesm_45_tmax<-as.data.frame(rowMeans(bnuesm_45_tmax))
bnuesm_85_tmax<-as.data.frame(rowMeans(bnuesm_85_tmax))
canesm2_26_tmax<-as.data.frame(rowMeans(canesm2_26_tmax))
canesm2_45_tmax<-as.data.frame(rowMeans(canesm2_45_tmax))
canesm2_85_tmax<-as.data.frame(rowMeans(canesm2_85_tmax))
ccsm4_26_tmax<-as.data.frame(rowMeans(ccsm4_26_tmax))
ccsm4_45_tmax<-as.data.frame(rowMeans(ccsm4_45_tmax))
ccsm4_85_tmax<-as.data.frame(rowMeans(ccsm4_85_tmax))
cesm1cam5_26_tmax<-as.data.frame(rowMeans(cesm1cam5_26_tmax))
cesm1cam5_45_tmax<-as.data.frame(rowMeans(cesm1cam5_45_tmax))
cesm1cam5_85_tmax<-as.data.frame(rowMeans(cesm1cam5_85_tmax))
cnrmcm5_26_tmax<-as.data.frame(rowMeans(cnrmcm5_26_tmax))
cnrmcm5_45_tmax<-as.data.frame(rowMeans(cnrmcm5_45_tmax))
cnrmcm5_85_tmax<-as.data.frame(rowMeans(cnrmcm5_85_tmax))
csiromk360_26_tmax<-as.data.frame(rowMeans(csiromk360_26_tmax))
csiromk360_45_tmax<-as.data.frame(rowMeans(csiromk360_45_tmax))
csiromk360_85_tmax<-as.data.frame(rowMeans(csiromk360_85_tmax))
fgoalsg2_26_tmax<-as.data.frame(rowMeans(fgoalsg2_26_tmax))
fgoalsg2_45_tmax<-as.data.frame(rowMeans(fgoalsg2_45_tmax))
fgoalsg2_85_tmax<-as.data.frame(rowMeans(fgoalsg2_85_tmax))
gfdlcm3_26_tmax<-as.data.frame(rowMeans(gfdlcm3_26_tmax))
gfdlcm3_45_tmax<-as.data.frame(rowMeans(gfdlcm3_45_tmax))
gfdlcm3_85_tmax<-as.data.frame(rowMeans(gfdlcm3_85_tmax))
gfdlesm2g_26_tmax<-as.data.frame(rowMeans(gfdlesm2g_26_tmax))
gfdlesm2g_45_tmax<-as.data.frame(rowMeans(gfdlesm2g_45_tmax))
gfdlesm2g_85_tmax<-as.data.frame(rowMeans(gfdlesm2g_85_tmax))
gfdlesm2m_26_tmax<-as.data.frame(rowMeans(gfdlesm2m_26_tmax))
gfdlesm2m_45_tmax<-as.data.frame(rowMeans(gfdlesm2m_45_tmax))
gfdlesm2m_85_tmax<-as.data.frame(rowMeans(gfdlesm2m_85_tmax))
hadgem2ao_26_tmax<-as.data.frame(rowMeans(hadgem2ao_26_tmax))
hadgem2ao_45_tmax<-as.data.frame(rowMeans(hadgem2ao_45_tmax))
hadgem2ao_85_tmax<-as.data.frame(rowMeans(hadgem2ao_85_tmax))
hadgem2es_26_tmax<-as.data.frame(rowMeans(hadgem2es_26_tmax))
hadgem2es_45_tmax<-as.data.frame(rowMeans(hadgem2es_45_tmax))
hadgem2es_85_tmax<-as.data.frame(rowMeans(hadgem2es_85_tmax))
ipslcm5alr_26_tmax<-as.data.frame(rowMeans(ipslcm5alr_26_tmax))
ipslcm5alr_45_tmax<-as.data.frame(rowMeans(ipslcm5alr_45_tmax))
ipslcm5alr_85_tmax<-as.data.frame(rowMeans(ipslcm5alr_85_tmax))
ipslcm5amr_26_tmax<-as.data.frame(rowMeans(ipslcm5amr_26_tmax))
ipslcm5amr_45_tmax<-as.data.frame(rowMeans(ipslcm5amr_45_tmax))
ipslcm5amr_85_tmax<-as.data.frame(rowMeans(ipslcm5amr_85_tmax))
miroc5_26_tmax<-as.data.frame(rowMeans(miroc5_26_tmax))
miroc5_45_tmax<-as.data.frame(rowMeans(miroc5_45_tmax))
miroc5_85_tmax<-as.data.frame(rowMeans(miroc5_85_tmax))
mirocesm_26_tmax<-as.data.frame(rowMeans(mirocesm_26_tmax))
mirocesm_45_tmax<-as.data.frame(rowMeans(mirocesm_45_tmax))
mirocesm_85_tmax<-as.data.frame(rowMeans(mirocesm_85_tmax))
mirocesmchem_26_tmax<-as.data.frame(rowMeans(mirocesmchem_26_tmax))
mirocesmchem_45_tmax<-as.data.frame(rowMeans(mirocesmchem_45_tmax))
mirocesmchem_85_tmax<-as.data.frame(rowMeans(mirocesmchem_85_tmax))
mpiesmlr_26_tmax<-as.data.frame(rowMeans(mpiesmlr_26_tmax))
mpiesmlr_45_tmax<-as.data.frame(rowMeans(mpiesmlr_45_tmax))
mpiesmlr_85_tmax<-as.data.frame(rowMeans(mpiesmlr_85_tmax))
mpiesmmr_26_tmax<-as.data.frame(rowMeans(mpiesmmr_26_tmax))
mpiesmmr_45_tmax<-as.data.frame(rowMeans(mpiesmmr_45_tmax))
mpiesmmr_85_tmax<-as.data.frame(rowMeans(mpiesmmr_85_tmax))
mricgcm3_26_tmax<-as.data.frame(rowMeans(mricgcm3_26_tmax))
mricgcm3_45_tmax<-as.data.frame(rowMeans(mricgcm3_45_tmax))
mricgcm3_85_tmax<-as.data.frame(rowMeans(mricgcm3_85_tmax))
noresm1m_26_tmax<-as.data.frame(rowMeans(noresm1m_26_tmax))
noresm1m_45_tmax<-as.data.frame(rowMeans(noresm1m_45_tmax))
noresm1m_85_tmax<-as.data.frame(rowMeans(noresm1m_85_tmax))
noresm1me_26_tmax<-as.data.frame(rowMeans(noresm1me_26_tmax))
noresm1me_45_tmax<-as.data.frame(rowMeans(noresm1me_45_tmax))
noresm1me_85_tmax<-as.data.frame(rowMeans(noresm1me_85_tmax))


#bcccsm11 365-day--
#bcccsm11m 365-day--
#bnuesm 365-day--
#canesm2 365-day--
#ccsm4 365-day--
#cesm1cam5 365-day--
#cnrmcm5 standard--
#csiromk360 365-day--
#fgoalsg2 365-day--
#gfdlcm3 365-day--
#gfdlesm2g 365-day--
#gfdlesm2m 365-day--
#hadgem2ao 360-day--
#hadgem2es 360-day--
#ipslcm5alr 365-day--
#ipslcm5amr 365-day--
#miroc5 365-day--
#mirocesm standard--
#mirocesmchem standard--
#mpiesmlr proleptic gregorian
#mpiesmmr proleptic greogorian
#mricgcm3 standard--
#noresm1m 365-day--
#noresm1me 365-day--

#360-day calendar dataframe
#####Missing data in original files:
#hadgem2es_26: missing hadgem2es_85[20131:20160] 2001-12-01 to 2005-12-30
#hadgem2es_45: missing hadgem2es_85[20131:20160; 54001:54360] 2100-01-01 to 2100-12-30

#Add fill value (-9999) to HadGem2-ES RCP 2.6 file
hadgem2es_26_tmax_a <- as.data.frame(hadgem2es_26_tmax[1:20130,1])
hadgem2es_26_tmax_b <- as.data.frame(hadgem2es_26_tmax[20131:54330,1])
fill <- as.data.frame(rep(-9999, 30))
names(fill) <- names(hadgem2es_26_tmax_a) <- names(hadgem2es_26_tmax_b) <- "hadgem2es_26_tmax"
hadgem2es_26_tmax <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_26_tmax <- rbind(hadgem2es_26_tmax_a,fill,hadgem2es_26_tmax_b)

#Add fill value (-9999) to HadGem2-ES RCP 4.5 file
hadgem2es_45_tmax_a <- as.data.frame(hadgem2es_45_tmax[1:20130,1])
hadgem2es_45_tmax_b <- as.data.frame(hadgem2es_45_tmax[20131:53970,1])
fill <- as.data.frame(rep(-9999, 30))
fill2 <- as.data.frame(rep(-9999,360))
names(fill) <- names(fill2) <- names(hadgem2es_45_tmax_a) <- names(hadgem2es_45_tmax_b) <- "hadgem2es_45_tmax"
hadgem2es_45_tmax <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_45_tmax <- rbind(hadgem2es_45_tmax_a,fill,hadgem2es_45_tmax_b,fill2)

#Bind all 360-day calendar datasets together

tmax_360 <- as.data.frame(matrix(NA, nrow=54360, ncol=9))

tmax_360[,1] <- as.numeric(format(threesixtytime,"%Y"))
tmax_360[,2] <- as.numeric(format(threesixtytime,"%m"))
tmax_360[,3] <- as.numeric(format(threesixtytime,"%d"))
tmax_360[,4] <- as.numeric(tmax_360[,1])*100+as.numeric(tmax_360[,2])
tmax_360[,5] <- hadgem2ao_26_tmax[,1]
tmax_360[,6] <- hadgem2ao_45_tmax[,1]
tmax_360[,7] <- hadgem2ao_85_tmax[,1]
tmax_360[,8] <- hadgem2es_26_tmax[,1] #54330
tmax_360[,9] <- hadgem2es_45_tmax[,1] #53970
tmax_360[,10] <- hadgem2es_85_tmax[,1]
names(tmax_360) <- c("year","month","day","code",
                     "hadgem2ao_26", "hadgem2ao_45", "hadgem2ao_85",
                     "hadgem2es_26", "hadgem2es_45", "hadgem2es_85")

tmax_360[tmax_360==-9999] <- NA #replace fill value with NA

#365-day calendar dataframe
#####Missing data values in original files:
#bcccsm11_85_tmax[,1], #bcccsm11_26[54750] 2099-12-31 msg
#bcccsm11m_45_tmax[,1], #bcccsm11_26[55115] 2100-12-31


#Add fill value (-9999) to bcc-csm-1-1 RCP 8.5 file
bcccsm11_85_tmax_a <- as.data.frame(bcccsm11_85_tmax[1:54749,1])
bcccsm11_85_tmax_b <- as.data.frame(bcccsm11_85_tmax[54750:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11_85_tmax_a) <- names(bcccsm11_85_tmax_b) <- "bcccsm11_85_tmax"
bcccsm11_85_tmax <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11_85_tmax <- rbind(bcccsm11_85_tmax_a,fill,bcccsm11_85_tmax_b)

#Add fill value (-9999) to bcc-csm-1-1-m RCP 4.5 file
bcccsm11m_45_tmax_a <- as.data.frame(bcccsm11m_45_tmax[1:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11m_45_tmax_a) <- "bcccsm11m_45_tmax"
bcccsm11m_45_tmax <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11m_45_tmax <- rbind(bcccsm11m_45_tmax_a,fill)

#Bind all 360-day calendar datasets together

tmax_365 <- cbind.data.frame(as.numeric(format(threesixtyfivetime, "%Y")),
                             as.numeric(format(threesixtyfivetime, "%m")),
                             as.numeric(format(threesixtyfivetime, "%d")),
                             as.numeric(format(threesixtyfivetime,"%Y"))*100+as.numeric(format(threesixtyfivetime,"%m")),
                             bcccsm11_26_tmax[,1],
                             bcccsm11_45_tmax[,1],
                             bcccsm11_85_tmax[,1], 
                             bcccsm11m_26_tmax[,1],
                             bcccsm11m_45_tmax[,1], 
                             bcccsm11m_85_tmax[,1],
                             bnuesm_26_tmax[,1],
                             bnuesm_45_tmax[,1],
                             bnuesm_85_tmax[,1],
                             canesm2_26_tmax[,1],
                             canesm2_45_tmax[,1],
                             canesm2_85_tmax[,1],
                             ccsm4_26_tmax[,1],
                             ccsm4_45_tmax[,1],
                             ccsm4_85_tmax[,1],
                             cesm1cam5_26_tmax[,1],
                             cesm1cam5_45_tmax[,1],
                             cesm1cam5_85_tmax[,1],
                             csiromk360_26_tmax[,1],
                             csiromk360_45_tmax[,1],
                             csiromk360_85_tmax[,1],
                             fgoalsg2_26_tmax[,1],
                             fgoalsg2_45_tmax[,1],
                             fgoalsg2_85_tmax[,1],
                             gfdlcm3_26_tmax[,1],
                             gfdlcm3_45_tmax[,1],
                             gfdlcm3_85_tmax[,1],
                             gfdlesm2g_26_tmax[,1],
                             gfdlesm2g_45_tmax[,1],
                             gfdlesm2g_85_tmax[,1],
                             gfdlesm2m_26_tmax[,1],
                             gfdlesm2m_45_tmax[,1],
                             gfdlesm2m_85_tmax[,1],
                             ipslcm5alr_26_tmax[,1],
                             ipslcm5alr_45_tmax[,1],
                             ipslcm5alr_85_tmax[,1],
                             ipslcm5amr_26_tmax[,1],
                             ipslcm5amr_45_tmax[,1],
                             ipslcm5amr_85_tmax[,1],
                             miroc5_26_tmax[,1],
                             miroc5_45_tmax[,1],
                             miroc5_85_tmax[,1],
                             noresm1m_26_tmax[,1],
                             noresm1m_45_tmax[,1],
                             noresm1m_85_tmax[,1],
                             noresm1me_26_tmax[,1],
                             noresm1me_45_tmax[,1],
                             noresm1me_85_tmax[,1])

names(tmax_365) <- c("year","month","day","code",
                     "bcccsm11_26","bcccsm11_45","bcccsm11_85",
                     "bcccsm11m_26","bcccsm11m_45","bcccsm11m_85",
                     "bnuesm_26","bnuesm_45","bnuesm_85",
                     "canesm2_26","canesm2_45","canesm2_85",
                     "ccsm4_26","ccsm4_45","ccsm4_85",
                     "cesm1cam5_26","cesm1cam5_45","cesm1cam5_85",
                     "csiromk360_26","csiromk360_45","csiromk360_85",
                     "fgoalsg2_26","fgoalsg2_45","fgoalsg2_85",
                     "gfdlcm3_26","gfdlcm3_45","gfdlcm3_85",
                     "gfdlesm2g_26","gfdlesm2g_45","gfdlesm2g_85",
                     "gfdlesm2m_26","gfdlesm2m_45","gfdlesm2m_85",
                     "ipslcm5alr_26","ipslcm5alr_45","ipslcm5alr_85",
                     "ipslcm5amr_26","ipslcm5amr_45","ipslcm5amr_85",
                     "miroc5_26","miroc5_45","miroc5_85",
                     "noresm1m_26","noresm1m_45","noresm1m_85",
                     "noresm1me_26","noresm1me_45","noresm1me_85")

tmax_365[tmax_365==-9999] <- NA #replace fill value with NA

#Standard Calendar

tmax_std <- cbind.data.frame(as.numeric(format(standardtime, "%Y")),
                             as.numeric(format(standardtime, "%m")),
                             as.numeric(format(standardtime, "%d")),
                             as.numeric(format(standardtime,"%Y"))*100+as.numeric(format(standardtime,"%m")),
                             cnrmcm5_26_tmax[,1],
                             cnrmcm5_45_tmax[,1],
                             cnrmcm5_85_tmax[,1],
                             mirocesm_26_tmax[,1],
                             mirocesm_45_tmax[,1],
                             mirocesm_85_tmax[,1],
                             mirocesmchem_26_tmax[,1],
                             mirocesmchem_45_tmax[,1],
                             mirocesmchem_85_tmax[,1],
                             mricgcm3_26_tmax[,1],
                             mricgcm3_45_tmax[,1],
                             mricgcm3_85_tmax[,1])

names(tmax_std) <- c("year","month","day","code",
                     "cnrmcm5_26","cnrmcm5_45","cnrmcm5_85",
                     "mirocesm_26","mirocesm_45","mirocesm_85",
                     "mirocesmchem_26","mirocesmchem_45","mirocesmchem_85",
                     "mricgcm3_26","mricgcm3_45","mricgcm3_85")

tmax_std[tmax_std==-9999] <- NA #replace fill value with NA

#Proleptic Gregorian Calendar

tmax_greg <- cbind.data.frame(as.numeric(format(prolepticgregtime, "%Y")),
                              as.numeric(format(prolepticgregtime, "%m")),
                              as.numeric(format(prolepticgregtime, "%d")),
                              as.numeric(format(prolepticgregtime,"%Y"))*100+as.numeric(format(prolepticgregtime,"%m")),
                              mpiesmlr_26_tmax[,1],
                              mpiesmlr_45_tmax[,1],
                              mpiesmlr_85_tmax[,1],
                              mpiesmmr_26_tmax[,1],
                              mpiesmmr_45_tmax[,1],
                              mpiesmmr_85_tmax[,1])

names(tmax_greg) <- c("year","month","day","code",
                      "mpiesmlr_26","mpiesmlr_45","mpiesmlr_85",
                      "mpiesmmr_26","mpiesmmr_45","mpiesmmr_85")

tmax_greg[tmax_greg==-9999] <- NA #replace fill value with NA


##############################################
############MINIMUM TEMPERATURE###############
##############################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./HBL_SmBoundBox_MinimumTemperature")

bcccsm11_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bnuesm_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bnuesm_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bnuesm_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
canesm2_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
canesm2_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
canesm2_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ccsm4_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ccsm4_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ccsm4_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
csiromk360_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
csiromk360_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
csiromk360_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
miroc5_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
miroc5_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
miroc5_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesm_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesm_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesm_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1m_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1m_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
noresm1m_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1me_26_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_45_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_85_tmin <- nc_open("tasmin_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")


#Identify type of calendar used by each model

threesixtytime <- nc.get.time.series(hadgem2ao_85_tmin, v = "tasmin",
                                     time.dim.name = "time")
threesixtytime <- as.Date(format(threesixtytime,"%Y-%m-%d"))


threesixtyfivetime <- nc.get.time.series(bcccsm11_26_tmin, v = "tasmin",
                                         time.dim.name = "time")
threesixtyfivetime <- as.Date(format(threesixtyfivetime,"%Y-%m-%d"))

standardtime <- nc.get.time.series(mirocesm_85_tmin, v = "tasmin",
                                   time.dim.name = "time")
standardtime <- as.Date(format(standardtime,"%Y-%m-%d"))

prolepticgregtime <- nc.get.time.series(mpiesmlr_85_tmin, v = "tasmin",
                                        time.dim.name = "time")
prolepticgregtime <- as.Date(format(prolepticgregtime,"%Y-%m-%d"))


#Get tasmin for each file
bcccsm11_26_tmin<-ncvar_get(bcccsm11_26_tmin,"tasmin")
bcccsm11_45_tmin<-ncvar_get(bcccsm11_45_tmin,"tasmin")
bcccsm11_85_tmin<-ncvar_get(bcccsm11_85_tmin,"tasmin")
bcccsm11m_26_tmin<-ncvar_get(bcccsm11m_26_tmin,"tasmin")
bcccsm11m_45_tmin<-ncvar_get(bcccsm11m_45_tmin,"tasmin")
bcccsm11m_85_tmin<-ncvar_get(bcccsm11m_85_tmin,"tasmin")
bnuesm_26_tmin<-ncvar_get(bnuesm_26_tmin,"tasmin")
bnuesm_45_tmin<-ncvar_get(bnuesm_45_tmin,"tasmin")
bnuesm_85_tmin<-ncvar_get(bnuesm_85_tmin,"tasmin")
canesm2_26_tmin<-ncvar_get(canesm2_26_tmin,"tasmin")
canesm2_45_tmin<-ncvar_get(canesm2_45_tmin,"tasmin")
canesm2_85_tmin<-ncvar_get(canesm2_85_tmin,"tasmin")
ccsm4_26_tmin<-ncvar_get(ccsm4_26_tmin,"tasmin")
ccsm4_45_tmin<-ncvar_get(ccsm4_45_tmin,"tasmin")
ccsm4_85_tmin<-ncvar_get(ccsm4_85_tmin,"tasmin")
cesm1cam5_26_tmin<-ncvar_get(cesm1cam5_26_tmin,"tasmin")
cesm1cam5_45_tmin<-ncvar_get(cesm1cam5_45_tmin,"tasmin")
cesm1cam5_85_tmin<-ncvar_get(cesm1cam5_85_tmin,"tasmin")
cnrmcm5_26_tmin<-ncvar_get(cnrmcm5_26_tmin,"tasmin")
cnrmcm5_45_tmin<-ncvar_get(cnrmcm5_45_tmin,"tasmin")
cnrmcm5_85_tmin<-ncvar_get(cnrmcm5_85_tmin,"tasmin")
csiromk360_26_tmin<-ncvar_get(csiromk360_26_tmin,"tasmin")
csiromk360_45_tmin<-ncvar_get(csiromk360_45_tmin,"tasmin")
csiromk360_85_tmin<-ncvar_get(csiromk360_85_tmin,"tasmin")
fgoalsg2_26_tmin<-ncvar_get(fgoalsg2_26_tmin,"tasmin")
fgoalsg2_45_tmin<-ncvar_get(fgoalsg2_45_tmin,"tasmin")
fgoalsg2_85_tmin<-ncvar_get(fgoalsg2_85_tmin,"tasmin")
gfdlcm3_26_tmin<-ncvar_get(gfdlcm3_26_tmin,"tasmin")
gfdlcm3_45_tmin<-ncvar_get(gfdlcm3_45_tmin,"tasmin")
gfdlcm3_85_tmin<-ncvar_get(gfdlcm3_85_tmin,"tasmin")
gfdlesm2g_26_tmin<-ncvar_get(gfdlesm2g_26_tmin,"tasmin")
gfdlesm2g_45_tmin<-ncvar_get(gfdlesm2g_45_tmin,"tasmin")
gfdlesm2g_85_tmin<-ncvar_get(gfdlesm2g_85_tmin,"tasmin")
gfdlesm2m_26_tmin<-ncvar_get(gfdlesm2m_26_tmin,"tasmin")
gfdlesm2m_45_tmin<-ncvar_get(gfdlesm2m_45_tmin,"tasmin")
gfdlesm2m_85_tmin<-ncvar_get(gfdlesm2m_85_tmin,"tasmin")
hadgem2ao_26_tmin<-ncvar_get(hadgem2ao_26_tmin,"tasmin")
hadgem2ao_45_tmin<-ncvar_get(hadgem2ao_45_tmin,"tasmin")
hadgem2ao_85_tmin<-ncvar_get(hadgem2ao_85_tmin,"tasmin")
hadgem2es_26_tmin<-ncvar_get(hadgem2es_26_tmin,"tasmin")
hadgem2es_45_tmin<-ncvar_get(hadgem2es_45_tmin,"tasmin")
hadgem2es_85_tmin<-ncvar_get(hadgem2es_85_tmin,"tasmin")
ipslcm5alr_26_tmin<-ncvar_get(ipslcm5alr_26_tmin,"tasmin")
ipslcm5alr_45_tmin<-ncvar_get(ipslcm5alr_45_tmin,"tasmin")
ipslcm5alr_85_tmin<-ncvar_get(ipslcm5alr_85_tmin,"tasmin")
ipslcm5amr_26_tmin<-ncvar_get(ipslcm5amr_26_tmin,"tasmin")
ipslcm5amr_45_tmin<-ncvar_get(ipslcm5amr_45_tmin,"tasmin")
ipslcm5amr_85_tmin<-ncvar_get(ipslcm5amr_85_tmin,"tasmin")
miroc5_26_tmin<-ncvar_get(miroc5_26_tmin,"tasmin")
miroc5_45_tmin<-ncvar_get(miroc5_45_tmin,"tasmin")
miroc5_85_tmin<-ncvar_get(miroc5_85_tmin,"tasmin")
mirocesm_26_tmin<-ncvar_get(mirocesm_26_tmin,"tasmin")
mirocesm_45_tmin<-ncvar_get(mirocesm_45_tmin,"tasmin")
mirocesm_85_tmin<-ncvar_get(mirocesm_85_tmin,"tasmin")
mirocesmchem_26_tmin<-ncvar_get(mirocesmchem_26_tmin,"tasmin")
mirocesmchem_45_tmin<-ncvar_get(mirocesmchem_45_tmin,"tasmin")
mirocesmchem_85_tmin<-ncvar_get(mirocesmchem_85_tmin,"tasmin")
mpiesmlr_26_tmin<-ncvar_get(mpiesmlr_26_tmin,"tasmin")
mpiesmlr_45_tmin<-ncvar_get(mpiesmlr_45_tmin,"tasmin")
mpiesmlr_85_tmin<-ncvar_get(mpiesmlr_85_tmin,"tasmin")
mpiesmmr_26_tmin<-ncvar_get(mpiesmmr_26_tmin,"tasmin")
mpiesmmr_45_tmin<-ncvar_get(mpiesmmr_45_tmin,"tasmin")
mpiesmmr_85_tmin<-ncvar_get(mpiesmmr_85_tmin,"tasmin")
mricgcm3_26_tmin<-ncvar_get(mricgcm3_26_tmin,"tasmin")
mricgcm3_45_tmin<-ncvar_get(mricgcm3_45_tmin,"tasmin")
mricgcm3_85_tmin<-ncvar_get(mricgcm3_85_tmin,"tasmin")
noresm1m_26_tmin<-ncvar_get(noresm1m_26_tmin,"tasmin")
noresm1m_45_tmin<-ncvar_get(noresm1m_45_tmin,"tasmin")
noresm1m_85_tmin<-ncvar_get(noresm1m_85_tmin,"tasmin")
noresm1me_26_tmin<-ncvar_get(noresm1me_26_tmin,"tasmin")
noresm1me_45_tmin<-ncvar_get(noresm1me_45_tmin,"tasmin")
noresm1me_85_tmin<-ncvar_get(noresm1me_85_tmin,"tasmin")


#Spatially average across 29 CD.ca grid cells
bcccsm11_26_tmin<-as.data.frame(rowMeans(bcccsm11_26_tmin))
bcccsm11_45_tmin<-as.data.frame(rowMeans(bcccsm11_45_tmin))
bcccsm11_85_tmin<-as.data.frame(rowMeans(bcccsm11_85_tmin))
bcccsm11m_26_tmin<-as.data.frame(rowMeans(bcccsm11m_26_tmin))
bcccsm11m_45_tmin<-as.data.frame(rowMeans(bcccsm11m_45_tmin))
bcccsm11m_85_tmin<-as.data.frame(rowMeans(bcccsm11m_85_tmin))
bnuesm_26_tmin<-as.data.frame(rowMeans(bnuesm_26_tmin))
bnuesm_45_tmin<-as.data.frame(rowMeans(bnuesm_45_tmin))
bnuesm_85_tmin<-as.data.frame(rowMeans(bnuesm_85_tmin))
canesm2_26_tmin<-as.data.frame(rowMeans(canesm2_26_tmin))
canesm2_45_tmin<-as.data.frame(rowMeans(canesm2_45_tmin))
canesm2_85_tmin<-as.data.frame(rowMeans(canesm2_85_tmin))
ccsm4_26_tmin<-as.data.frame(rowMeans(ccsm4_26_tmin))
ccsm4_45_tmin<-as.data.frame(rowMeans(ccsm4_45_tmin))
ccsm4_85_tmin<-as.data.frame(rowMeans(ccsm4_85_tmin))
cesm1cam5_26_tmin<-as.data.frame(rowMeans(cesm1cam5_26_tmin))
cesm1cam5_45_tmin<-as.data.frame(rowMeans(cesm1cam5_45_tmin))
cesm1cam5_85_tmin<-as.data.frame(rowMeans(cesm1cam5_85_tmin))
cnrmcm5_26_tmin<-as.data.frame(rowMeans(cnrmcm5_26_tmin))
cnrmcm5_45_tmin<-as.data.frame(rowMeans(cnrmcm5_45_tmin))
cnrmcm5_85_tmin<-as.data.frame(rowMeans(cnrmcm5_85_tmin))
csiromk360_26_tmin<-as.data.frame(rowMeans(csiromk360_26_tmin))
csiromk360_45_tmin<-as.data.frame(rowMeans(csiromk360_45_tmin))
csiromk360_85_tmin<-as.data.frame(rowMeans(csiromk360_85_tmin))
fgoalsg2_26_tmin<-as.data.frame(rowMeans(fgoalsg2_26_tmin))
fgoalsg2_45_tmin<-as.data.frame(rowMeans(fgoalsg2_45_tmin))
fgoalsg2_85_tmin<-as.data.frame(rowMeans(fgoalsg2_85_tmin))
gfdlcm3_26_tmin<-as.data.frame(rowMeans(gfdlcm3_26_tmin))
gfdlcm3_45_tmin<-as.data.frame(rowMeans(gfdlcm3_45_tmin))
gfdlcm3_85_tmin<-as.data.frame(rowMeans(gfdlcm3_85_tmin))
gfdlesm2g_26_tmin<-as.data.frame(rowMeans(gfdlesm2g_26_tmin))
gfdlesm2g_45_tmin<-as.data.frame(rowMeans(gfdlesm2g_45_tmin))
gfdlesm2g_85_tmin<-as.data.frame(rowMeans(gfdlesm2g_85_tmin))
gfdlesm2m_26_tmin<-as.data.frame(rowMeans(gfdlesm2m_26_tmin))
gfdlesm2m_45_tmin<-as.data.frame(rowMeans(gfdlesm2m_45_tmin))
gfdlesm2m_85_tmin<-as.data.frame(rowMeans(gfdlesm2m_85_tmin))
hadgem2ao_26_tmin<-as.data.frame(rowMeans(hadgem2ao_26_tmin))
hadgem2ao_45_tmin<-as.data.frame(rowMeans(hadgem2ao_45_tmin))
hadgem2ao_85_tmin<-as.data.frame(rowMeans(hadgem2ao_85_tmin))
hadgem2es_26_tmin<-as.data.frame(rowMeans(hadgem2es_26_tmin))
hadgem2es_45_tmin<-as.data.frame(rowMeans(hadgem2es_45_tmin))
hadgem2es_85_tmin<-as.data.frame(rowMeans(hadgem2es_85_tmin))
ipslcm5alr_26_tmin<-as.data.frame(rowMeans(ipslcm5alr_26_tmin))
ipslcm5alr_45_tmin<-as.data.frame(rowMeans(ipslcm5alr_45_tmin))
ipslcm5alr_85_tmin<-as.data.frame(rowMeans(ipslcm5alr_85_tmin))
ipslcm5amr_26_tmin<-as.data.frame(rowMeans(ipslcm5amr_26_tmin))
ipslcm5amr_45_tmin<-as.data.frame(rowMeans(ipslcm5amr_45_tmin))
ipslcm5amr_85_tmin<-as.data.frame(rowMeans(ipslcm5amr_85_tmin))
miroc5_26_tmin<-as.data.frame(rowMeans(miroc5_26_tmin))
miroc5_45_tmin<-as.data.frame(rowMeans(miroc5_45_tmin))
miroc5_85_tmin<-as.data.frame(rowMeans(miroc5_85_tmin))
mirocesm_26_tmin<-as.data.frame(rowMeans(mirocesm_26_tmin))
mirocesm_45_tmin<-as.data.frame(rowMeans(mirocesm_45_tmin))
mirocesm_85_tmin<-as.data.frame(rowMeans(mirocesm_85_tmin))
mirocesmchem_26_tmin<-as.data.frame(rowMeans(mirocesmchem_26_tmin))
mirocesmchem_45_tmin<-as.data.frame(rowMeans(mirocesmchem_45_tmin))
mirocesmchem_85_tmin<-as.data.frame(rowMeans(mirocesmchem_85_tmin))
mpiesmlr_26_tmin<-as.data.frame(rowMeans(mpiesmlr_26_tmin))
mpiesmlr_45_tmin<-as.data.frame(rowMeans(mpiesmlr_45_tmin))
mpiesmlr_85_tmin<-as.data.frame(rowMeans(mpiesmlr_85_tmin))
mpiesmmr_26_tmin<-as.data.frame(rowMeans(mpiesmmr_26_tmin))
mpiesmmr_45_tmin<-as.data.frame(rowMeans(mpiesmmr_45_tmin))
mpiesmmr_85_tmin<-as.data.frame(rowMeans(mpiesmmr_85_tmin))
mricgcm3_26_tmin<-as.data.frame(rowMeans(mricgcm3_26_tmin))
mricgcm3_45_tmin<-as.data.frame(rowMeans(mricgcm3_45_tmin))
mricgcm3_85_tmin<-as.data.frame(rowMeans(mricgcm3_85_tmin))
noresm1m_26_tmin<-as.data.frame(rowMeans(noresm1m_26_tmin))
noresm1m_45_tmin<-as.data.frame(rowMeans(noresm1m_45_tmin))
noresm1m_85_tmin<-as.data.frame(rowMeans(noresm1m_85_tmin))
noresm1me_26_tmin<-as.data.frame(rowMeans(noresm1me_26_tmin))
noresm1me_45_tmin<-as.data.frame(rowMeans(noresm1me_45_tmin))
noresm1me_85_tmin<-as.data.frame(rowMeans(noresm1me_85_tmin))



#360-day calendar dataframe
#####Missing data in original files:
#hadgem2es_26: missing hadgem2es_85[20131:20160] 2001-12-01 to 2005-12-30
#hadgem2es_45: missing hadgem2es_85[20131:20160; 54001:54360] 2100-01-01 to 2100-12-30

#Add fill value (-9999) to HadGem2-ES RCP 2.6 file
hadgem2es_26_tmin_a <- as.data.frame(hadgem2es_26_tmin[1:20130,1])
hadgem2es_26_tmin_b <- as.data.frame(hadgem2es_26_tmin[20131:54330,1])
fill <- as.data.frame(rep(-9999, 30))
names(fill) <- names(hadgem2es_26_tmin_a) <- names(hadgem2es_26_tmin_b) <- "hadgem2es_26_tmin"
hadgem2es_26_tmin <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_26_tmin <- rbind(hadgem2es_26_tmin_a,fill,hadgem2es_26_tmin_b)

#Add fill value (-9999) to HadGem2-ES RCP 4.5 file
hadgem2es_45_tmin_a <- as.data.frame(hadgem2es_45_tmin[1:20130,1])
hadgem2es_45_tmin_b <- as.data.frame(hadgem2es_45_tmin[20131:53970,1])
fill <- as.data.frame(rep(-9999, 30))
fill2 <- as.data.frame(rep(-9999,360))
names(fill) <- names(fill2) <- names(hadgem2es_45_tmin_a) <- names(hadgem2es_45_tmin_b) <- "hadgem2es_45_tmin"
hadgem2es_45_tmin <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_45_tmin <- rbind(hadgem2es_45_tmin_a,fill,hadgem2es_45_tmin_b,fill2)

#Bind all 360-day calendar datasets together

tmin_360 <- as.data.frame(matrix(NA, nrow=54360, ncol=9))

tmin_360[,1] <- as.numeric(format(threesixtytime,"%Y"))
tmin_360[,2] <- as.numeric(format(threesixtytime,"%m"))
tmin_360[,3] <- as.numeric(format(threesixtytime,"%d"))
tmin_360[,4] <- as.numeric(tmin_360[,1])*100+as.numeric(tmin_360[,2])
tmin_360[,5] <- hadgem2ao_26_tmin[,1]
tmin_360[,6] <- hadgem2ao_45_tmin[,1]
tmin_360[,7] <- hadgem2ao_85_tmin[,1]
tmin_360[,8] <- hadgem2es_26_tmin[,1] #54330
tmin_360[,9] <- hadgem2es_45_tmin[,1] #53970
tmin_360[,10] <- hadgem2es_85_tmin[,1]
names(tmin_360) <- c("year","month","day","code",
                     "hadgem2ao_26", "hadgem2ao_45", "hadgem2ao_85",
                     "hadgem2es_26", "hadgem2es_45", "hadgem2es_85")

tmin_360[tmin_360==-9999] <- NA #replace fill value with NA

#365-day calendar dataframe
#####Missing data values in original files:
#bcccsm11_85_tmin[,1], #bcccsm11_26[54750] 2099-12-31 msg
#bcccsm11m_45_tmin[,1], #bcccsm11_26[55115] 2100-12-31


#Add fill value (-9999) to bcc-csm-1-1 RCP 8.5 file
bcccsm11_85_tmin_a <- as.data.frame(bcccsm11_85_tmin[1:54749,1])
bcccsm11_85_tmin_b <- as.data.frame(bcccsm11_85_tmin[54750:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11_85_tmin_a) <- names(bcccsm11_85_tmin_b) <- "bcccsm11_85_tmin"
bcccsm11_85_tmin <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11_85_tmin <- rbind(bcccsm11_85_tmin_a,fill,bcccsm11_85_tmin_b)

#Add fill value (-9999) to bcc-csm-1-1-m RCP 4.5 file
bcccsm11m_45_tmin_a <- as.data.frame(bcccsm11m_45_tmin[1:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11m_45_tmin_a) <- "bcccsm11m_45_tmin"
bcccsm11m_45_tmin <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11m_45_tmin <- rbind(bcccsm11m_45_tmin_a,fill)

#Bind all 360-day calendar datasets together

tmin_365 <- cbind.data.frame(as.numeric(format(threesixtyfivetime, "%Y")),
                             as.numeric(format(threesixtyfivetime, "%m")),
                             as.numeric(format(threesixtyfivetime, "%d")),
                             as.numeric(format(threesixtyfivetime,"%Y"))*100+as.numeric(format(threesixtyfivetime,"%m")),
                             bcccsm11_26_tmin[,1],
                             bcccsm11_45_tmin[,1],
                             bcccsm11_85_tmin[,1], 
                             bcccsm11m_26_tmin[,1],
                             bcccsm11m_45_tmin[,1], 
                             bcccsm11m_85_tmin[,1],
                             bnuesm_26_tmin[,1],
                             bnuesm_45_tmin[,1],
                             bnuesm_85_tmin[,1],
                             canesm2_26_tmin[,1],
                             canesm2_45_tmin[,1],
                             canesm2_85_tmin[,1],
                             ccsm4_26_tmin[,1],
                             ccsm4_45_tmin[,1],
                             ccsm4_85_tmin[,1],
                             cesm1cam5_26_tmin[,1],
                             cesm1cam5_45_tmin[,1],
                             cesm1cam5_85_tmin[,1],
                             csiromk360_26_tmin[,1],
                             csiromk360_45_tmin[,1],
                             csiromk360_85_tmin[,1],
                             fgoalsg2_26_tmin[,1],
                             fgoalsg2_45_tmin[,1],
                             fgoalsg2_85_tmin[,1],
                             gfdlcm3_26_tmin[,1],
                             gfdlcm3_45_tmin[,1],
                             gfdlcm3_85_tmin[,1],
                             gfdlesm2g_26_tmin[,1],
                             gfdlesm2g_45_tmin[,1],
                             gfdlesm2g_85_tmin[,1],
                             gfdlesm2m_26_tmin[,1],
                             gfdlesm2m_45_tmin[,1],
                             gfdlesm2m_85_tmin[,1],
                             ipslcm5alr_26_tmin[,1],
                             ipslcm5alr_45_tmin[,1],
                             ipslcm5alr_85_tmin[,1],
                             ipslcm5amr_26_tmin[,1],
                             ipslcm5amr_45_tmin[,1],
                             ipslcm5amr_85_tmin[,1],
                             miroc5_26_tmin[,1],
                             miroc5_45_tmin[,1],
                             miroc5_85_tmin[,1],
                             noresm1m_26_tmin[,1],
                             noresm1m_45_tmin[,1],
                             noresm1m_85_tmin[,1],
                             noresm1me_26_tmin[,1],
                             noresm1me_45_tmin[,1],
                             noresm1me_85_tmin[,1])

names(tmin_365) <- c("year","month","day","code",
                     "bcccsm11_26","bcccsm11_45","bcccsm11_85",
                     "bcccsm11m_26","bcccsm11m_45","bcccsm11m_85",
                     "bnuesm_26","bnuesm_45","bnuesm_85",
                     "canesm2_26","canesm2_45","canesm2_85",
                     "ccsm4_26","ccsm4_45","ccsm4_85",
                     "cesm1cam5_26","cesm1cam5_45","cesm1cam5_85",
                     "csiromk360_26","csiromk360_45","csiromk360_85",
                     "fgoalsg2_26","fgoalsg2_45","fgoalsg2_85",
                     "gfdlcm3_26","gfdlcm3_45","gfdlcm3_85",
                     "gfdlesm2g_26","gfdlesm2g_45","gfdlesm2g_85",
                     "gfdlesm2m_26","gfdlesm2m_45","gfdlesm2m_85",
                     "ipslcm5alr_26","ipslcm5alr_45","ipslcm5alr_85",
                     "ipslcm5amr_26","ipslcm5amr_45","ipslcm5amr_85",
                     "miroc5_26","miroc5_45","miroc5_85",
                     "noresm1m_26","noresm1m_45","noresm1m_85",
                     "noresm1me_26","noresm1me_45","noresm1me_85")

tmin_365[tmin_365==-9999] <- NA #replace fill value with NA

#Standard Calendar

tmin_std <- cbind.data.frame(as.numeric(format(standardtime, "%Y")),
                             as.numeric(format(standardtime, "%m")),
                             as.numeric(format(standardtime, "%d")),
                             as.numeric(format(standardtime,"%Y"))*100+as.numeric(format(standardtime,"%m")),
                             cnrmcm5_26_tmin[,1],
                             cnrmcm5_45_tmin[,1],
                             cnrmcm5_85_tmin[,1],
                             mirocesm_26_tmin[,1],
                             mirocesm_45_tmin[,1],
                             mirocesm_85_tmin[,1],
                             mirocesmchem_26_tmin[,1],
                             mirocesmchem_45_tmin[,1],
                             mirocesmchem_85_tmin[,1],
                             mricgcm3_26_tmin[,1],
                             mricgcm3_45_tmin[,1],
                             mricgcm3_85_tmin[,1])

names(tmin_std) <- c("year","month","day","code",
                     "cnrmcm5_26","cnrmcm5_45","cnrmcm5_85",
                     "mirocesm_26","mirocesm_45","mirocesm_85",
                     "mirocesmchem_26","mirocesmchem_45","mirocesmchem_85",
                     "mricgcm3_26","mricgcm3_45","mricgcm3_85")

tmin_std[tmin_std==-9999] <- NA #replace fill value with NA

#Proleptic Gregorian Calendar

tmin_greg <- cbind.data.frame(as.numeric(format(prolepticgregtime, "%Y")),
                              as.numeric(format(prolepticgregtime, "%m")),
                              as.numeric(format(prolepticgregtime, "%d")),
                              as.numeric(format(prolepticgregtime,"%Y"))*100+as.numeric(format(prolepticgregtime,"%m")),
                              mpiesmlr_26_tmin[,1],
                              mpiesmlr_45_tmin[,1],
                              mpiesmlr_85_tmin[,1],
                              mpiesmmr_26_tmin[,1],
                              mpiesmmr_45_tmin[,1],
                              mpiesmmr_85_tmin[,1])

names(tmin_greg) <- c("year","month","day","code",
                      "mpiesmlr_26","mpiesmlr_45","mpiesmlr_85",
                      "mpiesmmr_26","mpiesmmr_45","mpiesmmr_85")

tmin_greg[tmin_greg==-9999] <- NA #replace fill value with NA


##############################################
#####################PRECIPITATION############
##############################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./HBL_SmBoundBox_Precipitation")

bcccsm11_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bcccsm11m_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_bcc-csm1-1-m_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
bnuesm_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
bnuesm_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
bnuesm_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_BNU-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
canesm2_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
canesm2_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
canesm2_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CanESM2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ccsm4_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ccsm4_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ccsm4_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CCSM4_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cesm1cam5_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CESM1-CAM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
cnrmcm5_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CNRM-CM5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
csiromk360_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
csiromk360_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
csiromk360_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_CSIRO-Mk3-6-0_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
fgoalsg2_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_FGOALS-g2_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlcm3_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-CM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2g_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2G_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
gfdlesm2m_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_GFDL-ESM2M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2ao_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-AO_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
hadgem2es_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_HadGEM2-ES_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5alr_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
ipslcm5amr_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_IPSL-CM5A-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
miroc5_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
miroc5_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
miroc5_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC5_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesm_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesm_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesm_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mirocesmchem_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MIROC-ESM-CHEM_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmlr_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-LR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mpiesmmr_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MPI-ESM-MR_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
mricgcm3_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1m_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1m_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp45_r1i1p1_19500101-21001231_sub.nc")
noresm1m_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-M_historical+rcp85_r1i1p1_19500101-21001231_sub.nc")
noresm1me_26_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_45_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")
noresm1me_85_pr <- nc_open("pr_day_BCCAQv2+ANUSPLIN300_NorESM1-ME_historical+rcp26_r1i1p1_19500101-21001231_sub.nc")


#Identify type of calendar used by each model

threesixtytime <- nc.get.time.series(hadgem2ao_85_pr, v = "pr",
                                     time.dim.name = "time")
threesixtytime <- as.Date(format(threesixtytime,"%Y-%m-%d"))


threesixtyfivetime <- nc.get.time.series(bcccsm11_26_pr, v = "pr",
                                         time.dim.name = "time")
threesixtyfivetime <- as.Date(format(threesixtyfivetime,"%Y-%m-%d"))

standardtime <- nc.get.time.series(mirocesm_85_pr, v = "pr",
                                   time.dim.name = "time")
standardtime <- as.Date(format(standardtime,"%Y-%m-%d"))

prolepticgregtime <- nc.get.time.series(mpiesmlr_85_pr, v = "pr",
                                        time.dim.name = "time")
prolepticgregtime <- as.Date(format(prolepticgregtime,"%Y-%m-%d"))


#Get pr for each file
bcccsm11_26_pr<-ncvar_get(bcccsm11_26_pr,"pr")
bcccsm11_45_pr<-ncvar_get(bcccsm11_45_pr,"pr")
bcccsm11_85_pr<-ncvar_get(bcccsm11_85_pr,"pr")
bcccsm11m_26_pr<-ncvar_get(bcccsm11m_26_pr,"pr")
bcccsm11m_45_pr<-ncvar_get(bcccsm11m_45_pr,"pr")
bcccsm11m_85_pr<-ncvar_get(bcccsm11m_85_pr,"pr")
bnuesm_26_pr<-ncvar_get(bnuesm_26_pr,"pr")
bnuesm_45_pr<-ncvar_get(bnuesm_45_pr,"pr")
bnuesm_85_pr<-ncvar_get(bnuesm_85_pr,"pr")
canesm2_26_pr<-ncvar_get(canesm2_26_pr,"pr")
canesm2_45_pr<-ncvar_get(canesm2_45_pr,"pr")
canesm2_85_pr<-ncvar_get(canesm2_85_pr,"pr")
ccsm4_26_pr<-ncvar_get(ccsm4_26_pr,"pr")
ccsm4_45_pr<-ncvar_get(ccsm4_45_pr,"pr")
ccsm4_85_pr<-ncvar_get(ccsm4_85_pr,"pr")
cesm1cam5_26_pr<-ncvar_get(cesm1cam5_26_pr,"pr")
cesm1cam5_45_pr<-ncvar_get(cesm1cam5_45_pr,"pr")
cesm1cam5_85_pr<-ncvar_get(cesm1cam5_85_pr,"pr")
cnrmcm5_26_pr<-ncvar_get(cnrmcm5_26_pr,"pr")
cnrmcm5_45_pr<-ncvar_get(cnrmcm5_45_pr,"pr")
cnrmcm5_85_pr<-ncvar_get(cnrmcm5_85_pr,"pr")
csiromk360_26_pr<-ncvar_get(csiromk360_26_pr,"pr")
csiromk360_45_pr<-ncvar_get(csiromk360_45_pr,"pr")
csiromk360_85_pr<-ncvar_get(csiromk360_85_pr,"pr")
fgoalsg2_26_pr<-ncvar_get(fgoalsg2_26_pr,"pr")
fgoalsg2_45_pr<-ncvar_get(fgoalsg2_45_pr,"pr")
fgoalsg2_85_pr<-ncvar_get(fgoalsg2_85_pr,"pr")
gfdlcm3_26_pr<-ncvar_get(gfdlcm3_26_pr,"pr")
gfdlcm3_45_pr<-ncvar_get(gfdlcm3_45_pr,"pr")
gfdlcm3_85_pr<-ncvar_get(gfdlcm3_85_pr,"pr")
gfdlesm2g_26_pr<-ncvar_get(gfdlesm2g_26_pr,"pr")
gfdlesm2g_45_pr<-ncvar_get(gfdlesm2g_45_pr,"pr")
gfdlesm2g_85_pr<-ncvar_get(gfdlesm2g_85_pr,"pr")
gfdlesm2m_26_pr<-ncvar_get(gfdlesm2m_26_pr,"pr")
gfdlesm2m_45_pr<-ncvar_get(gfdlesm2m_45_pr,"pr")
gfdlesm2m_85_pr<-ncvar_get(gfdlesm2m_85_pr,"pr")
hadgem2ao_26_pr<-ncvar_get(hadgem2ao_26_pr,"pr")
hadgem2ao_45_pr<-ncvar_get(hadgem2ao_45_pr,"pr")
hadgem2ao_85_pr<-ncvar_get(hadgem2ao_85_pr,"pr")
hadgem2es_26_pr<-ncvar_get(hadgem2es_26_pr,"pr")
hadgem2es_45_pr<-ncvar_get(hadgem2es_45_pr,"pr")
hadgem2es_85_pr<-ncvar_get(hadgem2es_85_pr,"pr")
ipslcm5alr_26_pr<-ncvar_get(ipslcm5alr_26_pr,"pr")
ipslcm5alr_45_pr<-ncvar_get(ipslcm5alr_45_pr,"pr")
ipslcm5alr_85_pr<-ncvar_get(ipslcm5alr_85_pr,"pr")
ipslcm5amr_26_pr<-ncvar_get(ipslcm5amr_26_pr,"pr")
ipslcm5amr_45_pr<-ncvar_get(ipslcm5amr_45_pr,"pr")
ipslcm5amr_85_pr<-ncvar_get(ipslcm5amr_85_pr,"pr")
miroc5_26_pr<-ncvar_get(miroc5_26_pr,"pr")
miroc5_45_pr<-ncvar_get(miroc5_45_pr,"pr")
miroc5_85_pr<-ncvar_get(miroc5_85_pr,"pr")
mirocesm_26_pr<-ncvar_get(mirocesm_26_pr,"pr")
mirocesm_45_pr<-ncvar_get(mirocesm_45_pr,"pr")
mirocesm_85_pr<-ncvar_get(mirocesm_85_pr,"pr")
mirocesmchem_26_pr<-ncvar_get(mirocesmchem_26_pr,"pr")
mirocesmchem_45_pr<-ncvar_get(mirocesmchem_45_pr,"pr")
mirocesmchem_85_pr<-ncvar_get(mirocesmchem_85_pr,"pr")
mpiesmlr_26_pr<-ncvar_get(mpiesmlr_26_pr,"pr")
mpiesmlr_45_pr<-ncvar_get(mpiesmlr_45_pr,"pr")
mpiesmlr_85_pr<-ncvar_get(mpiesmlr_85_pr,"pr")
mpiesmmr_26_pr<-ncvar_get(mpiesmmr_26_pr,"pr")
mpiesmmr_45_pr<-ncvar_get(mpiesmmr_45_pr,"pr")
mpiesmmr_85_pr<-ncvar_get(mpiesmmr_85_pr,"pr")
mricgcm3_26_pr<-ncvar_get(mricgcm3_26_pr,"pr")
mricgcm3_45_pr<-ncvar_get(mricgcm3_45_pr,"pr")
mricgcm3_85_pr<-ncvar_get(mricgcm3_85_pr,"pr")
noresm1m_26_pr<-ncvar_get(noresm1m_26_pr,"pr")
noresm1m_45_pr<-ncvar_get(noresm1m_45_pr,"pr")
noresm1m_85_pr<-ncvar_get(noresm1m_85_pr,"pr")
noresm1me_26_pr<-ncvar_get(noresm1me_26_pr,"pr")
noresm1me_45_pr<-ncvar_get(noresm1me_45_pr,"pr")
noresm1me_85_pr<-ncvar_get(noresm1me_85_pr,"pr")


#Spatially average across 29 CD.ca grid cells
bcccsm11_26_pr<-as.data.frame(rowMeans(bcccsm11_26_pr))
bcccsm11_45_pr<-as.data.frame(rowMeans(bcccsm11_45_pr))
bcccsm11_85_pr<-as.data.frame(rowMeans(bcccsm11_85_pr))
bcccsm11m_26_pr<-as.data.frame(rowMeans(bcccsm11m_26_pr))
bcccsm11m_45_pr<-as.data.frame(rowMeans(bcccsm11m_45_pr))
bcccsm11m_85_pr<-as.data.frame(rowMeans(bcccsm11m_85_pr))
bnuesm_26_pr<-as.data.frame(rowMeans(bnuesm_26_pr))
bnuesm_45_pr<-as.data.frame(rowMeans(bnuesm_45_pr))
bnuesm_85_pr<-as.data.frame(rowMeans(bnuesm_85_pr))
canesm2_26_pr<-as.data.frame(rowMeans(canesm2_26_pr))
canesm2_45_pr<-as.data.frame(rowMeans(canesm2_45_pr))
canesm2_85_pr<-as.data.frame(rowMeans(canesm2_85_pr))
ccsm4_26_pr<-as.data.frame(rowMeans(ccsm4_26_pr))
ccsm4_45_pr<-as.data.frame(rowMeans(ccsm4_45_pr))
ccsm4_85_pr<-as.data.frame(rowMeans(ccsm4_85_pr))
cesm1cam5_26_pr<-as.data.frame(rowMeans(cesm1cam5_26_pr))
cesm1cam5_45_pr<-as.data.frame(rowMeans(cesm1cam5_45_pr))
cesm1cam5_85_pr<-as.data.frame(rowMeans(cesm1cam5_85_pr))
cnrmcm5_26_pr<-as.data.frame(rowMeans(cnrmcm5_26_pr))
cnrmcm5_45_pr<-as.data.frame(rowMeans(cnrmcm5_45_pr))
cnrmcm5_85_pr<-as.data.frame(rowMeans(cnrmcm5_85_pr))
csiromk360_26_pr<-as.data.frame(rowMeans(csiromk360_26_pr))
csiromk360_45_pr<-as.data.frame(rowMeans(csiromk360_45_pr))
csiromk360_85_pr<-as.data.frame(rowMeans(csiromk360_85_pr))
fgoalsg2_26_pr<-as.data.frame(rowMeans(fgoalsg2_26_pr))
fgoalsg2_45_pr<-as.data.frame(rowMeans(fgoalsg2_45_pr))
fgoalsg2_85_pr<-as.data.frame(rowMeans(fgoalsg2_85_pr))
gfdlcm3_26_pr<-as.data.frame(rowMeans(gfdlcm3_26_pr))
gfdlcm3_45_pr<-as.data.frame(rowMeans(gfdlcm3_45_pr))
gfdlcm3_85_pr<-as.data.frame(rowMeans(gfdlcm3_85_pr))
gfdlesm2g_26_pr<-as.data.frame(rowMeans(gfdlesm2g_26_pr))
gfdlesm2g_45_pr<-as.data.frame(rowMeans(gfdlesm2g_45_pr))
gfdlesm2g_85_pr<-as.data.frame(rowMeans(gfdlesm2g_85_pr))
gfdlesm2m_26_pr<-as.data.frame(rowMeans(gfdlesm2m_26_pr))
gfdlesm2m_45_pr<-as.data.frame(rowMeans(gfdlesm2m_45_pr))
gfdlesm2m_85_pr<-as.data.frame(rowMeans(gfdlesm2m_85_pr))
hadgem2ao_26_pr<-as.data.frame(rowMeans(hadgem2ao_26_pr))
hadgem2ao_45_pr<-as.data.frame(rowMeans(hadgem2ao_45_pr))
hadgem2ao_85_pr<-as.data.frame(rowMeans(hadgem2ao_85_pr))
hadgem2es_26_pr<-as.data.frame(rowMeans(hadgem2es_26_pr))
hadgem2es_45_pr<-as.data.frame(rowMeans(hadgem2es_45_pr))
hadgem2es_85_pr<-as.data.frame(rowMeans(hadgem2es_85_pr))
ipslcm5alr_26_pr<-as.data.frame(rowMeans(ipslcm5alr_26_pr))
ipslcm5alr_45_pr<-as.data.frame(rowMeans(ipslcm5alr_45_pr))
ipslcm5alr_85_pr<-as.data.frame(rowMeans(ipslcm5alr_85_pr))
ipslcm5amr_26_pr<-as.data.frame(rowMeans(ipslcm5amr_26_pr))
ipslcm5amr_45_pr<-as.data.frame(rowMeans(ipslcm5amr_45_pr))
ipslcm5amr_85_pr<-as.data.frame(rowMeans(ipslcm5amr_85_pr))
miroc5_26_pr<-as.data.frame(rowMeans(miroc5_26_pr))
miroc5_45_pr<-as.data.frame(rowMeans(miroc5_45_pr))
miroc5_85_pr<-as.data.frame(rowMeans(miroc5_85_pr))
mirocesm_26_pr<-as.data.frame(rowMeans(mirocesm_26_pr))
mirocesm_45_pr<-as.data.frame(rowMeans(mirocesm_45_pr))
mirocesm_85_pr<-as.data.frame(rowMeans(mirocesm_85_pr))
mirocesmchem_26_pr<-as.data.frame(rowMeans(mirocesmchem_26_pr))
mirocesmchem_45_pr<-as.data.frame(rowMeans(mirocesmchem_45_pr))
mirocesmchem_85_pr<-as.data.frame(rowMeans(mirocesmchem_85_pr))
mpiesmlr_26_pr<-as.data.frame(rowMeans(mpiesmlr_26_pr))
mpiesmlr_45_pr<-as.data.frame(rowMeans(mpiesmlr_45_pr))
mpiesmlr_85_pr<-as.data.frame(rowMeans(mpiesmlr_85_pr))
mpiesmmr_26_pr<-as.data.frame(rowMeans(mpiesmmr_26_pr))
mpiesmmr_45_pr<-as.data.frame(rowMeans(mpiesmmr_45_pr))
mpiesmmr_85_pr<-as.data.frame(rowMeans(mpiesmmr_85_pr))
mricgcm3_26_pr<-as.data.frame(rowMeans(mricgcm3_26_pr))
mricgcm3_45_pr<-as.data.frame(rowMeans(mricgcm3_45_pr))
mricgcm3_85_pr<-as.data.frame(rowMeans(mricgcm3_85_pr))
noresm1m_26_pr<-as.data.frame(rowMeans(noresm1m_26_pr))
noresm1m_45_pr<-as.data.frame(rowMeans(noresm1m_45_pr))
noresm1m_85_pr<-as.data.frame(rowMeans(noresm1m_85_pr))
noresm1me_26_pr<-as.data.frame(rowMeans(noresm1me_26_pr))
noresm1me_45_pr<-as.data.frame(rowMeans(noresm1me_45_pr))
noresm1me_85_pr<-as.data.frame(rowMeans(noresm1me_85_pr))



#360-day calendar dataframe
#####Missing data in original files:
#hadgem2es_26: missing hadgem2ao_85[20131:20160] 2001-12-01 to 2005-12-30
#hadgem2es_45: missing hadgem2ao_85[20131:20160; 54001:54360] 2100-01-01 to 2100-12-30
#hadgem2es_85: missing hadgem2ao_85[54001:54360] 2100-01-01 to 2100-12-30

#Add fill value (-9999) to HadGem2-ES RCP 2.6 file
hadgem2es_26_pr_a <- as.data.frame(hadgem2es_26_pr[1:20130,1])
hadgem2es_26_pr_b <- as.data.frame(hadgem2es_26_pr[20131:54330,1])
fill <- as.data.frame(rep(-9999, 30))
names(fill) <- names(hadgem2es_26_pr_a) <- names(hadgem2es_26_pr_b) <- "hadgem2es_26_pr"
hadgem2es_26_pr <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_26_pr <- rbind(hadgem2es_26_pr_a,fill,hadgem2es_26_pr_b)

#Add fill value (-9999) to HadGem2-ES RCP 4.5 file
hadgem2es_45_pr_a <- as.data.frame(hadgem2es_45_pr[1:20130,1])
hadgem2es_45_pr_b <- as.data.frame(hadgem2es_45_pr[20131:53970,1])
fill <- as.data.frame(rep(-9999, 30))
fill2 <- as.data.frame(rep(-9999,360))
names(fill) <- names(fill2) <- names(hadgem2es_45_pr_a) <- names(hadgem2es_45_pr_b) <- "hadgem2es_45_pr"
hadgem2es_45_pr <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_45_pr <- rbind(hadgem2es_45_pr_a,fill,hadgem2es_45_pr_b,fill2)

#Add fill value (-9999) to HadGem2-ES RCP 8.5 file
hadgem2es_85_pr_a <- as.data.frame(hadgem2es_85_pr[1:54000,1])
fill <- as.data.frame(rep(-9999,360))
names(fill) <- names(hadgem2es_85_pr_a) <- "hadgem2es_85_pr"
hadgem2es_85_pr <- as.data.frame(matrix(NA, nrow=54360, ncol=1))
hadgem2es_85_pr <- rbind(hadgem2es_85_pr_a,fill)



#If some models are missing data for certain dates, perform the following test to identify which dates are missing, then replace with NAs
#Must be performed on files in nc format - e.g. re-run first section of code (open netcdf files), then run the following:

hes26pr <- nc.get.time.series(hadgem2es_26_pr, v = "pr",
                                     time.dim.name = "time") #DF of dates for incomplete df
hes26pr <- as.Date(format(hes26pr,"%Y-%m-%d"))

hao85pr <- nc.get.time.series(hadgem2ao_85_pr, v = "pr",
                                     time.dim.name = "time") #DF of dates for complete dataframe (in this case 54360 values)
hao85pr <- as.Date(format(hao85pr,"%Y-%m-%d"))

which(!(hao85pr %in% hes26pr)) #check which indexed cells are missing from complete (hao85pr as ex) df of dates




#Bind all 360-day calendar datasets together

pr_360 <- as.data.frame(matrix(NA, nrow=54360, ncol=9))

pr_360[,1] <- as.numeric(format(threesixtytime,"%Y"))
pr_360[,2] <- as.numeric(format(threesixtytime,"%m"))
pr_360[,3] <- as.numeric(format(threesixtytime,"%d"))
pr_360[,4] <- as.numeric(pr_360[,1])*100+as.numeric(pr_360[,2])
pr_360[,5] <- hadgem2ao_26_pr[,1]
pr_360[,6] <- hadgem2ao_45_pr[,1]
pr_360[,7] <- hadgem2ao_85_pr[,1]
pr_360[,8] <- hadgem2es_26_pr[,1] #54330
pr_360[,9] <- hadgem2es_45_pr[,1] #53970
pr_360[,10] <- hadgem2es_85_pr[,1]
names(pr_360) <- c("year","month","day","code",
                     "hadgem2ao_26", "hadgem2ao_45", "hadgem2ao_85",
                     "hadgem2es_26", "hadgem2es_45", "hadgem2es_85")

pr_360[pr_360==-9999] <- NA #replace fill value with NA

#365-day calendar dataframe
#####Missing data values in original files:
#bcccsm11_85_pr[,1], #bcccsm11_26[54750] 2099-12-31 msg
#bcccsm11m_45_pr[,1], #bcccsm11_26[55115] 2100-12-31


#Add fill value (-9999) to bcc-csm-1-1 RCP 8.5 file
bcccsm11_85_pr_a <- as.data.frame(bcccsm11_85_pr[1:54749,1])
bcccsm11_85_pr_b <- as.data.frame(bcccsm11_85_pr[54750:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11_85_pr_a) <- names(bcccsm11_85_pr_b) <- "bcccsm11_85_pr"
bcccsm11_85_pr <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11_85_pr <- rbind(bcccsm11_85_pr_a,fill,bcccsm11_85_pr_b)

#Add fill value (-9999) to bcc-csm-1-1-m RCP 4.5 file
bcccsm11m_45_pr_a <- as.data.frame(bcccsm11m_45_pr[1:55114,1])
fill <- as.data.frame(rep(-9999, 1))
names(fill) <- names(bcccsm11m_45_pr_a) <- "bcccsm11m_45_pr"
bcccsm11m_45_pr <- as.data.frame(matrix(NA, nrow=55115, ncol=1))
bcccsm11m_45_pr <- rbind(bcccsm11m_45_pr_a,fill)

#Bind all 360-day calendar datasets together

pr_365 <- cbind.data.frame(as.numeric(format(threesixtyfivetime, "%Y")),
                             as.numeric(format(threesixtyfivetime, "%m")),
                             as.numeric(format(threesixtyfivetime, "%d")),
                             as.numeric(format(threesixtyfivetime,"%Y"))*100+as.numeric(format(threesixtyfivetime,"%m")),
                             bcccsm11_26_pr[,1],
                             bcccsm11_45_pr[,1],
                             bcccsm11_85_pr[,1], 
                             bcccsm11m_26_pr[,1],
                             bcccsm11m_45_pr[,1], 
                             bcccsm11m_85_pr[,1],
                             bnuesm_26_pr[,1],
                             bnuesm_45_pr[,1],
                             bnuesm_85_pr[,1],
                             canesm2_26_pr[,1],
                             canesm2_45_pr[,1],
                             canesm2_85_pr[,1],
                             ccsm4_26_pr[,1],
                             ccsm4_45_pr[,1],
                             ccsm4_85_pr[,1],
                             cesm1cam5_26_pr[,1],
                             cesm1cam5_45_pr[,1],
                             cesm1cam5_85_pr[,1],
                             csiromk360_26_pr[,1],
                             csiromk360_45_pr[,1],
                             csiromk360_85_pr[,1],
                             fgoalsg2_26_pr[,1],
                             fgoalsg2_45_pr[,1],
                             fgoalsg2_85_pr[,1],
                             gfdlcm3_26_pr[,1],
                             gfdlcm3_45_pr[,1],
                             gfdlcm3_85_pr[,1],
                             gfdlesm2g_26_pr[,1],
                             gfdlesm2g_45_pr[,1],
                             gfdlesm2g_85_pr[,1],
                             gfdlesm2m_26_pr[,1],
                             gfdlesm2m_45_pr[,1],
                             gfdlesm2m_85_pr[,1],
                             ipslcm5alr_26_pr[,1],
                             ipslcm5alr_45_pr[,1],
                             ipslcm5alr_85_pr[,1],
                             ipslcm5amr_26_pr[,1],
                             ipslcm5amr_45_pr[,1],
                             ipslcm5amr_85_pr[,1],
                             miroc5_26_pr[,1],
                             miroc5_45_pr[,1],
                             miroc5_85_pr[,1],
                             noresm1m_26_pr[,1],
                             noresm1m_45_pr[,1],
                             noresm1m_85_pr[,1],
                             noresm1me_26_pr[,1],
                             noresm1me_45_pr[,1],
                             noresm1me_85_pr[,1])

names(pr_365) <- c("year","month","day","code",
                     "bcccsm11_26","bcccsm11_45","bcccsm11_85",
                     "bcccsm11m_26","bcccsm11m_45","bcccsm11m_85",
                     "bnuesm_26","bnuesm_45","bnuesm_85",
                     "canesm2_26","canesm2_45","canesm2_85",
                     "ccsm4_26","ccsm4_45","ccsm4_85",
                     "cesm1cam5_26","cesm1cam5_45","cesm1cam5_85",
                     "csiromk360_26","csiromk360_45","csiromk360_85",
                     "fgoalsg2_26","fgoalsg2_45","fgoalsg2_85",
                     "gfdlcm3_26","gfdlcm3_45","gfdlcm3_85",
                     "gfdlesm2g_26","gfdlesm2g_45","gfdlesm2g_85",
                     "gfdlesm2m_26","gfdlesm2m_45","gfdlesm2m_85",
                     "ipslcm5alr_26","ipslcm5alr_45","ipslcm5alr_85",
                     "ipslcm5amr_26","ipslcm5amr_45","ipslcm5amr_85",
                     "miroc5_26","miroc5_45","miroc5_85",
                     "noresm1m_26","noresm1m_45","noresm1m_85",
                     "noresm1me_26","noresm1me_45","noresm1me_85")

pr_365[pr_365==-9999] <- NA #replace fill value with NA

#Standard Calendar

pr_std <- cbind.data.frame(as.numeric(format(standardtime, "%Y")),
                             as.numeric(format(standardtime, "%m")),
                             as.numeric(format(standardtime, "%d")),
                             as.numeric(format(standardtime,"%Y"))*100+as.numeric(format(standardtime,"%m")),
                             cnrmcm5_26_pr[,1],
                             cnrmcm5_45_pr[,1],
                             cnrmcm5_85_pr[,1],
                             mirocesm_26_pr[,1],
                             mirocesm_45_pr[,1],
                             mirocesm_85_pr[,1],
                             mirocesmchem_26_pr[,1],
                             mirocesmchem_45_pr[,1],
                             mirocesmchem_85_pr[,1],
                             mricgcm3_26_pr[,1],
                             mricgcm3_45_pr[,1],
                             mricgcm3_85_pr[,1])

names(pr_std) <- c("year","month","day","code",
                     "cnrmcm5_26","cnrmcm5_45","cnrmcm5_85",
                     "mirocesm_26","mirocesm_45","mirocesm_85",
                     "mirocesmchem_26","mirocesmchem_45","mirocesmchem_85",
                     "mricgcm3_26","mricgcm3_45","mricgcm3_85")

pr_std[pr_std==-9999] <- NA #replace fill value with NA

#Proleptic Gregorian Calendar

pr_greg <- cbind.data.frame(as.numeric(format(prolepticgregtime, "%Y")),
                              as.numeric(format(prolepticgregtime, "%m")),
                              as.numeric(format(prolepticgregtime, "%d")),
                              as.numeric(format(prolepticgregtime,"%Y"))*100+as.numeric(format(prolepticgregtime,"%m")),
                              mpiesmlr_26_pr[,1],
                              mpiesmlr_45_pr[,1],
                              mpiesmlr_85_pr[,1],
                              mpiesmmr_26_pr[,1],
                              mpiesmmr_45_pr[,1],
                              mpiesmmr_85_pr[,1])

names(pr_greg) <- c("year","month","day","code",
                      "mpiesmlr_26","mpiesmlr_45","mpiesmlr_85",
                      "mpiesmmr_26","mpiesmmr_45","mpiesmmr_85")

pr_greg[pr_greg==-9999] <- NA #replace fill value with NA


###############################################################
########################MEAN TEMPERATURE#######################
###############################################################

tmean_360 <- (tmin_360+tmax_360)/2
tmean_365 <- (tmin_365+tmax_365)/2
tmean_std <- (tmin_std+tmax_std)/2
tmean_greg <- (tmin_greg+tmax_greg)/2


###Each section calculates the index of interest for each calendar in a loop, then groups values by RCP, resulting in a DF for each RCP with 24 GCM values per year
###There's code to develop the figures on the drive
###There's also code to calculate relative change for each GCM and take stats across the ensemble
###Annual precip section is most thoroughly commented, as well as snow and spei
###The exceedance code has been copied into the annual precip section
######It's not fully set up - may want to compare each model to itself (hist); but GCM column has to be specified (currently referencing a 25 col DF; GCM vals in cols 2:25, model names are in the DF)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################
############LENGTH OF THE FROST-FREE SEASON#####
################################################

#Number of days between date of the last spring frost and date of the first fall frost (w/o any daily tmin <= 0C)
#Date of last spring frost =  day after which Tmin > 0C; latest possible date set as July 15; determined by assessing Tmin backwards from July 15 - ID date where prev day Tmin <= 0C
#Date of first fall frost = first date where Tmin <= 0C; assessment of Tmin forwards from July 15, ID first occurrence of Tmin <=0C

#Subset of  seasonal daily tmin for given time period (grseas)
tmin_360_grseas <- subset(tmin_360, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmin_365_grseas <- subset(tmin_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmin_std_grseas <- subset(tmin_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmin_greg_grseas <- subset(tmin_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)


#Subset of  seasonal daily tmax for given time period (grseas)
tmax_360_grseas <- subset(tmax_360, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_grseas <- subset(tmax_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_std_grseas <- subset(tmax_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_greg_grseas <- subset(tmax_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
#Number days in calendar year


#Empty DF for total annual tminecipitation
tmin_gs_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmin_gs_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmin_gs_all[,1] <- as.data.frame(unique(tmin_greg$year))

#Empty DF for total annual tminecipitation
tmax_gs_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_gs_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmax_gs_all[,1] <- as.data.frame(unique(tmin_greg$year))

#Empty DF for total annual tminecipitation
gs_length <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(gs_length) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
gs_length[,1] <- as.data.frame(unique(tmin_greg$year))


#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmin_360  mean of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  tn <- tx <- NA #void tminevious data
  tn <- as.data.frame(subset(tmin_360_grseas, year==yr)) #subset of entire DF for year
  tx <- as.data.frame(subset(tmax_360_grseas, year==yr))
  jul15 <- which(tn$month==7 & tn$day==15)
  for (col in 5:ncol(tmin_360_grseas)) {
    col_yr <- cbind.data.frame(tn[,col], tx[,col]) #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      for (k in 1:jul15) {
        lastfrost <- col_yr[jul15-k,1] #tmin working backwards
        if (lastfrost <= 0) {
          lf <- lastfrost
          lf_date <- jul15-k
          break
        }
      }
      for (m in 1:jul15) {
        firstfrost <- col_yr[jul15+m,1] #tmin working forwards
        if (firstfrost <=0) {
          ff <- firstfrost
          ff_date <- jul15+m
          frostfree <- ff_date - lf_date
          gs_length[j,(col-3)] <- frostfree
          tmin_gs_all[j,(col-3)] <- mean(col_yr[lf_date:ff_date, 1])
          tmax_gs_all[j,(col-3)] <- mean(col_yr[lf_date:ff_date,2])
          break
        }
      }
    } else if (length(which(is.na(col_yr)))>0) {
      gs_length[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}



#Add tmin_365  mean of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  tn <- tx <- NA #void tminevious data
  tn <- as.data.frame(subset(tmin_365_grseas, year==yr)) #subset of entire DF for year
  tx <- as.data.frame(subset(tmax_365_grseas, year==yr))
  jul15 <- which(tn$month==7 & tn$day==15)
  for (col in 5:ncol(tmin_365_grseas)) {
    col_yr <- cbind.data.frame(tn[,col], tx[,col]) #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      for (k in 1:jul15) {
        lastfrost <- col_yr[jul15-k,1] #tmin working backwards
        if (lastfrost <= 0) {
          lf <- lastfrost
          lf_date <- jul15-k
          break
        }
      }
      for (m in 1:jul15) {
        firstfrost <- col_yr[jul15+m,1] #tmin working forwards
        if (firstfrost <=0) {
          ff <- firstfrost
          ff_date <- jul15+m
          frostfree <- ff_date - lf_date
          gs_length[j,(col+3)] <- frostfree
          tmin_gs_all[j,(col+3)] <- mean(col_yr[lf_date:ff_date, 1])
          tmax_gs_all[j,(col+3)] <- mean(col_yr[lf_date:ff_date,2])
          break
        }
      }
    } else if (length(which(is.na(col_yr)))>0) {
      gs_length[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}



#Add tmin_std  mean of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  tn <- tx <- NA #void tminevious data
  tn <- as.data.frame(subset(tmin_std_grseas, year==yr)) #subset of entire DF for year
  tx <- as.data.frame(subset(tmax_std_grseas, year==yr))
  jul15 <- which(tn$month==7 & tn$day==15)
  for (col in 5:ncol(tmin_std_grseas)) {
    col_yr <- cbind.data.frame(tn[,col], tx[,col]) #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      for (k in 1:jul15) {
        lastfrost <- col_yr[jul15-k,1] #tmin working backwards
        if (lastfrost <= 0) {
          lf <- lastfrost
          lf_date <- jul15-k
          break
        }
      }
      for (m in 1:jul15) {
        firstfrost <- col_yr[jul15+m,1] #tmin working forwards
        if (firstfrost <=0) {
          ff <- firstfrost
          ff_date <- jul15+m
          frostfree <- ff_date - lf_date
          gs_length[j,(col+51)] <- frostfree
          tmin_gs_all[j,(col+51)] <- mean(col_yr[lf_date:ff_date, 1])
          tmax_gs_all[j,(col+51)] <- mean(col_yr[lf_date:ff_date,2])
          break
        }
      }
    } else if (length(which(is.na(col_yr)))>0) {
      gs_length[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}



#Add tmin_greg  mean of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  tn <- tx <- NA #void tminevious data
  tn <- as.data.frame(subset(tmin_greg_grseas, year==yr)) #subset of entire DF for year
  tx <- as.data.frame(subset(tmax_greg_grseas, year==yr))
  jul15 <- which(tn$month==7 & tn$day==15)
  for (col in 5:ncol(tmin_greg_grseas)) {
    col_yr <- cbind.data.frame(tn[,col], tx[,col]) #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      for (k in 1:jul15) {
        lastfrost <- col_yr[jul15-k,1] #tmin working backwards
        if (lastfrost <= 0) {
          lf <- lastfrost
          lf_date <- jul15-k
          break
        }
      }
      for (m in 1:jul15) {
        firstfrost <- col_yr[jul15+m,1] #tmin working forwards
        if (firstfrost <=0) {
          ff <- firstfrost
          ff_date <- jul15+m
          frostfree <- ff_date - lf_date
          gs_length[j,(col+63)] <- frostfree
          tmin_gs_all[j,(col+63)] <- mean(col_yr[lf_date:ff_date, 1])
          tmax_gs_all[j,(col+63)] <- mean(col_yr[lf_date:ff_date,2])
          break
        }
      }
    } else if (length(which(is.na(col_yr)))>0) {
      gs_length[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}



tmin_grseas_26 <- cbind.data.frame(tmin_gs_all[,1], tmin_gs_all[,seq(2,73,3)])
tmin_grseas_45 <- cbind.data.frame(tmin_gs_all[,1], tmin_gs_all[,seq(3,73,3)])
names(tmin_grseas_26)[1] <- names(tmin_grseas_45)[1] <- "year"
tmin_grseas_85 <- tmin_gs_all[,seq(1,73,3)]


tmax_grseas_26 <- cbind.data.frame(tmax_gs_all[,1], tmax_gs_all[,seq(2,73,3)])
tmax_grseas_45 <- cbind.data.frame(tmax_gs_all[,1], tmax_gs_all[,seq(3,73,3)])
names(tmax_grseas_26)[1] <- names(tmax_grseas_45)[1] <- "year"
tmax_grseas_85 <- tmax_gs_all[,seq(1,73,3)]


gs_length_26 <- cbind.data.frame(gs_length[,1], gs_length[,seq(2,73,3)])
gs_length_45 <- cbind.data.frame(gs_length[,1], gs_length[,seq(3,73,3)])
names(gs_length_26)[1] <- names(gs_length_45)[1] <- "year"
gs_length_85 <- gs_length[,seq(1,73,3)]


tmin_grseas_26_hist <- colMeans(na.omit(tmin_grseas_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_grseas_26_nearfut <- colMeans(na.omit(tmin_grseas_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_grseas_26_farfut <- colMeans(na.omit(tmin_grseas_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_grseas_45_hist <- colMeans(na.omit(tmin_grseas_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_grseas_45_nearfut <- colMeans(na.omit(tmin_grseas_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_grseas_45_farfut <- colMeans(na.omit(tmin_grseas_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_grseas_85_hist <- colMeans(na.omit(tmin_grseas_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_grseas_85_nearfut <- colMeans(na.omit(tmin_grseas_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_grseas_85_farfut <- colMeans(na.omit(tmin_grseas_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period


#abs values
data <- c(round(mean(quantile(tmin_grseas_26_hist,0.1), quantile(tmin_grseas_45_hist,0.1), quantile(tmin_grseas_85_hist,0.1)),1),
          round(quantile(tmin_grseas_26_nearfut,0.1),1), round(quantile(tmin_grseas_45_nearfut,0.1),1), round(quantile(tmin_grseas_85_nearfut,0.1),1),
          round(quantile(tmin_grseas_26_farfut,0.1),1), round(quantile(tmin_grseas_45_farfut, 0.1),1), round(quantile(tmin_grseas_85_farfut, 0.1),1),
          round(mean(median(tmin_grseas_26_hist), median(tmin_grseas_45_hist), median(tmin_grseas_85_hist)),1),
          round(median(tmin_grseas_26_nearfut),1), round(median(tmin_grseas_45_nearfut),1), round(median(tmin_grseas_85_nearfut),1),
          round(median(tmin_grseas_26_farfut),1), round(median(tmin_grseas_45_farfut),1), round(median(tmin_grseas_85_farfut),1),
          round(mean(quantile(tmin_grseas_26_hist,0.9), quantile(tmin_grseas_45_hist,0.9), quantile(tmin_grseas_85_hist,0.9)),1),
          round(quantile(tmin_grseas_26_nearfut,0.9),1), round(quantile(tmin_grseas_45_nearfut,0.9),1), round(quantile(tmin_grseas_85_nearfut,0.9),1),
          round(quantile(tmin_grseas_26_farfut,0.9),1), round(quantile(tmin_grseas_45_farfut, 0.9), 1), round(quantile(tmin_grseas_85_farfut, 0.9),1))

print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmin_grseas_26_nearfut-tmin_grseas_26_hist),0.1),1), round(quantile((tmin_grseas_45_nearfut-tmin_grseas_45_hist),0.1),1), round(quantile((tmin_grseas_85_nearfut-tmin_grseas_85_hist),0.1),1),
           round(quantile((tmin_grseas_26_farfut-tmin_grseas_26_hist),0.1),1), round(quantile((tmin_grseas_45_farfut-tmin_grseas_45_hist), 0.1, 1)), round(quantile((tmin_grseas_85_farfut-tmin_grseas_85_hist), 0.1),1),
           round(median(tmin_grseas_26_nearfut-tmin_grseas_26_hist),1), round(median(tmin_grseas_45_nearfut-tmin_grseas_45_hist),1), round(median(tmin_grseas_85_nearfut-tmin_grseas_85_hist),1),
           round(median(tmin_grseas_26_farfut-tmin_grseas_26_hist),1), round(median(tmin_grseas_45_farfut-tmin_grseas_45_hist),1), round(median(tmin_grseas_85_farfut-tmin_grseas_85_hist),1),
           round(quantile((tmin_grseas_26_nearfut-tmin_grseas_26_hist),0.9),1), round(quantile((tmin_grseas_45_nearfut-tmin_grseas_45_hist),0.9),1), round(quantile((tmin_grseas_85_nearfut-tmin_grseas_85_hist),0.9),1),
           round(quantile((tmin_grseas_26_farfut-tmin_grseas_26_hist),0.9),1), round(quantile((tmin_grseas_45_farfut-tmin_grseas_45_hist), 0.9), 1), round(quantile((tmin_grseas_85_farfut-tmin_grseas_85_hist), 0.9),1))

print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Tmin during growing season

#RCP 2.6 plot
tmin_grseas_26 <- na.omit(tmin_grseas_26)
tmin_gs_26 <- gather(tmin_grseas_26, GCM, tmin_gs, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmin_gs_26_med <- cbind.data.frame(tmin_grseas_26[,1], apply(tmin_grseas_26[,2:25],1,median))
names(tmin_gs_26_med) <- c("year","Median")


ggplot(tmin_gs_26, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_gs_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_frostfreeseas_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmin_grseas_45 <- na.omit(tmin_grseas_45)
tmin_gs_45 <- gather(tmin_grseas_45, GCM, tmin_gs, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmin_gs_45_med <- cbind.data.frame(tmin_grseas_45$year, apply(tmin_grseas_45[,2:25],1,median))
names(tmin_gs_45_med) <- c("year","Median")


ggplot(tmin_gs_45, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_gs_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_frostfreeseas_RCP45.tiff", width = 30, height = 18, units="cm")

#RCP 8.5 plot
tmin_grseas_85 <- na.omit(tmin_grseas_85)
tmin_gs_85 <- gather(tmin_grseas_85, GCM, tmin_gs, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmin_gs_85_med <- cbind.data.frame(tmin_grseas_85$year, apply(tmin_grseas_85[,2:25],1,median))
names(tmin_gs_85_med) <- c("year","Median")


ggplot(tmin_gs_85, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_gs_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_frostfreeseas_RCP85.tiff", width = 30, height = 18, units="cm")


#Tmax during growing season

#RCP 2.6 plot
tmax_grseas_26 <- na.omit(tmax_grseas_26)
tmax_gs_26 <- gather(tmax_grseas_26, GCM, tmax_gs, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_gs_26_med <- cbind.data.frame(tmax_grseas_26[,1], apply(tmax_grseas_26[,2:25],1,median))
names(tmax_gs_26_med) <- c("year","Median")


ggplot(tmax_gs_26, aes(x=year,y=tmax_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_gs_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmax_frostfreeseas_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_grseas_45 <- na.omit(tmax_grseas_45)
tmax_gs_45 <- gather(tmax_grseas_45, GCM, tmax_gs, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_gs_45_med <- cbind.data.frame(tmax_grseas_45$year, apply(tmax_grseas_45[,2:25],1,median))
names(tmax_gs_45_med) <- c("year","Median")


ggplot(tmax_gs_45, aes(x=year,y=tmax_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_gs_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmax_frostfreeseas_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_grseas_85 <- na.omit(tmax_grseas_85)
tmax_gs_85 <- gather(tmax_grseas_85, GCM, tmax_gs, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_gs_85_med <- cbind.data.frame(tmax_grseas_85$year, apply(tmax_grseas_85[,2:25],1,median))
names(tmax_gs_85_med) <- c("year","Median")


ggplot(tmax_gs_85, aes(x=year,y=tmax_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_gs_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Frost-free season Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmax_frostfreeseas_RCP85.tiff", width = 30, height = 18, units="cm")


#Growing season length fig

#RCP 2.6 plot
gs_length_26 <- na.omit(gs_length_26)
gslen_26 <- gather(gs_length_26, GCM, tmin_gs, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

gslen_26_med <- cbind.data.frame(gs_length_26[,1], apply(gs_length_26[,2:25],1,median))
names(gslen_26_med) <- c("year","Median")


ggplot(gslen_26, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = gslen_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Length of the Frost-Free Season (days)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FrostFreeSeasonLength_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
gs_length_45 <- na.omit(gs_length_45)
gslen_45 <- gather(gs_length_45, GCM, tmin_gs, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

gslen_45_med <- cbind.data.frame(gs_length_45$year, apply(gs_length_45[,2:25],1,median))
names(gslen_45_med) <- c("year","Median")


ggplot(gslen_45, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = gslen_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Length of the Frost-Free Season (days)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FrostFreeSeasonLength_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
gs_length_85 <- na.omit(gs_length_85)
gslen_85 <- gather(gs_length_85, GCM, tmin_gs, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

gslen_85_med <- cbind.data.frame(gs_length_85$year, apply(gs_length_85[,2:25],1,median))
names(gslen_85_med) <- c("year","Median")


ggplot(gslen_85, aes(x=year,y=tmin_gs)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = gslen_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Length of the Frost-Free Season (days)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FrostFreeSeasonLength_RCP85.tiff", width = 30, height = 18, units="cm")



##############################################
###############Precipitation Totals###########
##############################################

##############################################
##############Annual Precipitation############
##############################################

#Subset of  seasonal daily pr for given time period (annual i.e. not necessary here, but done to keep naming convention (repeated throughout))
pr_360_annual <- subset(pr_360,month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_365_annual <- subset(pr_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_std_annual <- subset(pr_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
pr_greg_annual <- subset(pr_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)

#Empty DF for total annual precipitation
pr_annual_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(pr_annual_all) <- c("year",names(pr_360[,5:10]), names(pr_365[,5:52]), names(pr_std[,5:16]),names(pr_greg[,5:10]))
pr_annual_all[,1] <- as.data.frame(unique(pr_greg$year))

#Each loop calculates total annual precip for each year (done for other vars on a seasonal or monthly basis by specifying subset above)
#four slightly different calendars kept separate until have annual vals

#Add pr_360  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_360_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_360_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_annual_all[j, (col-3)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_annual_all[j,(col-3)] <- NA #NA all years with NA vals (some models are missing values in the daily dataset for certain years, this removes those years)
    } 
  }
}

#Add pr_365  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_365_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_365_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_annual_all[j, (col+3)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_annual_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_std  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_std_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_std_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_annual_all[j, (col+51)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_annual_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_greg  sum of daily precip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_greg_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_greg_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_annual_all[j, (col+63)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_annual_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

#separate dataframe for each rcp; each rcp has 24 GCMs

pr_annual_26 <- cbind.data.frame(pr_annual_all[,1], pr_annual_all[,seq(2,73,3)])
pr_annual_45 <- cbind.data.frame(pr_annual_all[,1], pr_annual_all[,seq(3,73,3)])
names(pr_annual_26)[1] <- names(pr_annual_45)[1] <- "year"
pr_annual_85 <- pr_annual_all[,seq(1,73,3)]

#Time periods - index
###1976-2005 - rows 27:56
###2040-69 - rows 91:120
###2070-99 - rows 121:150

#cumulative exceedance code

A <- pr_annual_85[121:150,2] #ie 2070-99, arbitrary gcm selected
A <- A[order(A, decreasing = TRUE)] 
# and then just being paired with an arbitrary increasing list of numbers: 
Rank <- 1:length(A)


DF <- data.frame(A, Rank)
DF$Prob <-  DF$Rank/(length(DF$Rank) + 1)

ggplot(data = DF, aes(x = Prob, y = A)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.20)) +
  #May not need the log scale, depending on the magnitudes
  scale_y_continuous(trans = "log10")

# And then plotting the different 30-year blocks against each other? Maybe with a tidyverse solution like "%>% group_by(30_year_block)"?



#for each time period take stats (median, 10th, 90th percentile)
#absolute values (first set); relative change (second set)
#note relative changes are calculated for each model (i.e. future-hist for same gcm) proir to taking median, etc. across ensemble

data <- c(round(mean(quantile(pr_annual_26_hist,0.1), quantile(pr_annual_45_hist,0.1), quantile(pr_annual_85_hist,0.1)),1),
          round(quantile(pr_annual_26_nearfut,0.1),1), round(quantile(pr_annual_45_nearfut,0.1),1), round(quantile(pr_annual_85_nearfut,0.1),1),
          round(quantile(pr_annual_26_farfut,0.1),1), round(quantile(pr_annual_45_farfut, 0.1),1), round(quantile(pr_annual_85_farfut, 0.1),1),
          round(mean(median(pr_annual_26_hist), median(pr_annual_45_hist), median(pr_annual_85_hist)),1),
          round(median(pr_annual_26_nearfut),1), round(median(pr_annual_45_nearfut),1), round(median(pr_annual_85_nearfut),1),
          round(median(pr_annual_26_farfut),1), round(median(pr_annual_45_farfut),1), round(median(pr_annual_85_farfut),1),
          round(mean(quantile(pr_annual_26_hist,0.9), quantile(pr_annual_45_hist,0.9), quantile(pr_annual_85_hist,0.9)),1),
          round(quantile(pr_annual_26_nearfut,0.9),1), round(quantile(pr_annual_45_nearfut,0.9),1), round(quantile(pr_annual_85_nearfut,0.9),1),
          round(quantile(pr_annual_26_farfut,0.9),1), round(quantile(pr_annual_45_farfut, 0.9), 1), round(quantile(pr_annual_85_farfut, 0.9),1))
print("Annual Precipitation (mm)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((100*(pr_annual_26_nearfut-pr_annual_26_hist)/pr_annual_26_hist),0.1),1), round(quantile((100*(pr_annual_45_nearfut-pr_annual_45_hist)/pr_annual_45_hist),0.1),1), round(quantile((100*(pr_annual_85_nearfut-pr_annual_85_hist)/pr_annual_85_hist),0.1),1),
           round(quantile((100*(pr_annual_26_farfut-pr_annual_26_hist)/pr_annual_26_hist),0.1),1), round(quantile((100*(pr_annual_45_farfut-pr_annual_45_hist)/pr_annual_45_hist), 0.1, 1)), round(quantile((100*(pr_annual_85_farfut-pr_annual_85_hist)/pr_annual_85_hist), 0.1),1),
           round(median(100*(pr_annual_26_nearfut-pr_annual_26_hist)/pr_annual_26_hist),1), round(median(100*(pr_annual_45_nearfut-pr_annual_45_hist)/pr_annual_45_hist),1), round(median(100*(pr_annual_85_nearfut-pr_annual_85_hist)/pr_annual_85_hist),1),
           round(median(100*(pr_annual_26_farfut-pr_annual_26_hist)/pr_annual_26_hist),1), round(median(100*(pr_annual_45_farfut-pr_annual_45_hist)/pr_annual_45_hist),1), round(median(100*(pr_annual_85_farfut-pr_annual_85_hist)/pr_annual_85_hist),1),
           round(quantile((100*(pr_annual_26_nearfut-pr_annual_26_hist)/pr_annual_26_hist),0.9),1), round(quantile((100*(pr_annual_45_nearfut-pr_annual_45_hist)/pr_annual_45_hist),0.9),1), round(quantile((100*(pr_annual_85_nearfut-pr_annual_85_hist)/pr_annual_85_hist),0.9),1),
           round(quantile((100*(pr_annual_26_farfut-pr_annual_26_hist)/pr_annual_26_hist),0.9),1), round(quantile((100*(pr_annual_45_farfut-pr_annual_45_hist)/pr_annual_45_hist), 0.9), 1), round(quantile((100*(pr_annual_85_farfut-pr_annual_85_hist)/pr_annual_85_hist), 0.9),1))
print("Annual Precipitation (%)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
pr_annual_26 <- na.omit(pr_annual_26)
pr_ann_26 <- pr_annual_26 %>%
  gather(GCM, pr_ann, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

pr_ann_26_med <- cbind.data.frame(pr_annual_26[,1], apply(pr_annual_26[,2:25],1,median))
names(pr_ann_26_med) <- c("year","Median")


ggplot(pr_ann_26, aes(x=year,y=pr_ann)) + 
  geom_fan(intervals=c(0,0.8, 1.0)) +
  theme_bw() +
  geom_line(data = pr_ann_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  #geom_line(data=pr_ann_26, aes(x=year, y=pr_ann, colour=GCM))+ #plot all gcms indiv.
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Annual_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
pr_annual_45 <- na.omit(pr_annual_45)
pr_ann_45 <- pr_annual_45 %>%
  gather(GCM, pr_ann, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

pr_ann_45_med <- cbind.data.frame(pr_annual_45$year, apply(pr_annual_45[,2:25],1,median))
names(pr_ann_45_med) <- c("year","Median")


ggplot(pr_ann_45, aes(x=year,y=pr_ann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_ann_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Annual_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
pr_annual_85 <- na.omit(pr_annual_85)
pr_ann_85 <- pr_annual_85 %>%
  gather(GCM, pr_ann, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

pr_ann_85_med <- cbind.data.frame(pr_annual_85$year, apply(pr_annual_85[,2:25],1,median))
names(pr_ann_85_med) <- c("year","Median")


ggplot(pr_ann_85, aes(x=year,y=pr_ann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_ann_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Annual_RCP85.tiff", width = 30, height = 18, units="cm")


##############################################
#######Maximum 1-day Precipitation############
##############################################

#Subset of  seasonal daily pr for given time period (maximum)
pr_360_maximum <- subset(pr_360,month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_365_maximum <- subset(pr_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_std_maximum <- subset(pr_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
pr_greg_maximum <- subset(pr_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)



#Empty DF for total maximum precipitation
pr_maximum_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(pr_maximum_all) <- c("year",names(pr_360[,5:10]), names(pr_365[,5:52]), names(pr_std[,5:16]),names(pr_greg[,5:10]))
pr_maximum_all[,1] <- as.data.frame(unique(pr_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add pr_360  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_360_maximum, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_360_maximum)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_maximum_all[j, (col-3)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_maximum_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_365  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_365_maximum, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_365_maximum)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_maximum_all[j, (col+3)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_maximum_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_std  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_std_maximum, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_std_maximum)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_maximum_all[j, (col+51)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_maximum_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_greg  sum of daily precip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_greg_maximum, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_greg_maximum)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_maximum_all[j, (col+63)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_maximum_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

pr_maximum_26 <- cbind.data.frame(pr_maximum_all[,1], pr_maximum_all[,seq(2,73,3)])
pr_maximum_45 <- cbind.data.frame(pr_maximum_all[,1], pr_maximum_all[,seq(3,73,3)])
names(pr_maximum_26)[1] <- names(pr_maximum_45)[1] <- "year"
pr_maximum_85 <- pr_maximum_all[,seq(1,73,3)]


pr_maximum_26_hist <- colMeans(na.omit(pr_maximum_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_maximum_26_nearfut <- colMeans(na.omit(pr_maximum_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_maximum_26_farfut <- colMeans(na.omit(pr_maximum_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

pr_maximum_45_hist <- colMeans(na.omit(pr_maximum_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_maximum_45_nearfut <- colMeans(na.omit(pr_maximum_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_maximum_45_farfut <- colMeans(na.omit(pr_maximum_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

pr_maximum_85_hist <- colMeans(na.omit(pr_maximum_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_maximum_85_nearfut <- colMeans(na.omit(pr_maximum_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_maximum_85_farfut <- colMeans(na.omit(pr_maximum_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period


#Print table of stats for tech sub
#ooo: for pr/pr, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 pr (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(pr_maximum_26_hist,0.1), quantile(pr_maximum_45_hist,0.1), quantile(pr_maximum_85_hist,0.1)),1),
          round(quantile(pr_maximum_26_nearfut,0.1),1), round(quantile(pr_maximum_45_nearfut,0.1),1), round(quantile(pr_maximum_85_nearfut,0.1),1),
          round(quantile(pr_maximum_26_farfut,0.1),1), round(quantile(pr_maximum_45_farfut, 0.1),1), round(quantile(pr_maximum_85_farfut, 0.1),1),
          round(mean(median(pr_maximum_26_hist), median(pr_maximum_45_hist), median(pr_maximum_85_hist)),1),
          round(median(pr_maximum_26_nearfut),1), round(median(pr_maximum_45_nearfut),1), round(median(pr_maximum_85_nearfut),1),
          round(median(pr_maximum_26_farfut),1), round(median(pr_maximum_45_farfut),1), round(median(pr_maximum_85_farfut),1),
          round(mean(quantile(pr_maximum_26_hist,0.9), quantile(pr_maximum_45_hist,0.9), quantile(pr_maximum_85_hist,0.9)),1),
          round(quantile(pr_maximum_26_nearfut,0.9),1), round(quantile(pr_maximum_45_nearfut,0.9),1), round(quantile(pr_maximum_85_nearfut,0.9),1),
          round(quantile(pr_maximum_26_farfut,0.9),1), round(quantile(pr_maximum_45_farfut, 0.9), 1), round(quantile(pr_maximum_85_farfut, 0.9),1))
print("maximum Precipitation (mm)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((100*(pr_maximum_26_nearfut-pr_maximum_26_hist)/pr_maximum_26_hist),0.1),1), round(quantile((100*(pr_maximum_45_nearfut-pr_maximum_45_hist)/pr_maximum_45_hist),0.1),1), round(quantile((100*(pr_maximum_85_nearfut-pr_maximum_85_hist)/pr_maximum_85_hist),0.1),1),
           round(quantile((100*(pr_maximum_26_farfut-pr_maximum_26_hist)/pr_maximum_26_hist),0.1),1), round(quantile((100*(pr_maximum_45_farfut-pr_maximum_45_hist)/pr_maximum_45_hist), 0.1, 1)), round(quantile((100*(pr_maximum_85_farfut-pr_maximum_85_hist)/pr_maximum_85_hist), 0.1),1),
           round(median(100*(pr_maximum_26_nearfut-pr_maximum_26_hist)/pr_maximum_26_hist),1), round(median(100*(pr_maximum_45_nearfut-pr_maximum_45_hist)/pr_maximum_45_hist),1), round(median(100*(pr_maximum_85_nearfut-pr_maximum_85_hist)/pr_maximum_85_hist),1),
           round(median(100*(pr_maximum_26_farfut-pr_maximum_26_hist)/pr_maximum_26_hist),1), round(median(100*(pr_maximum_45_farfut-pr_maximum_45_hist)/pr_maximum_45_hist),1), round(median(100*(pr_maximum_85_farfut-pr_maximum_85_hist)/pr_maximum_85_hist),1),
           round(quantile((100*(pr_maximum_26_nearfut-pr_maximum_26_hist)/pr_maximum_26_hist),0.9),1), round(quantile((100*(pr_maximum_45_nearfut-pr_maximum_45_hist)/pr_maximum_45_hist),0.9),1), round(quantile((100*(pr_maximum_85_nearfut-pr_maximum_85_hist)/pr_maximum_85_hist),0.9),1),
           round(quantile((100*(pr_maximum_26_farfut-pr_maximum_26_hist)/pr_maximum_26_hist),0.9),1), round(quantile((100*(pr_maximum_45_farfut-pr_maximum_45_hist)/pr_maximum_45_hist), 0.9), 1), round(quantile((100*(pr_maximum_85_farfut-pr_maximum_85_hist)/pr_maximum_85_hist), 0.9),1))
print("maximum Precipitation (%)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
pr_maximum_26 <- na.omit(pr_maximum_26)
pr_max_26 <- gather(pr_maximum_26, GCM, pr_max, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

pr_max_26_med <- cbind.data.frame(pr_maximum_26[,1], apply(pr_maximum_26[,2:25],1,median))
names(pr_max_26_med) <- c("year","Median")


ggplot(pr_max_26, aes(x=year,y=pr_max)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_max_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum 1-Day Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Maximum1Day_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
pr_maximum_45 <- na.omit(pr_maximum_45)
pr_max_45 <- gather(pr_maximum_45, GCM, pr_max, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

pr_max_45_med <- cbind.data.frame(pr_maximum_45$year, apply(pr_maximum_45[,2:25],1,median))
names(pr_max_45_med) <- c("year","Median")


ggplot(pr_max_45, aes(x=year,y=pr_max)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_max_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum 1-Day Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Maximum1Day_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
pr_maximum_85 <- na.omit(pr_maximum_85)
pr_max_85 <- gather(pr_maximum_85, GCM, pr_max, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

pr_max_85_med <- cbind.data.frame(pr_maximum_85$year, apply(pr_maximum_85[,2:25],1,median))
names(pr_max_85_med) <- c("year","Median")


ggplot(pr_max_85, aes(x=year,y=pr_max)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_max_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum 1-Day Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_Maximum1Day_RCP85.tiff", width = 30, height = 18, units="cm")



##############################################
###############summer Precipitation###########
##############################################

#Annual Precipitation

#Subset of  seasonal daily pr for given time period (summer)
pr_360_summer <- subset(pr_360, month ==6 | month==7 | month==8) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_365_summer <- subset(pr_365, month ==6 | month==7 | month==8) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
pr_std_summer <- subset(pr_std, month ==6 | month==7 | month==8)
pr_greg_summer <- subset(pr_greg, month ==6 | month==7 | month==8)


#Empty DF for total annual precipitation
pr_summer_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(pr_summer_all) <- c("year",names(pr_360[,5:10]), names(pr_365[,5:52]), names(pr_std[,5:16]),names(pr_greg[,5:10]))
pr_summer_all[,1] <- as.data.frame(unique(pr_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add pr_360  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_360_summer, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_360_summer)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_summer_all[j, (col-3)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_summer_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_365  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_365_summer, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_365_summer)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_summer_all[j, (col+3)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_summer_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_std  sum of daily precip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_std_summer, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_std_summer)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_summer_all[j, (col+51)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_summer_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add pr_greg  sum of daily precip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void previous data
  sub <- as.data.frame(subset(pr_greg_summer, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(pr_greg_summer)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      pr_summer_all[j, (col+63)] <- sum(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      pr_summer_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

pr_summer_26 <- cbind.data.frame(pr_summer_all[,1], pr_summer_all[,seq(2,73,3)])
pr_summer_45 <- cbind.data.frame(pr_summer_all[,1], pr_summer_all[,seq(3,73,3)])
names(pr_summer_26)[1] <- names(pr_summer_45)[1] <- "year"
pr_summer_85 <- pr_summer_all[,seq(1,73,3)]


pr_summer_26_hist <- colMeans(na.omit(pr_summer_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_summer_26_nearfut <- colMeans(na.omit(pr_summer_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_summer_26_farfut <- colMeans(na.omit(pr_summer_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

pr_summer_45_hist <- colMeans(na.omit(pr_summer_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_summer_45_nearfut <- colMeans(na.omit(pr_summer_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_summer_45_farfut <- colMeans(na.omit(pr_summer_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

pr_summer_85_hist <- colMeans(na.omit(pr_summer_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
pr_summer_85_nearfut <- colMeans(na.omit(pr_summer_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
pr_summer_85_farfut <- colMeans(na.omit(pr_summer_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for pr/pr, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 pr (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(pr_summer_26_hist,0.1), quantile(pr_summer_45_hist,0.1), quantile(pr_summer_85_hist,0.1)),1),
          round(quantile(pr_summer_26_nearfut,0.1),1), round(quantile(pr_summer_45_nearfut,0.1),1), round(quantile(pr_summer_85_nearfut,0.1),1),
          round(quantile(pr_summer_26_farfut,0.1),1), round(quantile(pr_summer_45_farfut, 0.1),1), round(quantile(pr_summer_85_farfut, 0.1),1),
          round(mean(median(pr_summer_26_hist), median(pr_summer_45_hist), median(pr_summer_85_hist)),1),
          round(median(pr_summer_26_nearfut),1), round(median(pr_summer_45_nearfut),1), round(median(pr_summer_85_nearfut),1),
          round(median(pr_summer_26_farfut),1), round(median(pr_summer_45_farfut),1), round(median(pr_summer_85_farfut),1),
          round(mean(quantile(pr_summer_26_hist,0.9), quantile(pr_summer_45_hist,0.9), quantile(pr_summer_85_hist,0.9)),1),
          round(quantile(pr_summer_26_nearfut,0.9),1), round(quantile(pr_summer_45_nearfut,0.9),1), round(quantile(pr_summer_85_nearfut,0.9),1),
          round(quantile(pr_summer_26_farfut,0.9),1), round(quantile(pr_summer_45_farfut, 0.9), 1), round(quantile(pr_summer_85_farfut, 0.9),1))

print("summer Precipitation (mm)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((100*(pr_summer_26_nearfut-pr_summer_26_hist)/pr_summer_26_hist),0.1),1), round(quantile((100*(pr_summer_45_nearfut-pr_summer_45_hist)/pr_summer_45_hist),0.1),1), round(quantile((100*(pr_summer_85_nearfut-pr_summer_85_hist)/pr_summer_85_hist),0.1),1),
           round(quantile((100*(pr_summer_26_farfut-pr_summer_26_hist)/pr_summer_26_hist),0.1),1), round(quantile((100*(pr_summer_45_farfut-pr_summer_45_hist)/pr_summer_45_hist), 0.1, 1)), round(quantile((100*(pr_summer_85_farfut-pr_summer_85_hist)/pr_summer_85_hist), 0.1),1),
           round(median(100*(pr_summer_26_nearfut-pr_summer_26_hist)/pr_summer_26_hist),1), round(median(100*(pr_summer_45_nearfut-pr_summer_45_hist)/pr_summer_45_hist),1), round(median(100*(pr_summer_85_nearfut-pr_summer_85_hist)/pr_summer_85_hist),1),
           round(median(100*(pr_summer_26_farfut-pr_summer_26_hist)/pr_summer_26_hist),1), round(median(100*(pr_summer_45_farfut-pr_summer_45_hist)/pr_summer_45_hist),1), round(median(100*(pr_summer_85_farfut-pr_summer_85_hist)/pr_summer_85_hist),1),
           round(quantile((100*(pr_summer_26_nearfut-pr_summer_26_hist)/pr_summer_26_hist),0.9),1), round(quantile((100*(pr_summer_45_nearfut-pr_summer_45_hist)/pr_summer_45_hist),0.9),1), round(quantile((100*(pr_summer_85_nearfut-pr_summer_85_hist)/pr_summer_85_hist),0.9),1),
           round(quantile((100*(pr_summer_26_farfut-pr_summer_26_hist)/pr_summer_26_hist),0.9),1), round(quantile((100*(pr_summer_45_farfut-pr_summer_45_hist)/pr_summer_45_hist), 0.9), 1), round(quantile((100*(pr_summer_85_farfut-pr_summer_85_hist)/pr_summer_85_hist), 0.9),1))

print("summer Precipitation (%)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
pr_summer_26 <- na.omit(pr_summer_26)
pr_sum_26 <- pr_summer_26 %>%
  gather(GCM, pr_sum, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

pr_sum_26_med <- cbind.data.frame(pr_summer_26[,1], apply(pr_summer_26[,2:25],1,median))
names(pr_sum_26_med) <- c("year","Median")


ggplot(pr_sum_26, aes(x=year,y=pr_sum)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_sum_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "summer Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_summer_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
pr_summer_45 <- na.omit(pr_summer_45)
pr_sum_45 <- pr_summer_45 %>%
  gather(GCM, pr_sum, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

pr_sum_45_med <- cbind.data.frame(pr_summer_45$year, apply(pr_summer_45[,2:25],1,median))
names(pr_sum_45_med) <- c("year","Median")


ggplot(pr_sum_45, aes(x=year,y=pr_sum)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_sum_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "summer Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_summer_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
pr_summer_85 <- na.omit(pr_summer_85)
pr_sum_85 <- pr_summer_85 %>%
  gather(GCM, pr_sum, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

pr_sum_85_med <- cbind.data.frame(pr_summer_85$year, apply(pr_summer_85[,2:25],1,median))
names(pr_sum_85_med) <- c("year","Median")


ggplot(pr_sum_85, aes(x=year,y=pr_sum)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pr_sum_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "summer Precipitation (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Precipitation_summer_RCP85.tiff", width = 30, height = 18, units="cm")



##############################################

##############################################
##############Hottest day - max Annual Tmax###
##############################################

#Subset of  seasonal daily tmax for given time period (max)
tmax_360_max <- subset(tmax_360, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_max <- subset(tmax_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_std_max <- subset(tmax_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_greg_max <- subset(tmax_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)

#Empty DF for total max tmaxecipitation
tmax_max_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_max_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_max_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_max, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_360_max)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_max_all[j, (col-3)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_max_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_max, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_365_max)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_max_all[j, (col+3)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_max_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_max, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_std_max)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_max_all[j, (col+51)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_max_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  mean of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_max, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_greg_max)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_max_all[j, (col+63)] <- max(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_max_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmax_max_26 <- cbind.data.frame(tmax_max_all[,1], tmax_max_all[,seq(2,73,3)])
tmax_max_45 <- cbind.data.frame(tmax_max_all[,1], tmax_max_all[,seq(3,73,3)])
names(tmax_max_26)[1] <- names(tmax_max_45)[1] <- "year"
tmax_max_85 <- tmax_max_all[,seq(1,73,3)]


tmax_max_26_hist <- colMeans(na.omit(tmax_max_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_max_26_nearfut <- colMeans(na.omit(tmax_max_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_max_26_farfut <- colMeans(na.omit(tmax_max_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_max_45_hist <- colMeans(na.omit(tmax_max_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_max_45_nearfut <- colMeans(na.omit(tmax_max_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_max_45_farfut <- colMeans(na.omit(tmax_max_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_max_85_hist <- colMeans(na.omit(tmax_max_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_max_85_nearfut <- colMeans(na.omit(tmax_max_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_max_85_farfut <- colMeans(na.omit(tmax_max_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

#Print table of stats for tech sub
#ooo: for tmax/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmax (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmax_max_26_hist,0.1), quantile(tmax_max_45_hist,0.1), quantile(tmax_max_85_hist,0.1)),1),
          round(quantile(tmax_max_26_nearfut,0.1),1), round(quantile(tmax_max_45_nearfut,0.1),1), round(quantile(tmax_max_85_nearfut,0.1),1),
          round(quantile(tmax_max_26_farfut,0.1),1), round(quantile(tmax_max_45_farfut, 0.1),1), round(quantile(tmax_max_85_farfut, 0.1),1),
          round(mean(median(tmax_max_26_hist), median(tmax_max_45_hist), median(tmax_max_85_hist)),1),
          round(median(tmax_max_26_nearfut),1), round(median(tmax_max_45_nearfut),1), round(median(tmax_max_85_nearfut),1),
          round(median(tmax_max_26_farfut),1), round(median(tmax_max_45_farfut),1), round(median(tmax_max_85_farfut),1),
          round(mean(quantile(tmax_max_26_hist,0.9), quantile(tmax_max_45_hist,0.9), quantile(tmax_max_85_hist,0.9)),1),
          round(quantile(tmax_max_26_nearfut,0.9),1), round(quantile(tmax_max_45_nearfut,0.9),1), round(quantile(tmax_max_85_nearfut,0.9),1),
          round(quantile(tmax_max_26_farfut,0.9),1), round(quantile(tmax_max_45_farfut, 0.9), 1), round(quantile(tmax_max_85_farfut, 0.9),1))
print("max Tmax (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_max_26_nearfut-tmax_max_26_hist),0.1),1), round(quantile((tmax_max_45_nearfut-tmax_max_45_hist),0.1),1), round(quantile((tmax_max_85_nearfut-tmax_max_85_hist),0.1),1),
           round(quantile((tmax_max_26_farfut-tmax_max_26_hist),0.1),1), round(quantile((tmax_max_45_farfut-tmax_max_45_hist), 0.1, 1)), round(quantile((tmax_max_85_farfut-tmax_max_85_hist), 0.1),1),
           round(median(tmax_max_26_nearfut-tmax_max_26_hist),1), round(median(tmax_max_45_nearfut-tmax_max_45_hist),1), round(median(tmax_max_85_nearfut-tmax_max_85_hist),1),
           round(median(tmax_max_26_farfut-tmax_max_26_hist),1), round(median(tmax_max_45_farfut-tmax_max_45_hist),1), round(median(tmax_max_85_farfut-tmax_max_85_hist),1),
           round(quantile((tmax_max_26_nearfut-tmax_max_26_hist),0.9),1), round(quantile((tmax_max_45_nearfut-tmax_max_45_hist),0.9),1), round(quantile((tmax_max_85_nearfut-tmax_max_85_hist),0.9),1),
           round(quantile((tmax_max_26_farfut-tmax_max_26_hist),0.9),1), round(quantile((tmax_max_45_farfut-tmax_max_45_hist), 0.9), 1), round(quantile((tmax_max_85_farfut-tmax_max_85_hist), 0.9),1))
print("max Tmax (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#Figures

#RCP 2.6 plot
tmax_max_26 <- na.omit(tmax_max_26)
tmax_mx_26 <- gather(tmax_max_26, GCM, tmax_mx, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_mx_26_med <- cbind.data.frame(tmax_max_26[,1], apply(tmax_max_26[,2:25],1,median))
names(tmax_mx_26_med) <- c("year","Median")


ggplot(tmax_mx_26, aes(x=year,y=tmax_mx)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_mx_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Hottest Day of the Year (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("HottestTmax_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_max_45 <- na.omit(tmax_max_45)
tmax_mx_45 <- gather(tmax_max_45, GCM, tmax_mx, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_mx_45_med <- cbind.data.frame(tmax_max_45$year, apply(tmax_max_45[,2:25],1,median))
names(tmax_mx_45_med) <- c("year","Median")


ggplot(tmax_mx_45, aes(x=year,y=tmax_mx)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_mx_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Hottest Day of the Year (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("HottestTmax_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_max_85 <- na.omit(tmax_max_85)
tmax_mx_85 <- gather(tmax_max_85, GCM, tmax_mx, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_mx_85_med <- cbind.data.frame(tmax_max_85$year, apply(tmax_max_85[,2:25],1,median))
names(tmax_mx_85_med) <- c("year","Median")


ggplot(tmax_mx_85, aes(x=year,y=tmax_mx)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_mx_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Hottest Day of the Year (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("HottestTmax_RCP85.tiff", width = 30, height = 18, units="cm")


########################################################
######################Winter Tmax#######################
########################################################

#Subset of  seasonal daily tmax for given time period (tp)
tmax_360_winter <- subset(tmax_360, month==12 | month==1 | month==2)
tmax_365_winter <- subset(tmax_365, month==12 | month==1 | month==2)
tmax_std_winter <- subset(tmax_std, month==12 | month==1 | month==2)
tmax_greg_winter <- subset(tmax_greg, month==12 | month==1 | month==2)

tmax_360_winter$mthyr <- tmax_360_winter$month*tmax_360_winter$year
tmax_365_winter$mthyr <- tmax_365_winter$month*tmax_365_winter$year
tmax_std_winter$mthyr <- tmax_std_winter$month*tmax_std_winter$year
tmax_greg_winter$mthyr <- tmax_greg_winter$month*tmax_greg_winter$year

#add mth col to df; mth*yr col; in for loop, select for 12*(yr-1), 1*yr, 2*yr==mth*yr

#Empty DF for total annual tmaxecipitation
tmax_winter_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_winter_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_winter_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  sum of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:(ncol(tmax_360_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_winter_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_winter_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  sum of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:ncol((tmax_365_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_winter_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_winter_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  sum of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:(ncol(tmax_std_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_winter_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_winter_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  sum of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:ncol((tmax_greg_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_winter_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_winter_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}


tmax_winter_26 <- cbind.data.frame(tmax_winter_all[,1], tmax_winter_all[,seq(2,73,3)])
tmax_winter_45 <- cbind.data.frame(tmax_winter_all[,1], tmax_winter_all[,seq(3,73,3)])
names(tmax_winter_26)[1] <- names(tmax_winter_45)[1] <- "year"
tmax_winter_85 <- tmax_winter_all[,seq(1,73,3)]


tmax_winter_26_hist <- colMeans(na.omit(tmax_winter_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_winter_26_nearfut <- colMeans(na.omit(tmax_winter_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_winter_26_farfut <- colMeans(na.omit(tmax_winter_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_winter_45_hist <- colMeans(na.omit(tmax_winter_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_winter_45_nearfut <- colMeans(na.omit(tmax_winter_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_winter_45_farfut <- colMeans(na.omit(tmax_winter_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_winter_85_hist <- colMeans(na.omit(tmax_winter_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_winter_85_nearfut <- colMeans(na.omit(tmax_winter_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_winter_85_farfut <- colMeans(na.omit(tmax_winter_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

#Print table of stats for tech sub
#ooo: for tmax/tmax, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmax (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmax_winter_26_hist,0.1), quantile(tmax_winter_45_hist,0.1), quantile(tmax_winter_85_hist,0.1)),1),
          round(quantile(tmax_winter_26_nearfut,0.1),1), round(quantile(tmax_winter_45_nearfut,0.1),1), round(quantile(tmax_winter_85_nearfut,0.1),1),
          round(quantile(tmax_winter_26_farfut,0.1),1), round(quantile(tmax_winter_45_farfut, 0.1),1), round(quantile(tmax_winter_85_farfut, 0.1),1),
          round(mean(median(tmax_winter_26_hist), median(tmax_winter_45_hist), median(tmax_winter_85_hist)),1),
          round(median(tmax_winter_26_nearfut),1), round(median(tmax_winter_45_nearfut),1), round(median(tmax_winter_85_nearfut),1),
          round(median(tmax_winter_26_farfut),1), round(median(tmax_winter_45_farfut),1), round(median(tmax_winter_85_farfut),1),
          round(mean(quantile(tmax_winter_26_hist,0.9), quantile(tmax_winter_45_hist,0.9), quantile(tmax_winter_85_hist,0.9)),1),
          round(quantile(tmax_winter_26_nearfut,0.9),1), round(quantile(tmax_winter_45_nearfut,0.9),1), round(quantile(tmax_winter_85_nearfut,0.9),1),
          round(quantile(tmax_winter_26_farfut,0.9),1), round(quantile(tmax_winter_45_farfut, 0.9), 1), round(quantile(tmax_winter_85_farfut, 0.9),1))
print("Winter Tmax (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_winter_26_nearfut-tmax_winter_26_hist),0.1),1), round(quantile((tmax_winter_45_nearfut-tmax_winter_45_hist),0.1),1), round(quantile((tmax_winter_85_nearfut-tmax_winter_85_hist),0.1),1),
           round(quantile((tmax_winter_26_farfut-tmax_winter_26_hist),0.1),1), round(quantile((tmax_winter_45_farfut-tmax_winter_45_hist), 0.1, 1)), round(quantile((tmax_winter_85_farfut-tmax_winter_85_hist), 0.1),1),
           round(median(tmax_winter_26_nearfut-tmax_winter_26_hist),1), round(median(tmax_winter_45_nearfut-tmax_winter_45_hist),1), round(median(tmax_winter_85_nearfut-tmax_winter_85_hist),1),
           round(median(tmax_winter_26_farfut-tmax_winter_26_hist),1), round(median(tmax_winter_45_farfut-tmax_winter_45_hist),1), round(median(tmax_winter_85_farfut-tmax_winter_85_hist),1),
           round(quantile((tmax_winter_26_nearfut-tmax_winter_26_hist),0.9),1), round(quantile((tmax_winter_45_nearfut-tmax_winter_45_hist),0.9),1), round(quantile((tmax_winter_85_nearfut-tmax_winter_85_hist),0.9),1),
           round(quantile((tmax_winter_26_farfut-tmax_winter_26_hist),0.9),1), round(quantile((tmax_winter_45_farfut-tmax_winter_45_hist), 0.9), 1), round(quantile((tmax_winter_85_farfut-tmax_winter_85_hist), 0.9),1))
print("Winter Tmax (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#Figures

#RCP 2.6 plot
tmax_winter_26 <- na.omit(tmax_winter_26)
tmax_win_26 <- tmax_winter_26 %>%
  gather(GCM, tmax_win, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_win_26_med <- cbind.data.frame(tmax_winter_26[,1], apply(tmax_winter_26[,2:25],1,median))
names(tmax_win_26_med) <- c("year","Median")

ggplot(tmax_win_26, aes(x=year,y=tmax_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_win_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Winter_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_winter_45 <- na.omit(tmax_winter_45)
tmax_win_45 <- tmax_winter_45 %>%
  gather(GCM, tmax_win, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_win_45_med <- cbind.data.frame(tmax_winter_45$year, apply(tmax_winter_45[,2:25],1,median))
names(tmax_win_45_med) <- c("year","Median")


ggplot(tmax_win_45, aes(x=year,y=tmax_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_win_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5") +
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Winter_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_winter_85 <- na.omit(tmax_winter_85)
tmax_win_85 <- tmax_winter_85 %>%
  gather(GCM, tmax_win, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_win_85_med <- cbind.data.frame(tmax_winter_85$year, apply(tmax_winter_85[,2:25],1,median))
names(tmax_win_85_med) <- c("year","Median")


ggplot(tmax_win_85, aes(x=year,y=tmax_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_win_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Winter_RCP85.tiff", width = 30, height = 18, units="cm")



####################################################
#####################June Tmax####################
####################################################

#Subset of  seasonal daily tmax for given time period (tp)
tmax_360_june <- subset(tmax_360, month ==6) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_june <- subset(tmax_365, month ==6)
tmax_std_june <- subset(tmax_std, month ==6)
tmax_greg_june <- subset(tmax_greg, month ==6)


#Empty DF for total annual tmaxecipitation
tmax_june_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_june_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_june_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_360_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_june_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_june_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_365_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_june_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_june_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_std_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_june_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_june_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  mean of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_greg_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_june_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_june_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmax_june_26 <- cbind.data.frame(tmax_june_all[,1], tmax_june_all[,seq(2,73,3)])
tmax_june_45 <- cbind.data.frame(tmax_june_all[,1], tmax_june_all[,seq(3,73,3)])
names(tmax_june_26)[1] <- names(tmax_june_45)[1] <- "year"
tmax_june_85 <- tmax_june_all[,seq(1,73,3)]


tmax_june_26_hist <- colMeans(na.omit(tmax_june_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_june_26_nearfut <- colMeans(na.omit(tmax_june_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_june_26_farfut <- colMeans(na.omit(tmax_june_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_june_45_hist <- colMeans(na.omit(tmax_june_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_june_45_nearfut <- colMeans(na.omit(tmax_june_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_june_45_farfut <- colMeans(na.omit(tmax_june_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_june_85_hist <- colMeans(na.omit(tmax_june_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_june_85_nearfut <- colMeans(na.omit(tmax_june_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_june_85_farfut <- colMeans(na.omit(tmax_june_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for tmax/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmax (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmax_june_26_hist,0.1), quantile(tmax_june_45_hist,0.1), quantile(tmax_june_85_hist,0.1)),1),
          round(quantile(tmax_june_26_nearfut,0.1),1), round(quantile(tmax_june_45_nearfut,0.1),1), round(quantile(tmax_june_85_nearfut,0.1),1),
          round(quantile(tmax_june_26_farfut,0.1),1), round(quantile(tmax_june_45_farfut, 0.1),1), round(quantile(tmax_june_85_farfut, 0.1),1),
          round(mean(median(tmax_june_26_hist), median(tmax_june_45_hist), median(tmax_june_85_hist)),1),
          round(median(tmax_june_26_nearfut),1), round(median(tmax_june_45_nearfut),1), round(median(tmax_june_85_nearfut),1),
          round(median(tmax_june_26_farfut),1), round(median(tmax_june_45_farfut),1), round(median(tmax_june_85_farfut),1),
          round(mean(quantile(tmax_june_26_hist,0.9), quantile(tmax_june_45_hist,0.9), quantile(tmax_june_85_hist,0.9)),1),
          round(quantile(tmax_june_26_nearfut,0.9),1), round(quantile(tmax_june_45_nearfut,0.9),1), round(quantile(tmax_june_85_nearfut,0.9),1),
          round(quantile(tmax_june_26_farfut,0.9),1), round(quantile(tmax_june_45_farfut, 0.9), 1), round(quantile(tmax_june_85_farfut, 0.9),1))
print("june Tmax (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_june_26_nearfut-tmax_june_26_hist),0.1),1), round(quantile((tmax_june_45_nearfut-tmax_june_45_hist),0.1),1), round(quantile((tmax_june_85_nearfut-tmax_june_85_hist),0.1),1),
           round(quantile((tmax_june_26_farfut-tmax_june_26_hist),0.1),1), round(quantile((tmax_june_45_farfut-tmax_june_45_hist), 0.1, 1)), round(quantile((tmax_june_85_farfut-tmax_june_85_hist), 0.1),1),
           round(median(tmax_june_26_nearfut-tmax_june_26_hist),1), round(median(tmax_june_45_nearfut-tmax_june_45_hist),1), round(median(tmax_june_85_nearfut-tmax_june_85_hist),1),
           round(median(tmax_june_26_farfut-tmax_june_26_hist),1), round(median(tmax_june_45_farfut-tmax_june_45_hist),1), round(median(tmax_june_85_farfut-tmax_june_85_hist),1),
           round(quantile((tmax_june_26_nearfut-tmax_june_26_hist),0.9),1), round(quantile((tmax_june_45_nearfut-tmax_june_45_hist),0.9),1), round(quantile((tmax_june_85_nearfut-tmax_june_85_hist),0.9),1),
           round(quantile((tmax_june_26_farfut-tmax_june_26_hist),0.9),1), round(quantile((tmax_june_45_farfut-tmax_june_45_hist), 0.9), 1), round(quantile((tmax_june_85_farfut-tmax_june_85_hist), 0.9),1))
print("june Tmax (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#RCP 2.6 plot
tmax_june_26 <- na.omit(tmax_june_26)
tmax_jun_26 <- gather(tmax_june_26, GCM, tmax_jun, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_jun_26_med <- cbind.data.frame(tmax_june_26[,1], apply(tmax_june_26[,2:25],1,median))
names(tmax_jun_26_med) <- c("year","Median")


ggplot(tmax_jun_26, aes(x=year,y=tmax_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jun_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_June_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_june_45 <- na.omit(tmax_june_45)
tmax_jun_45 <- gather(tmax_june_45, GCM, tmax_jun, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_jun_45_med <- cbind.data.frame(tmax_june_45$year, apply(tmax_june_45[,2:25],1,median))
names(tmax_jun_45_med) <- c("year","Median")


ggplot(tmax_jun_45, aes(x=year,y=tmax_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jun_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_June_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_june_85 <- na.omit(tmax_june_85)
tmax_jun_85 <- gather(tmax_june_85, GCM, tmax_jun, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_jun_85_med <- cbind.data.frame(tmax_june_85$year, apply(tmax_june_85[,2:25],1,median))
names(tmax_jun_85_med) <- c("year","Median")


ggplot(tmax_jun_85, aes(x=year,y=tmax_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jun_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_June_RCP85.tiff", width = 30, height = 18, units="cm")

####################################################
#####################July Tmax####################
####################################################

#Subset of  seasonal daily tmax for given time period (tp)
tmax_360_july <- subset(tmax_360, month ==7) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_july <- subset(tmax_365, month ==7)
tmax_std_july <- subset(tmax_std, month ==7)
tmax_greg_july <- subset(tmax_greg, month ==7)


#Empty DF for total annual tmaxecipitation
tmax_july_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_july_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_july_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_360_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_july_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_july_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_365_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_july_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_july_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_std_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_july_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_july_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  mean of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_greg_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_july_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_july_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmax_july_26 <- cbind.data.frame(tmax_july_all[,1], tmax_july_all[,seq(2,73,3)])
tmax_july_45 <- cbind.data.frame(tmax_july_all[,1], tmax_july_all[,seq(3,73,3)])
names(tmax_july_26)[1] <- names(tmax_july_45)[1] <- "year"
tmax_july_85 <- tmax_july_all[,seq(1,73,3)]


#abs values
data <- c(round(mean(quantile(tmax_july_26_hist,0.1), quantile(tmax_july_45_hist,0.1), quantile(tmax_july_85_hist,0.1)),1),
          round(quantile(tmax_july_26_nearfut,0.1),1), round(quantile(tmax_july_45_nearfut,0.1),1), round(quantile(tmax_july_85_nearfut,0.1),1),
          round(quantile(tmax_july_26_farfut,0.1),1), round(quantile(tmax_july_45_farfut, 0.1),1), round(quantile(tmax_july_85_farfut, 0.1),1),
          round(mean(median(tmax_july_26_hist), median(tmax_july_45_hist), median(tmax_july_85_hist)),1),
          round(median(tmax_july_26_nearfut),1), round(median(tmax_july_45_nearfut),1), round(median(tmax_july_85_nearfut),1),
          round(median(tmax_july_26_farfut),1), round(median(tmax_july_45_farfut),1), round(median(tmax_july_85_farfut),1),
          round(mean(quantile(tmax_july_26_hist,0.9), quantile(tmax_july_45_hist,0.9), quantile(tmax_july_85_hist,0.9)),1),
          round(quantile(tmax_july_26_nearfut,0.9),1), round(quantile(tmax_july_45_nearfut,0.9),1), round(quantile(tmax_july_85_nearfut,0.9),1),
          round(quantile(tmax_july_26_farfut,0.9),1), round(quantile(tmax_july_45_farfut, 0.9), 1), round(quantile(tmax_july_85_farfut, 0.9),1))
print("july Tmax (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_july_26_nearfut-tmax_july_26_hist),0.1),1), round(quantile((tmax_july_45_nearfut-tmax_july_45_hist),0.1),1), round(quantile((tmax_july_85_nearfut-tmax_july_85_hist),0.1),1),
           round(quantile((tmax_july_26_farfut-tmax_july_26_hist),0.1),1), round(quantile((tmax_july_45_farfut-tmax_july_45_hist), 0.1, 1)), round(quantile((tmax_july_85_farfut-tmax_july_85_hist), 0.1),1),
           round(median(tmax_july_26_nearfut-tmax_july_26_hist),1), round(median(tmax_july_45_nearfut-tmax_july_45_hist),1), round(median(tmax_july_85_nearfut-tmax_july_85_hist),1),
           round(median(tmax_july_26_farfut-tmax_july_26_hist),1), round(median(tmax_july_45_farfut-tmax_july_45_hist),1), round(median(tmax_july_85_farfut-tmax_july_85_hist),1),
           round(quantile((tmax_july_26_nearfut-tmax_july_26_hist),0.9),1), round(quantile((tmax_july_45_nearfut-tmax_july_45_hist),0.9),1), round(quantile((tmax_july_85_nearfut-tmax_july_85_hist),0.9),1),
           round(quantile((tmax_july_26_farfut-tmax_july_26_hist),0.9),1), round(quantile((tmax_july_45_farfut-tmax_july_45_hist), 0.9), 1), round(quantile((tmax_july_85_farfut-tmax_july_85_hist), 0.9),1))
print("july Tmax (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#RCP 2.6 plot
tmax_july_26 <- na.omit(tmax_july_26)
tmax_jul_26 <- gather(tmax_july_26, GCM, tmax_jul, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_jul_26_med <- cbind.data.frame(tmax_july_26[,1], apply(tmax_july_26[,2:25],1,median))
names(tmax_jul_26_med) <- c("year","Median")


ggplot(tmax_jul_26, aes(x=year,y=tmax_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jul_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_July_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_july_45 <- na.omit(tmax_july_45)
tmax_jul_45 <- gather(tmax_july_45, GCM, tmax_jul, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_jul_45_med <- cbind.data.frame(tmax_july_45$year, apply(tmax_july_45[,2:25],1,median))
names(tmax_jul_45_med) <- c("year","Median")


ggplot(tmax_jul_45, aes(x=year,y=tmax_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jul_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_July_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_july_85 <- na.omit(tmax_july_85)
tmax_jul_85 <- gather(tmax_july_85, GCM, tmax_jul, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_jul_85_med <- cbind.data.frame(tmax_july_85$year, apply(tmax_july_85[,2:25],1,median))
names(tmax_jul_85_med) <- c("year","Median")


ggplot(tmax_jul_85, aes(x=year,y=tmax_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_jul_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_July_RCP85.tiff", width = 30, height = 18, units="cm")



###################################
##########Fall Tmax################
###################################

tmax_360_fall <- subset(tmax_360, month==9 | month==10 | month==11) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_fall <- subset(tmax_365, month==9 | month==10 | month==11)
tmax_std_fall <- subset(tmax_std, month==9 | month==10 | month==11)
tmax_greg_fall <- subset(tmax_greg, month==9 | month==10 | month==11)

#Empty DF for total annual tmaxecipitation
tmax_fall_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_fall_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_fall_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_360_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_fall_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_fall_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_365_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_fall_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_fall_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_std_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_fall_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_fall_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  mean of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_greg_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_fall_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_fall_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmax_fall_26 <- cbind.data.frame(tmax_fall_all[,1], tmax_fall_all[,seq(2,73,3)])
tmax_fall_45 <- cbind.data.frame(tmax_fall_all[,1], tmax_fall_all[,seq(3,73,3)])
names(tmax_fall_26)[1] <- names(tmax_fall_45)[1] <- "year"
tmax_fall_85 <- tmax_fall_all[,seq(1,73,3)]


tmax_fall_26_hist <- colMeans(na.omit(tmax_fall_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_fall_26_nearfut <- colMeans(na.omit(tmax_fall_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_fall_26_farfut <- colMeans(na.omit(tmax_fall_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_fall_45_hist <- colMeans(na.omit(tmax_fall_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_fall_45_nearfut <- colMeans(na.omit(tmax_fall_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_fall_45_farfut <- colMeans(na.omit(tmax_fall_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_fall_85_hist <- colMeans(na.omit(tmax_fall_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_fall_85_nearfut <- colMeans(na.omit(tmax_fall_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_fall_85_farfut <- colMeans(na.omit(tmax_fall_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period


#Print table of stats for tech sub
#ooo: for tmax/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmax (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmax_fall_26_hist,0.1), quantile(tmax_fall_45_hist,0.1), quantile(tmax_fall_85_hist,0.1)),1),
          round(quantile(tmax_fall_26_nearfut,0.1),1), round(quantile(tmax_fall_45_nearfut,0.1),1), round(quantile(tmax_fall_85_nearfut,0.1),1),
          round(quantile(tmax_fall_26_farfut,0.1),1), round(quantile(tmax_fall_45_farfut, 0.1),1), round(quantile(tmax_fall_85_farfut, 0.1),1),
          round(mean(median(tmax_fall_26_hist), median(tmax_fall_45_hist), median(tmax_fall_85_hist)),1),
          round(median(tmax_fall_26_nearfut),1), round(median(tmax_fall_45_nearfut),1), round(median(tmax_fall_85_nearfut),1),
          round(median(tmax_fall_26_farfut),1), round(median(tmax_fall_45_farfut),1), round(median(tmax_fall_85_farfut),1),
          round(mean(quantile(tmax_fall_26_hist,0.9), quantile(tmax_fall_45_hist,0.9), quantile(tmax_fall_85_hist,0.9)),1),
          round(quantile(tmax_fall_26_nearfut,0.9),1), round(quantile(tmax_fall_45_nearfut,0.9),1), round(quantile(tmax_fall_85_nearfut,0.9),1),
          round(quantile(tmax_fall_26_farfut,0.9),1), round(quantile(tmax_fall_45_farfut, 0.9), 1), round(quantile(tmax_fall_85_farfut, 0.9),1))
print("Fall Tmax (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_fall_26_nearfut-tmax_fall_26_hist),0.1),1), round(quantile((tmax_fall_45_nearfut-tmax_fall_45_hist),0.1),1), round(quantile((tmax_fall_85_nearfut-tmax_fall_85_hist),0.1),1),
           round(quantile((tmax_fall_26_farfut-tmax_fall_26_hist),0.1),1), round(quantile((tmax_fall_45_farfut-tmax_fall_45_hist), 0.1, 1)), round(quantile((tmax_fall_85_farfut-tmax_fall_85_hist), 0.1),1),
           round(median(tmax_fall_26_nearfut-tmax_fall_26_hist),1), round(median(tmax_fall_45_nearfut-tmax_fall_45_hist),1), round(median(tmax_fall_85_nearfut-tmax_fall_85_hist),1),
           round(median(tmax_fall_26_farfut-tmax_fall_26_hist),1), round(median(tmax_fall_45_farfut-tmax_fall_45_hist),1), round(median(tmax_fall_85_farfut-tmax_fall_85_hist),1),
           round(quantile((tmax_fall_26_nearfut-tmax_fall_26_hist),0.9),1), round(quantile((tmax_fall_45_nearfut-tmax_fall_45_hist),0.9),1), round(quantile((tmax_fall_85_nearfut-tmax_fall_85_hist),0.9),1),
           round(quantile((tmax_fall_26_farfut-tmax_fall_26_hist),0.9),1), round(quantile((tmax_fall_45_farfut-tmax_fall_45_hist), 0.9), 1), round(quantile((tmax_fall_85_farfut-tmax_fall_85_hist), 0.9),1))
print("Fall Tmax (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#Figures

#RCP 2.6 plot
tmax_fall_26 <- na.omit(tmax_fall_26)
tmax_fa_26 <- tmax_fall_26 %>%
  gather(GCM, tmax_fa, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmax_fa_26_med <- cbind.data.frame(tmax_fall_26[,1], apply(tmax_fall_26[,2:25],1,median))
names(tmax_fa_26_med) <- c("year","Median")


ggplot(tmax_fa_26, aes(x=year,y=tmax_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_fa_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Fall_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmax_fall_45 <- na.omit(tmax_fall_45)
tmax_fa_45 <- tmax_fall_45 %>%
  gather(GCM, tmax_fa, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmax_fa_45_med <- cbind.data.frame(tmax_fall_45$year, apply(tmax_fall_45[,2:25],1,median))
names(tmax_fa_45_med) <- c("year","Median")


ggplot(tmax_fa_45, aes(x=year,y=tmax_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_fa_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Fall_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmax_fall_85 <- na.omit(tmax_fall_85)
tmax_fa_85 <- tmax_fall_85 %>%
  gather(GCM, tmax_fa, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmax_fa_85_med <- cbind.data.frame(tmax_fall_85$year, apply(tmax_fall_85[,2:25],1,median))
names(tmax_fa_85_med) <- c("year","Median")


ggplot(tmax_fa_85, aes(x=year,y=tmax_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmax_fa_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Maximum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmax_Fall_RCP85.tiff", width = 30, height = 18, units="cm")




#######################################################
##################Winter Tmin##########################
#######################################################

#Subset of  seasonal daily tmin for given time period (winter)
tmin_360_winter <- subset(tmin_360, month==12 | month==1 | month==2)
tmin_365_winter <- subset(tmin_365, month==12 | month==1 | month==2)
tmin_std_winter <- subset(tmin_std, month==12 | month==1 | month==2)
tmin_greg_winter <- subset(tmin_greg, month==12 | month==1 | month==2)

tmin_360_winter$mthyr <- tmin_360_winter$month*tmin_360_winter$year
tmin_365_winter$mthyr <- tmin_365_winter$month*tmin_365_winter$year
tmin_std_winter$mthyr <- tmin_std_winter$month*tmin_std_winter$year
tmin_greg_winter$mthyr <- tmin_greg_winter$month*tmin_greg_winter$year

#add mth col to df; mth*yr col; in for loop, select for 12*(yr-1), 1*yr, 2*yr==mth*yr

#Empty DF for total annual tminecipitation
tmin_winter_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmin_winter_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmin_winter_all[,1] <- as.data.frame(unique(tmin_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmin_360  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_360_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:(ncol(tmin_360_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_winter_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_winter_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_365  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_365_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:ncol((tmin_365_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_winter_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_winter_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_std  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_std_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:(ncol(tmin_std_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_winter_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_winter_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_greg  sum of daily tminecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_greg_winter, mthyr==12*(yr-1) | mthyr==1*yr | mthyr==2*yr)) #subset of entire DF for year
  for (col in 5:ncol((tmin_greg_winter)-1)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_winter_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_winter_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}


tmin_winter_26 <- cbind.data.frame(tmin_winter_all[,1], tmin_winter_all[,seq(2,73,3)])
tmin_winter_45 <- cbind.data.frame(tmin_winter_all[,1], tmin_winter_all[,seq(3,73,3)])
names(tmin_winter_26)[1] <- names(tmin_winter_45)[1] <- "year"
tmin_winter_85 <- tmin_winter_all[,seq(1,73,3)]


tmin_winter_26_hist <- colMeans(na.omit(tmin_winter_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_winter_26_nearfut <- colMeans(na.omit(tmin_winter_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_winter_26_farfut <- colMeans(na.omit(tmin_winter_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_winter_45_hist <- colMeans(na.omit(tmin_winter_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_winter_45_nearfut <- colMeans(na.omit(tmin_winter_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_winter_45_farfut <- colMeans(na.omit(tmin_winter_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_winter_85_hist <- colMeans(na.omit(tmin_winter_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_winter_85_nearfut <- colMeans(na.omit(tmin_winter_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_winter_85_farfut <- colMeans(na.omit(tmin_winter_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for tmin/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmin (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmin_winter_26_hist,0.1), quantile(tmin_winter_45_hist,0.1), quantile(tmin_winter_85_hist,0.1)),1),
          round(quantile(tmin_winter_26_nearfut,0.1),1), round(quantile(tmin_winter_45_nearfut,0.1),1), round(quantile(tmin_winter_85_nearfut,0.1),1),
          round(quantile(tmin_winter_26_farfut,0.1),1), round(quantile(tmin_winter_45_farfut, 0.1),1), round(quantile(tmin_winter_85_farfut, 0.1),1),
          round(mean(median(tmin_winter_26_hist), median(tmin_winter_45_hist), median(tmin_winter_85_hist)),1),
          round(median(tmin_winter_26_nearfut),1), round(median(tmin_winter_45_nearfut),1), round(median(tmin_winter_85_nearfut),1),
          round(median(tmin_winter_26_farfut),1), round(median(tmin_winter_45_farfut),1), round(median(tmin_winter_85_farfut),1),
          round(mean(quantile(tmin_winter_26_hist,0.9), quantile(tmin_winter_45_hist,0.9), quantile(tmin_winter_85_hist,0.9)),1),
          round(quantile(tmin_winter_26_nearfut,0.9),1), round(quantile(tmin_winter_45_nearfut,0.9),1), round(quantile(tmin_winter_85_nearfut,0.9),1),
          round(quantile(tmin_winter_26_farfut,0.9),1), round(quantile(tmin_winter_45_farfut, 0.9), 1), round(quantile(tmin_winter_85_farfut, 0.9),1))
print("Winter Tmin (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmin_winter_26_nearfut-tmin_winter_26_hist),0.1),1), round(quantile((tmin_winter_45_nearfut-tmin_winter_45_hist),0.1),1), round(quantile((tmin_winter_85_nearfut-tmin_winter_85_hist),0.1),1),
           round(quantile((tmin_winter_26_farfut-tmin_winter_26_hist),0.1),1), round(quantile((tmin_winter_45_farfut-tmin_winter_45_hist), 0.1, 1)), round(quantile((tmin_winter_85_farfut-tmin_winter_85_hist), 0.1),1),
           round(median(tmin_winter_26_nearfut-tmin_winter_26_hist),1), round(median(tmin_winter_45_nearfut-tmin_winter_45_hist),1), round(median(tmin_winter_85_nearfut-tmin_winter_85_hist),1),
           round(median(tmin_winter_26_farfut-tmin_winter_26_hist),1), round(median(tmin_winter_45_farfut-tmin_winter_45_hist),1), round(median(tmin_winter_85_farfut-tmin_winter_85_hist),1),
           round(quantile((tmin_winter_26_nearfut-tmin_winter_26_hist),0.9),1), round(quantile((tmin_winter_45_nearfut-tmin_winter_45_hist),0.9),1), round(quantile((tmin_winter_85_nearfut-tmin_winter_85_hist),0.9),1),
           round(quantile((tmin_winter_26_farfut-tmin_winter_26_hist),0.9),1), round(quantile((tmin_winter_45_farfut-tmin_winter_45_hist), 0.9), 1), round(quantile((tmin_winter_85_farfut-tmin_winter_85_hist), 0.9),1))
print("Winter Tmin (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#Figures

#RCP 2.6 plot
tmin_winter_26 <- na.omit(tmin_winter_26)
tmin_win_26 <- tmin_winter_26 %>%
  gather(GCM, tmin_win, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmin_win_26_med <- cbind.data.frame(tmin_winter_26[,1], apply(tmin_winter_26[,2:25],1,median))
names(tmin_win_26_med) <- c("year","Median")

ggplot(tmin_win_26, aes(x=year,y=tmin_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_win_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Winter_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmin_winter_45 <- na.omit(tmin_winter_45)
tmin_win_45 <- tmin_winter_45 %>%
  gather(GCM, tmin_win, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmin_win_45_med <- cbind.data.frame(tmin_winter_45$year, apply(tmin_winter_45[,2:25],1,median))
names(tmin_win_45_med) <- c("year","Median")


ggplot(tmin_win_45, aes(x=year,y=tmin_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_win_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5") +
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Winter_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmin_winter_85 <- na.omit(tmin_winter_85)
tmin_win_85 <- tmin_winter_85 %>%
  gather(GCM, tmin_win, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmin_win_85_med <- cbind.data.frame(tmin_winter_85$year, apply(tmin_winter_85[,2:25],1,median))
names(tmin_win_85_med) <- c("year","Median")


ggplot(tmin_win_85, aes(x=year,y=tmin_win)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_win_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Winter Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Winter_RCP85.tiff", width = 30, height = 18, units="cm")




##############################################
##############june Tmin#####################
##############################################


#Subset of  seasonal daily tmin for given time period (june)
tmin_360_june <- subset(tmin_360, month==6)
tmin_365_june <- subset(tmin_365, month==6)
tmin_std_june <- subset(tmin_std, month==6)
tmin_greg_june <- subset(tmin_greg, month==6)

#Empty DF for total annual tminecipitation
tmin_june_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmin_june_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmin_june_all[,1] <- as.data.frame(unique(tmin_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmin_360  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_360_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_360_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_june_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_june_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_365  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_365_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_365_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_june_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_june_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_std  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_std_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_std_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_june_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_june_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_greg  sum of daily tminecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_greg_june, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_greg_june)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_june_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_june_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}


tmin_june_26 <- cbind.data.frame(tmin_june_all[,1], tmin_june_all[,seq(2,73,3)])
tmin_june_45 <- cbind.data.frame(tmin_june_all[,1], tmin_june_all[,seq(3,73,3)])
names(tmin_june_26)[1] <- names(tmin_june_45)[1] <- "year"
tmin_june_85 <- tmin_june_all[,seq(1,73,3)]


tmin_june_26_hist <- colMeans(na.omit(tmin_june_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_june_26_nearfut <- colMeans(na.omit(tmin_june_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_june_26_farfut <- colMeans(na.omit(tmin_june_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_june_45_hist <- colMeans(na.omit(tmin_june_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_june_45_nearfut <- colMeans(na.omit(tmin_june_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_june_45_farfut <- colMeans(na.omit(tmin_june_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_june_85_hist <- colMeans(na.omit(tmin_june_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_june_85_nearfut <- colMeans(na.omit(tmin_june_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_june_85_farfut <- colMeans(na.omit(tmin_june_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for tmin/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmin (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmin_june_26_hist,0.1), quantile(tmin_june_45_hist,0.1), quantile(tmin_june_85_hist,0.1)),1),
          round(quantile(tmin_june_26_nearfut,0.1),1), round(quantile(tmin_june_45_nearfut,0.1),1), round(quantile(tmin_june_85_nearfut,0.1),1),
          round(quantile(tmin_june_26_farfut,0.1),1), round(quantile(tmin_june_45_farfut, 0.1),1), round(quantile(tmin_june_85_farfut, 0.1),1),
          round(mean(median(tmin_june_26_hist), median(tmin_june_45_hist), median(tmin_june_85_hist)),1),
          round(median(tmin_june_26_nearfut),1), round(median(tmin_june_45_nearfut),1), round(median(tmin_june_85_nearfut),1),
          round(median(tmin_june_26_farfut),1), round(median(tmin_june_45_farfut),1), round(median(tmin_june_85_farfut),1),
          round(mean(quantile(tmin_june_26_hist,0.9), quantile(tmin_june_45_hist,0.9), quantile(tmin_june_85_hist,0.9)),1),
          round(quantile(tmin_june_26_nearfut,0.9),1), round(quantile(tmin_june_45_nearfut,0.9),1), round(quantile(tmin_june_85_nearfut,0.9),1),
          round(quantile(tmin_june_26_farfut,0.9),1), round(quantile(tmin_june_45_farfut, 0.9), 1), round(quantile(tmin_june_85_farfut, 0.9),1))
print("june Tmin (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmin_june_26_nearfut-tmin_june_26_hist),0.1),1), round(quantile((tmin_june_45_nearfut-tmin_june_45_hist),0.1),1), round(quantile((tmin_june_85_nearfut-tmin_june_85_hist),0.1),1),
           round(quantile((tmin_june_26_farfut-tmin_june_26_hist),0.1),1), round(quantile((tmin_june_45_farfut-tmin_june_45_hist), 0.1, 1)), round(quantile((tmin_june_85_farfut-tmin_june_85_hist), 0.1),1),
           round(median(tmin_june_26_nearfut-tmin_june_26_hist),1), round(median(tmin_june_45_nearfut-tmin_june_45_hist),1), round(median(tmin_june_85_nearfut-tmin_june_85_hist),1),
           round(median(tmin_june_26_farfut-tmin_june_26_hist),1), round(median(tmin_june_45_farfut-tmin_june_45_hist),1), round(median(tmin_june_85_farfut-tmin_june_85_hist),1),
           round(quantile((tmin_june_26_nearfut-tmin_june_26_hist),0.9),1), round(quantile((tmin_june_45_nearfut-tmin_june_45_hist),0.9),1), round(quantile((tmin_june_85_nearfut-tmin_june_85_hist),0.9),1),
           round(quantile((tmin_june_26_farfut-tmin_june_26_hist),0.9),1), round(quantile((tmin_june_45_farfut-tmin_june_45_hist), 0.9), 1), round(quantile((tmin_june_85_farfut-tmin_june_85_hist), 0.9),1))
print("june Tmin (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
tmin_june_26 <- na.omit(tmin_june_26)
tmin_jun_26 <- gather(tmin_june_26, GCM, tmin_jun, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmin_jun_26_med <- cbind.data.frame(tmin_june_26[,1], apply(tmin_june_26[,2:25],1,median))
names(tmin_jun_26_med) <- c("year","Median")


ggplot(tmin_jun_26, aes(x=year,y=tmin_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jun_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_June_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmin_june_45 <- na.omit(tmin_june_45)
tmin_jun_45 <- gather(tmin_june_45, GCM, tmin_jun, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmin_jun_45_med <- cbind.data.frame(tmin_june_45$year, apply(tmin_june_45[,2:25],1,median))
names(tmin_jun_45_med) <- c("year","Median")


ggplot(tmin_jun_45, aes(x=year,y=tmin_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jun_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_June_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmin_june_85 <- na.omit(tmin_june_85)
tmin_jun_85 <- gather(tmin_june_85, GCM, tmin_jun, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmin_jun_85_med <- cbind.data.frame(tmin_june_85$year, apply(tmin_june_85[,2:25],1,median))
names(tmin_jun_85_med) <- c("year","Median")


ggplot(tmin_jun_85, aes(x=year,y=tmin_jun)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jun_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "June Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_June_RCP85.tiff", width = 30, height = 18, units="cm")


##############################################
##############july Tmin#####################
##############################################


#Subset of  seasonal daily tmin for given time period (july)
tmin_360_july <- subset(tmin_360, month==7)
tmin_365_july <- subset(tmin_365, month==7)
tmin_std_july <- subset(tmin_std, month==7)
tmin_greg_july <- subset(tmin_greg, month==7)

#Empty DF for total annual tminecipitation
tmin_july_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmin_july_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmin_july_all[,1] <- as.data.frame(unique(tmin_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmin_360  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_360_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_360_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_july_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_july_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_365  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_365_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_365_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_july_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_july_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_std  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_std_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_std_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_july_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_july_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_greg  sum of daily tminecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_greg_july, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_greg_july)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_july_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_july_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}


tmin_july_26 <- cbind.data.frame(tmin_july_all[,1], tmin_july_all[,seq(2,73,3)])
tmin_july_45 <- cbind.data.frame(tmin_july_all[,1], tmin_july_all[,seq(3,73,3)])
names(tmin_july_26)[1] <- names(tmin_july_45)[1] <- "year"
tmin_july_85 <- tmin_july_all[,seq(1,73,3)]


tmin_july_26_hist <- colMeans(na.omit(tmin_july_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_july_26_nearfut <- colMeans(na.omit(tmin_july_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_july_26_farfut <- colMeans(na.omit(tmin_july_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_july_45_hist <- colMeans(na.omit(tmin_july_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_july_45_nearfut <- colMeans(na.omit(tmin_july_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_july_45_farfut <- colMeans(na.omit(tmin_july_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_july_85_hist <- colMeans(na.omit(tmin_july_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_july_85_nearfut <- colMeans(na.omit(tmin_july_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_july_85_farfut <- colMeans(na.omit(tmin_july_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for tmin/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmin (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmin_july_26_hist,0.1), quantile(tmin_july_45_hist,0.1), quantile(tmin_july_85_hist,0.1)),1),
          round(quantile(tmin_july_26_nearfut,0.1),1), round(quantile(tmin_july_45_nearfut,0.1),1), round(quantile(tmin_july_85_nearfut,0.1),1),
          round(quantile(tmin_july_26_farfut,0.1),1), round(quantile(tmin_july_45_farfut, 0.1),1), round(quantile(tmin_july_85_farfut, 0.1),1),
          round(mean(median(tmin_july_26_hist), median(tmin_july_45_hist), median(tmin_july_85_hist)),1),
          round(median(tmin_july_26_nearfut),1), round(median(tmin_july_45_nearfut),1), round(median(tmin_july_85_nearfut),1),
          round(median(tmin_july_26_farfut),1), round(median(tmin_july_45_farfut),1), round(median(tmin_july_85_farfut),1),
          round(mean(quantile(tmin_july_26_hist,0.9), quantile(tmin_july_45_hist,0.9), quantile(tmin_july_85_hist,0.9)),1),
          round(quantile(tmin_july_26_nearfut,0.9),1), round(quantile(tmin_july_45_nearfut,0.9),1), round(quantile(tmin_july_85_nearfut,0.9),1),
          round(quantile(tmin_july_26_farfut,0.9),1), round(quantile(tmin_july_45_farfut, 0.9), 1), round(quantile(tmin_july_85_farfut, 0.9),1))
print("july Tmin (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmin_july_26_nearfut-tmin_july_26_hist),0.1),1), round(quantile((tmin_july_45_nearfut-tmin_july_45_hist),0.1),1), round(quantile((tmin_july_85_nearfut-tmin_july_85_hist),0.1),1),
           round(quantile((tmin_july_26_farfut-tmin_july_26_hist),0.1),1), round(quantile((tmin_july_45_farfut-tmin_july_45_hist), 0.1, 1)), round(quantile((tmin_july_85_farfut-tmin_july_85_hist), 0.1),1),
           round(median(tmin_july_26_nearfut-tmin_july_26_hist),1), round(median(tmin_july_45_nearfut-tmin_july_45_hist),1), round(median(tmin_july_85_nearfut-tmin_july_85_hist),1),
           round(median(tmin_july_26_farfut-tmin_july_26_hist),1), round(median(tmin_july_45_farfut-tmin_july_45_hist),1), round(median(tmin_july_85_farfut-tmin_july_85_hist),1),
           round(quantile((tmin_july_26_nearfut-tmin_july_26_hist),0.9),1), round(quantile((tmin_july_45_nearfut-tmin_july_45_hist),0.9),1), round(quantile((tmin_july_85_nearfut-tmin_july_85_hist),0.9),1),
           round(quantile((tmin_july_26_farfut-tmin_july_26_hist),0.9),1), round(quantile((tmin_july_45_farfut-tmin_july_45_hist), 0.9), 1), round(quantile((tmin_july_85_farfut-tmin_july_85_hist), 0.9),1))
print("july Tmin (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
tmin_july_26 <- na.omit(tmin_july_26)
tmin_jul_26 <- gather(tmin_july_26, GCM, tmin_jul, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmin_jul_26_med <- cbind.data.frame(tmin_july_26[,1], apply(tmin_july_26[,2:25],1,median))
names(tmin_jul_26_med) <- c("year","Median")


ggplot(tmin_jul_26, aes(x=year,y=tmin_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jul_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_July_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmin_july_45 <- na.omit(tmin_july_45)
tmin_jul_45 <- gather(tmin_july_45, GCM, tmin_jul, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmin_jul_45_med <- cbind.data.frame(tmin_july_45$year, apply(tmin_july_45[,2:25],1,median))
names(tmin_jul_45_med) <- c("year","Median")


ggplot(tmin_jul_45, aes(x=year,y=tmin_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jul_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_July_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmin_july_85 <- na.omit(tmin_july_85)
tmin_jul_85 <- gather(tmin_july_85, GCM, tmin_jul, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmin_jul_85_med <- cbind.data.frame(tmin_july_85$year, apply(tmin_july_85[,2:25],1,median))
names(tmin_jul_85_med) <- c("year","Median")


ggplot(tmin_jul_85, aes(x=year,y=tmin_jul)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_jul_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "July Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_July_RCP85.tiff", width = 30, height = 18, units="cm")



##############################################
################Fall Tmin#####################
##############################################

#Subset of  seasonal daily tmin for given time period (tp)
tmin_360_fall <- subset(tmin_360, month==9 | month==10 | month==11)
tmin_365_fall <- subset(tmin_365, month==9 | month==10 | month==11)
tmin_std_fall <- subset(tmin_std, month==9 | month==10 | month==11)
tmin_greg_fall <- subset(tmin_greg, month==9 | month==10 | month==11)

#Empty DF for total annual tminecipitation
tmin_fall_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmin_fall_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
tmin_fall_all[,1] <- as.data.frame(unique(tmin_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmin_360  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_360_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_360_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_fall_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_fall_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_365  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_365_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_365_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_fall_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_fall_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_std  sum of daily tminecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_std_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_std_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_fall_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_fall_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmin_greg  sum of daily tminecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tminevious data
  sub <- as.data.frame(subset(tmin_greg_fall, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmin_greg_fall)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmin_fall_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmin_fall_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}


tmin_fall_26 <- cbind.data.frame(tmin_fall_all[,1], tmin_fall_all[,seq(2,73,3)])
tmin_fall_45 <- cbind.data.frame(tmin_fall_all[,1], tmin_fall_all[,seq(3,73,3)])
names(tmin_fall_26)[1] <- names(tmin_fall_45)[1] <- "year"
tmin_fall_85 <- tmin_fall_all[,seq(1,73,3)]


tmin_fall_26_hist <- colMeans(na.omit(tmin_fall_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_fall_26_nearfut <- colMeans(na.omit(tmin_fall_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_fall_26_farfut <- colMeans(na.omit(tmin_fall_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_fall_45_hist <- colMeans(na.omit(tmin_fall_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_fall_45_nearfut <- colMeans(na.omit(tmin_fall_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_fall_45_farfut <- colMeans(na.omit(tmin_fall_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmin_fall_85_hist <- colMeans(na.omit(tmin_fall_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmin_fall_85_nearfut <- colMeans(na.omit(tmin_fall_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmin_fall_85_farfut <- colMeans(na.omit(tmin_fall_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period




#Print table of stats for tech sub
#ooo: for tmin/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmin (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmin_fall_26_hist,0.1), quantile(tmin_fall_45_hist,0.1), quantile(tmin_fall_85_hist,0.1)),1),
          round(quantile(tmin_fall_26_nearfut,0.1),1), round(quantile(tmin_fall_45_nearfut,0.1),1), round(quantile(tmin_fall_85_nearfut,0.1),1),
          round(quantile(tmin_fall_26_farfut,0.1),1), round(quantile(tmin_fall_45_farfut, 0.1),1), round(quantile(tmin_fall_85_farfut, 0.1),1),
          round(mean(median(tmin_fall_26_hist), median(tmin_fall_45_hist), median(tmin_fall_85_hist)),1),
          round(median(tmin_fall_26_nearfut),1), round(median(tmin_fall_45_nearfut),1), round(median(tmin_fall_85_nearfut),1),
          round(median(tmin_fall_26_farfut),1), round(median(tmin_fall_45_farfut),1), round(median(tmin_fall_85_farfut),1),
          round(mean(quantile(tmin_fall_26_hist,0.9), quantile(tmin_fall_45_hist,0.9), quantile(tmin_fall_85_hist,0.9)),1),
          round(quantile(tmin_fall_26_nearfut,0.9),1), round(quantile(tmin_fall_45_nearfut,0.9),1), round(quantile(tmin_fall_85_nearfut,0.9),1),
          round(quantile(tmin_fall_26_farfut,0.9),1), round(quantile(tmin_fall_45_farfut, 0.9), 1), round(quantile(tmin_fall_85_farfut, 0.9),1))
print("Fall Tmin (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmin_fall_26_nearfut-tmin_fall_26_hist),0.1),1), round(quantile((tmin_fall_45_nearfut-tmin_fall_45_hist),0.1),1), round(quantile((tmin_fall_85_nearfut-tmin_fall_85_hist),0.1),1),
           round(quantile((tmin_fall_26_farfut-tmin_fall_26_hist),0.1),1), round(quantile((tmin_fall_45_farfut-tmin_fall_45_hist), 0.1, 1)), round(quantile((tmin_fall_85_farfut-tmin_fall_85_hist), 0.1),1),
           round(median(tmin_fall_26_nearfut-tmin_fall_26_hist),1), round(median(tmin_fall_45_nearfut-tmin_fall_45_hist),1), round(median(tmin_fall_85_nearfut-tmin_fall_85_hist),1),
           round(median(tmin_fall_26_farfut-tmin_fall_26_hist),1), round(median(tmin_fall_45_farfut-tmin_fall_45_hist),1), round(median(tmin_fall_85_farfut-tmin_fall_85_hist),1),
           round(quantile((tmin_fall_26_nearfut-tmin_fall_26_hist),0.9),1), round(quantile((tmin_fall_45_nearfut-tmin_fall_45_hist),0.9),1), round(quantile((tmin_fall_85_nearfut-tmin_fall_85_hist),0.9),1),
           round(quantile((tmin_fall_26_farfut-tmin_fall_26_hist),0.9),1), round(quantile((tmin_fall_45_farfut-tmin_fall_45_hist), 0.9), 1), round(quantile((tmin_fall_85_farfut-tmin_fall_85_hist), 0.9),1))
print("Fall Tmin (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))


#Figures

#RCP 2.6 plot
tmin_fall_26 <- na.omit(tmin_fall_26)
tmin_fa_26 <- tmin_fall_26 %>%
  gather(GCM, tmin_fa, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmin_fa_26_med <- cbind.data.frame(tmin_fall_26[,1], apply(tmin_fall_26[,2:25],1,median))
names(tmin_fa_26_med) <- c("year","Median")


ggplot(tmin_fa_26, aes(x=year,y=tmin_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_fa_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Fall_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmin_fall_45 <- na.omit(tmin_fall_45)
tmin_fa_45 <- tmin_fall_45 %>%
  gather(GCM, tmin_fa, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmin_fa_45_med <- cbind.data.frame(tmin_fall_45$year, apply(tmin_fall_45[,2:25],1,median))
names(tmin_fa_45_med) <- c("year","Median")


ggplot(tmin_fa_45, aes(x=year,y=tmin_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_fa_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Fall_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmin_fall_85 <- na.omit(tmin_fall_85)
tmin_fa_85 <- tmin_fall_85 %>%
  gather(GCM, tmin_fa, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmin_fa_85_med <- cbind.data.frame(tmin_fall_85$year, apply(tmin_fall_85[,2:25],1,median))
names(tmin_fa_85_med) <- c("year","Median")


ggplot(tmin_fa_85, aes(x=year,y=tmin_fa)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmin_fa_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fall Minimum Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Tmin_Fall_RCP85.tiff", width = 30, height = 18, units="cm")



################################################
############NUMBER OF VERY HOT (+30)#######
################################################


#Subset of  seasonal daily tmax for given time period (tp)
tmax_360_tp <- subset(tmax_360, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmax_365_tp <- subset(tmax_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_std_tp <- subset(tmax_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmax_greg_tp <- subset(tmax_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)



#Empty DF for total annual tmaxecipitation
tmax_tp_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmax_tp_all) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
tmax_tp_all[,1] <- as.data.frame(unique(tmax_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmax_360  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_360_tp, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_360_tp)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_tp_all[j, (col-3)] <- length(which(col_yr>=30))
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_tp_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_365  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_365_tp, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_365_tp)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_tp_all[j, (col+3)] <- length(which(col_yr>=30))
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_tp_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_std  mean of daily tmaxecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_std_tp, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_std_tp)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_tp_all[j, (col+51)] <- length(which(col_yr>=30))
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_tp_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmax_greg  mean of daily tmaxecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmaxevious data
  sub <- as.data.frame(subset(tmax_greg_tp, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmax_greg_tp)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmax_tp_all[j, (col+63)] <- length(which(col_yr>=30))
    } else if (length(which(is.na(col_yr)))>0) {
      tmax_tp_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmax_tp_26 <- cbind.data.frame(tmax_tp_all[,1], tmax_tp_all[,seq(2,73,3)])
tmax_tp_45 <- cbind.data.frame(tmax_tp_all[,1], tmax_tp_all[,seq(3,73,3)])
names(tmax_tp_26)[1] <- names(tmax_tp_45)[1] <- "year"
tmax_tp_85 <- tmax_tp_all[,seq(1,73,3)]


tmax_tp_26_hist <- colMeans(na.omit(tmax_tp_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_tp_26_nearfut <- colMeans(na.omit(tmax_tp_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_tp_26_farfut <- colMeans(na.omit(tmax_tp_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_tp_45_hist <- colMeans(na.omit(tmax_tp_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_tp_45_nearfut <- colMeans(na.omit(tmax_tp_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_tp_45_farfut <- colMeans(na.omit(tmax_tp_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmax_tp_85_hist <- colMeans(na.omit(tmax_tp_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmax_tp_85_nearfut <- colMeans(na.omit(tmax_tp_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmax_tp_85_farfut <- colMeans(na.omit(tmax_tp_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

#abs values
data <- c(round(mean(quantile(tmax_tp_26_hist,0.1), quantile(tmax_tp_45_hist,0.1), quantile(tmax_tp_85_hist,0.1)),1),
          round(quantile(tmax_tp_26_nearfut,0.1),1), round(quantile(tmax_tp_45_nearfut,0.1),1), round(quantile(tmax_tp_85_nearfut,0.1),1),
          round(quantile(tmax_tp_26_farfut,0.1),1), round(quantile(tmax_tp_45_farfut, 0.1),1), round(quantile(tmax_tp_85_farfut, 0.1),1),
          round(mean(median(tmax_tp_26_hist), median(tmax_tp_45_hist), median(tmax_tp_85_hist)),1),
          round(median(tmax_tp_26_nearfut),1), round(median(tmax_tp_45_nearfut),1), round(median(tmax_tp_85_nearfut),1),
          round(median(tmax_tp_26_farfut),1), round(median(tmax_tp_45_farfut),1), round(median(tmax_tp_85_farfut),1),
          round(mean(quantile(tmax_tp_26_hist,0.9), quantile(tmax_tp_45_hist,0.9), quantile(tmax_tp_85_hist,0.9)),1),
          round(quantile(tmax_tp_26_nearfut,0.9),1), round(quantile(tmax_tp_45_nearfut,0.9),1), round(quantile(tmax_tp_85_nearfut,0.9),1),
          round(quantile(tmax_tp_26_farfut,0.9),1), round(quantile(tmax_tp_45_farfut, 0.9), 1), round(quantile(tmax_tp_85_farfut, 0.9),1))
print("Number of +30 Days (absolute)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmax_tp_26_nearfut-tmax_tp_26_hist),0.1),1), round(quantile((tmax_tp_45_nearfut-tmax_tp_45_hist),0.1),1), round(quantile((tmax_tp_85_nearfut-tmax_tp_85_hist),0.1),1),
           round(quantile((tmax_tp_26_farfut-tmax_tp_26_hist),0.1),1), round(quantile((tmax_tp_45_farfut-tmax_tp_45_hist), 0.1, 1)), round(quantile((tmax_tp_85_farfut-tmax_tp_85_hist), 0.1),1),
           round(median(tmax_tp_26_nearfut-tmax_tp_26_hist),1), round(median(tmax_tp_45_nearfut-tmax_tp_45_hist),1), round(median(tmax_tp_85_nearfut-tmax_tp_85_hist),1),
           round(median(tmax_tp_26_farfut-tmax_tp_26_hist),1), round(median(tmax_tp_45_farfut-tmax_tp_45_hist),1), round(median(tmax_tp_85_farfut-tmax_tp_85_hist),1),
           round(quantile((tmax_tp_26_nearfut-tmax_tp_26_hist),0.9),1), round(quantile((tmax_tp_45_nearfut-tmax_tp_45_hist),0.9),1), round(quantile((tmax_tp_85_nearfut-tmax_tp_85_hist),0.9),1),
           round(quantile((tmax_tp_26_farfut-tmax_tp_26_hist),0.9),1), round(quantile((tmax_tp_45_farfut-tmax_tp_45_hist), 0.9), 1), round(quantile((tmax_tp_85_farfut-tmax_tp_85_hist), 0.9),1))
print("Number of +30 Days (change)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))




################################################
####################SPEI########################
################################################

library(SPEI)

pet <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(pet) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
pet[,1]<- as.data.frame(unique(tmax_greg$year))

bal <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(bal) <- c("year",names(tmax_360[,5:10]), names(tmax_365[,5:52]), names(tmax_std[,5:16]),names(tmax_greg[,5:10]))
bal[,1]<- as.data.frame(unique(tmax_greg$year))

#fill values
tmin_360[is.na(tmin_360)]<- -9999
tmax_360[is.na(tmax_360)]<- -9999
pr_360[is.na(pr_360)]<- -9999

#360 calendar

for (col in 5:ncol(tmax_360)) {
  tmin <- tmax <- precip <- df <- NA
  tmin <- aggregate(tmin_360[,col] ~ code, tmin_360, mean) #for each model aggregate mean monthly (sum for precip) values (DF: mm-yy - tmin)
  tmax <- aggregate(tmax_360[,col] ~ code, tmax_360, mean)
  precip <- aggregate(pr_360[,col] ~ code, pr_360, sum)
  df <- cbind.data.frame(tmin[,1], round(tmin[,1]/100,0), tmin[,2], tmax[,2], precip[,2]) #combine code (yyyymm), year, tmin, tmax, precip
  df[df<=-9999] <- NA #remove fill val
  for (yr in 1950:2100) {
    j <- yr - 1949 #index
    df_code <- subset(df, df[,2]==yr) #subset for each year (to enable if loop to skip years w/ NAs)
    if (length(which(is.na(df_code)))==0) {
      df_code$pet <- hargreaves(Tmin=df_code[,3], Tmax=df_code[,4], Ra=NA, lat=58.7033, Pre=df_code[,5]) #Hargreaves calc for PET, uses SPEI pkg
      df_code$bal <- df_code[,5] - df_code[,6] #pr-pet= water bal (monthly basis)
      pet[j, col-3] <- sum(df_code[,6]) #summed for each year
      bal[j,col-3] <- sum(df_code[,7])
    }
    else if (length(which(is.na(df_code)))>0) {
      # pet_code[index,col-3] <- NA
      pet[j,col-3] <- NA
      bal[j,col-3] <- NA
    } 
  }
}

#365 calendar

for (col in 5:ncol(tmax_365)) {
  tmin <- tmax <- precip <- df <- NA
  tmin <- aggregate(tmin_365[,col] ~ code, tmin_365, mean)
  tmax <- aggregate(tmax_365[,col] ~ code, tmax_365, mean)
  precip <- aggregate(pr_365[,col] ~ code, pr_365, sum)
  df <- cbind.data.frame(tmin[,1], round(tmin[,1]/100,0), tmin[,2], tmax[,2], precip[,2])
  df[df<=-9999] <- NA
  for (yr in 1950:2100) {
    j <- yr - 1949
    df_code <- subset(df, df[,2]==yr)
    if (length(which(is.na(df_code)))==0) {
      df_code$pet <- hargreaves(Tmin=df_code[,3], Tmax=df_code[,4], Ra=NA, lat=58.7033, Pre=df_code[,5])
      df_code$bal <- df_code[,5] - df_code[,6] #pr-pet
      pet[j, col+3] <- sum(df_code[,6])
      bal[j,col+3] <- sum(df_code[,7])
    }
    else if (length(which(is.na(df_code)))>0) {
      pet[j,col+3] <- NA
      bal[j,col+3] <- NA
    } 
  }
}

#std calendar

for (col in 5:ncol(tmax_std)) {
  tmin <- tmax <- precip <- df <- NA
  tmin <- aggregate(tmin_std[,col] ~ code, tmin_std, mean)
  tmax <- aggregate(tmax_std[,col] ~ code, tmax_std, mean)
  precip <- aggregate(pr_std[,col] ~ code, pr_std, sum)
  df <- cbind.data.frame(tmin[,1], round(tmin[,1]/100,0), tmin[,2], tmax[,2], precip[,2])
  df[df<=-9999] <- NA
  for (yr in 1950:2100) {
    j <- yr - 1949
    df_code <- subset(df, df[,2]==yr)
    if (length(which(is.na(df_code)))==0) {
      df_code$pet <- hargreaves(Tmin=df_code[,3], Tmax=df_code[,4], Ra=NA, lat=58.7033, Pre=df_code[,5])
      df_code$bal <- df_code[,5] - df_code[,6] #pr-pet
      pet[j, col+51] <- sum(df_code[,6])
      bal[j,col+51] <- sum(df_code[,7])
    }
    else if (length(which(is.na(df_code)))>0) {
      pet[j,col+51] <- NA
      bal[j,col+51] <- NA
    } 
  }
}

#greg calendar

for (col in 5:ncol(tmax_greg)) {
  tmin <- tmax <- precip <- df <- NA
  tmin <- aggregate(tmin_greg[,col] ~ code, tmin_greg, mean)
  tmax <- aggregate(tmax_greg[,col] ~ code, tmax_greg, mean)
  precip <- aggregate(pr_greg[,col] ~ code, pr_greg, sum)
  df <- cbind.data.frame(tmin[,1], round(tmin[,1]/100,0), tmin[,2], tmax[,2], precip[,2])
  df[df<=-9999] <- NA
  for (yr in 1950:2100) {
    j <- yr - 1949
    df_code <- subset(df, df[,2]==yr)
    if (length(which(is.na(df_code)))==0) {
      df_code$pet <- hargreaves(Tmin=df_code[,3], Tmax=df_code[,4], Ra=NA, lat=58.7033, Pre=df_code[,5])
      df_code$bal <- df_code[,5] - df_code[,6] #pr-pet
      pet[j, col+63] <- sum(df_code[,6])
      bal[j,col+63] <- sum(df_code[,7])
    }
    else if (length(which(is.na(df_code)))>0) {
      pet[j,col+63] <- NA
      bal[j,col+63] <- NA
    } 
  }
}


#PET fig

pet_26 <- cbind.data.frame(pet[,1], pet[,seq(2,73,3)])
pet_45 <- cbind.data.frame(pet[,1], pet[,seq(3,73,3)])
names(pet_26)[1] <- names(pet_45)[1] <- "year"
pet_85 <- cbind.data.frame(pet[,1], pet[,seq(1,73,3)])


#RCP 2.6 plot
pet_26 <- na.omit(pet_26)
pet_harg_26 <- gather(pet_26, GCM, pet_harg, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                      bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                      gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                      noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

pet_harg_26_med <- cbind.data.frame(pet_26[,1], apply(pet_26[,2:25],1,median))
names(pet_harg_26_med) <- c("year","Median")


ggplot(pet_harg_26, aes(x=year,y=pet_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pet_harg_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Potential Evapotranspiration (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("PET_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
pet_45 <- na.omit(pet_45)
pet_harg_45 <- gather(pet_45, GCM, pet_harg, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                      bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                      gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                      noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

pet_harg_45_med <- cbind.data.frame(pet_45$year, apply(pet_45[,2:25],1,median))
names(pet_harg_45_med) <- c("year","Median")


ggplot(pet_harg_45, aes(x=year,y=pet_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pet_harg_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Potential Evapotranspiration (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("PET_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
pet_85 <- na.omit(pet_85)
pet_harg_85 <- gather(pet_85, GCM, pet_harg, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                      bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                      gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                      noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

pet_harg_85_med <- cbind.data.frame(pet_85$year, apply(pet_85[,2:25],1,median))
names(pet_harg_85_med) <- c("year","Median")


ggplot(pet_harg_85, aes(x=year,y=pet_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = pet_harg_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Potential Evapotranspiration (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("PET_RCP85.tiff", width = 30, height = 18, units="cm")

#Balance = PR-PET

bal_26 <- cbind.data.frame(bal[,1], bal[,seq(2,73,3)])
bal_45 <- cbind.data.frame(bal[,1], bal[,seq(3,73,3)])
names(bal_26)[1] <- names(bal_45)[1] <- "year"
bal_85 <- cbind.data.frame(bal[,1], bal[,seq(1,73,3)])


#RCP 2.6 plot
bal_26 <- na.omit(bal_26)
bal_harg_26 <- gather(bal_26, GCM, bal_harg, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                      bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                      gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                      noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

bal_harg_26_med <- cbind.data.frame(bal_26[,1], apply(bal_26[,2:25],1,median))
names(bal_harg_26_med) <- c("year","Median")


ggplot(bal_harg_26, aes(x=year,y=bal_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = bal_harg_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Water Balance = Precip - PET (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("AnnualWaterBalance_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
bal_45 <- na.omit(bal_45)
bal_harg_45 <- gather(bal_45, GCM, bal_harg, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                      bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                      gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                      noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

bal_harg_45_med <- cbind.data.frame(bal_45$year, apply(bal_45[,2:25],1,median))
names(bal_harg_45_med) <- c("year","Median")


ggplot(bal_harg_45, aes(x=year,y=bal_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = bal_harg_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Water Balance = Precip - PET (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("AnnualWaterBalance_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
bal_85 <- na.omit(bal_85)
bal_harg_85 <- gather(bal_85, GCM, bal_harg, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                      bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                      gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                      noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

bal_harg_85_med <- cbind.data.frame(bal_85$year, apply(bal_85[,2:25],1,median))
names(bal_harg_85_med) <- c("year","Median")


ggplot(bal_harg_85, aes(x=year,y=bal_harg)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = bal_harg_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Water Balance = Precip - PET (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("AnnualWaterBalance_RCP85.tiff", width = 30, height = 18, units="cm")

###########################################
#####SNOW##################################
###########################################

library(EcoHydRology)

#Shrestha et al., 2020: snowmeltrate = Mean SMRs were calculated from the differences in sequential
#daily SWE values, with rates <=1 mm/day excluded from the calculation (Musselman et al., 2017), which were also averaged over
#31-year periods.

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

#empty DFs
smr_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(smr_all) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
smr_all[,1] <- as.data.frame(unique(tmin_greg$year))
smr_all <- smr_all[-c(1, 151),]


scd <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(scd) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
scd[,1] <- as.data.frame(unique(tmin_greg$year))
scd <- scd[-c(1, 151),]


max_swe <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(max_swe) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
max_swe[,1] <- as.data.frame(unique(tmin_greg$year))
max_swe <- max_swe[-c(1, 151),]

#df formatting to accept mm-dd
datemaxswe <- sm_stdate <- sm_enddate <- data.frame(year = integer(),
                 hadgem2ao_26 = as.Date(character()),
                 hadgem2ao_45 = as.Date(character()),
                 hadgem2ao_85 = as.Date(character()),
                 hadgem2es_26 = as.Date(character()),
                 hadgem2es_45 = as.Date(character()),
                 hadgem2es_85 = as.Date(character()),
                 bcccsm11_26 = as.Date(character()),
                 bcccsm11_45 = as.Date(character()),
                 bcccsm11_85 = as.Date(character()),
                 bcccsm11m_26 = as.Date(character()),
                 bcccsm11m_45 = as.Date(character()),
                 bcccsm11m_85 = as.Date(character()),
                 bnuesm_26 = as.Date(character()),
                 bnuesm_45 = as.Date(character()),
                 bnuesm_85 = as.Date(character()),
                 canesm2_26 = as.Date(character()),
                 canesm2_45 = as.Date(character()),
                 canesm2_85 = as.Date(character()),
                 ccsm4_26 = as.Date(character()),
                 ccsm4_45 = as.Date(character()),
                 ccsm4_85 = as.Date(character()),
                 cesm1cam5_26 = as.Date(character()),
                 cesm1cam5_45 = as.Date(character()),
                 cesm1cam5_85 = as.Date(character()),
                 csiromk360_26 = as.Date(character()),
                 csiromk360_45 = as.Date(character()),
                 csiromk360_85 = as.Date(character()),
                 fgoalsg2_26 = as.Date(character()),
                 fgoalsg2_45 = as.Date(character()),
                 fgoalsg2_85 = as.Date(character()),
                 gfdlcm3_26 = as.Date(character()),
                 gfdlcm3_45 = as.Date(character()),
                 gfdlcm3_85 = as.Date(character()),
                 gfdlesm2g_26 = as.Date(character()),
                 gfdlesm2g_45 = as.Date(character()),
                 gfdlesm2g_85 = as.Date(character()),
                 gfdlesm2m_26 = as.Date(character()),
                 gfdlesm2m_45 = as.Date(character()),
                 gfdlesm2m_85 = as.Date(character()),
                 ipslcm5alr_26 = as.Date(character()),
                 ipslcm5alr_45 = as.Date(character()),
                 ipslcm5alr_85 = as.Date(character()),
                 ipslcm5amr_26 = as.Date(character()),
                 ipslcm5amr_45 = as.Date(character()),
                 ipslcm5amr_85 = as.Date(character()),
                 miroc5_26 = as.Date(character()),
                 miroc5_45 = as.Date(character()),
                 miroc5_85 = as.Date(character()),
                 noresm1m_26 = as.Date(character()),
                 noresm1m_45 = as.Date(character()),
                 noresm1m_85 = as.Date(character()),
                 noresm1me_26 = as.Date(character()),
                 noresm1me_45 = as.Date(character()),
                 noresm1me_85 = as.Date(character()),
                 cnrmcm5_26 = as.Date(character()),
                 cnrmcm5_45 = as.Date(character()),
                 cnrmcm5_85 = as.Date(character()),
                 mirocesm_26 = as.Date(character()),
                 mirocesm_45 = as.Date(character()),
                 mirocesm_85 = as.Date(character()),
                 mirocesmchem_26 = as.Date(character()),
                 mirocesmchem_45 = as.Date(character()),
                 mirocesmchem_85 = as.Date(character()),
                 mricgcm3_26 = as.Date(character()),
                 mricgcm3_45 = as.Date(character()),
                 mricgcm3_85 = as.Date(character()),
                 mpiesmlr_26 = as.Date(character()),
                 mpiesmlr_45 = as.Date(character()),
                 mpiesmlr_85 = as.Date(character()),
                 mpiesmmr_26 = as.Date(character()),
                 mpiesmmr_45 = as.Date(character()),
                 mpiesmmr_85 = as.Date(character()))


sm_vol <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(sm_vol) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
sm_vol[,1] <- as.data.frame(unique(tmin_greg$year))
sm_vol <- sm_vol[-c(1, 151),]


frac_snow <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(frac_snow) <- c("year",names(tmin_360[,5:10]), names(tmin_365[,5:52]), names(tmin_std[,5:16]),names(tmin_greg[,5:10]))
frac_snow[,1] <- as.data.frame(unique(tmin_greg$year))
frac_snow <- frac_snow[-c(1, 151),]

library(lubridate)


#Add tmin_360  mean of daily  values to DF

for (col in 5:ncol(tmin_360)) {
  input <- sm <- sm_yr <- smr <- date_index <- sm_stdate_index <- sm_enddate_index <- NA #void tminevious data
  input <- cbind.data.frame(threesixtytime, pr_360[,col], tmax_360[,col], tmin_360[,col])
  input$tmin_adj <- ifelse((input[,3]<input[,4]), input[,3], input[,4]) #tmax, tmin
  input[,4] <- NULL
  input$wtryr <- wtr_yr(input[,1], 10)
  names(input) <- c("date","pr","tmax","tmin","wtryr")
  for (yr in 1951:2099) {
    j <- yr-1950 # row index
    input2 <- subset(input, wtryr==yr)
    datemaxswe[j,1] <- sm_stdate[j,1] <- sm_enddate[j,1] <- yr
    if (length(which(is.na(input2)))==0) {
      sm<-SnowMelt(Date=input2$date, precip_mm=input2$pr, Tmax_C=input2$tmax, Tmin_C=input2$tmin, lat_deg=58.7033, slope = 0,
                   aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                   SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                   startingSnowDensity_kg_m3=450)
      smr <- subset(sm, SnowMelt_mm>=1)
      smr_all[j,col-3] <- mean(smr[,7])
      scd[j,col-3] <- length(which(sm$SnowDepth_m>0)) #number of days with snow on ground (depth > 0)
      max_swe[j,col-3] <- max(sm$SnowWaterEq_mm)
      date_index<-which(sm$SnowWaterEq_mm==max(sm$SnowWaterEq_mm))
      dtmxswe <- sm[date_index[1],1] #in case of consecutive days choose first occurrence of max swe
      year(dtmxswe) <- ifelse(month(dtmxswe)<10, 1952, 1951) #Oct-Dec, 1951; Jan-Sept, 1952 for all water years (mm-dd)
      datemaxswe[j,col-3] <-  dtmxswe  
      sm_stdate_index <- min(which(sm[date_index[1]:nrow(sm),7]>1)) #ID first day after maxSWE where snowmelt>1mm/d
      smeltstartdate <- sm[(sm_stdate_index[1]+date_index[1]-1),1]  #get date for start of snowmelt (adjust index for date_index)
      year(smeltstartdate) <- ifelse(month(smeltstartdate)<10, 1952, 1951) #Adjust year to common water year calendar
      sm_enddate_index <- max(which(sm[date_index[1]:nrow(sm),7]>1)) 
      smeltenddate <- sm[(sm_enddate_index[1]+date_index[1]-1),1] 
      year(smeltenddate) <- ifelse(month(smeltenddate)<10, 1952, 1951) 
      sm_stdate[j,col-3] <- smeltstartdate 
      sm_enddate[j,col-3] <- smeltenddate  
      sm_vol[j,col-3] <- sum(sm[date_index[1]:nrow(sm),7]) #total annual snowmelt[mm/yr]
      frac_snow[j,col-3] <- sum(sm[,6])/sum(sm[,4])
    }
    else if (length(which(is.na(input2)))>0) {
      smr_all[j,col-3] <- NA #NA all years with NA vals
      scd[j,col-3] <- NA
      max_swe[j,col-3] <-NA
      datemaxswe[j,col-3] <-NA
      sm_stdate[j,col-3] <- NA
      sm_enddate[j,col-3] <- NA
      sm_vol[j,col-3] <- NA
      frac_snow[j,col-3] <- NA
    }
  }
}

#Add tmin_365  mean of daily  values to DF

for (col in 5:ncol(tmin_365)) {
  input <- sm <- sm_yr <- smr <- date_index <- sm_stdate_index <- sm_enddate_index <- NA #void tminevious data
  input <- cbind.data.frame(threesixtyfivetime, pr_365[,col], tmax_365[,col], tmin_365[,col])
  input$tmin_adj <- ifelse((input[,3]<input[,4]), input[,3], input[,4]) #tmax, tmin
  input[,4] <- NULL
  input$wtryr <- wtr_yr(input[,1], 10)
  names(input) <- c("date","pr","tmax","tmin","wtryr")
  for (yr in 1951:2099) {
    j <- yr-1950 # row index
    input2 <- subset(input, wtryr==yr)
    datemaxswe[j,1] <- sm_stdate[j,1] <- sm_enddate[j,1] <- yr
    if (length(which(is.na(input2)))==0) {
      sm<-SnowMelt(Date=input2$date, precip_mm=input2$pr, Tmax_C=input2$tmax, Tmin_C=input2$tmin, lat_deg=58.7033, slope = 0,
                   aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                   SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                   startingSnowDensity_kg_m3=450)
      smr <- subset(sm, SnowMelt_mm>=1)
      smr_all[j,col+3] <- mean(smr[,7])
      scd[j,col+3] <- length(which(sm$SnowDepth_m>0)) #number of days with snow on ground (depth > 0)
      max_swe[j,col+3] <- max(sm$SnowWaterEq_mm)
      date_index<-which(sm$SnowWaterEq_mm==max(sm$SnowWaterEq_mm))
      dtmxswe <- sm[date_index[1],1] #in case of consecutive days choose first occurrence of max swe
      year(dtmxswe) <- ifelse(month(dtmxswe)<10, 1952, 1951) #Oct-Dec, 1951; Jan-Sept, 1952 for all water years (mm-dd)
      datemaxswe[j,col+3] <-  dtmxswe  
      sm_stdate_index <- min(which(sm[date_index[1]:nrow(sm),7]>1)) #ID first day after maxSWE where snowmelt>1mm/d
      smeltstartdate <- sm[(sm_stdate_index[1]+date_index[1]-1),1]  #get date for start of snowmelt (adjust index for date_index)
      year(smeltstartdate) <- ifelse(month(smeltstartdate)<10, 1952, 1951) #Adjust year to common water year calendar
      sm_enddate_index <- max(which(sm[date_index[1]:nrow(sm),7]>1)) 
      smeltenddate <- sm[(sm_enddate_index[1]+date_index[1]-1),1] 
      year(smeltenddate) <- ifelse(month(smeltenddate)<10, 1952, 1951) 
      sm_stdate[j,col+3] <- smeltstartdate 
      sm_enddate[j,col+3] <- smeltenddate  
      sm_vol[j,col+3] <- sum(sm[date_index[1]:nrow(sm),7]) #total annual snowmelt[mm/yr]
      frac_snow[j,col+3] <- sum(sm[,6])/sum(sm[,4])
    }
    else if (length(which(is.na(input2)))>0) {
      smr_all[j,col+3] <- NA #NA all years with NA vals
      scd[j,col+3] <- NA
      max_swe[j,col+3] <-NA
      datemaxswe[j,col+3] <-NA
      sm_stdate[j,col+3] <- NA
      sm_enddate[j,col+3] <- NA
      sm_vol[j,col+3] <- NA
      frac_snow[j,col+3] <- NA
    }
  }
}

#Add tmin_std  mean of daily tminecip values to DF

for (col in 5:ncol(tmin_std)) {
  input <- sm <- sm_yr <- smr <- date_index <- sm_stdate_index <- sm_enddate_index <- NA #void tminevious data
  input <- cbind.data.frame(standardtime, pr_std[,col], tmax_std[,col], tmin_std[,col])
  input$tmin_adj <- ifelse((input[,3]<input[,4]), input[,3], input[,4]) #tmax, tmin
  input[,4] <- NULL
  input$wtryr <- wtr_yr(input[,1], 10)
  names(input) <- c("date","pr","tmax","tmin","wtryr")
  for (yr in 1951:2099) {
    j <- yr-1950 # row index
    input2 <- subset(input, wtryr==yr)
    datemaxswe[j,1] <- sm_stdate[j,1] <- sm_enddate[j,1] <- yr
    if (length(which(is.na(input2)))==0) {
      sm<-SnowMelt(Date=input2$date, precip_mm=input2$pr, Tmax_C=input2$tmax, Tmin_C=input2$tmin, lat_deg=58.7033, slope = 0,
                   aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                   SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                   startingSnowDensity_kg_m3=450)
      smr <- subset(sm, SnowMelt_mm>=1)
      smr_all[j,col+51] <- mean(smr[,7])
      scd[j,col+51] <- length(which(sm$SnowDepth_m>0)) #number of days with snow on ground (depth > 0)
      max_swe[j,col+51] <- max(sm$SnowWaterEq_mm)
      date_index<-which(sm$SnowWaterEq_mm==max(sm$SnowWaterEq_mm))
      dtmxswe <- sm[date_index[1],1] #in case of consecutive days choose first occurrence of max swe
      year(dtmxswe) <- ifelse(month(dtmxswe)<10, 1952, 1951) #Oct-Dec, 1951; Jan-Sept, 1952 for all water years (mm-dd)
      datemaxswe[j,col+51] <-  dtmxswe  
      sm_stdate_index <- min(which(sm[date_index[1]:nrow(sm),7]>1)) #ID first day after maxSWE where snowmelt>1mm/d
      smeltstartdate <- sm[(sm_stdate_index[1]+date_index[1]-1),1]  #get date for start of snowmelt (adjust index for date_index)
      year(smeltstartdate) <- ifelse(month(smeltstartdate)<10, 1952, 1951) #Adjust year to common water year calendar
      sm_enddate_index <- max(which(sm[date_index[1]:nrow(sm),7]>1)) 
      smeltenddate <- sm[(sm_enddate_index[1]+date_index[1]-1),1] 
      year(smeltenddate) <- ifelse(month(smeltenddate)<10, 1952, 1951) 
      sm_stdate[j,col+51] <- smeltstartdate 
      sm_enddate[j,col+51] <- smeltenddate  
      sm_vol[j,col+51] <- sum(sm[date_index[1]:nrow(sm),7]) #total annual snowmelt[mm/yr]
      frac_snow[j,col+51] <- sum(sm[,6])/sum(sm[,4])
    }
    else if (length(which(is.na(input2)))>0) {
      smr_all[j,col+51] <- NA #NA all years with NA vals
      scd[j,col+51] <- NA
      max_swe[j,col+51] <-NA
      datemaxswe[j,col+51] <-NA
      sm_stdate[j,col+51] <- NA
      sm_enddate[j,col+51] <- NA
      sm_vol[j,col+51] <- NA
      frac_snow[j,col+51] <- NA
    }
  }
}

#Add tmin_greg  mean of daily tminecip values to DF

for (col in 5:ncol(tmin_greg)) {
  input <- sm <- sm_yr <- smr <- date_index <- sm_stdate_index <- sm_enddate_index <- NA #void tminevious data
  input <- cbind.data.frame(prolepticgregtime, pr_greg[,col], tmax_greg[,col], tmin_greg[,col])
  input$tmin_adj <- ifelse((input[,3]<input[,4]), input[,3], input[,4]) #tmax, tmin
  input[,4] <- NULL
  input$wtryr <- wtr_yr(input[,1], 10)
  names(input) <- c("date","pr","tmax","tmin","wtryr")
  for (yr in 1951:2099) {
    j <- yr-1950 # row index
    input2 <- subset(input, wtryr==yr)
    datemaxswe[j,1] <- sm_stdate[j,1] <- sm_enddate[j,1] <- yr
    if (length(which(is.na(input2)))==0) {
      sm<-SnowMelt(Date=input2$date, precip_mm=input2$pr, Tmax_C=input2$tmax, Tmin_C=input2$tmin, lat_deg=58.7033, slope = 0,
                   aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                   SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                   startingSnowDensity_kg_m3=450)
      smr <- subset(sm, SnowMelt_mm>=1)
      smr_all[j,col+63] <- mean(smr[,7])
      scd[j,col+63] <- length(which(sm$SnowDepth_m>0)) #number of days with snow on ground (depth > 0)
      max_swe[j,col+63] <- max(sm$SnowWaterEq_mm)
      date_index<-which(sm$SnowWaterEq_mm==max(sm$SnowWaterEq_mm))
      dtmxswe <- sm[date_index[1],1] #in case of consecutive days choose first occurrence of max swe
      year(dtmxswe) <- ifelse(month(dtmxswe)<10, 1952, 1951) #Oct-Dec, 1951; Jan-Sept, 1952 for all water years (mm-dd)
      datemaxswe[j,col+63] <-  dtmxswe  
      sm_stdate_index <- min(which(sm[date_index[1]:nrow(sm),7]>1)) #ID first day after maxSWE where snowmelt>1mm/d
      smeltstartdate <- sm[(sm_stdate_index[1]+date_index[1]-1),1]  #get date for start of snowmelt (adjust index for date_index)
      year(smeltstartdate) <- ifelse(month(smeltstartdate)<10, 1952, 1951) #Adjust year to common water year calendar
      sm_enddate_index <- max(which(sm[date_index[1]:nrow(sm),7]>1)) 
      smeltenddate <- sm[(sm_enddate_index[1]+date_index[1]-1),1] 
      year(smeltenddate) <- ifelse(month(smeltenddate)<10, 1952, 1951) 
      sm_stdate[j,col+63] <- smeltstartdate 
      sm_enddate[j,col+63] <- smeltenddate  
      sm_vol[j,col+63] <- sum(sm[date_index[1]:nrow(sm),7]) #total annual snowmelt[mm/yr]
      frac_snow[j,col+63] <- sum(sm[,6])/sum(sm[,4])
    }
    else if (length(which(is.na(input2)))>0) {
      smr_all[j,col+63] <- NA #NA all years with NA vals
      scd[j,col+63] <- NA
      max_swe[j,col+63] <-NA
      datemaxswe[j,col+63] <-NA
      sm_stdate[j,col+63] <- NA
      sm_enddate[j,col+63] <- NA
      sm_vol[j,col+63] <- NA
      frac_snow[j,col+63] <- NA
    }
  }
}


#################################
#Snowmelt rate figures
#################################
smr_26 <- cbind.data.frame(smr_all[,1], smr_all[,seq(2,73,3)])
smr_45 <- cbind.data.frame(smr_all[,1], smr_all[,seq(3,73,3)])
names(smr_26)[1] <- names(smr_45)[1] <- "year"
smr_85 <- smr_all[,seq(1,73,3)]


#RCP 2.6 plot
smr_26 <- na.omit(smr_26)
smrann_26 <- gather(smr_26, GCM, smrann, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

smrann_26_med <- cbind.data.frame(smr_26[,1], apply(smr_26[,2:25],1,median))
names(smrann_26_med) <- c("year","Median")


ggplot(smrann_26, aes(x=year,y=smrann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smrann_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Average Snowmelt Rate (mm/d)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltRate_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
smr_45 <- na.omit(smr_45)
smrann_45 <- gather(smr_45, GCM, smrann, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

smrann_45_med <- cbind.data.frame(smr_45$year, apply(smr_45[,2:25],1,median))
names(smrann_45_med) <- c("year","Median")


ggplot(smrann_45, aes(x=year,y=smrann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smrann_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Average Snowmelt Rate (mm/d)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltRate_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
smr_85 <- na.omit(smr_85)
smrann_85 <- gather(smr_85, GCM, smrann, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

smrann_85_med <- cbind.data.frame(smr_85$year, apply(smr_85[,2:25],1,median))
names(smrann_85_med) <- c("year","Median")


ggplot(smrann_85, aes(x=year,y=smrann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smrann_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Average Snowmelt Rate (mm/d)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltRate_RCP85.tiff", width = 30, height = 18, units="cm")

############################
#Snow cover duration figures
############################
scd_26 <- cbind.data.frame(scd[,1], scd[,seq(2,73,3)])
scd_45 <- cbind.data.frame(scd[,1], scd[,seq(3,73,3)])
names(scd_26)[1] <- names(scd_45)[1] <- "year"
scd_85 <- scd[,seq(1,73,3)]


#RCP 2.6 plot
scd_26 <- na.omit(scd_26)
scdann_26 <- gather(scd_26, GCM, scdann, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                    bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                    gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                    noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

scdann_26_med <- cbind.data.frame(scd_26[,1], apply(scd_26[,2:25],1,median))
names(scdann_26_med) <- c("year","Median")


ggplot(scdann_26, aes(x=year,y=scdann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = scdann_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snow Cover Duration (days)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowCoverDuration_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
scd_45 <- na.omit(scd_45)
scdann_45 <- gather(scd_45, GCM, scdann, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                    bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                    gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                    noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

scdann_45_med <- cbind.data.frame(scd_45$year, apply(scd_45[,2:25],1,median))
names(scdann_45_med) <- c("year","Median")


ggplot(scdann_45, aes(x=year,y=scdann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = scdann_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snow Cover Duration (days)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowCoverDuration_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
scd_85 <- na.omit(scd_85)
scdann_85 <- gather(scd_85, GCM, scdann, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                    bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                    gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                    noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

scdann_85_med <- cbind.data.frame(scd_85$year, apply(scd_85[,2:25],1,median))
names(scdann_85_med) <- c("year","Median")


ggplot(scdann_85, aes(x=year,y=scdann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = scdann_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snow Cover Duration (days)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowCoverDuration_RCP85.tiff", width = 30, height = 18, units="cm")

##############################
#Maximum SWE
##############################
maxswe_26 <- cbind.data.frame(max_swe[,1], max_swe[,seq(2,73,3)])
maxswe_45 <- cbind.data.frame(max_swe[,1], max_swe[,seq(3,73,3)])
names(maxswe_26)[1] <- names(maxswe_45)[1] <- "year"
maxswe_85 <- max_swe[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
maxswe_26 <- na.omit(maxswe_26)
maximumswe_26 <- gather(maxswe_26, GCM, maximumswe, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                    bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                    gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                    noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

maximumswe_26_med <- cbind.data.frame(maxswe_26[,1], apply(maxswe_26[,2:25],1,median))
names(maximumswe_26_med) <- c("year","Median")


ggplot(maximumswe_26, aes(x=year,y=maximumswe)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = maximumswe_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum SWE (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("MaxSWE_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
maxswe_45 <- na.omit(maxswe_45)
maximumswe_45 <- gather(maxswe_45, GCM, maximumswe, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                    bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                    gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                    noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

maximumswe_45_med <- cbind.data.frame(maxswe_45$year, apply(maxswe_45[,2:25],1,median))
names(maximumswe_45_med) <- c("year","Median")


ggplot(maximumswe_45, aes(x=year,y=maximumswe)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = maximumswe_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum SWE (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("MaxSWE_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
maxswe_85 <- na.omit(maxswe_85)
maximumswe_85 <- gather(maxswe_85, GCM, maximumswe, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                    bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                    gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                    noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

maximumswe_85_med <- cbind.data.frame(maxswe_85$year, apply(maxswe_85[,2:25],1,median))
names(maximumswe_85_med) <- c("year","Median")


ggplot(maximumswe_85, aes(x=year,y=maximumswe)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = maximumswe_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Maximum SWE (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("MaxSWE_RCP85.tiff", width = 30, height = 18, units="cm")


##################################
#Date of Maximum SWE
##################################
datemaximumswe_26 <- cbind.data.frame(datemaxswe[,1], datemaxswe[,seq(2,73,3)])
datemaximumswe_45 <- cbind.data.frame(datemaxswe[,1], datemaxswe[,seq(3,73,3)])
names(datemaximumswe_26)[1] <- names(datemaximumswe_45)[1] <- "year"
datemaximumswe_85 <- datemaxswe[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
datemaximumswe_26 <- na.omit(datemaximumswe_26)
datemaxswe_26 <- gather(datemaximumswe_26, GCM, datemaxswe, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                    bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                    gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                    noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

#datemaxswe_26_med <- cbind.data.frame(datemaximumswe_26[,1], apply(datemaximumswe_26[,2:25],1,median))
#names(datemaxswe_26_med) <- c("year","Median")


ggplot(datemaxswe_26, aes(x=year,y=datemaxswe)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = datemaxswe_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Maximum SWE", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("DateMaxSWE_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
datemaximumswe_45 <- na.omit(datemaximumswe_45)
datemaxswe_45 <- gather(datemaximumswe_45, GCM, datemaxswe, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                    bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                    gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                    noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

#datemaxswe_45_med <- cbind.data.frame(datemaximumswe_45$year, apply(datemaximumswe_45[,2:25],1,median))
#names(datemaxswe_45_med) <- c("year","Median")


ggplot(datemaxswe_45, aes(x=year,y=datemaxswe)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = datemaxswe_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Maximum SWE", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("DateMaxSWE_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
datemaximumswe_85 <- na.omit(datemaximumswe_85)
datemaxswe_85 <- gather(datemaximumswe_85, GCM, datemaxswe, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                    bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                    gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                    noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

#datemaxswe_85_med <- cbind.data.frame(datemaximumswe_85$year, apply(datemaximumswe_85[,2:25],1,median))
#names(datemaxswe_85_med) <- c("year","Median")


ggplot(datemaxswe_85, aes(x=year,y=datemaxswe)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = datemaxswe_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Maximum SWE", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("DateMaxSWE_RCP85.tiff", width = 30, height = 18, units="cm")

############################
#Total volume of snowmelt
############################
snowmeltvolume_26 <- cbind.data.frame(sm_vol[,1], sm_vol[,seq(2,73,3)])
snowmeltvolume_45 <- cbind.data.frame(sm_vol[,1], sm_vol[,seq(3,73,3)])
names(snowmeltvolume_26)[1] <- names(snowmeltvolume_45)[1] <- "year"
snowmeltvolume_85 <- sm_vol[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
snowmeltvolume_26 <- na.omit(snowmeltvolume_26)
snvol_26 <- gather(snowmeltvolume_26, GCM, snvol, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                        bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                        gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                        noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

snvol_26_med <- cbind.data.frame(snowmeltvolume_26[,1], apply(snowmeltvolume_26[,2:25],1,median))
names(snvol_26_med) <- c("year","Median")


ggplot(snvol_26, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = snvol_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume (mm)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_Vol_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
snowmeltvolume_45 <- na.omit(snowmeltvolume_45)
snvol_45 <- gather(snowmeltvolume_45, GCM, snvol, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                        bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                        gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                        noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

snvol_45_med <- cbind.data.frame(snowmeltvolume_45$year, apply(snowmeltvolume_45[,2:25],1,median))
names(snvol_45_med) <- c("year","Median")


ggplot(snvol_45, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = snvol_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume (mm)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_Vol_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
snowmeltvolume_85 <- na.omit(snowmeltvolume_85)
snvol_85 <- gather(snowmeltvolume_85, GCM, snvol, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                        bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                        gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                        noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

snvol_85_med <- cbind.data.frame(snowmeltvolume_85$year, apply(snowmeltvolume_85[,2:25],1,median))
names(snvol_85_med) <- c("year","Median")


ggplot(snvol_85, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = snvol_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume (mm)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_Vol_RCP85.tiff", width = 30, height = 18, units="cm")

#################################
#Ratio of snowmelt:summer precip
#################################
#from above - run JJA precip code
pr_summer_26 <- cbind.data.frame(pr_summer_all[2:150,1], pr_summer_all[2:150,seq(2,73,3)]) #run this again to remove years 1950, 2099 to match sm_vol
pr_summer_45 <- cbind.data.frame(pr_summer_all[2:150,1], pr_summer_all[2:150,seq(3,73,3)])
names(pr_summer_26)[1] <- names(pr_summer_45)[1] <- "year"
pr_summer_85 <- pr_summer_all[2:150,seq(1,73,3)]


snowmeltvolume_26 <- cbind.data.frame(sm_vol[,1], sm_vol[,seq(2,73,3)])
snowmeltvolume_45 <- cbind.data.frame(sm_vol[,1], sm_vol[,seq(3,73,3)])
names(snowmeltvolume_26)[1] <- names(snowmeltvolume_45)[1] <- "year"
snowmeltvolume_85 <- sm_vol[,seq(1,73,3)]

#Total snowmelt volume / JJA precip
sm_jjapr_ratio_26 <- cbind.data.frame(snowmeltvolume_26[,1], snowmeltvolume_26[,2:25]/pr_summer_26[,2:25])
sm_jjapr_ratio_45 <- cbind.data.frame(snowmeltvolume_45[,1], snowmeltvolume_45[,2:25]/pr_summer_45[,2:25])
sm_jjapr_ratio_85 <- cbind.data.frame(snowmeltvolume_85[,1], snowmeltvolume_85[,2:25]/pr_summer_85[,2:25])
names(sm_jjapr_ratio_26)[1] <- names(sm_jjapr_ratio_45)[1] <- names(sm_jjapr_ratio_85)[1] <- "year"

#Figures

#RCP 2.6 plot
sm_jjapr_ratio_26 <- na.omit(sm_jjapr_ratio_26)
smelt_jjapr_26 <- gather(sm_jjapr_ratio_26, GCM, snvol, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                   bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                   gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                   noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

smelt_jjapr_26_med <- cbind.data.frame(sm_jjapr_ratio_26[,1], apply(sm_jjapr_ratio_26[,2:25],1,median))
names(smelt_jjapr_26_med) <- c("year","Median")


ggplot(smelt_jjapr_26, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smelt_jjapr_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume : JJA Precipitation", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_JJAPrecip_Ratio_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
sm_jjapr_ratio_45 <- na.omit(sm_jjapr_ratio_45)
smelt_jjapr_45 <- gather(sm_jjapr_ratio_45, GCM, snvol, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                   bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                   gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                   noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

smelt_jjapr_45_med <- cbind.data.frame(sm_jjapr_ratio_45$year, apply(sm_jjapr_ratio_45[,2:25],1,median))
names(smelt_jjapr_45_med) <- c("year","Median")


ggplot(smelt_jjapr_45, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smelt_jjapr_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume : JJA Precipitation", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_JJAPrecip_Ratio_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
sm_jjapr_ratio_85 <- na.omit(sm_jjapr_ratio_85)
smelt_jjapr_85 <- gather(sm_jjapr_ratio_85, GCM, snvol, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                   bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                   gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                   noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

smelt_jjapr_85_med <- cbind.data.frame(sm_jjapr_ratio_85$year, apply(sm_jjapr_ratio_85[,2:25],1,median))
names(smelt_jjapr_85_med) <- c("year","Median")


ggplot(smelt_jjapr_85, aes(x=year,y=snvol)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = smelt_jjapr_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Snowmelt Volume : JJA Precipitation", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("Snowmelt_JJAPrecip_Ratio_RCP85.tiff", width = 30, height = 18, units="cm")




#####################################
#Date of Start of Snowmelt
#####################################
snowmeltfirstdate_26 <- cbind.data.frame(sm_stdate[,1], sm_stdate[,seq(2,73,3)])
snowmeltfirstdate_45 <- cbind.data.frame(sm_stdate[,1], sm_stdate[,seq(3,73,3)])
names(snowmeltfirstdate_26)[1] <- names(snowmeltfirstdate_45)[1] <- "year"
snowmeltfirstdate_85 <- sm_stdate[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
snowmeltfirstdate_26 <- na.omit(snowmeltfirstdate_26)
smfirstdate_26 <- gather(snowmeltfirstdate_26, GCM, smfirstdate, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                        bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                        gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                        noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

#smfirstdate_26_med <- cbind.data.frame(snowmeltfirstdate_26[,1], apply(snowmeltfirstdate_26[,2:25],1,median))
#names(smfirstdate_26_med) <- c("year","Median")


ggplot(smfirstdate_26, aes(x=year,y=smfirstdate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
 # geom_line(data = smfirstdate_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Start of Snowmelt", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltStartDate_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
snowmeltfirstdate_45 <- na.omit(snowmeltfirstdate_45)
smfirstdate_45 <- gather(snowmeltfirstdate_45, GCM, smfirstdate, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                        bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                        gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                        noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

#smfirstdate_45_med <- cbind.data.frame(snowmeltfirstdate_45$year, apply(snowmeltfirstdate_45[,2:25],1,median))
#names(smfirstdate_45_med) <- c("year","Median")


ggplot(smfirstdate_45, aes(x=year,y=smfirstdate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = smfirstdate_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Start of Snowmelt", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltStartDate_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
snowmeltfirstdate_85 <- na.omit(snowmeltfirstdate_85)
smfirstdate_85 <- gather(snowmeltfirstdate_85, GCM, smfirstdate, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                        bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                        gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                        noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

#smfirstdate_85_med <- cbind.data.frame(snowmeltfirstdate_85$year, apply(snowmeltfirstdate_85[,2:25],1,median))
#names(smfirstdate_85_med) <- c("year","Median")


ggplot(smfirstdate_85, aes(x=year,y=smfirstdate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = smfirstdate_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Start of Snowmelt", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltStartDate_RCP85.tiff", width = 30, height = 18, units="cm")

##################################
#Date of Last Day of Snowmelt
##################################
snowmeltenddate_26 <- cbind.data.frame(sm_enddate[,1], sm_enddate[,seq(2,73,3)])
snowmeltenddate_45 <- cbind.data.frame(sm_enddate[,1], sm_enddate[,seq(3,73,3)])
names(snowmeltenddate_26)[1] <- names(snowmeltenddate_45)[1] <- "year"
snowmeltenddate_85 <- sm_enddate[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
snowmeltenddate_26 <- na.omit(snowmeltenddate_26)
smenddate_26 <- gather(snowmeltenddate_26, GCM, smenddate, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                        bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                        gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                        noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

#smenddate_26_med <- cbind.data.frame(snowmeltenddate_26[,1], apply(snowmeltenddate_26[,2:25],1,median))
#names(smenddate_26_med) <- c("year","Median")


ggplot(smenddate_26, aes(x=year,y=smenddate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = smenddate_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Snowmelt End Date", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltEndDate_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
snowmeltenddate_45 <- na.omit(snowmeltenddate_45)
smenddate_45 <- gather(snowmeltenddate_45, GCM, smenddate, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                        bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                        gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                        noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

#smenddate_45_med <- cbind.data.frame(snowmeltenddate_45$year, apply(snowmeltenddate_45[,2:25],1,median))
#names(smenddate_45_med) <- c("year","Median")


ggplot(smenddate_45, aes(x=year,y=smenddate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = smenddate_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Snowmelt End Date", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltEndDate_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
snowmeltenddate_85 <- na.omit(snowmeltenddate_85)
smenddate_85 <- gather(snowmeltenddate_85, GCM, smenddate, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                        bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                        gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                        noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

#smenddate_85_med <- cbind.data.frame(snowmeltenddate_85$year, apply(snowmeltenddate_85[,2:25],1,median))
#names(smenddate_85_med) <- c("year","Median")


ggplot(smenddate_85, aes(x=year,y=smenddate)) + 
  geom_fan(intervals=c(0.025,0.8,1.0)) +
  theme_bw() +
  #geom_line(data = smenddate_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Date of Snowmelt End Date", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("SnowmeltEndDate_RCP85.tiff", width = 30, height = 18, units="cm")


#################################
#Fraction of precip as snow
#################################
fractionsnow_26 <- cbind.data.frame(frac_snow[,1], frac_snow[,seq(2,73,3)])
fractionsnow_45 <- cbind.data.frame(frac_snow[,1], frac_snow[,seq(3,73,3)])
names(fractionsnow_26)[1] <- names(fractionsnow_45)[1] <- "year"
fractionsnow_85 <- frac_snow[,seq(1,73,3)]


#Figures

#RCP 2.6 plot
fractionsnow_26 <- na.omit(fractionsnow_26)
fracsnow_26 <- gather(fractionsnow_26, GCM, fracsnow, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
                        bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
                        gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
                        noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

fracsnow_26_med <- cbind.data.frame(fractionsnow_26[,1], apply(fractionsnow_26[,2:25],1,median))
names(fracsnow_26_med) <- c("year","Median")


ggplot(fracsnow_26, aes(x=year,y=fracsnow)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = fracsnow_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fraction of Precipitation as Snow (-)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FracSnow_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
fractionsnow_45 <- na.omit(fractionsnow_45)
fracsnow_45 <- gather(fractionsnow_45, GCM, fracsnow, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
                        bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
                        gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
                        noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

fracsnow_45_med <- cbind.data.frame(fractionsnow_45$year, apply(fractionsnow_45[,2:25],1,median))
names(fracsnow_45_med) <- c("year","Median")


ggplot(fracsnow_45, aes(x=year,y=fracsnow)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = fracsnow_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fraction of Precipitation as Snow (-)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FracSnow_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
fractionsnow_85 <- na.omit(fractionsnow_85)
fracsnow_85 <- gather(fractionsnow_85, GCM, fracsnow, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
                        bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
                        gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
                        noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

fracsnow_85_med <- cbind.data.frame(fractionsnow_85$year, apply(fractionsnow_85[,2:25],1,median))
names(fracsnow_85_med) <- c("year","Median")


ggplot(fracsnow_85, aes(x=year,y=fracsnow)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = fracsnow_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Fraction of Precipitation as Snow (-)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("FracSnow_RCP85.tiff", width = 30, height = 18, units="cm")


##############################################
##############Annual tmean#####################
##############################################

#Subset of  seasonal daily tmean for given time period (annual)
tmean_360_annual <- subset(tmean_360, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12) #month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12
tmean_365_annual <- subset(tmean_365, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmean_std_annual <- subset(tmean_std, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)
tmean_greg_annual <- subset(tmean_greg, month==1 | month==2 | month==3 | month==4 | month==5 | month ==6 | month==7 | month==8 | month==9 | month==10 | month==11 | month==12)

#Empty DF for total annual tmeanecipitation
tmean_annual_all <- as.data.frame(matrix(NA, nrow=151, ncol=73))
names(tmean_annual_all) <- c("year",names(tmean_360[,5:10]), names(tmean_365[,5:52]), names(tmean_std[,5:16]),names(tmean_greg[,5:10]))
tmean_annual_all[,1] <- as.data.frame(unique(tmean_greg$year))

#360 2:7; 365 8:55; std 56:67; greg 68:73
#col-3; col+3; col+51; col+63 (col starts at 5)

#Add tmean_360  mean of daily tmeanecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmeanevious data
  sub <- as.data.frame(subset(tmean_360_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmean_360_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmean_annual_all[j, (col-3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmean_annual_all[j,(col-3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmean_365  mean of daily tmeanecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmeanevious data
  sub <- as.data.frame(subset(tmean_365_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmean_365_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmean_annual_all[j, (col+3)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmean_annual_all[j,(col+3)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmean_std  mean of daily tmeanecip values to DF
for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmeanevious data
  sub <- as.data.frame(subset(tmean_std_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmean_std_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmean_annual_all[j, (col+51)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmean_annual_all[j,(col+51)] <- NA #NA all years with NA vals
    } 
  }
}

#Add tmean_greg  mean of daily tmeanecip values to DF

for (yr in 1950:2100) {
  j <- yr - 1949 #row index
  sub <- NA #void tmeanevious data
  sub <- as.data.frame(subset(tmean_greg_annual, year==yr)) #subset of entire DF for year
  for (col in 5:ncol(tmean_greg_annual)) {
    col_yr <- sub[,col] #all data in given year for given mod/rcp
    if (length(which(is.na(col_yr)))==0) {
      tmean_annual_all[j, (col+63)] <- mean(col_yr)
    } else if (length(which(is.na(col_yr)))>0) {
      tmean_annual_all[j,(col+63)] <- NA #NA all years with NA vals
    } 
  }
}

tmean_annual_26 <- cbind.data.frame(tmean_annual_all[,1], tmean_annual_all[,seq(2,73,3)])
tmean_annual_45 <- cbind.data.frame(tmean_annual_all[,1], tmean_annual_all[,seq(3,73,3)])
names(tmean_annual_26)[1] <- names(tmean_annual_45)[1] <- "year"
tmean_annual_85 <- tmean_annual_all[,seq(1,73,3)]


tmean_annual_26_hist <- colMeans(na.omit(tmean_annual_26[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmean_annual_26_nearfut <- colMeans(na.omit(tmean_annual_26[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmean_annual_26_farfut <- colMeans(na.omit(tmean_annual_26[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmean_annual_45_hist <- colMeans(na.omit(tmean_annual_45[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmean_annual_45_nearfut <- colMeans(na.omit(tmean_annual_45[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmean_annual_45_farfut <- colMeans(na.omit(tmean_annual_45[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

tmean_annual_85_hist <- colMeans(na.omit(tmean_annual_85[27:56,2:25])) #1976-2005, mean for each model over the 30-year time period
tmean_annual_85_nearfut <- colMeans(na.omit(tmean_annual_85[91:120,2:25])) #2040-69 mean for each model over the 30-year time period
tmean_annual_85_farfut <- colMeans(na.omit(tmean_annual_85[121:150,2:25])) #2070-99 mean for each model over the 30-year time period

#Print table of stats for tech sub
#ooo: for tmean/tmin, take avg temp for each year/defined period (pr take sum) for each gcm
#for each gcm take average/sum over defined thirty-year period resulting in df of avg 2040-69 tmean (ex)
#for each time period take stats (median, 10th, 90th percentile)

data <- c(round(mean(quantile(tmean_annual_26_hist,0.1), quantile(tmean_annual_45_hist,0.1), quantile(tmean_annual_85_hist,0.1)),1),
          round(quantile(tmean_annual_26_nearfut,0.1),1), round(quantile(tmean_annual_45_nearfut,0.1),1), round(quantile(tmean_annual_85_nearfut,0.1),1),
          round(quantile(tmean_annual_26_farfut,0.1),1), round(quantile(tmean_annual_45_farfut, 0.1),1), round(quantile(tmean_annual_85_farfut, 0.1),1),
          round(mean(median(tmean_annual_26_hist), median(tmean_annual_45_hist), median(tmean_annual_85_hist)),1),
          round(median(tmean_annual_26_nearfut),1), round(median(tmean_annual_45_nearfut),1), round(median(tmean_annual_85_nearfut),1),
          round(median(tmean_annual_26_farfut),1), round(median(tmean_annual_45_farfut),1), round(median(tmean_annual_85_farfut),1),
          round(mean(quantile(tmean_annual_26_hist,0.9), quantile(tmean_annual_45_hist,0.9), quantile(tmean_annual_85_hist,0.9)),1),
          round(quantile(tmean_annual_26_nearfut,0.9),1), round(quantile(tmean_annual_45_nearfut,0.9),1), round(quantile(tmean_annual_85_nearfut,0.9),1),
          round(quantile(tmean_annual_26_farfut,0.9),1), round(quantile(tmean_annual_45_farfut, 0.9), 1), round(quantile(tmean_annual_85_farfut, 0.9),1))
print("Annual tmean (C)")
print(matrix(data, nrow=3, ncol=7, byrow=TRUE))


#relative change - calc relative change for each model; then take stats across models projections of relative change (DO only present future results as compared to the historical simulation for the same global climate model)

data2 <- c(round(quantile((tmean_annual_26_nearfut-tmean_annual_26_hist),0.1),1), round(quantile((tmean_annual_45_nearfut-tmean_annual_45_hist),0.1),1), round(quantile((tmean_annual_85_nearfut-tmean_annual_85_hist),0.1),1),
           round(quantile((tmean_annual_26_farfut-tmean_annual_26_hist),0.1),1), round(quantile((tmean_annual_45_farfut-tmean_annual_45_hist), 0.1, 1)), round(quantile((tmean_annual_85_farfut-tmean_annual_85_hist), 0.1),1),
           round(median(tmean_annual_26_nearfut-tmean_annual_26_hist),1), round(median(tmean_annual_45_nearfut-tmean_annual_45_hist),1), round(median(tmean_annual_85_nearfut-tmean_annual_85_hist),1),
           round(median(tmean_annual_26_farfut-tmean_annual_26_hist),1), round(median(tmean_annual_45_farfut-tmean_annual_45_hist),1), round(median(tmean_annual_85_farfut-tmean_annual_85_hist),1),
           round(quantile((tmean_annual_26_nearfut-tmean_annual_26_hist),0.9),1), round(quantile((tmean_annual_45_nearfut-tmean_annual_45_hist),0.9),1), round(quantile((tmean_annual_85_nearfut-tmean_annual_85_hist),0.9),1),
           round(quantile((tmean_annual_26_farfut-tmean_annual_26_hist),0.9),1), round(quantile((tmean_annual_45_farfut-tmean_annual_45_hist), 0.9), 1), round(quantile((tmean_annual_85_farfut-tmean_annual_85_hist), 0.9),1))
print("Annual tmean (deltaC)")
print(matrix(data2, nrow=3, ncol=6, byrow=TRUE))

#Figures

#RCP 2.6 plot
tmean_annual_26 <- na.omit(tmean_annual_26)
tmean_ann_26 <-   gather(tmean_annual_26, GCM, tmean_ann, hadgem2ao_26, hadgem2es_26, bcccsm11_26, bcccsm11m_26,
         bnuesm_26, canesm2_26, ccsm4_26, cesm1cam5_26, csiromk360_26, fgoalsg2_26, gfdlcm3_26,
         gfdlesm2g_26, gfdlesm2m_26, ipslcm5alr_26, ipslcm5amr_26, miroc5_26, noresm1m_26,
         noresm1me_26, cnrmcm5_26, mirocesm_26, mirocesmchem_26, mricgcm3_26, mpiesmlr_26, mpiesmmr_26) 

tmean_ann_26_med <- cbind.data.frame(tmean_annual_26[,1], apply(tmean_annual_26[,2:25],1,median))
names(tmean_ann_26_med) <- c("year","Median")


ggplot(tmean_ann_26, aes(x=year,y=tmean_ann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmean_ann_26_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Mean Temperature (degC)", x = "Year") + 
  ggtitle("RCP 2.6")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmean_Annual_RCP26.tiff", width = 30, height = 18, units="cm")

#RCP 4.5 plot
tmean_annual_45 <- na.omit(tmean_annual_45)
tmean_ann_45 <- gather(tmean_annual_45, GCM, tmean_ann, hadgem2ao_45, hadgem2es_45, bcccsm11_45, bcccsm11m_45,
         bnuesm_45, canesm2_45, ccsm4_45, cesm1cam5_45, csiromk360_45, fgoalsg2_45, gfdlcm3_45,
         gfdlesm2g_45, gfdlesm2m_45, ipslcm5alr_45, ipslcm5amr_45, miroc5_45, noresm1m_45,
         noresm1me_45, cnrmcm5_45, mirocesm_45, mirocesmchem_45, mricgcm3_45, mpiesmlr_45, mpiesmmr_45) 

tmean_ann_45_med <- cbind.data.frame(tmean_annual_45$year, apply(tmean_annual_45[,2:25],1,median))
names(tmean_ann_45_med) <- c("year","Median")


ggplot(tmean_ann_45, aes(x=year,y=tmean_ann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmean_ann_45_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Mean Temperature (degC)", x = "Year") + 
  ggtitle("RCP 4.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmean_Annual_RCP45.tiff", width = 30, height = 18, units="cm")


#RCP 8.5 plot
tmean_annual_85 <- na.omit(tmean_annual_85)
tmean_ann_85 <- gather(tmean_annual_85, GCM, tmean_ann, hadgem2ao_85, hadgem2es_85, bcccsm11_85, bcccsm11m_85,
         bnuesm_85, canesm2_85, ccsm4_85, cesm1cam5_85, csiromk360_85, fgoalsg2_85, gfdlcm3_85,
         gfdlesm2g_85, gfdlesm2m_85, ipslcm5alr_85, ipslcm5amr_85, miroc5_85, noresm1m_85,
         noresm1me_85, cnrmcm5_85, mirocesm_85, mirocesmchem_85, mricgcm3_85, mpiesmlr_85, mpiesmmr_85) 

tmean_ann_85_med <- cbind.data.frame(tmean_annual_85$year, apply(tmean_annual_85[,2:25],1,median))
names(tmean_ann_85_med) <- c("year","Median")


ggplot(tmean_ann_85, aes(x=year,y=tmean_ann)) + 
  geom_fan(intervals=c(0.0,0.8,1.0)) +
  theme_bw() +
  geom_line(data = tmean_ann_85_med, aes(x=year,y=Median), colour = "black", size = 2) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Annual Mean Temperature (degC)", x = "Year") + 
  ggtitle("RCP 8.5")+
  theme(text=element_text(face="bold", size=20, colour="black")) +
  theme(axis.text.y=element_text(size=20, face="bold", colour="black")) +
  theme(axis.text.x=element_text(size=20, face="bold", colour="black")) 
ggsave("tmean_Annual_RCP85.tiff", width = 30, height = 18, units="cm")

