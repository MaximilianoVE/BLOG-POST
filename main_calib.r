#define vector for output
  root <- "C:/~Model Development/TechChange-RDM/"
  setwd(root)     # fija el directorio de trabajo (opcional, pero práctico)

#this script has been created to find the optimal value of policies for a future id,
  library(deSolve,lib=paste(root,"Rlibraries\\",sep=""))
  library(optimx,lib=paste(root,"Rlibraries\\",sep=""))
  dir.harness<-paste(root,"RDM Harness\\",sep="")
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  experiment.version<-"Exp.design_calib.csv"
  Exp.design<-read.csv(paste(dir.exp,experiment.version,sep=""))
#run the model once

#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  model.version<-"InternationalGreenTechChangeModel_9_19_2015_calib.r"
  source(paste(dir.model,model.version,sep=""))

target.run<-1
params<-c(
                         S.0=as.numeric(Exp.design[target.run,'S.0']),
                         TimeStep=as.numeric(Exp.design[target.run,'TimeStep']),
                         EndTime=as.numeric(Exp.design[target.run,'EndTime']),
                         alfa=as.numeric(Exp.design[target.run,'alfa']),
                         epsilon=as.numeric(Exp.design[target.run,'epsilon']),
                         Gamma.re=as.numeric(Exp.design[target.run,'Gamma.re']),
                         k.re=as.numeric(Exp.design[target.run,'k.re']),
                         Gamma.ce=as.numeric(Exp.design[target.run,'Gamma.ce']),
                         k.ce=as.numeric(Exp.design[target.run,'k.ce']),
                         Eta.re=as.numeric(Exp.design[target.run,'Eta.re']),
                         Eta.ce=as.numeric(Exp.design[target.run,'Eta.ce']),
                         Nu.re=as.numeric(Exp.design[target.run,'Nu.re']),
                         Nu.ce=as.numeric(Exp.design[target.run,'Nu.ce']),
                         qsi=as.numeric(Exp.design[target.run,'qsi']),
                         Delta.S=as.numeric(Exp.design[target.run,'Delta.S']),
						 Delta.Temp.Disaster=as.numeric(Exp.design[target.run,'Delta.Temp.Disaster']),
						 Beta.Delta.Temp=as.numeric(Exp.design[target.run,'Beta.Delta.Temp']),
						 CO2.base=as.numeric(Exp.design[target.run,'CO2.base']),
						 CO2.Disaster=as.numeric(Exp.design[target.run,'CO2.Disaster']),
                         labor.growth_N=as.numeric(Exp.design[target.run,'labor.growth_N']),
						 labor.growth_S=as.numeric(Exp.design[target.run,'labor.growth_S']),
                         lambda.S=as.numeric(Exp.design[target.run,'lambda.S']),
						 sigma.utility=as.numeric(Exp.design[target.run,'sigma.utility']),
						 rho=as.numeric(Exp.design[target.run,'rho']),
                         Yre.0_N=as.numeric(Exp.design[target.run,'Yre.0_N']),
                         Yce.0_N=as.numeric(Exp.design[target.run,'Yce.0_N']),
                         Yre.0_S=as.numeric(Exp.design[target.run,'Yre.0_S']),
                         Yce.0_S=as.numeric(Exp.design[target.run,'Yce.0_S']),
						 size.factor=as.numeric(Exp.design[target.run,'size.factor']),
						 Run.ID= as.numeric(Exp.design[target.run,'Run.ID']),
						 policy.name = as.character(Exp.design[target.run,'policy.name']),
						 dir.harness=dir.harness)

TechChangeMod(c(0.03,1.0,0.02,0.01,0.05,1.0,0.02,0.5),params)

## =====================================================================================================
## This section reads the output of simulations and reshapes it into time series split by region,
## =====================================================================================================

## 1. carpeta de librerías del proyecto
proj_lib <- "C:/~Model Development/TechChange-RDM/Rlibraries"
.libPaths(c(proj_lib, .libPaths()))   # que sea la primera en la búsqueda

## 2. instala reshape2 solo si no lo tienes ahí
if (!requireNamespace("reshape2", lib.loc = proj_lib, quietly = TRUE)) {
  install.packages("reshape2",
                   lib         = proj_lib,
                   dependencies = TRUE,
                   repos        = "https://cloud.r-project.org")
}

list.files(dir.harness, pattern = "^output_run_")

## 3. cárgalo explícitamente
library(reshape2, lib.loc = proj_lib)


#Define directory parameters
 dir.inputs<-paste(root,"RDM Inputs\\",sep="")
 dir.harness<-paste(root,"RDM Harness\\",sep="")
 dir.output<-paste(root,"RDM Outputs\\",sep="")

#create vector with file names
 experiment.version<-"Exp.design_calib.csv"
 filenames <- list.files(dir.harness, pattern="*.csv", full.names=FALSE)
#source function to process harnessed output data
 source(paste(dir.inputs,"harness_processing.r",sep=""))
#run post-processing in parallel
  library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
  modelruns<-process.harness.data(filenames[1],dir.inputs,experiment.version,dir.harness)
#print time series for model
  write.csv(modelruns, paste(root,"ParameterCalibration\\", "model.runs_calib.csv", sep=""), row.names=FALSE)


## =====================================================================================================
## This section merges the historical table with the calibration run
## =====================================================================================================

#this script puts together the data for the historic calibration
  dir.output<-paste(root,"RDM Outputs\\",sep="")
  dir.historic<-paste(root,"ParameterCalibration\\",sep="")

#read historic data
  historic<-read.csv(paste(dir.historic,"historic_energy_both_regions _v1.csv",sep=""))
#read simulated data
  data<-read.csv(paste(dir.historic,"model.runs_calib.csv",sep=""))
  data<-data[,c("Run.ID","time","Region","Y","Yce","Yre","Ace","Are","L","policy.name",
                "epsilon","rho","alfa","Eta.re","Eta.ce","Gamma.re","Gamma.ce","Nu.re","Nu.ce",
   				"k.re","k.ce","labor.growth","size.factor")]
  data$time<-data$time-29
 data<-Reduce(function(...) { merge(..., ) }, list(data,historic))
 #subset and create OECD and NONOECD regions
 data.historic<-subset(data,data$Run.ID==1)
 data.historic$Region<-gsub("N","OECD",data.historic$Region)
 data.historic$Region<-gsub("S","NONOECD",data.historic$Region)
 data.historic$Run.ID<-data.historic$Run.ID+1
 data<-rbind(data,data.historic)
  write.csv(data, paste(dir.output, "historic_calib.csv", sep=""), row.names=FALSE)

head(read.csv("ParameterCalibration/model.runs_calib.csv"))
head(read.csv("RDM Outputs/historic_calib.csv"))



#Gráfica

## ------------------------------------------------------------------
## CONFIGURA PAQUETES Y RUTAS
## ------------------------------------------------------------------
root     <- "C:/~Model Development/TechChange-RDM/"
proj_lib <- file.path(root, "Rlibraries")
.libPaths(c(proj_lib, .libPaths()))

## si aún faltara ggplot2 o ggrepel dentro de proj_lib, los instala
for (pkg in c("ggplot2", "ggrepel", "data.table")) {
  if (!requireNamespace(pkg, lib.loc = proj_lib, quietly = TRUE)) {
    install.packages(pkg,
                     lib         = proj_lib,
                     dependencies = TRUE,
                     repos        = "https://cloud.r-project.org")
  }
}
library(ggplot2, lib.loc = proj_lib)
library(ggrepel, lib.loc = proj_lib)
library(data.table)

## ------------------------------------------------------------------
## CARGA DATOS Y LIMPIEZA
## ------------------------------------------------------------------
df <- fread(file.path(root, "RDM Outputs", "historic_calib.csv"))

## quita posibles filas sin región
df <- df[Region %in% c("OECD", "NONOECD", "N", "S")]

## Si todavía tienes “N” y “S”, cámbialas por OECD/NONOECD
df[Region == "N", Region := "OECD"]
df[Region == "S", Region := "NONOECD"]

## etiqueta tramo histórico vs simulado
df[ , tramo := ifelse(time < 2012, "Histórico", "Simulado")]

## último punto por región para la etiqueta
last_pts <- df[ , .SD[time == max(time)], by = Region]

## ------------------------------------------------------------------
## GRÁFICA
## ------------------------------------------------------------------
setDT(df)   # si todavía no es data.table

## ── 1.-  último año por región ─────────────────────────────
lab_pts <- df[Region %in% c("OECD","NONOECD"),
              .SD[ time == max(time) ],       # • último año
              by = Region]

## ── 2.-  gráfico con etiqueta numérica (2 decimales) ──────
ggplot(df[Region %in% c("OECD","NONOECD")], 
       aes(time, Y, colour = Region)) +
  geom_line() +
  geom_text(data = lab_pts,
            aes(label = sprintf("%.2f", Y)),
            hjust = 0,                # alinea a la izquierda del texto
            nudge_x = 2,              # desplaza 2 unidades a la derecha
            show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) + # 10 % extra
  coord_cartesian(clip = "off") +          # ← permite dibujar fuera
  theme_minimal() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))  # margen dcho. más ancho


setDT(df)   # por si aún no es data.table

# QUÉ regla usar depende de tu caso:
# aquí conservo la PRIMERA fila de cada Región-año
df_clean <- df[
  Region %in% c("OECD", "NONOECD") & time >= 2008 & time <= 2018,
  .SD[1],                                # ⇦ cambia por sum(), mean(), etc.
  by = .(Region, time)
]

setDT(df)   # por si aún no es data.table

# QUÉ regla usar depende de tu caso:
# aquí conservo la PRIMERA fila de cada Región-año
df_clean <- df[
  Region %in% c("OECD", "NONOECD") & time >= 2008 & time <= 2018,
  .SD[1],                                # ⇦ cambia por sum(), mean(), etc.
  by = .(Region, time)
]

df_clean[ , .(Region, time, PIB = round(Y, 2))][order(Region, time)]



