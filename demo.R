library(devtools)
library(readxl)
document()

#1-IniMedt----------
use_r("IniMedt")
document()
?IniMedt

eSet <- IniMedt(PID = "R1",
                Module = "Mediation")

#2-LoadDataMedt--------
#load package
load_all()
#create your own function, copy the tested code into that file
use_r("LoadDataMedt")
#read data
medtdata <- readxl::read_xlsx("E:/ShareCache/RMY_2011110154/2-MediationAnalyses/@mediation_demo_rmy/new_demo/data/Demodt-all-data.xlsx")
medtvoca <- readxl::read_xlsx("E:/ShareCache/RMY_2011110154/2-MediationAnalyses/@mediation_demo_rmy/new_demo/data/Demodt-dictionary.xlsx")
#save example data into your R package
use_data(medtdata)
#revise your data information for use
use_r("medtdata")
#save example voca data into your R package
use_data(medtvoca)
use_r("medtvoca")
#update the changes into your package
document()
?LoadDataMedt

eSet <- LoadDataMedt(eSet = eSet,
                     UseExample = "example1")

#3-TransImput--------
use_r("TransImput")
document()
?TransImput
eSet <- TransImput(eSet = eSet,
                   Group = F,
                   Vars = c("X3","M22","M29"),
                   Method = "lod")

#4-DelMiss--------
use_r("DelMiss")
document()
?DelMiss
eSet <- DelMiss(eSet = eSet)

#5-DelNearZeroVar----
use_r("DelNearZeroVar")
document()
?DelNearZeroVar
eSet <- DelNearZeroVar(eSet = eSet)

#6-TransType----
use_r("TransType")
document()
?TransType
eSet <- TransType(eSet = eSet,
                  Vars = c("C1", "C5", "C6"),
                  TypeTo = "factor")

#7-TransDistr----
use_r("TransDistr")
document()
?TransDistr
eSet <- TransDistr(eSet = eSet,
                   Vars = c("X1", "X2", "X3"),
                   Method = "log10")

#8-TransSubgroup----
use_r("TransSubgroup")
document()
?MedtXlist
?MedtMlist
eSet <- MedtXlist(eSet = eSet)
eSet <- MedtMlist(eSet = eSet)

#9-TransDummy----
use_r("TransDummy")
document()
?TransDummy
eSet <- TransDummy(eSet = eSet,
                   Vars = c("C1", "C5", "C6"))

#10-MedtPairWise----
use_r("MedtPairWise")
document()
?MedtPairWise
eSet <- MedtPairWise(eSet = eSet,
                     VarsY = "Y1",
                     VarsX = c("X1","X2","X3", "X10", "X11", "X12", "X19", "X20", "X21"),
                     VarsM = c("M1","M2","M3","M4","M5","M8","M8"),
                     VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
                     Iter = 500)

#11-MedtRedX----
use_r("MedtRedX")
document()
?MedtRedX
eSet <- MedtRedX(eSet = eSet,
                 VarsY = "Y1",
                 VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
                 VarsM = c("M1","M2","M3"),
                 Method = "mean",
                 Iter = 500)

#12-MedtImptM
use_r("MedtImptM")
document()
?MedtImptM
eSet <- MedtImptM(eSet = eSet,
                  VarsY = "Y1",
                  VarsX = c("X1", "X2", "X3"),
                  VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"))

#13-MedtRedM----
use_r("MedtRedM")
document()
?MedtRedM
eSet <- MedtRedM(eSet = eSet,
                 VarsY = "Y1",
                 VarsX = c("X1", "X2", "X3"),
                 VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
                 Method = "mean",
                 Iter = 500)

#14-MedtRedXM----
use_r("MedtRedXM")
document()
?MedtRedXM
eSet <- MedtRedXM(eSet = eSet,
                  Method = "mean",
                  Iter = 500)

#15-VizMedtPair----
use_r("VizMedtPair")
document()
?VizMedtPair
eSet <- VizMedtPair(eSet = eSet)

#16-VizMedtRedXM----
use_r("VizMedtRedXM")
document()
?VizMedtRedXM
eSet <- VizMedtRedXM(eSet = eSet)
