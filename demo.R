library(devtools)
library(readxl)

#1-IniMedt----------
use_r("IniMedt")
document()
?IniMedt

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

#3-TransImput--------



