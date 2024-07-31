# alle skripte

source("./Daten/Datenaufbereitung/#01 - Datenaufbereitung.R")
source("./Daten/Datenaufbereitung/#02 - Recodings - Migrationshintergrund.R")
source("./Daten/Datenaufbereitung/#03 - Recodings - factors.R")
source("./Daten/Datenaufbereitung/#04 - Recodings labels säubern.R")
source("./Daten/Datenaufbereitung/#05 - Recode Aussiedler.R")
source("./Daten/Datenaufbereitung/#07 - Recode sonstige.R")
source("./Daten/Datenaufbereitung/#08 - Recode Herkunft und MH.R")
source("./Daten/Datenaufbereitung/#09 - Recodes Intersektionalität.r")

save(nako, file = "nako.RData")
data_write(nako, "nako.sav")
