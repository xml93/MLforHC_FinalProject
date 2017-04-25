# Load libraries
library(plyr)
library(dplyr)
library(data.table)

# Load tables with needed data
adm <- data.frame(read.csv(file = "/Users/Xinmi/git/MLforHC_FinalProject/adm.csv", header = TRUE))
adm.original <- data.frame(read.csv(file = "/Users/Xinmi/git/MLforHC_FinalProject/ADMISSIONS.csv", header = TRUE))
ds <- data.frame(read.csv(file = "/Users/Xinmi/git/MLforHC_FinalProject/ds.csv", header = TRUE))
pts <- data.frame(read.csv(file = "/Users/Xinmi/git/MLforHC_FinalProject/pts.csv", header = TRUE))
surgpts <- data.frame(read.csv(file = "/Users/Xinmi/git/MLforHC_FinalProject/surgpts.csv", header = TRUE))
rx <- fread("/Users/Xinmi/git/MLforHC_FinalProject/PRESCRIPTIONS.csv", header = T, sep = ",") %>% 
  select(SUBJECT_ID, HADM_ID, DRUG, FORMULARY_DRUG_CD)%>% 
  filter(SUBJECT_ID %in% surgpts$SUBJECT_ID)
cesurg <- fread('/Users/Xinmi/git/MLforHC_FinalProject/cesurg.csv', header = T, sep = ',')

# Clean the table to leave only the most recent admission for each patient
adm2 <- adm.original %>% 
  filter(SUBJECT_ID %in% adm$SUBJECT_ID) %>% 
  group_by(SUBJECT_ID) %>%
  filter(as.numeric(ADMITTIME) == max(as.numeric(ADMITTIME))) %>%
  select(SUBJECT_ID, HADM_ID, DIAGNOSIS)

# Since multiple diagnoses are assigned with one patient, and the SEQ_NUM shows the priority
# of diagnoses, here only the prior diagnosis is kept.
ds2 <- ds %>%
  select(SUBJECT_ID, HADM_ID, SEQ_NUM, ICD9_CODE, SHORT_TITLE) %>% 
  filter(HADM_ID %in% adm2$HADM_ID & SEQ_NUM == 1)

rx2 <- rx %>%
  select(SUBJECT_ID, HADM_ID, DRUG, FORMULARY_DRUG_CD) %>% 
  filter(HADM_ID %in% adm2$HADM_ID)

# Select only the top 10 commonly used drugs (based on drug code)
drug10 <- table(rx2$FORMULARY_DRUG_CD) %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  head(10)
  
rx3 <- rx2 %>%
  filter(FORMULARY_DRUG_CD == drug10)

rx4 <- cbind(rx3[,1:3], model.matrix( ~ FORMULARY_DRUG_CD - 1, data=rx3)) %>%
  select(-DRUG)

SUBJECT_ID <- unique(rx4$SUBJECT_ID)
rx4.list <- do.call(rbind, by(rx4[,c(3:12)], rx4$SUBJECT_ID, FUN=colSums))
rx4.table <- as.data.frame(cbind(SUBJECT_ID, rx4.list))

# One patient can be given the same drug multiple times during one admission. Since we only
# care whether the patient is given this drug, all non-zero numbers in drug columns are mapped
# to 1.

rx5 <- cbind(SUBJECT_ID, apply(rx4.table[,2:11], 2, FUN = function(x) {as.numeric(x!=0)})) %>%
  as.data.frame()
colnames(rx5) <- c("SUBJECT_ID","D5W250", "FURO40I", "INSULIN", "LR1000", "MAG2PM", "METO5I", 
                          "NACLFLUSH", "NS1000", "NS250", "NS500")

# Transform 
cesurg2 <- cesurg %>%
  select(SUBJECT_ID, HADM_ID, ITEMID, VALUE, VALUENUM, VALUEUOM, LABEL) %>% 
  filter(HADM_ID %in% adm2$HADM_ID)

# Combine the tables into one 
df <- pts %>%
  inner_join(adm2 %>% select(DIAGNOSIS), by="SUBJECT_ID") %>%
  left_join(ds2 %>% select(SUBJECT_ID, ICD9_CODE, SHORT_TITLE), by="SUBJECT_ID") %>%
  left_join(rx5, by="SUBJECT_ID") 
  
  

# Patients with GI surgery
GIpts <- subset(adm2, grepl("GI | ABDOMINAL | GASTROINTESTINAL", adm$DIAGNOSIS))