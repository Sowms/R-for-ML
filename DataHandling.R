filename <- "caste_and_religion_info_ed.csv"
data1 <- read.csv(filename, header=TRUE)
# preview the first 5 rows
head(data1)
data2 <- read.csv("med_case_ed_1.csv", header=TRUE)
head(data2)
data3 <- merge(data1, data2, by.x = "IDPERSON", by.y = "IDPERSON", all.x = TRUE, all.y = FALSE)
head(data3)
head(data3[,c("IDPERSON","ID11")])