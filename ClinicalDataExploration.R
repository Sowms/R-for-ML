#Working on Clinical Data

filename <- "all_feat_best_hsm_10db_onehot.csv"
data <- read.csv(filename, header=TRUE)
print(length(data))
print(dim(data))
train <- data[1:3000,]
test <- data[3001:3091,]
print(dim(train))
print(dim(test))
print(head(train))
print(colnames(train))
print(colnames(test))
train <- na.omit(train)
## get mode of all vars
dat = train
var_mode <- sapply(dat, mode)

## produce error if complex or raw is found
if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")

## get class of all vars
var_class <- sapply(dat, class)

## produce error if an "AsIs" object has "logical" or "character" mode
if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
  stop("matrix variables with 'AsIs' class must be 'numeric'")
}

## identify columns that needs be coerced to factors
ind1 <- which(var_mode %in% c("logical", "character"))

## coerce logical / character to factor with `as.factor`
dat[ind1] <- lapply(dat[ind1], as.factor)
## index of factor columns
fctr <- which(sapply(dat, is.factor))

## factor variables that have skipped explicit conversion in step 2
## don't simply do `ind2 <- fctr[-ind1]`; buggy if `ind1` is `integer(0)`
ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr

## drop unused levels
dat[ind2] <- lapply(dat[ind2], droplevels)
# Create the relationship model.
model <- lm(best_hsm_10dB~(id_exam+id_patient+id_implant+implant_side+age_of_implatation+deaf_ipsi_month+deaf_contra_month+hardhearing_ipsi_month+hardhearing_contra_month+ipsi_AC_at250Hz+ipsi_AC_lowRangeAvg+ipsi_AC_allAvg+contra_AC_at250Hz+contra_AC_lowRangeAvg+contra_AC_allAvg+AC_lowRangeAvg+AC_allAvg+ipsi_BC_at250Hz+ipsi_BC_lowRangeAvg+ipsi_BC_allAvg+contra_BC_at250Hz+contra_BC_lowRangeAvg+contra_BC_allAvg+BC_lowRangeAvg+BC_allAvg+ipsi_AC_monosyllabic_level+ipsi_AC_monosyllabic_meetThres+ipsi_AC_multisyllabic_level+ipsi_AC_multisyllabic_meetThres+ipsi_BC_monosyllabic_level+ipsi_BC_monosyllabic_meetThres+ipsi_BC_multisyllabic_level+ipsi_BC_multisyllabic_meetThres+ipsi_FF_monosyllabic_level+ipsi_FF_monosyllabic_meetThres+ipsi_FF_multisyllabic_level+ipsi_FF_multisyllabic_meetThres+contra_AC_monosyllabic_level+contra_AC_monosyllabic_meetThres+contra_AC_multisyllabic_level+contra_AC_multisyllabic_meetThres+contra_BC_monosyllabic_level+contra_BC_monosyllabic_meetThres+contra_BC_multisyllabic_level+contra_BC_multisyllabic_meetThres+contra_FF_monosyllabic_level+contra_FF_monosyllabic_meetThres+contra_FF_multisyllabic_level+contra_FF_multisyllabic_meetThres+best_mono_ac+best_mono_ff+sex_0+sex_1+language_status_0+language_status_1+language_status_2+language_status_3+deafness_course_l_0+deafness_course_l_1+deafness_course_l_2+deafness_course_l_3+deafness_course_l_4+deafness_course_r_0+deafness_course_r_1+deafness_course_r_2+deafness_course_r_3+deafness_course_r_4+trim_language_native_0+trim_language_native_1+trim_deafness_reason_0+trim_deafness_reason_1+trim_deafness_reason_2+trim_deafness_reason_3+trim_deafness_reason_4+trim_deafness_reason_5+trim_deafness_reason_6+trim_deafness_reason_7+trim_deafness_reason_8+trim_deafness_reason_9+trim_deafness_reason_10+trim_deafness_reason_11+trim_deafness_reason_12+trim_deafness_reason_13+trim_deafness_reason_14+trim_deafness_reason_15+trim_deafness_reason_16+trim_disabilities_additional_0+trim_disabilities_additional_1+trim_disabilities_additional_2+trim_disabilities_additional_3+trim_disabilities_additional_4+trim_disabilities_additional_5+trim_disabilities_additional_6+trim_disabilities_additional_7+trim_disabilities_additional_8+trim_disabilities_additional_9+trim_disabilities_additional_10+trim_disabilities_additional_11+trim_disabilities_additional_12+trim_disabilities_additional_13+trim_disabilities_additional_14+trim_disabilities_additional_15+trim_disabilities_additional_16+trim_disabilities_additional_17+trim_disabilities_additional_18+trim_disabilities_additional_19+trim_implant_type_0+trim_implant_type_1+trim_implant_type_2+trim_implant_type_3+trim_has_hearingaid_experience_0+trim_has_hearingaid_experience_1+trim_has_hearingaid_experience_2), data = dat)

# Show the model.
print(model)