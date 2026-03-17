#Wave 1 analysis for the paper - environmental factors#

# Load necessary libraries
library(readr)
library(dplyr)
library(epitools)
library(gtsummary)
library(dplyr)
library(epiR)

##table 1 stats##

##sex##

sa_data %>%
  group_by(i_subject_type,msex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

ke_data %>%
  group_by(i_subject_type,msex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

wave1_comb_2025 %>%
  group_by(msex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##mother's age at birth##
sa_data <- sa_data %>%
  mutate(mum_agebirth = case_when(
    mom_age_at_birth != -555 & !is.na(mom_age_at_birth) ~ mom_age_at_birth,  # Use mom_age1 if valid
    mom_age_at_birthb != -555 & !is.na(mom_age_at_birthb) ~ mom_age_at_birthb,  # Else, use mom_age2 if valid
    TRUE ~ NA_real_  # If both are missing or -555, set to NA
  ))

sa_mum_agebirth <- sa_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(mum_agebirth, na.rm = TRUE),
    SD = sd(mum_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(mum_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(sa_mum_agebirth)

ke_data <- ke_data %>%
  mutate(mum_agebirth = case_when(
    mom_age_at_birth %in% c(-555, -444, 99) ~ mom_age_at_birthb,  # If invalid, check mom_age_at_birthb
    !is.na(mom_age_at_birth) ~ mom_age_at_birth,  # Use mom_age_at_birth if valid
    mom_age_at_birthb %in% c(-555, -444, 99) ~ NA_real_,  # If invalid, set NA
    !is.na(mom_age_at_birthb) ~ mom_age_at_birthb,  # Use mom_age_at_birthb if valid
    TRUE ~ NA_real_  # If both are missing/invalid, set to NA
  ))

ke_data <- ke_data %>%
  mutate(mum_agebirth = case_when(
    mum_agebirth %in% c(-555, -444, 99) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ mum_agebirth  # Keep other values unchanged
  ))

ke_mum_agebirth <- ke_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(mum_agebirth, na.rm = TRUE),
    SD = sd(mum_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(mum_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(ke_mum_agebirth)

##father's age at birth
##SA

sa_data <- sa_data %>%
  mutate(dad_agebirth = case_when(
    dad_age_at_birth %in% c(-555, -444) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ dad_age_at_birth  # Assign valid values
  ))


sa_dad_agebirth <- sa_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(dad_agebirth, na.rm = TRUE),
    SD = sd(dad_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(dad_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(sa_dad_agebirth)

##KE

ke_data <- ke_data %>%
  mutate(dad_agebirth = case_when(
    dad_age_at_birth %in% c(-555, -444, 99) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ dad_age_at_birth  # Assign valid values
  ))

ke_dad_agebirth <- ke_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(dad_agebirth, na.rm = TRUE),
    SD = sd(dad_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(dad_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(ke_dad_agebirth)

##parity##
##SA

sa_data <- sa_data %>%
  mutate(parity = case_when(
    fam_hist_q2_new %in% c(-555) ~ fam_hist_q1b,  # Use fam_hist_q1b if fam_hist_q2new is invalid
    TRUE ~ fam_hist_q2_new  # Otherwise, use fam_hist_q2new
  ))

sa_data <- sa_data %>%
  mutate(parity = case_when(
    parity %in% c(-555, -444) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ parity  # Assign valid values
  ))


sa_parity <- sa_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(parity, na.rm = TRUE),
    SD = sd(parity, na.rm = TRUE),
    Sample_Size = sum(!is.na(parity)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(sa_parity)

##KE

ke_data <- ke_data %>%
  mutate(parity = case_when(
    fam_hist_q2_new %in% c(-555, -444) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ fam_hist_q2_new  # Assign valid values
  ))


ke_parity <- ke_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(parity, na.rm = TRUE),
    SD = sd(parity, na.rm = TRUE),
    Sample_Size = sum(!is.na(parity)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(ke_parity)


##child age##

sa_childage <- sa_data %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(i_subj_age, na.rm = TRUE),
    SD = sd(i_subject_type, na.rm = TRUE),
    Sample_Size = sum(!is.na(i_subj_age)),
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(sa_childage)


###maternal education
##SA##
sa_data <- sa_data %>%
  mutate(maternal_edu = case_when(
    sa_asset_mom_edu == -1 ~ "Unknown",  # -1 becomes "Unknown/Missing"
    sa_asset_mom_edu == 0 ~ "0",                # 0 stays "0"
    sa_asset_mom_edu >= 1 & sa_asset_mom_edu <= 6 ~ "1",  # 1-6 become "1"
    TRUE ~ as.character(sa_asset_mom_edu)  # Keep other values unchanged
  ))


sa_data %>%
  group_by(i_subject_type,maternal_edu) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

sa_data <- sa_data %>%
  mutate(paternal_edu = case_when(
    sa_asset_spouse_edu == -1 ~ "Unknown",  # -1 becomes "Unknown/Missing"
    sa_asset_spouse_edu == 0 ~ "0",                # 0 stays "0"
    sa_asset_spouse_edu >= 1 & sa_asset_spouse_edu <= 6 ~ "1",  # 1-6 become "1"
    TRUE ~ as.character(sa_asset_spouse_edu)  # Keep other values unchanged
  ))

##KE##

ke_data %>%
  group_by(i_subject_type,k_asset_sch_yn) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


##paternal education##
##SA
sa_data %>%
  group_by(i_subject_type,paternal_edu) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##KE
ke_data %>%
  group_by(i_subject_type,k_asset_sch_yn2) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


# Run logistic regression

##SOUTH AFRICA##

install.packages("logistf")
library(logistf)

sa_data <- sa_data %>%
  mutate(across(c(mom_fever_pregn_final, birth_hist_8, hie, jaundice_yn, 
                    hosp_meningitis, maternal_edu, paternal_edu), 
                ~ replace(.x, .x %in% c(-555, -444, 777, "Unknown"), NA)))


sa_sociomed_model <- glm(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                        hosp_meningitis + i_subj_age + msex + maternal_edu + paternal_edu, 
             data = sa_data, family = binomial)

# View results
summary(sa_sociomed_model)

exp(cbind(OR = coef(sa_sociomed_model), confint(sa_sociomed_model)))

tbl_regression(sa_sociomed_model, exponentiate = TRUE)


library(broom)
sa_tidy_model <- tidy(sa_sociomed_model, exponentiate = TRUE, conf.int = TRUE)

sa_tidy_model <- subset(sa_tidy_model, term != "(Intercept)")

library(ggplot2)

# Clean up labels (optional)
sa_tidy_model$term <- gsub("_", " ", sa_tidy_model$term)

ggplot(sa_tidy_model, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  ylim(0, 5) +  # adjust this based on your CI range
  labs(title = "Sociomedical factors in NDDs - Cape Town",
       y = "Odds Ratio", x = "") +
  theme_minimal()


##penalised model with firth correction##

sa_model_firth <- logistf(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                                    hosp_meningitis + i_subj_age + msex + maternal_edu + paternal_edu, data = sa_data)
summary(sa_model_firth)

exp(cbind(OR = coef(sa_model_firth), confint(sa_model_firth)))

tbl_regression(sa_model_firth, exponentiate = TRUE)

# Create a data frame of ORs and CIs

sa_tidy_model_firth <- data.frame(
  term = names(sa_model_firth$coefficients),
  estimate = exp(sa_model_firth$coefficients),
  conf.low = exp(sa_model_firth$ci.lower),
  conf.high = exp(sa_model_firth$ci.upper),
  p.value = sa_model_firth$prob
)

library(ggplot2)

ggplot(sa_tidy_model_firth, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  ylim(0, 10) + 
  labs(title = "Firth-Adjusted Odds Ratios for NDD",
       y = "Odds Ratio", x = "") +
  theme_minimal()


ggplot(sa_tidy_model_firth, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_log10() + 
  labs(title = "Firth-Adjusted Odds Ratios for NDD",
       y = "Odds Ratio (log scale)", x = "") +
  theme_minimal()


sa_tidy_model_firth <- data.frame(
  term = rownames(sa_model_firth$coefficients),
  estimate = exp(sa_model_firth$coefficients),
  conf.low = exp(sa_model_firth$ci.lower),
  conf.high = exp(sa_model_firth$ci.upper),
  p.value = sa_model_firth$prob
)

# Remove the intercept for plotting (optional)
sa_tidy_model_firth <- subset(sa_tidy_model_firth, term != "(Intercept)")


library(broom)
library(dplyr)
library(ggplot2)

sa_tidy_model_firth <- tidy(sa_model_firth, exponentiate = TRUE, conf.int = TRUE)

sa_tidy_model_firth <- subset(sa_model_firth, term != "(Intercept)")

# Step 1: Filter out intercept and adjustment covariates

label_map <- c(
  hie = "HIE",
  mom_fever_pregn_final = "Maternal infection in pregnancy",
  jaundice_yn = "Neonatal jaundice",
  hosp_meningitis = "Brain infections",
  birth_hist_8 = "Labour and birth complications"
)

sa_tidy_model_filtered <- sa_tidy_model_firth %>%
  filter(
    term != "(Intercept)",
    !term %in% c("i_subj_age", "msex", "paternal_edu1", "maternal_edu1")
  ) %>%
  mutate(
    label = recode(term, !!!label_map),  # replace with pretty labels
    label = factor(label, levels = rev(label))  # maintain order, reverse for top-to-bottom
  )

# Format text labels (OR [CI])
sa_tidy_model_filtered <- sa_tidy_model_filtered %>%
  mutate(
    or_ci = sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high)
  )

# Plot

ggplot(sa_tidy_model_filtered, aes(x = label, y = estimate, color = label)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  coord_flip() +
  ylim(0, 20) +
  labs(
    title = "Sociomedical Factors Associated with NDDs — Cape Town",
    y = "Odds Ratio (95% CI)",
    x = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  )

###KENYA###
ke_data <- ke_data %>%
  mutate(across(c(mom_fever_pregn_final, birth_hist_8, hie, jaundice_yn, 
                  hosp_malaria, k_asset_sch_yn, k_asset_sch_yn2), 
                ~ replace(.x, .x %in% c(-555, -444, 777), NA)))


ke_sociomed_model <- glm(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                           hosp_malaria + i_subj_age + msex + k_asset_sch_yn + k_asset_sch_yn2, 
                         data = ke_data, family = binomial)

# View results
summary(ke_sociomed_model)

exp(cbind(OR = coef(ke_sociomed_model), confint(ke_sociomed_model)))

tbl_regression(ke_sociomed_model, exponentiate = TRUE)

##penalised model with firth correction##

ke_model_firth <- logistf(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                            hosp_malaria + i_subj_age + msex + k_asset_sch_yn + k_asset_sch_yn2, data = ke_data)
summary(ke_model_firth)

exp(cbind(OR = coef(ke_model_firth), confint(ke_model_firth)))

tbl_regression(ke_model_firth, exponentiate = TRUE)

# Create a data frame of ORs and CIs

ke_tidy_model_firth <- data.frame(
  term = names(ke_model_firth$coefficients),
  estimate = exp(ke_model_firth$coefficients),
  conf.low = exp(ke_model_firth$ci.lower),
  conf.high = exp(ke_model_firth$ci.upper),
  p.value = ke_model_firth$prob
)

# Step 1: Filter out intercept and adjustment covariates

label_map <- c(
  hie = "HIE",
  mom_fever_pregn_final = "Maternal infection in pregnancy",
  jaundice_yn = "Neonatal jaundice",
  hosp_malaria = "Cerebral malaria",
  birth_hist_8 = "Labour and birth complications"
)

ke_tidy_model_filtered <- ke_tidy_model_firth %>%
  filter(
    term != "(Intercept)",
    !term %in% c("i_subj_age", "msex", "k_asset_sch_yn", "k_asset_sch_yn2")
  ) %>%
  mutate(
    label = recode(term, !!!label_map),  # replace with pretty labels
    label = factor(label, levels = rev(label))  # maintain order, reverse for top-to-bottom
  )

# Format text labels (OR [CI])
ke_tidy_model_filtered <- ke_tidy_model_filtered %>%
  mutate(
    or_ci = sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high)
  )

# Plot

ggplot(ke_tidy_model_filtered, aes(x = label, y = estimate, color = label)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  coord_flip() +
  ylim(0, 25) +
  labs(
    title = "Sociomedical Factors Associated with NDDs — Kilifi",
    y = "Odds Ratio (95% CI)",
    x = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  )

##logistic regression for the combined sample##
##first we prepare the entire dataset
##mother's age at birth##
wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(mum_agebirth = case_when(
    mom_age_at_birth != -555 & !is.na(mom_age_at_birth) ~ mom_age_at_birth,  # Use mom_age1 if valid
    mom_age_at_birthb != -555 & !is.na(mom_age_at_birthb) ~ mom_age_at_birthb,  # Else, use mom_age2 if valid
    TRUE ~ NA_real_  # If both are missing or -555, set to NA
  ))

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(mum_agebirth = case_when(
    mum_agebirth %in% c(-444, 99) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ mum_agebirth  # Keep other values unchanged
  ))

combined_mum_agebirth <- wave1_comb_2025 %>%
  summarise(
    Mean = mean(mum_agebirth, na.rm = TRUE),
    SD = sd(mum_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(mum_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_mum_agebirth)

combined_mum_agebirth <- wave1_comb_2025 %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(mum_agebirth, na.rm = TRUE),
    SD = sd(mum_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(mum_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_mum_agebirth)

##paternal age at birth
wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(dad_agebirth = case_when(
    dad_age_at_birth %in% c(-555, -444, 99) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ dad_age_at_birth  # Assign valid values
  ))

combined_dad_agebirth <- wave1_comb_2025 %>%
  summarise(
    Mean = mean(dad_agebirth, na.rm = TRUE),
    SD = sd(dad_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(dad_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_dad_agebirth)

combined_dad_agebirth <- wave1_comb_2025 %>%
  group_by(i_subject_type) %>%
  summarise(
    Mean = mean(dad_agebirth, na.rm = TRUE),
    SD = sd(dad_agebirth, na.rm = TRUE),
    Sample_Size = sum(!is.na(dad_agebirth)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_dad_agebirth)

##child age
combined_child_agebirth <- wave1_comb_2025 %>%
  summarise(
    Mean = mean(i_subj_age, na.rm = TRUE),
    SD = sd(i_subj_age, na.rm = TRUE),
    Sample_Size = sum(!is.na(i_subj_age)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_child_agebirth)

##maternal education

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(
    mom_educ_level = case_when(
      sa_asset_mom_edu == -1 ~ NA,
      sa_asset_mom_edu == 0 ~ "0",
      sa_asset_mom_edu >= 1 & sa_asset_mom_edu <= 6 | k_asset_sch_yn == 1 ~ "1",
      k_asset_sch_yn == 0 ~ "0",
      TRUE ~ NA_character_ 
    )
  )

wave1_comb_2025%>%
  group_by(mom_educ_level) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##paternal education

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(
    dad_educ_level = case_when(
      sa_asset_spouse_edu == -1 ~ NA,
      sa_asset_spouse_edu == 0 ~ "0",
      sa_asset_spouse_edu >= 1 & sa_asset_spouse_edu <= 6 | k_asset_sch_yn2 == 1 ~ "1",
      k_asset_sch_yn2 == 0 ~ "0",
      TRUE ~ NA_character_  
    )
  )

wave1_comb_2025%>%
  group_by(dad_educ_level) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

wave1_comb_2025%>%
  group_by(fam_hist_q2_new) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##parity

wave1_comb_2025<- wave1_comb_2025 %>%
  mutate(parity = case_when(
    fam_hist_q2_new %in% c(-555) ~ fam_hist_q1b,  # Use fam_hist_q1b if fam_hist_q2new is invalid
    TRUE ~ fam_hist_q2_new  # Otherwise, use fam_hist_q2new
  ))

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(parity = case_when(
    parity %in% c(-555, -444) ~ NA_real_,  # Convert invalid values to NA
    TRUE ~ parity  # Assign valid values
  ))

combined_parity <- wave1_comb_2025 %>%
  summarise(
    Mean = mean(parity, na.rm = TRUE),
    SD = sd(parity, na.rm = TRUE),
    Sample_Size = sum(!is.na(parity)),  # Count non-missing values
    Lower_95CI = Mean - qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size)),
    Upper_95CI = Mean + qt(0.975, df = Sample_Size - 1) * (SD / sqrt(Sample_Size))
  )
View(combined_parity)

##fever during pregnancy

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(mom_fever_pregn_final = case_when(
    mom_fever_pregnb_yn %in% c(-555, -444, 777) & !mom_fever_pregn_yn %in% c(-555, -444, 777) ~ mom_fever_pregn_yn,
    mom_fever_pregn_yn %in% c(-555, -444, 777) & !mom_fever_pregnb_yn %in% c(-555, -444, 777) ~ mom_fever_pregnb_yn,
    TRUE ~ mom_fever_pregnb_yn  # Keep mom_fever_pregnb_yn if it's valid or both are invalid
  ))

##HIE - recode the question whether child cried at birth

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(hie = case_when(
    birth_hist_7a == 1 ~ 0,  # Recode 1 as 0 (No hie)
    birth_hist_7a == 0 ~ 1,  # Recode 0 as 1 (Yes hie)
    TRUE ~ NA_real_  # Keep NA values as NA
  ))


###actual logistic regression

wave1_sociomed_model <- glm(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                           i_subj_age + msex + mom_educ_level + dad_educ_level, 
                         data = wave1_comb_2025, family = binomial)

# View results
summary(wave1_sociomed_model)

exp(cbind(OR = coef(wave1_sociomed_model), confint(wave1_sociomed_model)))

tbl_regression(wave1_sociomed_model, exponentiate = TRUE)

##penalised model with firth correction##

wave1_comb_2025 <- wave1_comb_2025 %>%
  mutate(across(c(mom_fever_pregn_final, birth_hist_8, hie, jaundice_yn, 
                  dad_educ_level, mom_educ_level), 
                ~ replace(.x, .x %in% c(-555, -444, 777), NA)))

wave1_model_firth <- logistf(ndd_control ~ mom_fever_pregn_final + birth_hist_8 + hie + jaundice_yn +
                             i_subj_age + msex + mom_educ_level + dad_educ_level, data = wave1_comb_2025)
summary(wave1_model_firth)

exp(cbind(OR = coef(wave1_model_firth), confint(wave1_model_firth)))

tbl_regression(wave1_model_firth, exponentiate = TRUE)

# Create a data frame of ORs and CIs

wave1_tidy_model_firth <- data.frame(
  term = names(wave1_model_firth$coefficients), 
  estimate = exp(wave1_model_firth$coefficients),
  conf.low = exp(wave1_model_firth$ci.lower),
  conf.high = exp(wave1_model_firth$ci.upper),
  p.value = wave1_model_firth$prob
)

# Step 1: Filter out intercept and adjustment covariates

label_map <- c(
  hie = "HIE",
  mom_fever_pregn_final = "Maternal infection in pregnancy",
  jaundice_yn = "Neonatal jaundice",
  birth_hist_8 = "Labour and birth complications"
)

wave1_tidy_model_filtered <- wave1_tidy_model_firth %>%
  filter(
    term != "(Intercept)",
    !term %in% c("i_subj_age", "msex", "dad_educ_level1", "mom_educ_level1")
  ) %>%
  mutate(
    label = recode(term, !!!label_map),  # replace with pretty labels
    label = factor(label, levels = rev(label))  # maintain order, reverse for top-to-bottom
  )

# Format text labels (OR [CI])
wave1_tidy_model_filtered <- wave1_tidy_model_filtered %>%
  mutate(
    or_ci = sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high)
  )

# Plot
ggplot(wave1_tidy_model_filtered, aes(x = label, y = estimate, color = label)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  coord_flip() +
  ylim(0, 10) +
  labs(
    title = "Sociomedical Factors Associated with NDDs — Wave 1 Cohort",
    y = "Odds Ratio (95% CI)",
    x = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  )

##save datasets

write.csv(wave1_comb_2025, "wave1_merged_sake_09042025.csv", row.names = FALSE)

write.csv(sa_data, "wave1_sadata_09042025.csv", row.names = FALSE)

write.csv(ke_data, "wave1_kedata_09042025.csv", row.names = FALSE)

##exploring true missingness
##mother age

wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    mum_age_flag = case_when(
      is.na(mum_agebirth) & neuromed_resp_rel == 1 ~ "truly_missing",
      is.na(mum_agebirth) & neuromed_resp_rel != 1 ~ "coded_unknown",
      TRUE ~ "valid"
    )
  )

wave1_comb_2025_1%>%
  group_by(i_subject_type,location_id, mum_age_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

wave1_comb_2025_1%>%
  group_by(mum_age_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#father age

wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    dad_age_flag = case_when(
      is.na(dad_agebirth) & neuromed_resp_rel == 2 ~ "truly_missing",
      is.na(dad_agebirth) & neuromed_resp_rel %in% c(1, 999) ~ "coded_unknown",
      TRUE ~ "valid"
    )
  )

wave1_comb_2025_1%>%
  group_by(dad_age_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


##maternal education

wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    mum_educ_flag = case_when(
      is.na(mom_educ_level) & i_resp_rel == 1 ~ "truly_missing",
      is.na(mom_educ_level) & i_resp_rel != 1 ~ "coded_unknown",
      TRUE ~ "valid"
    )
  )

wave1_comb_2025_1%>%
  group_by(mum_educ_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)



wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    # Handle Site A: South Africa
    mom_educ_sa = case_when(
      sa_asset_mom_edu == -1 ~ NA_real_,
      sa_asset_mom_edu == 0 ~ 0,
      sa_asset_mom_edu %in% 1:2 ~ 1,
      sa_asset_mom_edu %in% 3:4 ~ 2,
      sa_asset_mom_edu %in% 5:6 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Handle Site K: Kenya 
    mom_educ_k = case_when(
      k_asset_sch_yn == 0 ~ 0,
      k_asset_edu %in% 1:2 ~ 1,
      k_asset_edu %in% 3:4 ~ 2,
      k_asset_edu %in% 5:6 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Combine both
    mom_educ_categ = coalesce(mom_educ_sa, mom_educ_k),
    )


wave1_comb_2025_1%>%
  group_by(i_subject_type,location_id, mom_educ_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

momeducateg_table <- wave1_comb_2025_1%>%
  group_by(i_subject_type,location_id, mom_educ_categ) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  group_by(i_subject_type, location_id) %>%
  mutate(Row_Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  ungroup() %>%
  group_by(i_subject_type, location_id) %>%
  mutate(Column_Percentage = round(Frequency / sum(Frequency) * 100, 2))

print(momeducateg_table)

wave1_comb_2025_1%>%
  group_by(mom_educ_categ) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##paternal education

wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    dad_educ_flag = case_when(
      is.na(dad_educ_level) & i_resp_rel == 2 ~ "truly_missing",
      is.na(dad_educ_level) & i_resp_rel != 2~ "coded_unknown",
      k_asset_edu_2 == 99 ~ "truly_missing",  
      TRUE ~ "valid"
    )
  )

wave1_comb_2025_1%>%
  group_by(i_subject_type,location_id, dad_educ_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

wave1_comb_2025_1%>%
  group_by(i_subject_type,location_id,dad_educ_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

wave1_comb_2025_1%>%
  group_by(dad_educ_flag) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


wave1_comb_2025_1 <- wave1_comb_2025 %>%
  mutate(
    #Site: South Africa
    dad_educ_sa = case_when(
      sa_asset_spouse_edu == -1 ~ NA_real_,
      sa_asset_spouse_edu == 0 ~ 0,
      sa_asset_spouse_edu %in% 1:2 ~ 1,
      sa_asset_spouse_edu %in% 3:4 ~ 2,
      sa_asset_spouse_edu %in% 5:6 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Site: Kenya 
    dad_educ_ke = case_when(
      k_asset_edu_2 == 99 ~ NA_real_,
      k_asset_sch_yn2 == 0 ~ 0,
      k_asset_edu_2 %in% 1:2 ~ 1,
      k_asset_edu_2 %in% 3:4 ~ 2,
      k_asset_edu_2 %in% 5:6 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Combine both
    dad_educ_categ = coalesce(dad_educ_sa, dad_educ_ke),
  )

wave1_comb_2025_1%>%
  group_by(dad_educ_categ) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

dadeducateg_table <- wave1_comb_2025_1 %>%
  group_by(i_subject_type,location_id, dad_educ_categ) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  group_by(i_subject_type,location_id) %>%
  mutate(Row_Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  ungroup() %>%
  group_by(i_subject_type,location_id) %>%
  mutate(Column_Percentage = round(Frequency / sum(Frequency) * 100, 2))

print(dadeducateg_table)


##save datasets

write.csv(wave1_comb_2025, "wave1_merged_sake_15042025.csv", row.names = FALSE)

write.csv(sa_data, "wave1_sadata_15042025.csv", row.names = FALSE)

write.csv(ke_data, "wave1_kedata_15042025.csv", row.names = FALSE)








