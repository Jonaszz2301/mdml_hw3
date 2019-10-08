#########################################################
#  Messy data and machine learning Homework 3           #
#  Group members: Jiaqian Xing, Yanjun Cheng, Zhen Zhang#
#########################################################
source("library.r")

## A1: Setup  -----------------------------------------------------------------------------------------
poll_data <- read_tsv("data_hw3/poll_data.tsv", quote= " ") %>% 
  as.tibble() 
poll_data <- within(poll_data, vote_2008 <- relevel(as.factor(vote_2008), ref = "john mcCain"))

## A2: Fitting a model  --------------------------------------------------------------------------------
model <- glm(vote_2008 ~  state + sex + race + age + education + party + ideology + state_contestedness, 
             data = poll_data,
             family = 'binomial')
require(broom)    
timodel <- tidy(model)
coeff <- data.frame(timodel[1], timodel[2]) 
names(coeff) <- c("coefficient_name", "coefficient_estimate")
coeff <- arrange(coeff, coefficient_name)
write_csv(coeff, path='data/question_a2_coefficients.csv')

variable <- colnames(poll_data[-8])
number_of_levels <- c(length(unique(poll_data$state)),
                      length(unique(poll_data$sex)),
                      length(unique(poll_data$race)),
                      length(unique(poll_data$age)),
                      length(unique(poll_data$education)),
                      length(unique(poll_data$party)),
                      length(unique(poll_data$ideology)),
                      length(unique(poll_data$state_contestedness)))
number_of_fitted_coefficients <- number_of_levels - 1
number_of_fitted_coefficients[8] = 0

l_vs_co <- data.frame(variable, number_of_levels, number_of_fitted_coefficients)
write_csv(l_vs_co, path='data/question_a2_levels_vs_coefficients.csv')

## A3: Evaluating the model  ---------------------------------------------------------------------------

prob <- predict(model, type = "response")
tiprob <- tidy(prob)
names(tiprob) <- c("predictions_point_5","predicted_probability")
tiprob$predictions_point_5[tiprob$predicted_probability >= 0.5] <- "barack obama"
tiprob$predictions_point_5[tiprob$predicted_probability < 0.5] <- "john mcCain"

tiprob$predictions_point_7 <- tiprob$predictions_point_5
tiprob$predictions_point_5[tiprob$predicted_probability >= 0.7] <- "barack obama"
tiprob$predictions_point_5[tiprob$predicted_probability < 0.7] <- "john mcCain"

pr_prob <- data.frame(tiprob[2], tiprob[1], tiprob[3])
write_csv(pr_prob, path='data/question_a3.csv')

compute_2 <- data.frame(poll_data$vote_2008, tiprob$predictions_point_5)

TP_2 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_5 == "barack obama")
FP_2 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_5 == "john mcCain")
TN_2 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_5 == "barack obama")
FN_2 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_5 == "john mcCain")

Ac_2 <- (TP_2 + TN_2) / 10000
Pr_2 <- TP_2 / (TP_2 + FP_2)
Re_2 <- TP_2 / (TP_2 + FN_2)

TP_3 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_7 == "barack obama")
FP_3 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_7 == "john mcCain")
TN_3 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_7 == "barack obama")
FN_3 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_7 == "john mcCain")

Ac_3 <- (TP_3 + TN_3) / 10000
Pr_3 <- TP_3 / (TP_3 + FP_3)
Re_3 <- TP_3 / (TP_3 + FN_3)

## A4: Another model  ----------------------------------------------------------------------------------
poll_data_full <- read_tsv("data_hw3/poll_data_full.tsv", quote= " ")%>% 
  as.tibble() 
poll_data_full$vote[poll_data_full$vote_2008 == "barack obama" ] <- 1
poll_data_full$vote[poll_data_full$vote_2008 == "john mcCain" ] <- 1
poll_data_full$vote[poll_data_full$vote_2008 == "other" ] <- 0
model_major <- glm(vote ~  state + sex + race + age + education + party + ideology + state_contestedness, 
              data = poll_data_full,
              family = 'binomial')
prob_major <- predict(model_major, type = "response")
require(broom)  
tiprob_major <- tidy(prob_major)
names(tiprob_major) <- c("x", "pr_major")

poll_data_full_major <- subset(poll_data_full, vote_2008 != "other")
poll_data_full_major$vote[poll_data_full_major$vote_2008 == "barack obama" ] <- 1
poll_data_full_major$vote[poll_data_full_major$vote_2008 == "john mcCain" ] <- 0

model_major_ob <- glm(vote ~  state + sex + race + age + education + party + ideology + state_contestedness, 
              data = poll_data_full_major,
              family = 'binomial')
prob_major_ob <- predict(model_major_ob, type = "response")
require(broom)  
tiprob_major_ob <- tidy(prob_major_ob)
names(tiprob_major_ob) <- c("x", "pr_obama_given_major")

pr_obama <- tiprob_major$pr_major * tiprob_major_ob$pr_obama_given_major
pr_mccain <- tiprob_major$pr_major * (1 - tiprob_major_ob$pr_obama_given_major)
pr_other <- 1 - tiprob_major$pr_major 
qa4 <- data.frame(tiprob_major$pr_major, tiprob_major_ob$pr_obama_given_major, pr_obama, pr_mccain, pr_other)
qa4$predictions[qa4$pr_obama > qa4$pr_mccain & qa4$pr_obama > qa4$pr_other] <- "barack obama"
qa4$predictions[qa4$pr_mccain > qa4$pr_obama & qa4$pr_mccain > qa4$pr_other] <- "john mcCain"
qa4$predictions[qa4$pr_other > qa4$pr_obama & qa4$pr_other > qa4$pr_mccain] <- "other"
