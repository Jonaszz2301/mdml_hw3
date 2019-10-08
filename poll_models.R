#########################################################
#  Messy data and machine learning Homework 3           #
#  Group members: Jiaqian Xing, Yanjun Cheng, Zhen Zhang#
#########################################################

source("library.r")

## A1: Setup  -----------------------------------------------------------------------------------------
# Import data "poll_data"
poll_data <- read_tsv("data_hw3/poll_data.tsv", quote= " ") %>% 
  as.tibble()  

# Covert vote_2008 into a factor and make "john mcCain" the reference category
poll_data <- within(poll_data, vote_2008 <- relevel(as.factor(vote_2008), ref = "john mcCain"))

## A2: Fitting a model  --------------------------------------------------------------------------------
# Fit a binary logistic regression
model <- glm(vote_2008 ~  state + sex + race + age + education + party + ideology + state_contestedness, 
             data = poll_data,
             family = 'binomial')

# Store the coefficient names and the estimates in a tibble
require(broom)    
timodel <- tidy(model)
coeff <- data.frame(timodel[1], timodel[2]) 
names(coeff) <- c("coefficient_name", "coefficient_estimate")

# Order the rows of tibble alphabetically according to coefficient_name
coeff <- arrange(coeff, coefficient_name)

# Save data "question_a2_coefficients.csv"
write_csv(coeff, path='data/question_a2_coefficients.csv')

# Create three counting variables as required
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

# Save data "question_a2_coefficients.csv"
l_vs_co <- data.frame(variable, number_of_levels, number_of_fitted_coefficients)
write_csv(l_vs_co, path='data/question_a2_levels_vs_coefficients.csv')

## A3: Evaluating the model  ---------------------------------------------------------------------------
# Use the binary logistic regression model above to predict probabilities
prob <- predict(model, type = "response")

# Convert probabilistic predictions into binary predictions (for level 0.5 and 0.7)
tiprob <- tidy(prob)
names(tiprob) <- c("predictions_point_5","predicted_probability")
tiprob$predictions_point_5[tiprob$predicted_probability >= 0.5] <- "barack obama"
tiprob$predictions_point_5[tiprob$predicted_probability < 0.5] <- "john mcCain"
tiprob$predictions_point_7 <- tiprob$predictions_point_5
tiprob$predictions_point_5[tiprob$predicted_probability >= 0.7] <- "barack obama"
tiprob$predictions_point_5[tiprob$predicted_probability < 0.7] <- "john mcCain"

# Save data "question_a3.csv"
pr_prob <- data.frame(tiprob[2], tiprob[1], tiprob[3])
write_csv(pr_prob, path='data/question_a3.csv')

# Compute the accuracy, precision, and recall metrics for predictions produced in step 2
TP_2 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_5 == "barack obama")
FP_2 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_5 == "john mcCain")
TN_2 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_5 == "barack obama")
FN_2 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_5 == "john mcCain")
Ac_2 <- (TP_2 + TN_2) / 10000
Pr_2 <- TP_2 / (TP_2 + FP_2)
Re_2 <- TP_2 / (TP_2 + FN_2)

# Compute the accuracy, precision, and recall metrics for predictions produced in step 3
TP_3 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_7 == "barack obama")
FP_3 <- sum(poll_data$vote_2008 == "barack obama" & tiprob$predictions_point_7 == "john mcCain")
TN_3 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_7 == "barack obama")
FN_3 <- sum(poll_data$vote_2008 == "john mcCain" & tiprob$predictions_point_7 == "john mcCain")
Ac_3 <- (TP_3 + TN_3) / 10000
Pr_3 <- TP_3 / (TP_3 + FP_3)
Re_3 <- TP_3 / (TP_3 + FN_3)

## A4: Another model  ----------------------------------------------------------------------------------
# Import data "poll_data_full"
poll_data_full <- read_tsv("data_hw3/poll_data_full.tsv", quote= " ")%>% 
  as.tibble() 

# Predict whether voting for major-party candidate
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

# Filter who only vote for two major-party
full_sub <- subset(poll_data_full, vote_2008 != "other")
full_sub$vote[full_sub$vote_2008 == "barack obama" ] <- 1
full_sub$vote[full_sub$vote_2008 == "john mcCain" ] <- 0

# Predict whether voting for Obama
model_major_ob <- glm(vote ~  state + sex + race + age + education + party + ideology + state_contestedness, 
              data = full_sub,
              family = 'binomial')
poll_data_full$pr_obama_given_major <- predict(model_major_ob, newdata= poll_data_full, type = "response")

# Compute the probabilities for voting obama, mcCain and other
pr_obama <- tiprob_major$pr_major * poll_data_full$pr_obama_given_major
pr_mccain <- tiprob_major$pr_major * (1 - poll_data_full$pr_obama_given_major)
pr_other <- 1 - tiprob_major$pr_major 
pr_major <- tiprob_major$pr_major
pr_obama_given_major <- poll_data_full$pr_obama_given_major

# Generate categorical predictions
qa4 <- data.frame(pr_major, pr_obama_given_major, pr_obama, pr_mccain, pr_other)
qa4$predictions[qa4$pr_obama > qa4$pr_mccain & qa4$pr_obama > qa4$pr_other] <- "barack obama"
qa4$predictions[qa4$pr_mccain > qa4$pr_obama & qa4$pr_mccain > qa4$pr_other] <- "john mcCain"
qa4$predictions[qa4$pr_other > qa4$pr_obama & qa4$pr_other > qa4$pr_mccain] <- "other"

# Save data "question_a4.csv"
write_csv(qa4, path='data/question_a4.csv')

# Create one figure with two subplots
png(file="figures/question_a4.png", width=600, height=350)
par(mfrow=c(1,2))
hist(pr_major)
hist(pr_obama_given_major)
dev.off()
