# Project 8

#Set Up #####
pacman::p_load(pacman, tidyverse, pscl)

# Prompt 1 #####
breast_cancer_data <- read.csv("Project8.csv")
head(breast_cancer_data, 10)

str(breast_cancer_data)

# Prompt 2 #####
BoxplotPredictionOnTarget <- function(predictor, target){
  ggplot(data = breast_cancer_data,
         aes(target, predictor)) +
    geom_boxplot(aes(col = target),
                 outlier.color = "darkblue",
                 outlier.shape = 1,
                 outlier.size = 5,
                 notch = T,
                 ) +
    ggtitle("Box Plot of Diagnosis") +
    xlab("Diagnosis") +
    ylab("Predictor")
}

BoxplotPredictionOnTarget(breast_cancer_data$area_mean, breast_cancer_data$diagnosis)
BoxplotPredictionOnTarget(breast_cancer_data$area_se, breast_cancer_data$diagnosis)
BoxplotPredictionOnTarget(breast_cancer_data$texture_mean, breast_cancer_data$diagnosis)


iqr<- IQR(breast_cancer_data$area_se, na.rm = TRUE)
Q <- quantile(breast_cancer_data$area_se, na.rm= TRUE)
breast_cancer_data_rm<- subset(breast_cancer_data, area_se > ( Q["25%"] - 1.5*iqr) & area_se < (Q["75%"]+1.5*iqr))
breast_cancer_data_rm$diagnosis <- as.factor(breast_cancer_data_rm$diagnosis)

# Prompt 3A #####
glm.result3a <- glm(diagnosis ~ area_mean,
                    family = binomial,
                    data = breast_cancer_data_rm)
pR2(glm.result3a)

predict.proba <- predict(glm.result3a, breast_cancer_data, type = "response")
breast_cancer_data$predict.proba <- ifelse(predict.proba >= 0.5, "M", "B")
message("The accuracy is ", round(mean(breast_cancer_data$predict.proba == breast_cancer_data$diagnosis), 5))

# Prompt 3B #####
glm.result3b <- glm(diagnosis ~ area_mean + area_se,
                    family = binomial,
                    data = breast_cancer_data_rm)
pR2(glm.result3b)

predict.probb <- predict(glm.result3b, breast_cancer_data, type = "response")
breast_cancer_data$predict.probb <- ifelse(predict.probb >= 0.5, "M", "B")
message("The accuracy is ", round(mean(breast_cancer_data$predict.probb == breast_cancer_data$diagnosis), 5))

# Prompt 3C #####
glm.result3c <- glm(diagnosis ~ area_mean + area_se + texture_mean,
                    family = binomial,
                    data = breast_cancer_data_rm)
pR2(glm.result3c)

predict.probc <- predict(glm.result3c, breast_cancer_data, type = "response")
breast_cancer_data$predict.probc <- ifelse(predict.probc >= 0.5, "M", "B")
message("The accuracy is ", round(mean(breast_cancer_data$predict.probc == breast_cancer_data$diagnosis), 5))

# Prompt 3D #####
glm.result3d <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst,
                    family = binomial,
                    data = breast_cancer_data_rm)
pR2(glm.result3d)

predict.probd <- predict(glm.result3d, breast_cancer_data, type = "response")
breast_cancer_data$predict.probd <- ifelse(predict.probd >= 0.5, "M", "B")
message("The accuracy is ", round(mean(breast_cancer_data$predict.probd == breast_cancer_data$diagnosis), 5))

# Prompt 3E #####
glm.result3e <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst + concavity_mean,
                    family = binomial,
                    data = breast_cancer_data_rm)
pR2(glm.result3e)

predict.probe <- predict(glm.result3e, breast_cancer_data, type = "response")
breast_cancer_data$predict.probe <- ifelse(predict.probe >= 0.5, "M", "B")
message("The accuracy is ", round(mean(breast_cancer_data$predict.probe == breast_cancer_data$diagnosis), 5))

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

#Clear Plots
dev.off()

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

