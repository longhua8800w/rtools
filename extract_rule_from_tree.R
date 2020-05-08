library("tidyrules")
library("dplyr")
library("C50")
library("pander")
c5_model <- C5.0(Species ~ ., data = iris,rule)
mod1 <- C5.0(Species ~ ., data = iris)
plot(c5_model)


tidy_rules <- tidyRules(c5_model)

################

library(rpart.plot)
library(partykit)

data(stagec)
fit <- rpart(
  formula = pgstat ~ age + eet + g2 + grade + gleason + ploidy,
  data = stagec,
  method = "class",
  control = rpart.control(cp = 0.05)
)

party_obj <- as.party.rpart(fit, data = TRUE)
decisions <- partykit:::.list.rules.party(party_obj)
cat(paste(decisions, collapse = "\n"))


###########

library(rpart.plot)
data(stagec)
fit <- rpart(formula = pgstat ~ ., data = stagec, method = "class", control=rpart.control(cp=0.05))
rpart.rules(fit)
rpart.rules(fit, roundint=FALSE, clip.facs=TRUE)


#####################

data(iris)
library(RRF)
library(inTrees)
X <- iris[,1:(ncol(iris)-1)]
target <- iris[,"Species"]
rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF
treeList <- RF2List(rf)
ruleExec <- extractRules(treeList,X)
ruleExec <- unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
ruleMetric <- pruneRule(ruleMetric,X,target) # prune each rule
#ruleMetric <- selectRuleRRF(ruleMetric,X,target) # rule selection
learner <- buildLearner(ruleMetric,X,target)
pred <- applyLearner(learner,X)
read <- presentRules(learner,colnames(X)) # more readable format
# format the rule and metrics as a table in latex code
library(xtable)
print(xtable(read), include.rownames=FALSE)
print(xtable(ruleMetric[1:2,]), include.rownames=FALSE)




library(data.table)
library(xgboost)
# test data set 1: iris
X <- within(iris,rm("Species")); Y <- iris[,"Species"]
X <- within(iris,rm("Species")); Y <- iris[,"Species"]
model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 20,
               objective = "multi:softprob", num_class = 3 )
tree_list <- XGB2List(xgb,model_mat)
ruleExec <- extractRules(tree_list,X)
ruleExec <- unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
ruleMetric <- pruneRule(ruleMetric,X,target)








