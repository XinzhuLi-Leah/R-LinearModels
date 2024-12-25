#线性回归 使用women这个数据集
fit <- lm(weight ~ height, data=women)
fit

plot(women$height,women$weight)
#加入拟合线的两个方法【都需要现有图形才能加入线哦】：
#1.在这个语句中，abline(fit) 会根据 fit 这个线性回归模型绘制一条回归直线到现有的图中。它会根据 fit 模型的截距和斜率，画出 weight 和 height 的回归关系。
abline(fit)
#2.lines() 函数只能在已有的图形上添加线条。如果没有先调用 plot() 创建图形，lines() 会抛出这个错误。
lines(women$height, fitted(fit))

#系数
coefficients(fit)
#预测值
fitted(fit)
#残差
residuals(fit)

#加入二次项
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight)
#看看是一次项/还是二次项更合适？
lines(women$height,fitted(fit2),col="red")
lines(women$height, fitted(fit),col="green")

# fit3  加入三次项
fit3 <- lm (weight~ height+I(height^2)+I(height^3),data=women)
plot(women$height,women$weight)
lines(women$height,fitted(fit),col="blue")
lines(women$height,fitted(fit2),col="red")
lines(women$height,fitted(fit3),col="green")

#多元线性回归 
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fitx <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fitx)

#加入交互效应
fity <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
#	hp:wt: 表示马力和重量的交互效应，即这两个变量的联合影响。如果交互项显著，意味着马力对油耗的影响会因重量不同而有所变化
# 或者反过来，重量对油耗的影响会因马力不同而不同。
summary(fity)

#effects包中effect()函数，展示交互项的结果
#执行这段代码后，你将看到一幅图，展示了在不同车重下，马力与每加仑的英里数之间的关系。
library(effects)
plot(effect("hp:wt", fity,,list(wt=c(2.2, 3.2, 4.2))), multiline=TRUE)

#回归诊断技术（目前为止我们上来就是做，没有任何去证明这个模型是否合适呀）
#OLS回归的统计假设：正态性，独立性，线性，同方差性
#1.正态性-qqplot()
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fitx <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fitx,labels=row.names(states),id=list(method="identify"),simulate=TRUE,main="Q-Q Plot")

#2.独立性（自变量之间相互独立，误差相互独立）
durbinWatsonTest(fitx)
#p值不显著 说明没有自相关性，误差之间独立

#3.同方差
ncvTest(fitx)
#p值不显著 说明满足同方差性（误差项的方差为定值不变）

#4.线性相关（自变量和因变量要是线性相关，非线性就无法用线性模型进行预测）--成分残差图
crPlots(fitx)

#5.多重共线性
vif(fitx)
#均小于10

#回归诊断之后，如果发现了问题，怎么解决，处理违背回归假设的问题的方法如下：
#删除变量，删除观测点，变量变换（log之类的）,使用其他回归方法

#选择最佳模型回归
#1.嵌套模型比较--anova()【必须是嵌套模型啊啊啊啊啊！！】
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fit_a <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
fit_b<- lm(Murder ~ Population + Illiteracy , data=states)
anova(fit_b,fit_a)
#p 值不显著，所以不需要将a中的两个变量添加到模型中，可以考虑删除的，选择b

#2.使用AIC【其实不需要使用嵌套模型也行哦！】
fit_a <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
fit_b<- lm(Murder ~ Population + Illiteracy , data=states)
AIC(fit_b,fit_a)
#b的AIC更小

#3.如果涉及到变量选择（也就是哪些变量最终被选进来会比较好呢）
#3-1 逐步回归 stepwise
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
step(fit)

#3-2 全子集回归
library(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
#regsubsets 函数用于进行回归子集选择。它会尝试不同的自变量组合，以找到最佳模型。nbest=4 表示返回前四个最佳模型。
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +Frost, data=states, nbest=4)
leaps
summary(leaps)
leaps_summary <- summary(leaps)
max_adjr2_index <- which.max(leaps_summary $adjr2)
best_model_by_adjr2 <- leaps_summary$which[max_adjr2_index, ]
#最大的r2就是截距项+populaiton+illiteracy
#图形展示
plot(leaps, scale="adjr2")

