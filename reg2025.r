# 1장 단순회귀모형
market = read.csv("market-1.csv")
market_lm = lm(Y ~ X, data = market)
summary(market_lm)

plot(market$X, market$Y,
     xlab="인테리어비",
     ylab="총판매액",
     pch=19,
     main="인테리어비와 판매액의 산점도")
abline(market_lm)

anova(market_lm)

pred_frame = data.frame(X = c(5, 6, 10))  # 예측 원하는 X값
predict(market_lm, newdata = pred_frame, interval = "confidence")
predict(market_lm, newdata = pred_frame, interval = "prediction")


# 2장 중회귀모형
market2 = read.csv("market-2.csv")

X = market2[,c(2:3)]
X = cbind(1, X)
Y = market2[,4]
X = as.matrix(X) # design matrix
Y = as.matrix(Y)

# 정규방정식의 해
hat_beta = solve(t(X) %*% X) %*% t(X) %*% Y
hat_beta

# 중회귀분석
market2_lm = lm(Y ~ X1+X2, data=market2)
summary(market2_lm)


# 중회귀모형 분산분석
fit0 <- lm(Y ~ 1, data = market2)
fit1 <- lm(Y ~ X1, data = market2)
fit12 <- lm(Y ~ X1+X2, data = market2)
anova(fit0, fit1, fit12)
anova(fit0, fit12)
anova(fit12)

#            Df Sum Sq Mean Sq F value    Pr(>F)    
#  Reg        2 507.88  253.94  292.49 6.597e-11 ***
#  Residuals 12  10.42    0.87                      

# 중상관계수
cor(market2$Y, market2_lm$fitted.values)
cor(market2$Y, market2_lm$fitted.values)^2

# 표준화된 중회귀분석
install.packages("lm.beta")
library(lm.beta)

market2_beta = lm.beta(market2_lm)
summary(market2_beta)

# 추정과 검정
pred.x = data.frame(X1=10, X2=10)
pc = predict(market2_lm, newdata=pred.x, interval ="confidence")
pc

pc99 = predict(market2_lm, newdata=pred.x, interval ="confidence", level=0.99)
pc99

summary(market2_lm)

p1 = 2*pt(abs(1.55811/0.14793), df=12, lower.tail = F)
p1

p2 = 2*pt(abs((0.42736 - 1)/0.08431), df=12, lower.tail = F)
p2

# 추가제곱합
health = read.csv("health.csv")

h1.lm = lm(Y ~ X1, data=health)
h2.lm = lm(Y ~ X1+X4, data=health)
h3.lm = lm(Y ~ X1+X3+X4, data=health)
h4.lm = lm(Y ~ X1+X2+X3+X4, data=health)

anova(h1.lm, h2.lm)
anova(h2.lm, h3.lm)
anova(h3.lm, h4.lm)
