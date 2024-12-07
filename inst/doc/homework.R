## -----------------------------------------------------------------------------
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt")) #创建了一个包含20个元素的因子，其中前10个元素标记为"Ctl"，后10个元素标记为"Trt"
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)#进行线性回归分析
summary(lm.D9)$coef

## ----echo=FALSE---------------------------------------------------------------
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
summary(lm.D9)$coef

## ----eval=FALSE---------------------------------------------------------------
#  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#  weight <- c(ctl, trt)
#  lm.D9 <- lm(weight ~ group)
#  summary(lm.D9)$coef

## -----------------------------------------------------------------------------
library(xtable)
xtable::xtable(head(iris))

## ----fig.width=6, fig.height=4------------------------------------------------
par(mfrow=c(2,2))#图形参数设置
plot(lm.D9)

## -----------------------------------------------------------------------------

# 设置参数
sigma <- 2 
n=10000

# 生成瑞利分布样本
set.seed(24002027)
u <- runif(n) # 生成均匀分布的随机数
x <- sigma * sqrt(-2 * log(1 - u)) # 逆函数

# 画出直方图和理论sigma线
hist(x, breaks = 50, ylab = "y", xlab = "x", main = "瑞利分布随机数直方图") 
abline(v = sigma, col = "red", lwd = 3)


## -----------------------------------------------------------------------------

# 设置参数
n <- 1000
p1 <- 0.75

# 生成混合正态分布样本
set.seed(24002027)
x <- rbinom(n, 1, p1) # 生成概率为0.75的二项分布来决定样本是来自哪个正态分布的
sample <- x * rnorm(n, 0, 1) + (1 - x) * rnorm(n, 3, 1) # 利用二项分布生成混合正态分布

# 绘制直方图和密度曲线
hist(sample, breaks = 30, probability = TRUE,  main = paste("p1 =", p1), xlab = "Sample Value", ylab = "Density")
lines(density(sample), col = "black", lwd = 2)


## ----fig.width=6, fig.height=4------------------------------------------------

# 测试不同概率下的直方图和密度曲线
n <- 1000
p <- seq(0.2, 0.8, by = 0.2)

par(mfrow = c(2, 2))

for (p1 in p){
  
# 生成混合正态分布样本
set.seed(24002027)
x <- rbinom(n, 1, p1) # 生成概率为0.75的二项分布来决定样本是来自哪个正态分布的
sample <- x * rnorm(n, 0, 1) + (1 - x) * rnorm(n, 3, 1) # 利用二项分布生成混合正态分布

# 绘制直方图和密度曲线
hist(sample, breaks = 30, probability = TRUE,  main = paste("p1 =", p1), xlab = "Sample Value", ylab = "Density")
lines(density(sample), col = "black", lwd = 2)

}



## -----------------------------------------------------------------------------
set.seed(24002027)  # 设置随机数种子

lambda_p <- c(3,5,7)
alpha_p <- c(2,4,6)
beta_p <- c(1,3,5) # 设置三组参数

# 分别计算三组参数下的结果
for (i in 1:3) {
# 定义参数
lambda <- lambda_p[i]  # 泊松过程的参数
alpha <- alpha_p[i]   # 伽马分布的形状参数
beta <- beta_p[i]   # 伽马分布的尺度参数
t <- 10      # 时间
n <- 10000  # 模拟次数

# 模拟复合泊松-伽马过程
poisson_gamma <- function(lambda, alpha, beta, t, n_sim) {
  X_t <- numeric(n)  # 初始化X(t)的存储向量
  for (i in 1:n) {
    N_t <- rpois(1, lambda * t)  # 模拟泊松过程N(t)
    Y_i <- rgamma(N_t, alpha, beta)  # 模拟伽马分布随机变量
    X_t[i] <- sum(Y_i)  # 计算X(t)
  }
  return(X_t)
}

# 模拟过程并计算均值和方差
X_10 <- poisson_gamma(lambda, alpha, beta, t, n)
s_mean <- mean(X_10)
s_var <- var(X_10)

# 计算理论均值和方差
E_Y1 <- alpha / beta  # 伽马分布的均值
E_Y1_2 <- (alpha*(alpha+1)) / beta^2 # 伽马分布随机变量Y_1的二阶矩
t_mean <- lambda * t * E_Y1
t_var <- lambda * t * E_Y1_2

# 输出结果
cat("参数设置为lambda=",lambda_p[i],"alpha=", alpha_p[i], "beta=", beta_p[i], "的结果", "\n")
cat("模拟得到的X(10)的均值:", s_mean, "\n")
cat("模拟得到的X(10)的方差:", s_var, "\n")
cat("理论得到的X(10)的均值:", t_mean, "\n")
cat("理论得到的X(10)的方差:", t_var, "\n")
}

## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

# 定义快速排序函数
quick_sort <- function(x) {
  num <- length(x)
  if (num == 0 || num == 1) {
    return(x)
  } else {
    a <- x[1]
    y <- x[-1]
    lower <- y[y < a]
    upper <- y[y >= a]
    return(c(quick_sort(lower), a, quick_sort(upper)))
  }
}

# 生成数据
# 定义n的取值
n_values <- c(1e4, 2e4, 4e4, 6e4, 8e4)

# 初始化存储计算时间的向量
a_n <- numeric(length(n_values))

# 统计推理
# 进行100次模拟并计算平均时间
for (i in seq_along(n_values)) {
  n <- n_values[i]
  times <- numeric(100)
  for (j in 1:100) {
    test <- sample(1:n)
    times[j] <- system.time(quick_sort(test))[1]
  }
  a_n[i] <- mean(times)
}

cat("平均时间分别为",a_n)

# 计算tn = n * log(n)
t_n <- n_values * log(n_values)

# 线性回归
model <- lm(a_n ~ t_n)

# 输出结果
# 绘图
plot(t_n, a_n, main = "log(n)对应的平均时间", xlab = "n log(n)", ylab = "平均时间 (a_n)", pch = 1)
abline(model, col = "red")


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

#生成数据
# 定义蒙特卡洛法估计Beta(3, 3)的累积分布函数函数
mc_beta_cdf <- function(x, n_sim = 10000) {
  samples <- rbeta(n_sim, 3, 3) # 生成Beta(3, 3)分布
  cdf_estimates <- sapply(x, function(q) mean(samples <= q)) # 生成Beta(3, 3)分布的累积分布函数
  return(cdf_estimates)
}

# 定义x的取值
x_values <- seq(0.1, 0.9, by = 0.1)

# 统计推理
# 计算蒙特卡洛估计的CDF值
mc_estimates <- mc_beta_cdf(x_values)

# 使用pbeta函数计算真实的CDF值
pbeta_values <- pbeta(x_values, 3, 3)

# 输出结果
results <- data.frame(x = x_values, MonteCarlo = mc_estimates, Pbeta = pbeta_values)
print(results)

# 绘制比较图
plot(x_values, mc_estimates, type = "b", col = "blue", pch = 1, 
     xlab = "x", ylab = "CDF", main = "蒙特卡洛估计和pbeta函数得到的CDF")
lines(x_values, pbeta_values, type = "b", col = "red", pch = 2)
legend("bottomright", legend = c("蒙特卡洛估计", "pbeta函数"), 
       col = c("blue", "red"), pch = c(1, 2))


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027) # 种子

# 生成数据
# 瑞利分布
generate_rayleigh <- function(sigma, n, antithetic = FALSE) {
  u <- runif(n / 2) # 生成 n/2 个均匀随机数 u
  if (antithetic) { #生成对偶变量
    v <- 1 - u
  } else {
    v <- runif(n / 2) # 正常生成瑞利分布
  }
  u <- c(u, v)
  rayleigh_samples <- sigma * sqrt(-2 * log(u))
  return(rayleigh_samples)
}
sigma <- 1
n <- 10000
m <- 1000

# 统计推理
# 使用对偶变量生成样本
samples_antithetic <- replicate(m, mean(generate_rayleigh(sigma, n, antithetic = TRUE)))
# 使用独立变量生成样本
samples_independent <- replicate(m, mean(generate_rayleigh(sigma, n, antithetic = FALSE)))

# 计算方差
var_antithetic <- var(samples_antithetic)
var_independent <- var(samples_independent)

# 计算方差减少百分比
percent_reduction <- (var_independent - var_antithetic) / var_independent * 100

#输出结果
cat("方差减少百分比为：", percent_reduction, "%\n")


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

# 生成数据
# 定义给定的函数 g(x)
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 定义两个重要性函数 f1 和 f2
f1 <- function(x) {
  dnorm(x, mean = 1, sd = 1)  # 正态分布，均值为2，标准差为1
}

f2 <- function(x) {
  dexp(x - 1, rate = 1)  # 指数分布，移位参数为1
}

# 统计推理
# 计算积分的真实值
true_value <- integrate(g, lower = 1, upper = Inf)$value

# 重要性抽样函数
importance_sampling <- function(f, n = 10000) {
  samples <- rexp(n, rate = 1) + 1  # 生成样本
  weights <- g(samples) / f(samples)  # 计算权重
  estimate <- mean(weights)  # 估计值
  variance <- var(weights) / n  # 方差
  list(estimate = estimate, variance = variance)
}

# 计算 f1 和 f2 的估计值和方差
result_f1 <- importance_sampling(f1)
result_f2 <- importance_sampling(f2)

# 输出结果
cat("f1 的估计值:", result_f1$estimate, "方差:", result_f1$variance, "\n")
cat("f2 的估计值:", result_f2$estimate, "方差:", result_f2$variance, "\n")



## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)
# 定义正态分布 N(1, 1) 的概率密度函数
f1 <- function(x) {
  1 / sqrt(2 * pi) * exp(-0.5 * (x - 1)^2)
}

# 定义指数分布 e(1) 的概率密度函数
f2 <- function(x) {
  ifelse(x >= 1, exp(-(x - 1)), 0)
}

# 定义函数 g(x)
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 创建数据用于绘图
x_values <- seq(1, 5, length.out = 1000)
f1_values <- f1(x_values)
f2_values <- f2(x_values)
g_values <- g(x_values)

# 绘制对比图
plot(x_values, f1_values, type = "l", col = "blue", lwd = 2, ylim = c(0, 0.5), 
     ylab = "Density", xlab = "x", main = "正态分布 N(1, 1)、指数分布 e(1) 和函数 g(x) 的对比图 (x ≥ 1)")
lines(x_values, f2_values, col = "green", lwd = 2)
lines(x_values, g_values, col = "red", lwd = 2)
legend("topright", legend = c("f1: N(1, 1)", "f2: e(1)", "g(x)"), col = c("blue", "green", "red"), lwd = 2)


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

# 数据生成
# 参数设置
n <- 1000  # 样本大小
num_simulations <- 100000  # 模拟次数

# 生成偏度的Monte Carlo样本
skewness_samples <- numeric(num_simulations)
for (i in 1:num_simulations) {
  sample <- rnorm(n)
  skewness_samples[i] <- mean((sample - mean(sample))^3) / sd(sample)^3 # 计算偏度
}

# 统计推理
# 计算分位数
quantiles <- quantile(skewness_samples, probs = c(0.025, 0.05, 0.95, 0.975))

# 计算标准误差
q_values <- c(0.025, 0.05, 0.95, 0.975)
standard_errors <- numeric(length(q_values))
for (i in 1:length(q_values)) {
  f_q <- dnorm(qnorm(q_values[i]))
  standard_errors[i] <- sqrt(q_values[i] * (1 - q_values[i]) / (n * f_q^2))/sqrt(n) # 计算偏度标准误差
}

# 大数近似下的分位数
large_sample_quantiles <- qnorm(q_values, mean = 0, sd = sqrt(6/n))

# 输出结果
cat("Monte Carlo估计的分位数:\n",quantiles,"\n","标准误差:","\n",standard_errors,"\n","大样本近似的分位数:","\n",large_sample_quantiles)


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

# 数据生成
# 设置参数
library(MASS)
n <- 100
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
num_simulations <- 10000
alpha <- 0.05

# 存储拒绝次数
pearson_rejections <- 0
spearman_rejections <- 0
kendall_rejections <- 0

# 统计推理
# 进行模拟实验
for (i in 1:num_simulations) {
  data_normal <- mvrnorm(n, mu, Sigma)
  
  pearson_test <- cor.test(data_normal[,1], data_normal[,2], method = "pearson")
  spearman_test <- cor.test(data_normal[,1], data_normal[,2], method = "spearman")
  kendall_test <- cor.test(data_normal[,1], data_normal[,2], method = "kendall") # 计算相关系数
  
  if (pearson_test$p.value < alpha) pearson_rejections <- pearson_rejections + 1
  if (spearman_test$p.value < alpha) spearman_rejections <- spearman_rejections + 1
  if (kendall_test$p.value < alpha) kendall_rejections <- kendall_rejections + 1 # 如果p值小于alpha，计入拒绝次数 
}

# 计算拒绝率
pearson_power <- pearson_rejections / num_simulations
spearman_power <- spearman_rejections / num_simulations
kendall_power <- kendall_rejections / num_simulations

# 输出结果
cat("双变量正态分布Pearson测试功效: ", pearson_power, "\n","双变量正态分布Spearman测试功效: ", spearman_power, "\n","双变量正态分布Kendall测试功效: ", kendall_power, "\n")


## -----------------------------------------------------------------------------
# 存储结果
pearson_rejections2 <- 0
spearman_rejections2 <- 0
kendall_rejections2 <- 0

# 统计推理
# 进行模拟实验
for (i in 1:num_simulations) {
  X <- rnorm(n)
  Y <- 2 * X + rlnorm(n)  # 生成相关的双变量分布数据
  
  pearson_test2 <- cor.test(X, Y, method = "pearson")
  spearman_test2 <- cor.test(X, Y, method = "spearman")
  kendall_test2 <- cor.test(X, Y, method = "kendall") # 计算相关系数
  
  if (pearson_test2$p.value < alpha) pearson_rejections2 <- pearson_rejections2 + 1
  if (spearman_test2$p.value < alpha) spearman_rejections2 <- spearman_rejections2 + 1
  if (kendall_test2$p.value < alpha) kendall_rejections2 <- kendall_rejections2 + 1 # # 如果p值小于alpha，计入拒绝次数
}

# 计算拒绝率
pearson_power2 <- pearson_rejections2 / num_simulations
spearman_power2 <- spearman_rejections2 / num_simulations
kendall_power2 <- kendall_rejections2 / num_simulations

# 输出结果
cat("相关的双变量分布Pearson测试功效: ", pearson_power2, "\n","相关的双变量分布Spearman测试功效: ", spearman_power2, "\n","相关的双变量分布Kendall测试功效: ", kendall_power2, "\n")

## -----------------------------------------------------------------------------

rm(list=ls()) #清理内存
set.seed(24002027)

# 数据生成
# 参数设置
N <- 1000
m <- 10000
alpha <- 0.1

# 存储结果
FWER_Bonferroni <- 0
FDR_Bonferroni <- 0
TPR_Bonferroni <- 0
FWER_BH <- 0
FDR_BH <- 0
TPR_BH <- 0

# 统计推理
# 模拟过程
for (i in 1:m) {
  
  # 生成p值
  null_p_values <- runif(950)
  alternative_p_values <- rbeta(50, 0.1, 1)
  p_values <- c(null_p_values, alternative_p_values)
  
  # Bonferroni调整
  bonferroni_p_values <- p.adjust(p_values, method = "bonferroni")
  bonferroni_rejections <- bonferroni_p_values < alpha
  
  # 计算Bonferroni的FWER, FDR和TPR
  FWER_Bonferroni <- FWER_Bonferroni + (sum(bonferroni_rejections[1:950]) > 0) / m
  FDR_Bonferroni <- FDR_Bonferroni + (sum(bonferroni_rejections[1:950]) / max(sum(bonferroni_rejections), 1)) / m
  TPR_Bonferroni <- TPR_Bonferroni + (sum(bonferroni_rejections[951:1000]) / 50) / m
  
  # Benjamini-Hochberg调整
  bh_p_values <- p.adjust(p_values, method = "BH")
  bh_rejections <- bh_p_values < alpha
  
  # 计算B-H的FWER, FDR和TPR
  FWER_BH <- FWER_BH + (sum(bh_rejections[1:950]) > 0) / m
  FDR_BH <- FDR_BH + (sum(bh_rejections[1:950]) / max(sum(bh_rejections), 1)) / m
  TPR_BH <- TPR_BH + (sum(bh_rejections[951:1000]) / 50) / m
}

# 输出结果
result <- matrix(c(FWER_Bonferroni, FDR_Bonferroni, TPR_Bonferroni,
                   FWER_BH, FDR_BH, TPR_BH),
                 nrow = 3, ncol = 2,
                 dimnames = list(c("FWER", "FDR", "TPR"), 
                                 c("Bonferroni correction", "B-H correction")))
print(result)


## -----------------------------------------------------------------------------

rm(list=ls()) #清理内存
library (boot)
set.seed(24002027)

# 生成数据

times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 定义计算MLE的函数
lambda_mle <- function(data, indices) {
  d <- data[indices]
  return(1/mean(d))  # MLE for lambda
}

# 设置自助法的参数
R <- 1000  # 自助样本的数量

# 统计推理
# 执行自助法
boot_results <- boot(data=times, statistic=lambda_mle, R=R)

# 计算偏差和标准误差
bias <- mean(boot_results$t) - lambda_mle(times, 1:(length(times)))
se <- sd(boot_results$t)

# 输出结果
cat("MLE:", lambda_mle(times, 1:(length(times))), "\n")
cat("Bias:", bias, "\n")
cat("Standard Error:", se, "\n")

## -----------------------------------------------------------------------------

rm(list=ls()) #清理内存
library (boot)
set.seed(24002027)

# 生成数据
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 统计推理
# 定义统计量函数，计算均值
mean_fun <- function(data, indices) {
  return(mean(data[indices]))
}

# 进行自助抽样
boot_results <- boot(data, statistic = mean_fun, R = 1000)

# 计算95%置信区间
ci_normal <- boot.ci(boot_results, conf = 0.95, type = "norm")
ci_basic <- boot.ci(boot_results, conf = 0.95, type = "basic")
ci_percentile <- boot.ci(boot_results, conf = 0.95, type = "perc")
ci_bca <- boot.ci(boot_results, conf = 0.95, type = "bca")

# 输出结果
print(ci_normal)
print(ci_basic)
print(ci_percentile)
print(ci_bca)

## -----------------------------------------------------------------------------
rm(list = ls()) # 清理内存

# 加载数据
library(bootstrap)
data(scor)

# 统计推理
# 定义协方差矩阵和特征值的函数
cov_eigen <- function(data) {
  cov_mat <- cov(data)
  eigen_values <- eigen(cov_mat)$values
  return(list(cov = cov_mat, eigen = eigen_values))
}

# 原始数据的协方差矩阵和特征值
original_cov_eigen <- cov_eigen(scor)

# 计算原始样本的 \(\hat{\theta}\)
lambda1 <- max(original_cov_eigen$eigen)
sum_lambda <- sum(original_cov_eigen$eigen)
theta_hat <- lambda1 / sum_lambda

# Jackknife函数
jackknife_theta <- function(data) {
  n <- nrow(data)
  theta_hats <- numeric(n)
  for (i in 1:n) {
    data_minus_i <- data[-i, ]
    cov_eigen_data <- cov_eigen(data_minus_i)
    lambda1_jack <- max(cov_eigen_data$eigen)
    sum_lambda_jack <- sum(cov_eigen_data$eigen)
    theta_hats[i] <- lambda1_jack / sum_lambda_jack
  }
  return(theta_hats)
}

# 执行Jackknife
jackknife_results <- jackknife_theta(scor)

# 计算偏差和标准误差
bias <- mean(jackknife_results) - theta_hat
se <- sd(jackknife_results)

# 输出结果
cat("Jackknife Bias:", bias, "\n")
cat("Jackknife Standard Error:", se, "\n")


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理内存

# 生成数据
library(DAAG)
attach(ironslag)
# 获取磁性数据的长度
n <- length(magnetic)
# 初始化残差向量
e1 <- e2 <- e3 <- e4 <- e5 <- numeric(n)  # 为 n-fold 交叉验证创建五个残差向量

# 统计推理
# 留一法交叉验证
for (k in 1:n) {
    y <- magnetic[-k]  # 除去第 k 个数据点的响应变量
    x <- chemical[-k]  # 除去第 k 个数据点的自变量

    # 线性模型
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[k] <- magnetic[k] - yhat1  # 计算残差

    # 二次模型
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
    e2[k] <- magnetic[k] - yhat2  # 计算残差

    # 指数模型
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[k] <- magnetic[k] - yhat3  # 计算残差

    # 三次多项式模型
    J4 <- lm(y ~ x + I(x^2) + I(x^3))  # 新增三次多项式模型
    yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] + J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3
    e4[k] <- magnetic[k] - yhat4  # 计算残差
}


    # 计算均方根误差 (RMSE)
rmse1 <- sqrt(mean(e1^2))  # 线性模型
rmse2 <- sqrt(mean(e2^2))  # 二次模型
rmse3 <- sqrt(mean(e3^2))  # 指数模型
rmse4 <- sqrt(mean(e4^2))  # 三次多项式模型


# 计算每个模型的调整 R^2
adjusted_r2 <- function(model) {
    1 - (1 - summary(model)$r.squared) * (n - 1) / (n - length(model$coefficients) - 1)
}

# 拟合每个模型并计算调整 R^2
model1 <- lm(magnetic ~ chemical)  # 线性模型
model2 <- lm(magnetic ~ chemical + I(chemical^2))  # 二次模型
model3 <- lm(log(magnetic) ~ chemical)  # 指数模型
model4 <- lm(magnetic ~ chemical + I(chemical^2) + I(chemical^3))  # 三次多项式模型

# 计算调整 R^2
adj_r2_1 <- adjusted_r2(model1)
adj_r2_2 <- adjusted_r2(model2)
adj_r2_3 <- adjusted_r2(model3)
adj_r2_4 <- adjusted_r2(model4)

# 打印结果
cat("RMSE for Linear Model: ", rmse1, "\n")
cat("RMSE for Quadratic Model: ", rmse2, "\n")
cat("RMSE for Exponential Model: ", rmse3, "\n")
cat("RMSE for Cubic Polynomial Model: ", rmse4, "\n")

# 打印调整 R^2
cat("Adjusted R^2 for Linear Model: ", adj_r2_1, "\n")
cat("Adjusted R^2 for Quadratic Model: ", adj_r2_2, "\n")
cat("Adjusted R^2 for Exponential Model: ", adj_r2_3, "\n")
cat("Adjusted R^2 for Cubic Polynomial Model: ", adj_r2_4, "\n")


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理内存
set.seed(24002027) 

# 生成数据
# 定义数据X和Y
X <- c(158, 171, 193, 199, 230, 243, 248, 248, 250, 267, 271, 316, 327, 329)
Y <- c(141, 148, 169, 181, 203, 213, 229, 244, 257, 260, 271, 309)

# 设置置换次数
num_permutations <- 1000

# 初始化置换统计量的向量
permuted_cvm <- numeric(num_permutations)

# 统计推理
# 合并样本
combined <- c(X, Y)

# 定义Cramér-von Mises统计量的函数
cvm_statistic <- function(sample1, sample2) {
  n1 <- length(sample1)
  n2 <- length(sample2)
  ecdf1 <- ecdf(sample1)
  ecdf2 <- ecdf(sample2)
  cvm <- sum((ecdf1(combined) - ecdf2(combined))^2)
  return(cvm)
}

# 计算原始Cramér-von Mises统计量
original_cvm <- cvm_statistic(X, Y)


# 进行置换检验

for (i in 1:num_permutations) {
  # 随机置换合并样本
  permuted_samples <- sample(combined)
  # 将置换后的样本分成两部分
  permuted_X <- permuted_samples[1:length(X)]
  permuted_Y <- permuted_samples[(length(X) + 1):length(combined)]
  # 计算置换后的Cramér-von Mises统计量
  permuted_cvm[i] <- cvm_statistic(permuted_X, permuted_Y)
}

# 计算p值
p_value <- mean(permuted_cvm >= original_cvm)

# 打印结果
cat("原始Cramér-von Mises统计量:", original_cvm, "\n")
cat("p值:", p_value, "\n")


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理内存
set.seed(24002027)

# 生成数据
# 生成两个变量的样本
x <- rnorm(30)
y <- rnorm(30)

# 统计推理
# 使用 cor 计算实际的 Spearman 秩相关系数
actual_corr <- cor(x, y, method = "spearman")

# 定义排列测试函数
perm_test <- function(x, y, n_perm = 1000) {
  n <- length(y)
  perm_corrs <- numeric(n_perm)
  
  for (i in 1:n_perm) {
    # 随机打乱 y 的顺序
    perm_y <- sample(y)
    # 计算排列后的秩相关系数
    perm_corrs[i] <- cor(x, perm_y, method = "spearman")
  }
  
  # 计算显著性水平
  p_value <- mean(abs(perm_corrs) >= abs(actual_corr))
  return(p_value)
}

# 进行排列测试
perm_p_value <- perm_test(x, y)

# 使用 cor.test 函数计算 p 值
test <- cor.test(x, y, method = "spearman")
cor_test_p_value <- test$p.value

# 输出结果
cat("排列测试的显著性水平 (p值):", perm_p_value, "\n")
cat("cor.test 的 p 值:", cor_test_p_value, "\n")


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

# 生成数据
m <- 10000
sigma <- 1  # 选择一个合适的sigma值
x <- numeric(m)
x[1] <- rnorm(1)  # 初始值
u <- runif(m)
# 目标函数为dcauchy()

# 统计推理
for (i in 2:m) {
  xt = x[i-1]
  y <- rnorm(1, mean = xt, sd = sigma) # 从提议分布中获取一个值
  num <- (dcauchy(y) * dnorm(xt, mean = y, sd = sigma))
  den <- (dcauchy(xt) * dnorm(y, mean = xt, sd = sigma)) # 计算接受率
  
  # 判别是否接受
  if (u[i] <= num/den) { 
    x[i] <- y
  } else {
    x[i] <- x[i-1]
  }
}

# 丢弃前1000个样本
x <- x[1001:m]

# 计算分位数
a <- seq(0.1, 0.9, by = 0.1)
Q <- quantile(x, a)
QR <- qcauchy(a)  # 标准柯西分布的理论分位数

# 打印结果
print(Q)
print(QR)

## -----------------------------------------------------------------------------

rm(list=ls()) #清理内存
set.seed(24002027)

# 设置参数
n <- 10  
a <- 1   
b <- 1   
N <- 10000 
burn <- 1000 
X <- matrix(0, N, 2) # 存储样本链的矩阵

# 初始化
X[1, ] <- c(0, 0.5) # 假设的初始值

# the Gibbs sampler
for (i in 2:N) {
  x2 <- X[i-1, 2]
  x1 <- X[i, 1]
   # 从条件分布 P(x|y) 中抽样得到新的 x 值
  X[i, 1] <- rbinom(1, n, x2)
  # 从条件分布 P(y|x) 中抽样得到新的 y 值
  X[i, 2] <- rbeta(1, x1 + a, n - x1 + b)

}

# 去除烧入
b <- burn + 1
x <- X[b:N, ]

# 计算统计量
means <- colMeans(x)
std_errors <- apply(x, 2, sd)
correlation_coefficient <- cor(x[,1], x[,2])

# 输出结果
cat('Means: ', round(means, 2), '\n')
cat('Standard errors: ', round(std_errors, 2), '\n')
cat('Correlation coefficient: ', round(correlation_coefficient, 2), '\n')

plot(x[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(x[,2],col=2,lwd=2)
legend('bottomright',c(expression(X[1]),expression(X[2])), col=1:2,lwd=2)


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
library(coda)
set.seed(24002027)

    normal.chain <- function(sigma, N, X1) {
      x <- rep(0, N)
      x[1]=X1
      u <- runif(N)
      
     for (i in 2:N) {
     xt <- x[i-1]
     y <- rnorm(1, mean = xt, sd = sigma)
     num <- (dcauchy(y,0,1) * dnorm(xt, y, sigma))
     den <- (dcauchy(xt,0,1) * dnorm(y, xt, sigma))
     
     if (u[i] <= num / den) {
       x[i] <- y
     }
     else {
       x[i] <- x[i-1]
     }
     }
  return(x)
}

  # 设置参数
burn_in <- 1000
sigma <- 1
initial_values <- c(0, 5, -5)  # 初始值，用于生成多条链

# 定义样本数范围
sample_sizes <- seq(1000, 10000, by = 100)  # 每次增加500个样本
r_hat_values <- numeric(length(sample_sizes))  # 存储每个N下的R_hat值

# 计算每个样本数下的R_hat值
for (i in seq_along(sample_sizes)) {
  N <- sample_sizes[i]
  
  # 生成多条链
  chains <- lapply(initial_values, function(init) normal.chain(sigma, N, init))
  
  # 将每条链转换为mcmc对象并丢弃Burn-in部分
  chains <- lapply(chains, function(chain) mcmc(chain[-(1:burn_in)]))
  
  # 创建mcmc.list对象
  mcmc_chains <- mcmc.list(chains)
  
  # 计算Gelman-Rubin诊断的点估计值 (Point est.)
  r_hat_values[i] <- gelman.diag(mcmc_chains)$psrf[, "Point est."][1]
}

# 绘制N与R_hat的关系图
plot(sample_sizes, r_hat_values, type="l", pch = 19, col = "blue",
     xlab = "样本数 (N)", ylab = "R_hat 值",
     main = "样本数与Gelman-Rubin R_hat的关系")
abline(h = 1.2, col = "red", lty = 2)  # 添加1.2的参考线


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
library(coda)
set.seed(24002027)

# 设置Gibbs采样参数
n <- 10
a <- 1
b <- 1
burn <- 1000
initial_values <- list(c(0, 0.5), c(5, 0.8), c(9, 0.2))  # 初始值，用于生成多条链

# 定义样本数范围
sample_sizes <- seq(1000, 1500, by = 5)  # 每次增加500个样本
r_hat_x1_values <- numeric(length(sample_sizes))  # 存储每个样本数下X1的R_hat值
r_hat_x2_values <- numeric(length(sample_sizes))  # 存储每个样本数下X2的R_hat值

# 定义Gibbs采样函数
gibbs_sampler <- function(n, a, b, N, init) {
  X <- matrix(0, N, 2)  # 存储样本链的矩阵
  X[1, ] <- init         # 设置初始值
  for (i in 2:N) {
    x2 <- X[i-1, 2]
    # 从条件分布 P(x|y) 中抽样得到新的 x 值
    X[i, 1] <- rbinom(1, n, x2)
    x1 <- X[i, 1]
    # 从条件分布 P(y|x) 中抽样得到新的 y 值
    X[i, 2] <- rbeta(1, x1 + a, n - x1 + b)
  }
  return(X)
}

# 计算每个样本数下的R_hat值
for (i in seq_along(sample_sizes)) {
  N <- sample_sizes[i]
  
  # 生成多条链
  chains <- lapply(initial_values, function(init) gibbs_sampler(n, a, b, N, init))
  
  # 将每条链的后半段（去除burn-in）分别转化为mcmc对象
  chains_x1 <- lapply(chains, function(chain) mcmc(chain[-(1:burn), 1]))
  chains_x2 <- lapply(chains, function(chain) mcmc(chain[-(1:burn), 2]))
  
  # 创建mcmc.list对象
  mcmc_chains_x1 <- mcmc.list(chains_x1)
  mcmc_chains_x2 <- mcmc.list(chains_x2)
  
  # 计算Gelman-Rubin诊断的点估计值 (Point est.)
  r_hat_x1_values[i] <- gelman.diag(mcmc_chains_x1)$psrf[, "Point est."][1]
  r_hat_x2_values[i] <- gelman.diag(mcmc_chains_x2)$psrf[, "Point est."][1]
}

# 绘制N与R_hat的关系图
plot(sample_sizes, r_hat_x1_values, type="l", pch = 19, col = "blue",
     xlab = "样本数 (N)", ylab = "R_hat 值",
     main = "样本数与Gelman-Rubin R_hat的关系 (X1 和 X2)")
lines(sample_sizes, r_hat_x2_values, type="l", pch = 19, col = "green")
legend("topright", legend = c("X1", "X2"), col = c("blue", "green"), pch = 19)
abline(h = 1.2, col = "red", lty = 2)  # 添加1.2的参考线


## -----------------------------------------------------------------------------
rm(list=ls()) #清理内存
set.seed(24002027)

Gelman.Rubin <- function(psi) {
        # psi[i,j] is the statistic psi(X[i,1:j])
        # for chain in i-th row of X
        psi <- as.matrix(psi)
        n <- ncol(psi)
        k <- nrow(psi)

        psi.means <- rowMeans(psi)     #row means
        B <- n * var(psi.means)        #between variance est.
        psi.w <- apply(psi, 1, "var")  #within variances
        W <- mean(psi.w)               #within est.
        v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
        r.hat <- v.hat / W             #G-R statistic
        return(r.hat)
        }

 
  
    normal.chain <- function(sigma, N, X1) {
      x <- rep(0, N)
      x[1]=X1
      u <- runif(N)
      
     for (i in 2:N) {
     xt <- x[i-1]
     y <- rnorm(1, mean = xt, sd = sigma)
     num <- (dcauchy(y,0,1) * dnorm(xt, y, sigma))
     den <- (dcauchy(xt,0,1) * dnorm(y, xt, sigma))
     
     if (u[i] <= num / den) {
       x[i] <- y
     }
     else {
       x[i] <- x[i-1]
     }
     }
  return(x)
}

    
    
    sigma <- 1     #parameter of proposal distribution
    k <- 4          #number of chains to generate
    n <- 20000      #length of chains
    b <- 1000       #burn-in length

    # 初始值
    x0 <- c(-10, -5 ,5 , 10)

    #生成链
    set.seed(12345)
    X <- matrix(0, nrow=k, ncol=n)
    for (i in 1:k){
        X[i, ] <- normal.chain(sigma, n, x0[i])
}
    #compute diagnostic statistics
    psi <- t(apply(X, 1, cumsum))
    for (i in 1:nrow(psi)){
        psi[i,] <- psi[i,] / (1:ncol(psi))
}
    #plot psi for the four chains
#    par(mfrow=c(2,2))
    for (i in 1:k)
      if(i==1){
        plot((b+1):n,psi[i, (b+1):n],ylim=c(-5, 5), type="l",
            xlab='Index', ylab=bquote(phi))
      }else{
        lines(psi[i, (b+1):n], col=i)
    }
    par(mfrow=c(1,1)) #restore default
    

## -----------------------------------------------------------------------------
    
    # 计算R_hat的统计序列
    rhat <- rep(0, n)
    for (j in (b+1):n)
        rhat[j] <- Gelman.Rubin(psi[,1:j])
    plot(rhat[(b+1):n], ylim=c(0, 2), type="l", xlab="", ylab="R")
    abline(h=1.1, lty=2)
    

## -----------------------------------------------------------------------------
rm(list = ls())
# 计算第k项
compute_kth_term <- function(k, d, a) {
  norm_a <- sqrt(sum(a^2)) # 计算a的欧几里得范数
  term <- ((-1)^k / (factorial(k) * 2^k)) * ((norm_a^(2*k + 2)) / ((2*k + 1) * (2*k + 2))) * 
    gamma((d + 1) / 2) * gamma(k + 3/2) / gamma(k + d/2 + 1)
  return(term)
}

# 计算前k项和
compute_series_sum <- function(max_k, d, a) {
  sum_value <- 0
  for (k in 0:max_k) {
    sum_value <- sum_value + compute_kth_term(k, d, a)
  }
  return(sum_value)
}

# 给定a计算总和
a=c(1,2)
max_k <- 100 
sum_result <- compute_series_sum(max_k, length(a), a)
print(sum_result)

## -----------------------------------------------------------------------------
rm(list = ls())
solve_equation = function (k) {

  # 函数中的积分部分
  integration = function(u, n) {
    (1 + u^2/(n-1))^(-n/2)
  }
  
  # c_k
  c_k = function (n, a) {
    sqrt(a^2 * n / (n + 1 - a^2))
  }
  
  # 等号两边的函数有相同的形式，区别在于k或者k+1
  left_right = function (n, a) {
    
    this_integral = function (u) {
      integration(u, n)
    }
    
    c = c_k(n - 1, a)
    
    2/sqrt(pi*(n-1)) * exp(lgamma(n/2)-lgamma((n-1)/2)) * integrate(this_integral, lower = 0, upper = c)$value
  }
  
# 计算等号两边函数的差
  f = function (a) {
    left = left_right(k, a)
    right = left_right(k + 1, a)
    return (left - right)
  }
  
# 找零点，计算根
  eps = 1e-2
  if (f(eps) < 0 && f(sqrt(k) - eps) > 0 || f(eps) > 0 && f(sqrt(k) - eps) < 0) {
    r = uniroot(f, interval = c(eps, sqrt(k)-eps))$root
  } else {
    r = NA
  }
  return(r)
}

# 把11.4中的值带入
result = sapply(c(4:25, 100, 500, 1000), function (k) {
  solve_equation(k)
})
print(result)


## -----------------------------------------------------------------------------
rm(list = ls())
# 观测到的数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

# 截断值
tau <- 1

# 初始化lambda的值，使用没有被截断的数据的均值
lambda <- mean(Y[Y < tau])

# 设置EM算法的参数
tolerance <- 1e-6
max_iter <- 1000
iter <- 0
diff <- tolerance + 1

# EM算法
while (diff > tolerance && iter < max_iter) {
  iter <- iter + 1
  lambda_old <- lambda
  
  # E步骤：计算被截断的概率
  uncensored <- Y[Y < tau] # 没有被截断的数据
  censored <- Y[Y == tau]  # 被截断的数据
  
  # 对于指数分布的数据计算被截断数据的期望
  E_censored <- tau + 1 / lambda_old
  
  # M步骤：更新lambda
  lambda <- length(Y) / (sum(uncensored) + length(censored) * E_censored)
  
  # 检查收敛性
  diff <- abs(lambda - lambda_old)
}

# 输出结果
cat("通过EM算法估计的lambda:", lambda, "\n")

# 使用观测数据的MLE估计
lambda_mle <- 1 / mean(Y)
cat("观测数据的MLE估计的lambda:", lambda_mle, "\n")


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理缓存

# 加载 lpSolve 包
library(lpSolve)

# 定义目标函数的系数 (最小化目标)
obj_coef <- c(4, 2, 9)

# 定义约束矩阵
constraints <- matrix(c(
  2,  1,  1,  # 2x + y + z <= 2
  1, -1,  3   # x - y + 3z <= 3
), nrow = 2, byrow = TRUE)

# 定义约束的右侧值
rhs <- c(2, 3)

# 定义约束方向
directions <- c("<=", "<=")

# 求解线性规划
solution <- lp("min", obj_coef, constraints, directions, rhs, compute.sens = TRUE)

# 输出结果
if (solution$status == 0) {
  cat("目标函数最小值：", solution$objval, "\n")
  cat("决策变量的取值：\n")
  print(solution$solution)
} else {
  cat("未找到最优解。\n")
}

## -----------------------------------------------------------------------------
rm(list = ls()) # 清理缓存
# 准备数据和公式
data(mtcars)
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用 for 循环
models_for <- list()
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 使用 lapply()
models_lapply <- lapply(formulas, function(formula) lm(formula, data = mtcars))

# 查看结果
models_for
models_lapply

## -----------------------------------------------------------------------------
# 创建 bootstrap 样本
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})

# 使用 for 循环
bootstrap_models_for <- list()
for (i in seq_along(bootstraps)) {
  bootstrap_models_for[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 使用 lapply()
bootstrap_models_lapply <- lapply(bootstraps, lm, formula = mpg ~ disp)

# 查看结果
bootstrap_models_for
bootstrap_models_lapply

## -----------------------------------------------------------------------------
# 定义 R^2 提取函数
rsq <- function(mod) summary(mod)$r.squared

# 提取 for 循环中模型的 R^2
rsq_for <- sapply(models_for, rsq)
rsq_for_bootstrap <- sapply(bootstrap_models_for, rsq)

# 提取 lapply() 中模型的 R^2
rsq_lapply <- sapply(models_lapply, rsq)
rsq_lapply_bootstrap <- sapply(bootstrap_models_lapply, rsq)

# 查看结果
rsq_for
rsq_for_bootstrap
rsq_lapply
rsq_lapply_bootstrap

## -----------------------------------------------------------------------------
rm(list = ls()) # 清理缓存
# 模拟数据
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)), 
  simplify = FALSE
)

# 使用 sapply() 和匿名函数提取 p 值
p_values <- sapply(trials, function(test) test$p.value)

# 查看结果
p_values
hist(p_values, breaks = 20, main = "P-value Distribution", xlab = "P-value", col = "skyblue")


## -----------------------------------------------------------------------------
# 自定义函数：结合 Map() 和 vapply()
parallel_lapply <- function(FUN, ..., FUN.VALUE) {
  # 使用 Map() 并行迭代所有输入
  results <- Map(FUN, ...)
  # 使用 vapply() 将结果强制转换为指定类型（向量或矩阵）
  vapply(results, identity, FUN.VALUE)
}

# 示例 1：并行迭代两个列表并计算它们的和
list1 <- list(1, 2, 3)
list2 <- list(4, 5, 6)

result <- parallel_lapply(
  FUN = function(x, y) x + y,  # 目标函数
  list1, list2,               # 多个输入
  FUN.VALUE = numeric(1)      # 输出为数值向量
)
print(result)

# 示例 2：生成矩阵输出
list3 <- list(7, 8, 9)

result_matrix <- parallel_lapply(
  FUN = function(x, y, z) c(x, y, z),  # 目标函数，返回向量
  list1, list2, list3,                # 多个输入
  FUN.VALUE = numeric(3)              # 输出为矩阵
)
print(result_matrix)


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理缓存
# 更快的卡方检验函数
fast_chisq <- function(x, y) {
  # 确保输入为数值向量，且无缺失值
  stopifnot(is.numeric(x), is.numeric(y), !anyNA(x), !anyNA(y))
  
  # 构建二维列联表
  observed <- table(x, y)
  
  # 计算期望值
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)
  expected <- outer(row_totals, col_totals) / grand_total
  
  # 计算卡方检验统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

# 示例数据
x <- c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)
y <- c(1, 2, 2, 3, 3, 1, 2, 2, 3, 1, 3, 3)

# 调用函数
fast_chisq(x, y)

## -----------------------------------------------------------------------------
# 自定义快速 table() 函数
fast_table <- function(x, y) {
  # 确保输入为整数向量，且无缺失值
  stopifnot(is.integer(x), is.integer(y), !anyNA(x), !anyNA(y))
  
  # 确定唯一值及其范围
  x_levels <- sort(unique(x))
  y_levels <- sort(unique(y))
  
  # 创建结果矩阵
  result <- matrix(0L, nrow = length(x_levels), ncol = length(y_levels))
  rownames(result) <- x_levels
  colnames(result) <- y_levels
  
  # 填充计数
  for (i in seq_along(x)) {
    result[match(x[i], x_levels), match(y[i], y_levels)] <- 
      result[match(x[i], x_levels), match(y[i], y_levels)] + 1
  }
  
  return(result)
}

# 引入到卡方检验
fast_chisq_optimized <- function(x, y) {
  # 确保输入为整数向量
  stopifnot(is.integer(x), is.integer(y), !anyNA(x), !anyNA(y))
  
  # 使用快速 table 生成列联表
  observed <- fast_table(x, y)
  
  # 计算期望值
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)
  expected <- outer(row_totals, col_totals) / grand_total
  
  # 计算卡方检验统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

# 示例数据（整数向量）
x <- as.integer(c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4))
y <- as.integer(c(1, 2, 2, 3, 3, 1, 2, 2, 3, 1, 3, 3))

# 调用优化后的卡方检验函数
fast_chisq_optimized(x, y)


## -----------------------------------------------------------------------------
rm(list = ls()) # 清理内存

# r语言Gibbs 采样器
gibbs_r <- function(n_iter, n, a, b, burn) {
  chain <- matrix(0, nrow = n_iter, ncol = 2) # 存储最终保留的样本
  x <- 0  # 初始化 x
  y <- 0.5  # 初始化 y
  
  for (i in 1:(n_iter + burn)) {
    # 从条件分布 x | y (二项分布) 采样
    x <- rbinom(1, n, y)
    
    # 从条件分布 y | x (Beta 分布) 采样
    y <- rbeta(1, x + a, n - x + b)
    
    # 如果超出 burn-in 阶段，则存储样本
    if (i > burn) {
      chain[i - burn, ] <- c(x, y)
    }
  }
  return(chain) # 返回最终样本链
}


## -----------------------------------------------------------------------------
library(Rcpp)
cppFunction('NumericMatrix gibbs_cpp(int n_iter, int n, double a, double b, int burn) {
  NumericMatrix chain(n_iter, 2); // 存储最终保留的 (x, y) 样本
  double x = 0;                  // 初始化 x
  double y = 0.5;                // 初始化 y
  
  for (int i = 0; i < n_iter + burn; i++) {
    // 从条件分布 x | y (二项分布) 采样
    x = R::rbinom(n, y);
    
    // 从条件分布 y | x (Beta 分布) 采样
    y = R::rbeta(x + a, n - x + b);
    
    // 如果超出 burn-in 阶段，则存储样本
    if (i >= burn) {
      chain(i - burn, 0) = x; // 存储 x
      chain(i - burn, 1) = y; // 存储 y
    }
  }
  return chain; // 返回最终样本链
}
')


## -----------------------------------------------------------------------------

set.seed(24002027)


# 设置参数
n_iter <- 10000
burn <- 1000
n <- 10
a <- 1
b <- 1

# 生成数据
chain_r <- gibbs_r(n_iter, n, a, b, burn)
chain_cpp <- gibbs_cpp(n_iter, n, a, b, burn)

# 对比x
qqplot(chain_r[, 1], chain_cpp[, 1], main = "QQ Plot for x",
       xlab = "R samples", ylab = "Rcpp samples")
abline(0, 1, col = "red")

# 对比y
qqplot(chain_r[, 2], chain_cpp[, 2], main = "QQ Plot for y",
       xlab = "R samples", ylab = "Rcpp samples")
abline(0, 1, col = "red")


## -----------------------------------------------------------------------------
library(microbenchmark)

time <- microbenchmark(
  R = gibbs_r(n_iter, n, a, b, burn),
  Rcpp = gibbs_cpp(n_iter, n, a, b, burn),
  times = 10
)

print(time)


