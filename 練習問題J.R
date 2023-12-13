# 母数の確認
## 母集団A の母数と標本サイズ
mu_A <- 8
sigma_A <- 1
n_A <- 3

## 母集団B の母数と標本サイズ
mu_B <- 5
sigma_B <- 1.2
n_B <- 4

# 問1
print("問1:")
## A の標本平均が従う正規分布の期待値(平均)
print(paste("mu_xbar_A =", mu_A))
## その標準偏差
sigma_xbar_A <- sigma_A / sqrt(n_A)
print(paste("sigma_xbar_A =", sigma_xbar_A))
## B の標本平均が従う正規分布の期待値(平均)
print(paste("mu_xbar_B =", mu_B))
## その標準偏差
sigma_xbar_B <- sigma_B / sqrt(n_B)
print(paste("sigma_xbar_B =", sigma_xbar_B))

# 問2
print("問2:")
## 標本平均の差が従う正規分布の期待値(平均)
mu_xbar_AsubB <- mu_A - mu_B
print(paste("mu_xbar_AsubB =", mu_xbar_AsubB))
## その標準偏差
sigma_xbar_AsubB <- sqrt(sigma_A^2/n_A + sigma_B^2/n_B)
print(paste("sigma_xbar_AsubB =", sigma_xbar_AsubB))

# 問3
print("問3:")
z <- (4 - mu_xbar_AsubB) / sigma_xbar_AsubB
print(paste("z =", z))
## pnorm は正規分布における値に対応する確率を返します。
print(paste("p =", pnorm(z)))

# end of 練習問題J.R
