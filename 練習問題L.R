# 観測値
sample <- c(
  128, 132, 121, 133, 121,
  123, 124, 136, 132, 144
)

# 必要な数値
xbar <- mean(sample)
s <- sd(sample)
n <- length(sample)
df <- n - 1

# t分布における有意水準 5% の臨界値
## qt は t分布における確率に対応する値を返します。
t_005df <- abs(qt(0.025, df))

# 信頼限界を計算する
lower <- xbar - t_005df * s / sqrt(n)
upper <- xbar + t_005df * s / sqrt(n)

## sprintf は書式に合わせて文字列を作ります。
str <- sprintf("95%% CI = [%f, %f]", lower, upper)
print(str)

# 以上が教科書通りの解法です。以下は t検定を行います。
# ステップ実行で見てください。
t.test(sample)

# end of 練習問題L.R
