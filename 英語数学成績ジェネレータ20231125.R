install.packages("MASS")
library(MASS)

# 仮定する統計量
標準偏差 = 18
平均e = 57
平均m = 51
相関係数 = 0.67

# 共分散行列を設定する
共分散行列 <- matrix(c(標準偏差^2, 相関係数 * 標準偏差^2, 相関係数 * 標準偏差^2, 標準偏差^2), nrow=2)

# 多変量正規分布からデータを生成する
set.seed(31415)
多変量データ <- mvrnorm(n=100, mu=c(平均e, 平均m), Sigma=共分散行列)

英語 <- floor(pmin(pmax(多変量データ[,1], 10), 100))
数学 <- floor(pmin(pmax(多変量データ[,2], 5), 100))

df <- data.frame(英語, 数学)

par(pty="s")

plot(英語, 数学, xlim=c(0, 100), ylim=c(0, 100))

write.table(df, "英語数学成績.txt", row.names=F)

