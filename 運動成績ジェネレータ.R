install.packages("MASS")
library(MASS)

# 仮定する統計量
標準偏差r = 0.9
標準偏差t = 3.7
平均r = 15.2
平均t = 7.5
相関係数 = -0.5

# 共分散行列を設定する
共分散行列 <- matrix(c(
  標準偏差r^2, 相関係数 * 標準偏差t * 標準偏差r,
  相関係数 * 標準偏差r * 標準偏差t, 標準偏差t^2
), nrow=2)

# 多変量正規分布からデータを生成する
set.seed(28284)
多変量データ <- mvrnorm(n=100, mu=c(平均r, 平均t), Sigma=共分散行列)

短距離 <- pmin(pmax(多変量データ[,1], 13.0), 19.9)
課外運動時間 <- pmin(pmax(多変量データ[,2], 0.0), 20.0)

table <- data.frame(課外運動時間, 短距離)

plot(課外運動時間, 短距離, xlim=c(0, 20), ylim=c(12, 19),
    xlab="週当たりの課外運動時間", ylab="100m走のタイム(秒)")

write.table(table, "運動成績.txt", row.names=F)
