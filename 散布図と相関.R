# このサンプルは、1コマンドずつ実行してみてください。

表1 <- read.table("英語数学成績.txt", header=T)

plot(表1, xlim=c(0, 100), ylim=c(0, 100),
     xlab="英語の試験結果", ylab="数学の試験結果",
     main="正の相関の例")

表2 <- read.table("運動成績.txt", header=T)

plot(表2, xlim=c(0, 20.0), ylim=c(13.0, 18.0),
     xlab="週当たりの課外運動時間",
     ylab="100m走のタイム (秒)",
     main="負の相関の例")

plot(表1$英語, 表2$短距離,
     xlim=c(0, 100), ylim=c(13.0, 18.0),
     xlab="英語の試験結果", ylab="100m走のタイム (秒)",
     main="無相関の例")

表3 <- data.frame(英語=表1$英語, 数学=表1$数学,
                 運動時間=表2$課外運動時間,
                 短距離=表2$短距離)

plot(表3)

# end of 散布図と相関.R
