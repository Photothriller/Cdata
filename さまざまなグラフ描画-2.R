# このサンプルは、1コマンドずつ実行してみてください。

# 日付と2軸編
表1 <- read.table("予定と時間.txt", header=T)

par(oma=c(0, 0, 0, 2)) # 右側に余白を作る
plot(as.Date(表1$日付), 表1$件数, type="o",
     lty=1, pch=1, col=4, ylim=c(0, 8), axes=F,
     xlab="日付 (2023年)", ylab="予定の件数",
     main="ある1週間の予定件数と拘束時間")
axis.Date(1, format="%m/%d") # x軸 (月/日 表示)
axis(2, line=0.5, col=4) # 左y軸
par(new=T)
plot(as.Date(表1$日付), 表1$時間, type="o",
     lty=2, pch=2, col=2, ylim=c(0, 7),
     axes=F, ann=F)
mtext("拘束された時間", side=4, line=3)
axis(4, line=0.5, lty=2, col=2) # 右y軸
par(oma=c(0, 0, 0, 0)) # 余白の設定を戻す

# 3Dグラフ編
install.packages("scatterplot3d")
library(scatterplot3d)

表2 <- read.table("data-interval-100.txt")

## 3Dグラフ
scatterplot3d(x = 表2$V1, y = 表2$V2, z = 表2$V3,
              pch=20, highlight.3d = TRUE,
              type ="h", lwd = 2, angle = 50,
              xlab="パラメータ a の大きさ",
              ylab="データ損失率 (%)",
              zlab="得られたデータの検証可能性 (%)")

## 複数2Dグラフ
par(mfrow=c(2, 3))
for (x in 1:5) {
  a <- x * 10
  plot(表2[表2$V1==a, 2], 表2[表2$V1==a, 3],
       type="o", ylim=c(0, 100),
       xlab="データ損失率 (%)",
       ylab="検証可能性 (%)",
       main=paste("a=", a))
}
par(mfrow=c(1, 1))

# さまざまなグラフ描画-2.R
