# このサンプルは、1コマンドずつ実行してみてください。

# 折れ線グラフ編
表1 <- read.table("模試平均点.txt", header=T)

## A組単体の折れ線グラフ
plot(表1$模試回, 表1$A組, type="o", pch=0,
     ylim=c(40, 80), xaxp=c(1,4,3),
     ylab="平均点", xlab="模試回",
     main="A組の平均点の推移")

## A〜D組の折れ線グラフを 1枚に描く
plot(表1$模試回, 表1$A組, type="o", lty=1, pch=1, col=1,
     ylim=c(40, 80), xaxp=c(1,4,3),
     ylab="平均点", xlab="模試回",
     main="A,B,C,D組の平均点の推移")
par(new=T)
plot(表1$模試回, 表1$B組, type="o", lty=2, pch=2, col=2,
     ylim=c(40, 80), xaxp=c(1,4,3), axes=F, ann=F)
par(new=T)
plot(表1$模試回, 表1$C組, type="o", lty=3, pch=3, col=3,
     ylim=c(40, 80), xaxp=c(1,4,3), axes=F, ann=F)
par(new=T)
plot(表1$模試回, 表1$D組, type="o", lty=4, pch=4, col=4,
     ylim=c(40, 80), xaxp=c(1,4,3), axes=F, ann=F)

legend("topleft", legend=names(表1)[2:5],
       lty=1:4, pch=1:4, col=1:4)

# レイダーチャート編
install.packages("fmsb")
library("fmsb")

表2 <- read.table("ビッグファイブ.txt", header=T)

maxmin <- data.frame(
  協調性=c(7,0),
  誠実性=c(7,0),
  外向性=c(7,0),
  心配性=c(7,0),
  開放性=c(7,0))

data <- rbind(maxmin, 表2)
radarchart(data, seg=7, centerzero=T,
           title="GPT-4 による人間の性格の擬態")
legend("topleft", legend=c("擬態の対象", "擬態の結果"),
       lty=1:2, pch=16, col=c("black", "red"))

barplot(as.matrix(表2), beside=T, ylim=c(0, 7),
        yaxp=c(1,7,6), col=c("black", "red"),
        density=c(25, 50),
        legend.text=c("擬態の対象", "擬態の結果"),
        args.legend=list(x="topleft"),
        main="GPT-4 による人間の性格の擬態")

# さまざまなグラフ描画-1.R

