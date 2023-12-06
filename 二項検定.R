TOP_MARGIN <- 0.05 # グラフの上マージン

# 関数 bidist(n, p) : 二項分布を返す
# n : 標本サイズ
# p : 現象が起きる確率
# 戻り値 : 確率分布 (サイズ = n + 1)
bidist <- function(n, p) {
  確率 <- c() # 求める確率分布 (最初は空)

  # x が 0 から標本サイズまで以下を繰り返す。
  for (x in 0:n) {
    # 確率を求めて追記する。
    確率 <- c(確率, choose(n,x)*p^x*(1-p)^(n-x))
  }

  確率  
}

# 関数 binull(n, p) : 帰無分布を描画し棄却域を赤で示す
# n : 標本サイズ
# p : 帰無仮説が前提とする確率
binull <- function(n, p) {
  確率 <- bidist(n, p)

  top <- max(確率) + TOP_MARGIN

  if (確率[1] < 確率[n + 1]) {
    halfp <- 0 # 片側の棄却域の確率を求めたい

    # x が標本サイズから 0 まで以下を繰り返す。
    x <- n
    while (x >= 0) {
      # 棄却域の片側の確率が 0.025 を超えるなら停止する。
      if (halfp + 確率[x+1] > 0.025) {
        break    
      }
      halfp <- halfp + 確率[x+1] # 確率に加算する
      x <- x - 1
    }

    r <- n - x # 右側の棄却域の幅
    左側棄却域 <- 0.05 - halfp # 両側を足して 5% の棄却域

    halfp <- 0 # 反対側についても求めたい

    # x が 0 から標本サイズまで以下を繰り返す。
    for (x in 0:n) {
      # 確率が「左側棄却域」を超えるなら停止する。
      if (halfp + 確率[x+1] > 左側棄却域) {
        break    
      }
      halfp <- halfp + 確率[x+1] # 確率に加算する
    }

    l <- x # 左側の棄却域の幅

  } else {
    halfp <- 0 # 片側の棄却域の確率を求めたい

    # x が 0 から標本サイズまで以下を繰り返す。
    for (x in 0:n) {
      # 棄却域の片側の確率が 0.025 を超えるなら停止する。
      if (halfp + 確率[x+1] > 0.025) {
        break    
      }
      halfp <- halfp + 確率[x+1] # 確率に加算する
    }

    l <- x # 左側の棄却域の幅
    右側棄却域 <- 0.05 - halfp # 両側を足して 5% の棄却域

    halfp <- 0 # 反対側についても求めたい

    # x が標本サイズから 0 まで以下を繰り返す。
    x <- n
    while (x >= 0) {
      # 確率が「右側棄却域」を超えるなら停止する。
      if (halfp + 確率[x+1] > 右側棄却域) {
        break    
      }
      halfp <- halfp + 確率[x+1] # 確率に加算する
      x <- x - 1
    }

    r <- n - x # 右側の棄却域の幅
  }

  # 棄却域を赤で、そうでないなら黒で表示する準備
  lcolor <- rep(c("red"), l)
  rcolor <- rep(c("red"), r)
  color <- c(lcolor, rep(c("gray"), n + 1 - (l + r)), rcolor)

  検出回数 <- 0:n # ヒストグラムのx軸の目盛り

  # ヒストグラム型を指定して plot で描画する (lwd は線の太さ)。
  plot(検出回数, 確率, type="h", lwd=3, col=color, ylim=c(0, top))
}

# 関数 bidistg(n, p0, p) : 二項分布を重ねて描画する
# n  : 標本サイズ
# p0 : 帰無仮説が前提とする確率
# p  : 現象が起きる確率
bidistg <- function(n, p0, p) {
  # グラフの Y軸の範囲を得る
  確率 <- bidist(n, p0)
  top <- max(確率) + TOP_MARGIN

  binull(n, p0) # 帰無分布を描画する

  # 真の分布を取得する
  確率 <- bidist(n, p)
  検出回数 <- 0:n # ヒストグラムのx軸の目盛り

  # ヒストグラム型を指定して plot で重ねて描画する
  par(new=T) # 描画領域をクリアしない
  plot(検出回数, 確率, type="h", ann=F, col=c("blue"), ylim=c(0, top))
}

# end of 二項検定.R
