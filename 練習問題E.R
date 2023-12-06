sample <- c(4.2, 5.7, 9.3, 7.4, 4.4, 6.6, 6.1, 7.1)

# 標本平均、標本分散、標本標準偏差を計算する。
mean <- sum(sample)/length(sample)
var <- sum((sample[1:8] - mean)^2/(length(sample) - 1))
sd <- sqrt(var)

print(paste("標本平均", mean))
print(paste("標本分散", var))
print(paste("標本標準偏差", sd))

print(paste("関数による標本平均", mean(sample)))
print(paste("関数による標本分散", var(sample)))
print(paste("関数による標本標準偏差", sd(sample)))

# end of 練習問題E.R
