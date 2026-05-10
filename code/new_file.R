# 1. 准备数据：房屋面积（平方米）和对应的房价（万元）
# 在实际应用中，通常会用 read.csv() 读取外部数据文件
area <- c(50, 75, 90, 110, 120)
price <- c(200, 300, 350, 420, 480)

# 将数据合并为一个数据框
house_data <- data.frame(area = area, price = price)

# 2. 建立线性回归模型
# lm() 是 R 语言中专门用于拟合线性模型的函数 (Linear Model)
# price ~ area 表示用 area 来预测 price
model <- lm(price ~ area, data = house_data)

# 3. 查看模型结果
# summary() 可以输出模型的详细统计信息，包括系数、R方值等
print("=== 线性回归模型摘要 ===")
print(summary(model))

# 提取回归方程的截距(w0)和斜率(w1)
w0 <- coef(model)[1]
w1 <- coef(model)[2]
cat(sprintf("\n拟合出的回归方程为: 房价 = %.2f + %.2f * 面积\n", w0, w1))

# 4. 预测新数据
# 假设我们要预测面积为 80 平方米和 100 平方米的房屋价格
new_houses <- data.frame(area = c(80, 100))
predicted_prices <- predict(model, new_houses)

cat("\n=== 预测结果 ===\n")
for (i in 1:nrow(new_houses)) {
  cat(sprintf("面积为 %d 平方米的房屋，预测房价为: %.2f 万元\n", 
              new_houses$area[i], predicted_prices[i]))
}

# 5. 数据可视化（如果在支持图形界面的环境下运行，会弹出绘图窗口）
plot(house_data$area, house_data$price, 
     main = "房屋面积与房价的线性回归分析",
     xlab = "房屋面积 (平方米)", ylab = "房价 (万元)",
     pch = 19, col = "blue")
# 在散点图上画出拟合出的回归线
abline(model, col = "red", lwd = 2)
legend("topleft", legend = c("实际数据", "回归拟合线"), 
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = 2)