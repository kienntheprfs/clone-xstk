library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)
library(readxl)
library(corrplot)

# Đọc dữ liệu và gán các giá trị chuỗi rỗng và "N/A" thành NA
All_GPUs <- read.csv("All_GPUs.csv", na.strings = c("", "N/A"))

# Làm sạch dữ liệu

# Trích ra một dữ liệu con đặt tên là new_CPUs_DF chỉ bao gồm các biến mà ta quan tâm  # nolint
new_GPUs_DF <- All_GPUs[, c(1, 4, 13, 14, 15, 16, 17, 25), ]
new_GPUs_DF$Core_Speed <- gsub("- ", NA, new_GPUs_DF$Core_Speed)
head(new_GPUs_DF, 10)

# Tính toán tỉ lệ khuyết của mỗi cột dữ liệu và biểu diễn bằng đồ thị
missRateDataFrames <- (colMeans(is.na(new_GPUs_DF)))*100
mrdf <- data.frame(Category = names(missRateDataFrames), percentage = missRateDataFrames)

# Vẽ biểu đồ cột
ggplot(mrdf, aes(x = Category, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Missing Data Rate")

# Kiểm tra các dữ liệu bị khuyết
missRateArray = colSums(is.na(new_GPUs_DF))
for(i in 1:length(missRateArray)) {
  cat(names(missRateArray)[i], ": ", missRateArray[i], "\n")
}

# Xóa các hàng mà có giá trị NA hoặc trống ở các cột không thể thay số
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,1]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,2]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,6]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,7]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,17]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,21]), ]
new_GPUs_DF <- new_GPUs_DF[!is.na(new_GPUs_DF[,27]), ]

# Đổi tên cột và bỏ chuỗi Đơn vị và đem các đơn vị lên để chung với tên cột
new_GPUs_DF$Boost_Clock <- as.numeric(gsub("MHz", "", new_GPUs_DF$Boost_Clock))
colnames(new_GPUs_DF)[3] <- "Boost_Clock(MHz)"
new_GPUs_DF$Core_Speed <- as.numeric(gsub("MHz", "", new_GPUs_DF$Core_Speed))
colnames(new_GPUs_DF)[4] <- "Core_Speed(MHz)"
new_GPUs_DF$Max_Power <- as.numeric(gsub("Watts", "", new_GPUs_DF$Max_Power))
colnames(new_GPUs_DF)[12] <- "Max_Power(Watts)"
new_GPUs_DF$Memory <- as.numeric(gsub("MB", "", new_GPUs_DF$Memory))
colnames(new_GPUs_DF)[13] <- "Memory(MB)"
new_GPUs_DF$Memory_Bandwidth <- as.numeric(gsub("GB/sec", "", new_GPUs_DF$Memory_Bandwidth))
colnames(new_GPUs_DF)[14] <- "Memory_Bandwidth(GB/sec)"
new_GPUs_DF$Memory_Bus <- as.numeric(gsub("Bit", "", new_GPUs_DF$Memory_Bus))
colnames(new_GPUs_DF)[15] <- "Memory_Bus(Bit)"
new_GPUs_DF$Memory_Speed <- as.numeric(gsub("MHz", "", new_GPUs_DF$Memory_Speed))
colnames(new_GPUs_DF)[16] <- "Memory_Speed(MHz)"
new_GPUs_DF$Pixel_Rate <- as.numeric(gsub("GPixel/s", "", new_GPUs_DF$Pixel_Rate))
colnames(new_GPUs_DF)[22] <- "Pixel_Rate(GPixel/s)"
new_GPUs_DF$Process <- as.numeric(gsub("nm", "", new_GPUs_DF$Process))
colnames(new_GPUs_DF)[24] <- "Process(nm)"
new_GPUs_DF$Texture_Rate <- as.numeric(gsub("GTexel/s", "", new_GPUs_DF$Texture_Rate))
colnames(new_GPUs_DF)[30] <- "Texture_Rate(GTexel/s)"

# Loại bỏ các Outlier nằm ngoài đoạn [Q1 - 1.5 * (Q3 - Q1); Q3 + 1.5 * (Q3 - Q1)]
quartiles <- quantile(new_GPUs_DF$`Boost_Clock(MHz)`, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$`Boost_Clock(MHz)`, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$`Boost_Clock(MHz)` > Lower & new_GPUs_DF$`Boost_Clock(MHz)` < Upper)

quartiles <- quantile(new_GPUs_DF$`Core_Speed(MHz)`, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$`Core_Speed(MHz)`, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$`Core_Speed(MHz)` > Lower & new_GPUs_DF$`Core_Speed(MHz)` < Upper)

quartiles <- quantile(new_GPUs_DF$DVI_Connection, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$DVI_Connection, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$DVI_Connection > Lower & new_GPUs_DF$DVI_Connection < Upper)

quartiles <- quantile(new_GPUs_DF$HDMI_Connection, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$HDMI_Connection, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$HDMI_Connection > Lower & new_GPUs_DF$HDMI_Connection < Upper)

quartiles <- quantile(new_GPUs_DF$`Max_Power(Watts)`, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$`Max_Power(Watts)`, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$`Max_Power(Watts)` > Lower & new_GPUs_DF$`Max_Power(Watts)` < Upper)

quartiles <- quantile(new_GPUs_DF$`Memory(MB)`, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$`Memory(MB)`, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$`Memory(MB)` > Lower & new_GPUs_DF$`Memory(MB)` < Upper)

quartiles <- quantile(new_GPUs_DF$Open_GL, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$Open_GL, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$Open_GL > Lower & new_GPUs_DF$Open_GL < Upper)

quartiles <- quantile(new_GPUs_DF$Shader, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$Shader, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$Shader > Lower & new_GPUs_DF$Shader < Upper)

quartiles <- quantile(new_GPUs_DF$VGA_Connection, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(new_GPUs_DF$VGA_Connection, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
new_GPUs_DF <- subset(new_GPUs_DF, new_GPUs_DF$VGA_Connection > Lower & new_GPUs_DF$VGA_Connection < Upper)

# Tính giá trị trung bình cho các cột và gán vào các ô "NA"
mean_values <- mean(new_GPUs_DF$`Boost_Clock(MHz)`, na.rm = TRUE)
new_GPUs_DF$`Boost_Clock(MHz)`[is.na(new_GPUs_DF$`Boost_Clock(MHz)`)] <- mean_values
mean_values <- mean(new_GPUs_DF$`Core_Speed(MHz)`, na.rm = TRUE)
new_GPUs_DF$`Core_Speed(MHz)`[is.na(new_GPUs_DF$`Core_Speed(MHz)`)] <- mean_values
mean_values <- mean(new_GPUs_DF$DVI_Connection, na.rm = TRUE)
new_GPUs_DF$DVI_Connection[is.na(new_GPUs_DF$DVI_Connection)] <- mean_values
mean_values <- mean(new_GPUs_DF$HDMI_Connection, na.rm = TRUE)
new_GPUs_DF$HDMI_Connection[is.na(new_GPUs_DF$HDMI_Connection)] <- mean_values
mean_values <- mean(new_GPUs_DF$`Max_Power(Watts)`, na.rm = TRUE)
new_GPUs_DF$`Max_Power(Watts)`[is.na(new_GPUs_DF$`Max_Power(Watts)`)] <- mean_values
mean_values <- mean(new_GPUs_DF$`Memory(MB)`, na.rm = TRUE)
new_GPUs_DF$`Memory(MB)`[is.na(new_GPUs_DF$`Memory(MB)`)] <- mean_values
mean_values <- mean(new_GPUs_DF$Open_GL, na.rm = TRUE)
new_GPUs_DF$Open_GL[is.na(new_GPUs_DF$Open_GL)] <- mean_values
mean_values <- mean(new_GPUs_DF$Shader, na.rm = TRUE)
new_GPUs_DF$Shader[is.na(new_GPUs_DF$Shader)] <- mean_values
mean_values <- mean(new_GPUs_DF$VGA_Connection, na.rm = TRUE)
new_GPUs_DF$VGA_Connection[is.na(new_GPUs_DF$VGA_Connection)] <- mean_values

# Thống kê số lượng cho từng Architecture đối với cột Architecture
table(new_GPUs_DF$Architecture)

# Thống kê số lượng cho từng resolution đối với cột Best_Resolution và vẽ biểu đồ cột
frequencyOfBestResolution <- table(new_GPUs_DF$Best_Resolution)
BestResolutionColumnChart <- data.frame(BestResolution = names(frequencyOfBestResolution), Quantity = frequencyOfBestResolution)
colnames(BestResolutionColumnChart)[1] <- "Resolution"
colnames(BestResolutionColumnChart)[3] <- "Quantity"

ggplot(BestResolutionColumnChart, aes(x = Resolution, y = Quantity)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Best Resolution Data Visualization")

# Thống kê mô tả cho toàn bộ dữ liệu
summary_and_table <- function(data) {
  # Lấy danh sách các cột là biến liên tục
  continuous_cols <- sapply(data, is.numeric)
  
  # Tính toán các giá trị thống kê cho các cột là biến liên tục
  continuous_summary <- summary(data[, continuous_cols])
  
  # Lấy danh sách các cột là biến phân loại
  categorical_cols <- !continuous_cols
  
  # Thống kê số lượng cho từng củng loại 
  categorical_table <- lapply(data[, categorical_cols], table)
  
  # Kết hợp cả 2 kết quả
  list(continuous_summary = continuous_summary, categorical_table = categorical_table)
}

thong_ke_mo_ta_cho_bien_phan_loai <- summary_and_table(new_GPUs_DF)$categorical_table
thong_ke_mo_ta_cho_bien_lien_tuc <- summary_and_table(new_GPUs_DF)$continuous_summary
# print(thong_ke_mo_ta_cho_bien_phan_loai)
# print(thong_ke_mo_ta_cho_bien_lien_tuc)

# Vẽ đồ thị phân phối cho cột Boost_Clock(MHz)
hist(new_GPUs_DF$`Boost_Clock(MHz)`)
boxplot(new_GPUs_DF$`Boost_Clock(MHz)`)

# Vẽ đồ thị phân phối cho cột Core_Speed(MHz)
hist(new_GPUs_DF$`Core_Speed(MHz)`)
boxplot(new_GPUs_DF$`Core_Speed(MHz)`)

# Vẽ đồ thị phân phối cho cột DVI_Connection
hist(new_GPUs_DF$DVI_Connection)
boxplot(new_GPUs_DF$DVI_Connection)

# Vẽ đồ thị phân phối cho cột HDMI_Connection
hist(new_GPUs_DF$HDMI_Connection)
boxplot(new_GPUs_DF$HDMI_Connection)

# Vẽ đồ thị phân phối cho cột Max_Power(Watts)
hist(new_GPUs_DF$`Max_Power(Watts)`)
boxplot(new_GPUs_DF$`Max_Power(Watts)`)

# Vẽ đồ thị phân phối cho cột Memory(MB)
hist(new_GPUs_DF$`Memory(MB)`)
boxplot(new_GPUs_DF$`Memory(MB)`)

# Vẽ đồ thị phân phối cho cột Memory_Bandwidth(GB/sec)
hist(new_GPUs_DF$`Memory_Bandwidth(GB/sec)`)
boxplot(new_GPUs_DF$`Memory_Bandwidth(GB/sec)`)

# Vẽ đồ thị phân phối cho cột Memory_Bus(Bit)
hist(new_GPUs_DF$`Memory_Bus(Bit)`)
boxplot(new_GPUs_DF$`Memory_Bus(Bit)`)

# Vẽ đồ thị phân phối cho cột Memory_Speed(MHz)
hist(new_GPUs_DF$`Memory_Speed(MHz)`)
boxplot(new_GPUs_DF$`Memory_Speed(MHz)`)

# Vẽ đồ thị phân phối cho cột Open_GL
hist(new_GPUs_DF$Open_GL)
boxplot(new_GPUs_DF$Open_GL)

# Vẽ đồ thị phân phối cho cột Pixel_Rate(GPixel/s)
hist(new_GPUs_DF$`Pixel_Rate(GPixel/s)`)
boxplot(new_GPUs_DF$`Pixel_Rate(GPixel/s)`)

# Vẽ đồ thị phân phối cho cột Process(nm)
hist(new_GPUs_DF$`Process(nm)`)
boxplot(new_GPUs_DF$`Process(nm)`)

# Vẽ đồ thị phân phối cho cột Shader
hist(new_GPUs_DF$Shader)
boxplot(new_GPUs_DF$Shader)

# Vẽ đồ thị phân phối cho cột TMUs
hist(new_GPUs_DF$TMUs)
boxplot(new_GPUs_DF$TMUs)

# Vẽ đồ thị phân phối cho cột Texture_Rate(GTexel/s)
hist(new_GPUs_DF$`Texture_Rate(GTexel/s)`)
boxplot(new_GPUs_DF$`Texture_Rate(GTexel/s)`)

# Vẽ đồ thị phân phối cho cột VGA_Connection
hist(new_GPUs_DF$VGA_Connection)
boxplot(new_GPUs_DF$VGA_Connection)

# Vẽ ma trận tương quan
corrplot(cor(new_GPUs_DF[, sapply(new_GPUs_DF, is.numeric)]), method = "color", addCoef.col = "pink", number.cex = 0.5)