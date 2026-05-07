# 필수 패키지 설치 및 로드
if (!require("mclust")) install.packages("mclust")
if (!require("umap")) install.packages("umap")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("VIM")) install.packages("VIM") 

library(mclust)
library(umap)
library(dplyr)
library(ggplot2)
library(VIM)

# 1. 데이터 로드 (사용자 데이터셋 구조 가정)
# [지점명, TOC, T-N, T-P, Cl, B, 15N, 18O, 11B 등]
data <- read.csv("water_quality_data.csv", fileEncoding = "euc-kr") 
features <- data %>% select(-spot) # 지점명을 제외한 수질 및 동위원소 데이터만 추출

# 2. [추가] KNN 기반 결측치 보정
# 주변 데이터의 패턴을 분석하여 가장 합리적인 수치로 빈칸을 채웁니다.
imputed_features <- kNN(features, k = 5, imp_var = FALSE)

# 3. 데이터 표준화 (Scale)
scaled_features <- scale(features)

# 4. [ML 기법 1] Gaussian Mixture Model (GMM) - 오염원 그룹화
# 정답이 없는 상태에서 데이터가 어떤 '확률적 덩어리'를 형성하는지 찾아냅니다.
gmm_model <- Mclust(scaled_features)
data$Cluster <- as.factor(gmm_model$classification)

# 5. [ML 기법 2] UMAP - 고차원 현미경 시각화
# 10개 이상의 변수를 2차원 평면에 투영하여 오염원 간의 중첩과 분리 상태를 보여줍니다.
umap_results <- umap(scaled_features)
data$umap1 <- umap_results$layout[,1]
data$umap2 <- umap_results$layout[,2]

# 6. 결과 시각화 (연구자 간 의견 통일을 위한 시각적 지표)
ggplot(data, aes(x = umap1, y = umap2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Water Quality & Isotope Fingerprint Map",
       subtitle = "GMM Clustering on UMAP Projection")

# 7. [ML 기법 3] Cluster Profile - 표준 답안 제시
# 각 클러스터(오염원 그룹)의 평균 수치를 계산하여 어떤 특성이 강한지 리포트합니다.
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean))

print(cluster_summary)