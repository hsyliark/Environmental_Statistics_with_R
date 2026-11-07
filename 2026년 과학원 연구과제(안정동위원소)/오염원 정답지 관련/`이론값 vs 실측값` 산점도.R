# 필요한 패키지 로드
library(readxl)
library(ggplot2)
library(dplyr)

# 1. 엑셀 파일 경로 설정 (작업 디렉토리에 파일 위치 확인)
file_path <- "교차검증 관련 자료 정리 (y=x 직선과 비교).xlsx"

# ----------------------------------------------------
# 1) 질소 안정동위원소비 (d15N-NO3) 산점도
# ----------------------------------------------------
df_15n <- read_excel(file_path, sheet = "자료 정리(15N)")
r2_15n <- summary(lm(Theoretical ~ d15N, data = df_15n))$r.squared

p_15n <- ggplot(df_15n, aes(x = d15N, y = Theoretical)) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  labs(
    title = expression(bold("Cross-validation: " * delta^15 * "N-NO"[3])),
    x = expression("Measured (" * delta^15 * "N-NO"[3] * ", 실측값)"),
    y = expression("Theoretical (" * delta^15 * "N-NO"[3] * ", 이론값)")
  ) +
  annotate("text", x = min(df_15n$d15N), y = max(df_15n$Theoretical), 
           label = sprintf("R^2 == %.4f", r2_15n), parse = TRUE, hjust = 0, vjust = 1, size = 4.5) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_15n)

# ----------------------------------------------------
# 2) 산소 안정동위원소비 (d18O-NO3) 산점도
# ----------------------------------------------------
df_18o <- read_excel(file_path, sheet = "자료 정리(18O)")
r2_18o <- summary(lm(Theoretical ~ d18O, data = df_18o))$r.squared

p_18o <- ggplot(df_18o, aes(x = d18O, y = Theoretical)) +
  geom_point(color = "forestgreen", size = 2.5, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  labs(
    title = expression(bold("Cross-validation: " * delta^18 * "O-NO"[3])),
    x = expression("Measured (" * delta^18 * "O-NO"[3] * ", 실측값)"),
    y = expression("Theoretical (" * delta^18 * "O-NO"[3] * ", 이론값)")
  ) +
  annotate("text", x = min(df_18o$d18O), y = max(df_18o$Theoretical), 
           label = sprintf("R^2 == %.4f", r2_18o), parse = TRUE, hjust = 0, vjust = 1, size = 4.5) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_18o)

# ----------------------------------------------------
# 3) 오염원 기여율: 이론값 vs EMMTE 모델 산정값
# ----------------------------------------------------
df_contrib <- read_excel(file_path, sheet = "자료 정리(기여율)")
r2_emmte <- summary(lm(Theoretical ~ EMMTE, data = df_contrib))$r.squared

p_emmte <- ggplot(df_contrib, aes(x = EMMTE, y = Theoretical)) +
  geom_point(color = "darkorange", size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  xlim(0, 100) + ylim(0, 100) +
  labs(
    title = "Contr. Ratio: EMMTE vs Theoretical",
    x = "EMMTE Estimated (%)",
    y = "Theoretical (%)"
  ) +
  annotate("text", x = 5, y = 95, 
           label = sprintf("R^2 == %.4f", r2_emmte), parse = TRUE, hjust = 0, vjust = 1, size = 4.5) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_emmte)

# ----------------------------------------------------
# 4) 오염원 기여율: 이론값 vs MixSIAR 모델 산정값
# ----------------------------------------------------
r2_mixsiar <- summary(lm(Theoretical ~ MixSIAR, data = df_contrib))$r.squared

p_mixsiar <- ggplot(df_contrib, aes(x = MixSIAR, y = Theoretical)) +
  geom_point(color = "purple", size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  xlim(0, 100) + ylim(0, 100) +
  labs(
    title = "Contr. Ratio: MixSIAR vs Theoretical",
    x = "MixSIAR Estimated (%)",
    y = "Theoretical (%)"
  ) +
  annotate("text", x = 5, y = 95, 
           label = sprintf("R^2 == %.4f", r2_mixsiar), parse = TRUE, hjust = 0, vjust = 1, size = 4.5) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_mixsiar)

# ----------------------------------------------------
# 5) 모델 간 교차검증: EMMTE vs MixSIAR
# ----------------------------------------------------
r2_models <- summary(lm(MixSIAR ~ EMMTE, data = df_contrib))$r.squared

p_models <- ggplot(df_contrib, aes(x = EMMTE, y = MixSIAR)) +
  geom_point(color = "darkcyan", size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  xlim(0, 100) + ylim(0, 100) +
  labs(
    title = "Model Cross-Validation: EMMTE vs MixSIAR",
    x = "EMMTE (%)",
    y = "MixSIAR (%)"
  ) +
  annotate("text", x = 5, y = 95, 
           label = sprintf("R^2 == %.4f", r2_models), parse = TRUE, hjust = 0, vjust = 1, size = 4.5) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_models)