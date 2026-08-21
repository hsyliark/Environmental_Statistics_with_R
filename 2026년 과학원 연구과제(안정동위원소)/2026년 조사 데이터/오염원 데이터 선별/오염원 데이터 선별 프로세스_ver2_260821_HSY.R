# ==============================================================================
# [오염원 데이터 선별 통합 스크립트] 4단계 지구화학·통계 복합 정제 프레임워크
# 작성자: 안정동위원소 연구팀팀
# 목  적: Kendall Domain, KDE 95% HDR, 탈질화 벡터 및 Gate-Review 체계 구현
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 라이브러리 로드 및 환경 설정
# ------------------------------------------------------------------------------
library(dplyr)    # 데이터 파이프라인 처리 및 집계 연산
library(ggplot2)  # 고해상도 시각화 및 Kendall Plot 산출
library(ks)       # 2차원 비파라미터 커널밀도추정(Bivariate KDE) 수행

# 작업 경로 확인 및 파일 존재 여부 점검
file_path <- "(2026년) 오염원 데이터 선별_선행연구.csv"
if (!file.exists(file_path)) {
  stop("입력 데이터 파일이 존재하지 않습니다. 파일명을 확인해 주세요.")
}

# 원천 데이터 로드 (CP949 한글 인코딩 적용)
raw_df <- read.csv(file_path, fileEncoding = "CP949", stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------
# [1단계] 세부 오염원별 Kendall Bounding Polygon 1차 필터링 (Geochemical Masking)
# ------------------------------------------------------------------------------
# [분석 수행] 
#  - 문헌상(Kendall et al.) 입증된 오염원별 d15N, d18O 정상 도메인(Bounding Domain) 정의
#  - 통계적 거리에 의해 정상 시료가 탈락하는 왜곡을 막기 위해 1차 보존 마스크 적용
# [산출물] Kendall_Pass (TRUE/FALSE) 열이 포함된 데이터프레임 (df_step1)
# ------------------------------------------------------------------------------

apply_kendall_mask <- function(data) {
  data %>% mutate(
    Kendall_Pass = case_when(
      구분 == "강우계" & (d15N >= -15 & d15N <= 15) & (d18O >= 25 & d18O <= 75) ~ TRUE,
      구분 == "토양계" & (d15N >= 0 & d15N <= 10)  & (d18O >= -5 & d18O <= 15) ~ TRUE,
      구분 == "축산계" & (d15N >= 4 & d15N <= 30)   & (d18O >= -5 & d18O <= 15) ~ TRUE,
      구분 == "생활계" & (d15N >= 4 & d15N <= 25)   & (d18O >= -5 & d18O <= 15) ~ TRUE,
      구분 == "농업계" & (d15N >= -6 & d15N <= 25)  & (d18O >= -10 & d18O <= 25) ~ TRUE,
      TRUE ~ FALSE # 도메인을 완전히 벗어난 시료는 1차 이상치 후보(FALSE) 처리
    )
  )
}

df_step1 <- apply_kendall_mask(raw_df)


# ------------------------------------------------------------------------------
# [2단계] 비파라미터 커널밀도추정(KDE) 기반 고밀도 영역(HDR) 선별
# ------------------------------------------------------------------------------
# [분석 수행]
#  - 오염원 그룹별(group_by) 이변량 커널밀도 함수(ks::kde) 구축
#  - 단일 타원 가정이 아닌 실제 시료의 다봉성(Multimodality) 반영
#  - 확률밀도 상위 95% 고밀도 영역(High Density Region) 컷오프(fhat$cont["5%"]) 계산
# [산출물] KDE_Pass (TRUE/FALSE) 열이 결합된 데이터프레임 (df_step2)
# ------------------------------------------------------------------------------

df_step2 <- df_step1 %>%
  group_by(구분) %>%
  group_modify(~ {
    sub_df <- .x
    # 커널밀도추정을 위해 최소 5개 이상의 샘플 필요
    if (nrow(sub_df) >= 5) {
      # 2차원 가우시안 커널 밀도 추정 (자동 대역폭 행렬 계산)
      fhat <- ks::kde(x = sub_df[, c("d15N", "d18O")])
      # 개별 시료 위치에서의 확률밀도값 평가
      eval_pts <- ks::predict.kde(fhat, x = sub_df[, c("d15N", "d18O")])
      # 상위 95% 밀도 경계선(하위 5% 밀도 임계값) 이상 시료 선별
      sub_df$KDE_Pass <- eval_pts >= fhat$cont["5%"]
    } else {
      sub_df$KDE_Pass <- TRUE # 샘플 수 부족 시 전원 통과
    }
    return(sub_df)
  }) %>% 
  ungroup()


# ------------------------------------------------------------------------------
# [3단계] 생화학적 변환 벡터(탈질화 기울기) 연장선 보정
# ------------------------------------------------------------------------------
# [분석 수행]
#  - 1~2 단계를 통과한 고밀도 정상 시료들로부터 오염원별 대표 중심점(Mean Mode) 산출
#  - 2단계 KDE 경계 외곽 시료 중, 중심점 대비 탈질화 동역학 기울기(0.45 ~ 0.85) 범위 및
#    동시 증가 방향성(d15N > 0, d18O > 0)을 만족하는 생화학적 변환 시료 식별 및 구제
# [산출물] Denit_Pass (TRUE/FALSE) 열이 추가된 데이터프레임 (df_step3)
# ------------------------------------------------------------------------------

# 1) 1~2단계 검증 완료된 고밀도 시료 기반 오염원별 대표 중심점 산출
group_centers <- df_step2 %>%
  filter(Kendall_Pass == TRUE & KDE_Pass == TRUE) %>%
  group_by(구분) %>%
  summarise(
    center_d15N = mean(d15N, na.rm = TRUE),
    center_d18O = mean(d18O, na.rm = TRUE),
    .groups = "drop"
  )

# 2) 탈질화 벡터 검증 함수 적용
df_step3 <- df_step2 %>%
  left_join(group_centers, by = "구분") %>%
  mutate(
    delta_d15N = d15N - center_d15N,
    delta_d18O = d18O - center_d18O,
    vector_slope = delta_d18O / delta_d15N,
    # 탈질화 조건: d15N, d18O 모두 증가 방향이며 기울기가 0.45 ~ 0.85 범위 내에 위치
    Denit_Pass = ifelse(
      Kendall_Pass == TRUE & 
        delta_d15N > 0 & delta_d18O > 0 & 
        vector_slope >= 0.45 & vector_slope <= 0.85,
      TRUE, FALSE
    )
  )


# ------------------------------------------------------------------------------
# [4단계] 단계별 연구진 수시 검토 체계 (Interactive Gate-Review Protocol)
# ------------------------------------------------------------------------------
# [분석 수행]
#  - 1~3단계 통계·지구화학 판정 결과를 연계하여 final_decision 구분 플래그 부여
#  - Pass (Approved): 고밀도 중심 영역 시료 (자동 승인)
#  - Pass (Denitrification): 탈질화 변환 궤적 부합 시료 (복원 승인)
#  - Review Required: Kendall 도메인 내 존재하나 저밀도 및 벡터 미부합 (연구진 심의 대상)
#  - Drop (Outlier): Kendall 도메인 완전 이탈 이상치 (제외 승인)
# [산출물] 
#  1) 오염원_선별결과_GateReview_Log.csv (전체 샘플 이력 메타데이터)
#  2) 오염원_선별_단계별_집계.csv (그룹별 선별 요약 통계표)
#  3) 오염원_선별결과_KendallPlot.png (고해상도 2D 시각화 플롯 파일)
# ------------------------------------------------------------------------------

df_final <- df_step3 %>%
  mutate(
    Final_Decision = case_when(
      Kendall_Pass == TRUE & KDE_Pass == TRUE ~ "Pass (Approved)",
      Kendall_Pass == TRUE & Denit_Pass == TRUE ~ "Pass (Denitrification)",
      Kendall_Pass == TRUE & KDE_Pass == FALSE & Denit_Pass == FALSE ~ "Review Required",
      TRUE ~ "Drop (Outlier)"
    )
  )

# 1) 전체 메타데이터 및 Audit Log CSV 저장
write.csv(df_final, "오염원_선별결과_GateReview_Log.csv", row.names = FALSE, fileEncoding = "CP949")

# 2) 그룹별 단계별 처리 현황 요약 집계표 생성 및 저장
summary_table <- df_final %>%
  group_by(구분, Final_Decision) %>%
  summarise(Sample_Count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Final_Decision, values_from = Sample_Count, values_fill = 0)

write.csv(summary_table, "오염원_선별_단계별_집계.csv", row.names = FALSE, fileEncoding = "CP949")

# 3) 연구진 검토용 고해상도 시각화 Plot 생성
p <- ggplot(df_final, aes(x = d15N, y = d18O, color = 구분, shape = Final_Decision)) +
  geom_point(size = 3, alpha = 0.85) +
  scale_shape_manual(
    values = c(
      "Pass (Approved)" = 16,        # 채워진 원형
      "Pass (Denitrification)" = 17, # 채워진 삼각형
      "Review Required" = 15,       # 채워진 사각형 (심의 대상)
      "Drop (Outlier)" = 4           # X 표시 (이상치)
    )
  ) +
  theme_bw(base_size = 12) +
  labs(
    title = "오염원별 4단계 정제 프레임워크 선별 결과 (Kendall Plot)",
    subtitle = "1단계 Domain Masking -> 2단계 KDE 95% HDR -> 3단계 탈질화 벡터 -> 4단계 Gate-Review",
    x = expression(delta^{15}*N~"(‰)"),
    y = expression(delta^{18}*O~"(‰)"),
    color = "오염원 구분",
    shape = "선별 판정 상태"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# 시각화 이미지 파일 저장 (PNG, 300 DPI)
ggsave("오염원_선별결과_KendallPlot.png", plot = p, width = 10, height = 7, dpi = 300)

# 콘솔 화면 출력
print(summary_table)
print(p)