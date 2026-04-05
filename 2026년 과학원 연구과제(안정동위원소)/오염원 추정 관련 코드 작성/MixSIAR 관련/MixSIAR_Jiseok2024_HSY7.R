library(MixSIAR)

# Load mix data
mix <- load_mix_data(filename="C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/7차 분석/ver1/Jiseok2024_consumer_ver1_site.csv",
                     iso_names=c("BIX","FI"),
                     factors=c("site"),
                     fac_random=c(FALSE),
                     fac_nested=c(FALSE),
                     cont_effects=NULL)

# Load source data
source <- load_source_data(filename="C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/7차 분석/ver1/Jiseok2024_sources_raw_ver1.csv",
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="raw",
                           mix)

# Load discrimination/TDF data
discr <- load_discr_data(filename="C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/7차 분석/ver1/Jiseok2024_discrimination_no_ver1.csv", 
                         mix)

# Make isospace plot
plot_data(filename="isospace_plot",
          plot_save_pdf=FALSE,
          plot_save_png=TRUE,
          mix,source,discr)

# Calculate standardized convex hull area
if(mix$n.iso==2) calc_area(source=source,mix=mix,discr=discr)

# Plot your prior
plot_prior(alpha.prior=1,source)

# Define model structure and write JAGS model file
model_filename <- "MixSIAR_model.txt"
resid_err <- FALSE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

# Run model
# JAGS output will be saved as 'jags.1'

# MCMC run options:
# run <- "test"       # chainLength=1000, burn=500, thin=1, chains=3, calcDIC=TRUE
# run <- "very short" # chainLength=10000, burn=5000, thin=5, chains=3, calcDIC=TRUE
# run <- "short"      # chainLength=50000, burn=25000, thin=25, chains=3, calcDIC=TRUE
# run <- "normal"     # chainLength=100000, burn=50000, thin=50, chains=3, calcDIC=TRUE
# run <- "long"       # chainLength=300000, burn=200000, thin=100, chains=3, calcDIC=TRUE
# run <- "very long"  # chainLength=1000000, burn=500000, thin=500, chains=3, calcDIC=TRUE
# run <- "extreme"    # chainLength=3000000, burn=1500000, thin=500, chains=3, calcDIC=TRUE

# Can also set custom MCMC parameters
# run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

# Good idea to use 'test' first to check if
#   1) the data are loaded correctly, and
#   2) the model is specified correctly
jags.1 <- run_model(run="test", mix, source, discr, model_filename, alpha.prior = 1)

# After a test run works, increase the MCMC run to a value that may converge
jags.1 <- run_model(run="very short", mix, source, discr, model_filename, alpha.prior = 1)
jags.1 <- run_model(run="short", mix, source, discr, model_filename, alpha.prior = 1)
jags.1 <- run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1)
jags.1 <- run_model(run="long", mix, source, discr, model_filename, alpha.prior = 1)
jags.1 <- run_model(run="very long", mix, source, discr, model_filename, alpha.prior = 1)
jags.1 <- run_model(run="extreme", mix, source, discr, model_filename, alpha.prior = 1)



# Process JAGS output

# Choose output options (see ?output_options for details)
output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE,
                       diag_save_ggmcmc = FALSE)

# Create diagnostics, summary statistics, and posterior plots
options(max.print=999999)
output_JAGS(jags.1, mix, source, output_options)

# get multiplicative error term estimates, median(xi.N) and median(xi.O)
xi.var1 <- round(median(jags.1$BUGSoutput$sims.list$resid.prop[,1]),1)
xi.var2 <- round(median(jags.1$BUGSoutput$sims.list$resid.prop[,2]),1)
xi.var1
xi.var2

