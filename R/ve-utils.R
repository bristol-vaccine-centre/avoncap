
# This assumes that OR ~ RR which is only true if controls >> cases
odds_ratio_ve = function(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl, p=c(0.025,0.975)) {

  oddsR = case_when(
    vaccinatedCase == 0 | unvaccinatedCase == 0 | unvaccinatedControl == 0 | vaccinatedControl == 0 ~ NA_real_,
    TRUE ~ (vaccinatedCase/vaccinatedControl) / (unvaccinatedCase/unvaccinatedControl)
  )
  logOdds = log(oddsR)
  logOddsSD = sqrt(1/vaccinatedCase+1/unvaccinatedCase+1/vaccinatedControl+1/unvaccinatedControl)
  oddsQ = purrr::map(p, ~ exp(stats::qnorm(.x,logOdds, logOddsSD)))
  names(oddsQ) = paste0("OR.q.",p)
  veQ = purrr::map(rev(p), ~ 1 - exp(stats::qnorm(.x,logOdds, logOddsSD)))
  names(veQ) = paste0("VE.OR.q.",p)
  cbind(tibble::tibble(OR = oddsR),as.data.frame(oddsQ),tibble::tibble(VE.OR = 1-oddsR),as.data.frame(veQ))

}

# This is not calculatable in the context of a case control study I think
relative_risk_ve = function(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl, p=c(0.025,0.975)) {
  RR = case_when(
    vaccinatedCase == 0 | unvaccinatedCase == 0 | unvaccinatedControl == 0 | vaccinatedControl == 0 ~ NA_real_,
    TRUE ~ (vaccinatedCase/(vaccinatedCase+vaccinatedControl)) / (unvaccinatedCase/(unvaccinatedCase+unvaccinatedControl))
  )
  logRR = log(RR)
  logRRSD = sqrt(1/vaccinatedCase+1/unvaccinatedCase+1/vaccinatedControl+1/unvaccinatedControl)
  oddsQ = purrr::map(p, ~ exp(stats::qnorm(.x,logRR, logRRSD)))
  names(oddsQ) = paste0("RR.q.",p)
  veQ = purrr::map(rev(p), ~ 1 - exp(stats::qnorm(.x,logRR, logRRSD)))
  names(veQ) = paste0("VE.RR.q.",p)
  cbind(tibble::tibble(RR = RR),as.data.frame(oddsQ),tibble::tibble(VE.RR = 1-RR),as.data.frame(veQ))
}
