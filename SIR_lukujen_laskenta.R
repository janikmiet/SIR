library(readxl)
library(tidyverse)

## Luetaan originaali data ----
## Data tallennettu /data/ -kansioon
## Kaikki murtumat
dorg_kaikki <- readxl::read_xlsx("data/SIR-luvut.xlsx", sheet = "Taul2")
## Murtumat sleep apnea
dorg_slapnea <- readxl::read_xlsx("data/SIR-luvut.xlsx", sheet = "Taul3")
colnames(dorg_slapnea)[3:18] <- paste0(colnames(dorg_slapnea)[3:18], "_ref")
## Yhdistetään
dorg <- merge(dorg_kaikki, dorg_slapnea, by = c("SUKUP", "ika"))

## Laskenta ----
# ## TODO tämän voisi myös tehdä funktioilla, esim. niin että kaksi datasettiä jotka linkataan
# funk1 <- function(murt, murt_ref, n, n_ref){
#   name=deparse(substitute(murt))
#   n_eiref = n - n_ref
#   murt_eiref = murt - murt_ref
#   murt_eiref_ilmaantuvuus = murt_eiref/n_eiref*100000
#   murt_odotettu = murt_eiref_ilmaantuvuus/n_ref*100000
#   murt_SIR = murt_ref / murt_odotettu
#   murt_SIR_UCI = ifelse(murt_ref > 0, (qchisq(1-0.975, 2 * (murt_ref+1)) / 2) /murt_odotettu, qchisq(0.025,2)/2 / murt_odotettu)
#   list_results <- list(c(murt_eiref, murt_eiref_ilmaantuvuus, murt_odotettu, murt_SIR, murt_SIR_UCI))
#   names(list_results) <- name
#   return(list_results)
# }
# funk1(dorg$murt10, dorg$murt10_ref, dorg$n, dorg$n_ref)

## TODO tarkista laskennan tulokset
d <- dorg %>% 
  mutate(
    ## Murtumat ei apneasta
    n_eiref = n - n_ref,
    murt10_eiref = murt10 - murt10_ref,
    murt11_eiref = murt11 - murt11_ref,
    murt20_eiref = murt20 - murt20_ref,
    murt30_eiref = murt30 - murt30_ref,
    murt40_eiref = murt40 - murt40_ref,
    murt50_eiref = murt50 - murt50_ref,
    murt60_eiref = murt60 - murt60_ref,
    murt61_eiref = murt61 - murt61_ref,
    murt70_eiref = murt70 - murt70_ref,
    murt80_eiref = murt80 - murt80_ref,
    murt90_eiref = murt90 - murt90_ref,
    murt100_eiref = murt100 - murt100_ref,
    murt120_eiref = murt120 - murt120_ref,
    murt130_eiref = murt130 - murt130_ref,
    ## Ilmaantuvuus
    murt10_eiref_ilmaantuvuus = murt10_eiref/n_eiref*100000,
    murt11_eiref_ilmaantuvuus = murt11_eiref/n_eiref*100000,
    murt20_eiref_ilmaantuvuus = murt20_eiref/n_eiref*100000,
    murt30_eiref_ilmaantuvuus = murt30_eiref/n_eiref*100000,
    murt40_eiref_ilmaantuvuus = murt40_eiref/n_eiref*100000,
    murt50_eiref_ilmaantuvuus = murt50_eiref/n_eiref*100000,
    murt60_eiref_ilmaantuvuus = murt60_eiref/n_eiref*100000,
    murt61_eiref_ilmaantuvuus = murt61_eiref/n_eiref*100000,
    murt70_eiref_ilmaantuvuus = murt70_eiref/n_eiref*100000,
    murt80_eiref_ilmaantuvuus = murt80_eiref/n_eiref*100000,
    murt90_eiref_ilmaantuvuus = murt90_eiref/n_eiref*100000,
    murt100_eiref_ilmaantuvuus = murt100_eiref/n_eiref*100000,
    murt120_eiref_ilmaantuvuus = murt120_eiref/n_eiref*100000,
    murt130_eiref_ilmaantuvuus = murt130_eiref/n_eiref*100000,
    ## Odotetut ref
    murt10_odotettu = murt10_eiref_ilmaantuvuus/n_ref*100000,
    murt11_odotettu = murt11_eiref_ilmaantuvuus/n_ref*100000,
    murt20_odotettu = murt20_eiref_ilmaantuvuus/n_ref*100000,
    murt30_odotettu = murt30_eiref_ilmaantuvuus/n_ref*100000,
    murt40_odotettu = murt40_eiref_ilmaantuvuus/n_ref*100000,
    murt50_odotettu = murt50_eiref_ilmaantuvuus/n_ref*100000,
    murt60_odotettu = murt60_eiref_ilmaantuvuus/n_ref*100000,
    murt61_odotettu = murt61_eiref_ilmaantuvuus/n_ref*100000,
    murt70_odotettu = murt70_eiref_ilmaantuvuus/n_ref*100000,
    murt80_odotettu = murt80_eiref_ilmaantuvuus/n_ref*100000,
    murt90_odotettu = murt90_eiref_ilmaantuvuus/n_ref*100000,
    murt100_odotettu = murt100_eiref_ilmaantuvuus/n_ref*100000,
    murt120_odotettu = murt120_eiref_ilmaantuvuus/n_ref*100000,
    murt130_odotettu = murt130_eiref_ilmaantuvuus/n_ref*100000,
    ## SIR
    murt10_SIR = murt10_ref / murt10_odotettu,
    murt11_SIR = murt11_ref / murt11_odotettu,
    murt20_SIR = murt20_ref / murt20_odotettu,
    murt30_SIR = murt30_ref / murt30_odotettu,
    murt40_SIR = murt40_ref / murt40_odotettu,
    murt50_SIR = murt50_ref / murt50_odotettu,
    murt60_SIR = murt60_ref / murt60_odotettu,
    murt61_SIR = murt61_ref / murt61_odotettu,
    murt70_SIR = murt70_ref / murt70_odotettu,
    murt80_SIR = murt80_ref / murt80_odotettu,
    murt90_SIR = murt90_ref / murt90_odotettu,
    murt100_SIR = murt100_ref / murt100_odotettu,
    murt120_SIR = murt120_ref / murt120_odotettu,
    murt130_SIR = murt130_ref / murt130_odotettu,
    ## SIR LCI
    murt10_SIR_LCI = ifelse(murt10_ref > 0, (qchisq(0.975, 2 * murt10_ref) / 2) /murt10_odotettu,  0),
    murt11_SIR_LCI = ifelse(murt11_ref > 0, (qchisq(0.975, 2 * murt11_ref) / 2) /murt11_odotettu,  0),
    murt20_SIR_LCI = ifelse(murt20_ref > 0, (qchisq(0.975, 2 * murt20_ref) / 2) /murt20_odotettu,  0),
    murt30_SIR_LCI = ifelse(murt30_ref > 0, (qchisq(0.975, 2 * murt30_ref) / 2) /murt30_odotettu,  0),
    murt40_SIR_LCI = ifelse(murt40_ref > 0, (qchisq(0.975, 2 * murt40_ref) / 2) /murt40_odotettu,  0),
    murt50_SIR_LCI = ifelse(murt50_ref > 0, (qchisq(0.975, 2 * murt50_ref) / 2) /murt50_odotettu,  0),
    murt60_SIR_LCI = ifelse(murt60_ref > 0, (qchisq(0.975, 2 * murt60_ref) / 2) /murt60_odotettu,  0),
    murt61_SIR_LCI = ifelse(murt61_ref > 0, (qchisq(0.975, 2 * murt61_ref) / 2) /murt61_odotettu,  0),
    murt70_SIR_LCI = ifelse(murt70_ref > 0, (qchisq(0.975, 2 * murt70_ref) / 2) /murt70_odotettu,  0),
    murt80_SIR_LCI = ifelse(murt80_ref > 0, (qchisq(0.975, 2 * murt80_ref) / 2) /murt80_odotettu,  0),
    murt90_SIR_LCI = ifelse(murt90_ref > 0, (qchisq(0.975, 2 * murt90_ref) / 2) /murt90_odotettu,  0),
    murt100_SIR_LCI = ifelse(murt100_ref > 0, (qchisq(0.975, 2 * murt100_ref) / 2) /murt100_odotettu,  0),
    murt120_SIR_LCI = ifelse(murt120_ref > 0, (qchisq(0.975, 2 * murt120_ref) / 2) /murt120_odotettu,  0),
    murt130_SIR_LCI = ifelse(murt130_ref > 0, (qchisq(0.975, 2 * murt130_ref) / 2) /murt130_odotettu,  0),
    ## SIR UCI
    murt10_SIR_UCI = ifelse(murt10_ref > 0, (qchisq(1-0.975, 2 * (murt10_ref+1)) / 2) /murt10_odotettu, qchisq(0.025,2)/2 / murt10_odotettu),
    murt11_SIR_UCI = ifelse(murt11_ref > 0, (qchisq(1-0.975, 2 * (murt11_ref+1)) / 2) /murt11_odotettu, qchisq(0.025,2)/2 / murt11_odotettu),
    murt20_SIR_UCI = ifelse(murt20_ref > 0, (qchisq(1-0.975, 2 * (murt20_ref+1)) / 2) /murt20_odotettu, qchisq(0.025,2)/2 / murt20_odotettu),
    murt30_SIR_UCI = ifelse(murt30_ref > 0, (qchisq(1-0.975, 2 * (murt30_ref+1)) / 2) /murt30_odotettu, qchisq(0.025,2)/2 / murt30_odotettu),
    murt40_SIR_UCI = ifelse(murt40_ref > 0, (qchisq(1-0.975, 2 * (murt40_ref+1)) / 2) /murt40_odotettu, qchisq(0.025,2)/2 / murt40_odotettu),
    murt50_SIR_UCI = ifelse(murt50_ref > 0, (qchisq(1-0.975, 2 * (murt50_ref+1)) / 2) /murt50_odotettu, qchisq(0.025,2)/2 / murt50_odotettu),
    murt60_SIR_UCI = ifelse(murt60_ref > 0, (qchisq(1-0.975, 2 * (murt60_ref+1)) / 2) /murt60_odotettu, qchisq(0.025,2)/2 / murt60_odotettu),
    murt61_SIR_UCI = ifelse(murt61_ref > 0, (qchisq(1-0.975, 2 * (murt61_ref+1)) / 2) /murt61_odotettu, qchisq(0.025,2)/2 / murt61_odotettu),
    murt70_SIR_UCI = ifelse(murt70_ref > 0, (qchisq(1-0.975, 2 * (murt70_ref+1)) / 2) /murt70_odotettu, qchisq(0.025,2)/2 / murt70_odotettu),
    murt80_SIR_UCI = ifelse(murt80_ref > 0, (qchisq(1-0.975, 2 * (murt80_ref+1)) / 2) /murt80_odotettu, qchisq(0.025,2)/2 / murt80_odotettu),
    murt90_SIR_UCI = ifelse(murt90_ref > 0, (qchisq(1-0.975, 2 * (murt90_ref+1)) / 2) /murt90_odotettu, qchisq(0.025,2)/2 / murt90_odotettu),
    murt100_SIR_UCI = ifelse(murt100_ref > 0, (qchisq(1-0.975, 2 * (murt100_ref+1)) / 2) /murt100_odotettu, qchisq(0.025,2)/2 / murt100_odotettu),
    murt120_SIR_UCI = ifelse(murt120_ref > 0, (qchisq(1-0.975, 2 * (murt120_ref+1)) / 2) /murt120_odotettu, qchisq(0.025,2)/2 / murt120_odotettu),
    murt130_SIR_UCI = ifelse(murt130_ref > 0, (qchisq(1-0.975, 2 * (murt130_ref+1)) / 2) /murt130_odotettu, qchisq(0.025,2)/2 / murt130_odotettu)
  )


## Tallenna Exceliin
