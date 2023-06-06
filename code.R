require(TAM)
require(mirt)
require(tidyverse)

data(data.sim.rasch)
data <- data.frame(data.sim.rasch)




# TAM JML -----------------------------------------------------------------


# estimate the Rasch model with JML (function 'tam.jml')
tam_jml <- TAM::tam.jml(resp=data)

tam_jml_fit <- TAM::tam.fit(tam_jml)$fit.item

tam_jml_b <- data.frame(tam_jml["item"])



# TAM MML -----------------------------------------------------------------

tam_mml <- TAM::tam.mml(resp=data )

tam_mml_fit <- TAM::tam.fit(tam_jml)$fit.item

tam_mml_b <- data.frame(tam_mml["item"])


# TAM JML constrained -----------------------------------------------------


# constrain item difficulties to zero
tam_jml_con <- TAM::tam.jml(resp=data, constraint="items")

tam_jml_con_fit <- TAM::tam.fit(tam_jml_con)$fit.item

tam_jml_con_b <- data.frame(tam_jml_con["item"])




# MIRT --------------------------------------------------------------------

mirt_rasch <- "F = 1-40"
rasch_fit <- mirt(data = data,
                 model = mirt_rasch,
                 itemtype = "Rasch",
                 SE = TRUE)
rasch_params <- coef(rasch_fit, 
                     IRTpars = TRUE,
                     simplify = TRUE)

mirt_items <- data.frame(rasch_params$items) %>%
  tibble::rownames_to_column(., "item") 

mirt_fit <- itemfit(rasch_fit, 'infit', method = 'ML') #infit and outfit stats


# WINSTEPS ----------------------------------------------------------------


# This outputs the data for winsteps to use.
#write.csv(data, "data.csv", row.names = FALSE)

winsteps <- read.csv("winsteps_item.txt", skip = 1)



# Joins -------------------------------------------------------------------


JML <- tam_jml_b %>%
  left_join(tam_jml_fit, by = c("item.item" = "item")) %>%
  rename(NAME = item.item, 
         N.JML = item.N,
         B.JML = item.xsi.item,
         OUT.MSQ.JML = outfitItem,
         OUT.Z.JML = outfitItem_t) %>%
  select(NAME, N.JML, B.JML, OUT.MSQ.JML, OUT.Z.JML)



MML <- tam_mml_b %>%
  left_join(tam_mml_fit, by = c("item.item" = "item")) %>%
  rename(NAME = item.item, 
         N.MML = item.N,
         B.MML = item.xsi.item,
         OUT.MSQ.MML = outfitItem,
         OUT.Z.MML = outfitItem_t) %>%
  select(NAME, N.MML, B.MML, OUT.MSQ.MML, OUT.Z.MML)



JMLC <- tam_jml_con_b %>%
  left_join(tam_jml_con_fit, by = c("item.item" = "item")) %>%
  rename(NAME = item.item, 
         N.JMLC = item.N,
         B.JMLC = item.xsi.item,
         OUT.MSQ.JMLC = outfitItem,
         OUT.Z.JMLC = outfitItem_t) %>%
  select(NAME, N.JMLC, B.JMLC, OUT.MSQ.JMLC, OUT.Z.JMLC)


WIN <- winsteps %>%
  rename(N.WIN = COUNT,
         B.WIN = MEASURE,
         OUT.MSQ.WIN = OUT.MSQ,
         OUT.Z.WIN = OUT.ZSTD) %>%
  select(NAME, N.WIN, B.WIN, OUT.MSQ.WIN, OUT.Z.WIN)


MIRT <- mirt_items %>%
  left_join(mirt_fit, by = "item") %>%
  rename(NAME = item,
         B.MIRT = b,
         OUT.MSQ.MIRT = outfit,
         OUT.Z.MIRT = z.outfit) %>%
  select(NAME, B.MIRT, OUT.MSQ.MIRT, OUT.Z.MIRT)



# Comparisons -------------------------------------------------------------



comb <- JML %>%
  left_join(MML, by = "NAME") %>%
  left_join(JMLC, by = "NAME") %>%
  left_join(WIN, by = "NAME") %>%
  left_join(MIRT, by = "NAME")


compare_b <- comb %>%
  select(NAME, 
         B.JML, 
         B.MML, 
         B.JMLC, 
         B.WIN, 
         B.MIRT)

compare_outmsq <- comb %>%
  select(NAME, 
         OUT.MSQ.JML, 
         OUT.MSQ.MML, 
         OUT.MSQ.JMLC, 
         OUT.MSQ.WIN,
         OUT.MSQ.MIRT)

compare_outz <- comb %>%
  select(NAME, 
         OUT.Z.JML, 
         OUT.Z.MML, 
         OUT.Z.JMLC, 
         OUT.Z.WIN,
         OUT.Z.MIRT)



# Correlations ------------------------------------------------------------


plot(compare_b[,-1])

plot(compare_outmsq[,-1])

plot(compare_outz[,-1])
