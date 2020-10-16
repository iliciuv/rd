########### ARDL COINTEGRATION BPCGM ESTIMATION #############
impo <- ts(LimportsDL[, i])
inco <- ts(LincomeDL[, i])
consum <- ts(LconsumpDL[, i])
expo <- ts(LexportsDL[, i])
finco <- ts(LfincomeDL[, i])
rpri <- ts(LrpricesDL[, i])
t <- ts(c(1:27))
dum <- as.numeric(c(replicate(break_m[i], 0), replicate(27 - break_m[i], 1)))
eq_th <- as.data.frame(cbind(expo, inco, rpri, impo, finco, dum))

steps_xa <- ardlBoundOrders(
  formula = formula_x,
  data = eq_th,
  max.p = max_p,
  max.q = max_q,
  FullSearch = FALSE,
  ic = criterion
)
p_test_x <- data.frame(steps_xa$q, steps_xa$p)
p_test_x1 <- p_test_x
p_test_x1[p_test_x1 == 0] <- 1
p_test_xc <- rbind(p_test_x, p_test_x1, p_test_x1 + 1, ardl_order[1:3], new_order[1:3])
steps_x2 <- ardlBound(
  formula = formula_x,
  data = eq_th,
  #autoOrder = TRUE,
  max.p = max_p,
  max.q = max_q,
  #remove =   list(q =  c(1),p = list(inco = c(1), rpri = c(0))),
  case = case_x,
  ECM = TRUE, 
)
steps_ma <- ardlBoundOrders(
  formula = formula_m_dum,
  data = eq_th,
  max.p = max_p,
  max.q = max_q,
  FullSearch = FALSE,
  ic = criterion
)
p_test_m <- data.frame(steps_ma$q, steps_ma$p)
p_test_m1 <- p_test_m
p_test_m1[p_test_m1 == 0] <- 1
p_test_mc <- rbind(p_test_m, p_test_m1, p_test_m1 + 1, ardl_order, new_order)

steps_m2 <- ardlBound(
  formula = formula_m ,
  data = eq_th,
  #autoOrder = TRUE,
  max.p = max_p,
  max.q = max_q,
  #remove =   list(q =  c(1),p = list(inco = c(1), rpri = c(0))),
  case = case_m,
  ECM = TRUE, 
)
p_test_xf[i, ] <- p_test_x
p_test_mf[i, ] <- p_test_m
ardl_fx[i] <- steps_x2$F.stat
ardl_fm[i] <- steps_m2$F.stat