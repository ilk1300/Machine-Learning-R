##Question4

#build a vector of true classifications based on 15% criteria
hemo_table <- read.csv("hemo.csv", header = TRUE)
final_Hbf <- hemo_table$FHbF
true_class <- c()
for (value in final_Hbf) {
  if(value >= 15) {true_class <- c(true_class, 'R')}
  else {true_class <- c(true_class, 'NR')}
}

#build a function to calculate accuracy
cal_accuracy <- function(para_model) {
  right_count=0
  for (i in 1:72) {
    if(para_model[i] == true_class[i]) {right_count = right_count +1}
  }
  accuracy <- right_count / 72
  return(accuracy)
}

#build an empty matrix to hold parameter names and their accuracies
para_rank <- matrix(nrow = 23, ncol = 2)
colnames(para_rank) <- c("parameter", "accuracy")

##--------parameter1: Age
para_rank[[1,1]] <- c('Age')  #append to matrix with parameter name
age <- hemo_table$Age  #get Age values
#plot the pdf for Age values given it's a responder
R_age <- age[which(final_Hbf >= 15)]
R_age_pdf <- density(R_age, kernel = "gaussian", from=6000, to=20000)
plot(R_age_pdf, ylim=c(0.0000,0.0002), col='lawngreen', xlab='Age', main = "Conditional Probability Estiamtion")

par(new=TRUE)

#plot the pdf for Age values given it's a non-responder
NR_age <- age[which(final_Hbf < 15)]
NR_age_pdf <- density(NR_age, kernel = "gaussian", from=6000, to=20000)
plot(NR_age_pdf, ylim=c(0.0000,0.0002), col='tomato', xlab='Age', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

#get threshold for classification
threshold_age <- NR_age_pdf$x[which(diff((NR_age_pdf$y - R_age_pdf$y) >0) !=0) +1]
abline(v=threshold_age, col='royalblue')
#build a vector of threshold classification
age_model <- c()
for (i in age) {
  if(i >= threshold_age) {age_model <- c(age_model, 'R')}
  else {age_model <- c(age_model, 'NR')}
}

age_accuracy <- cal_accuracy(age_model)  #get accuracy from function: cal_accuracy
para_rank[[1,2]] <- c(age_accuracy)  #append to matrix with parameter accuracy

##--------parameter2: Nsex
para_rank[[2,1]] <- c('Nsex')
nsex <- hemo_table$Nsex
R_nsex <- nsex[which(final_Hbf >= 15)]
R_nsex_pdf <- density(R_nsex, kernel = "gaussian", from = 0, to = 2)
plot(R_nsex_pdf, ylim=c(0.1,1.0), col='lawngreen', xlab='Nsex', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_nsex <- nsex[which(final_Hbf < 15)]
NR_nsex_pdf <- density(NR_nsex, kernel = "gaussian", from = 0, to = 2)
plot(NR_nsex_pdf, ylim=c(0.1,1.0), col='tomato', xlab='Nsex', main = "Conditional Probability Estiamtion" )

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_nsex <- NR_nsex_pdf$x[which(diff((NR_nsex_pdf$y - R_nsex_pdf$y) >0) !=0) +1]
threshold_nsex <- inter_nsex[3]  #choose the intersection that is closest to the median of the parameter 
abline(v=threshold_nsex, col='royalblue')   

nsex_model <- c()
for (i in nsex) {
  if(i <= threshold_nsex) {nsex_model <- c(nsex_model, 'R')}
  else {nsex_model <- c(nsex_model, 'NR')}
}

nsex_accuracy <- cal_accuracy(nsex_model)
para_rank[[2,2]] <- c(nsex_accuracy)

##--------parameter3: NAGG
para_rank[[3,1]] <- c('NAGG')
nagg <- hemo_table$NAGG
R_nagg <- nagg[which(final_Hbf >= 15)]
R_nagg_pdf <- density(R_nagg, kernel = "gaussian", from = 0, to = 4)
plot(R_nagg_pdf, ylim=c(0.0,0.7), col='lawngreen', xlab='NAGG', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_nagg <- nagg[which(final_Hbf < 15)]
NR_nagg_pdf <- density(NR_nagg, kernel = "gaussian", from = 0, to = 4)
plot(NR_nagg_pdf, ylim=c(0.0,0.7), col='tomato', xlab='NAGG', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_nagg <- NR_nagg_pdf$x[which(diff((NR_nagg_pdf$y - R_nagg_pdf$y) >0) !=0) +1]
threshold_nagg <- inter_nagg[2]
abline(v=threshold_nagg, col='royalblue')

nagg_model <- c()
for (i in nagg) {
  if(i >= threshold_nagg) {nagg_model <- c(nagg_model, 'R')}
  else {nagg_model <- c(nagg_model, 'NR')}
}

nagg_accuracy <- cal_accuracy(nagg_model)
para_rank[[3,2]] <- c(nagg_accuracy)

##--------parameter4: N_Haplo
para_rank[[4,1]] <- c('N_Haplo')
nhaplo <- hemo_table$N_Haplo
R_nhaplo <- nhaplo[which(final_Hbf >= 15)]
R_nhaplo_pdf <- density(R_nhaplo, kernel = "gaussian", from = 0, to = 9)
plot(R_nhaplo_pdf, ylim=c(0.04,0.16), col='lawngreen', xlab='N_Haplo', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_nhaplo <- nhaplo[which(final_Hbf < 15)]
NR_nhaplo_pdf <- density(NR_nhaplo, kernel = "gaussian", from = 0, to = 9)
plot(NR_nhaplo_pdf, ylim=c(0.04,0.16), col='tomato', xlab='N_Haplo', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_nhaplo <- NR_nhaplo_pdf$x[which(diff((NR_nhaplo_pdf$y - R_nhaplo_pdf$y) >0) !=0) +1]
threshold_nhaplo <- inter_nhaplo[1]
abline(v=threshold_nhaplo, col='royalblue')

nhaplo_model <- c()
for (i in nhaplo) {
  if(i >= threshold_nhaplo) {nhaplo_model <- c(nhaplo_model, 'R')}
  else {nhaplo_model <- c(nhaplo_model, 'NR')}
}

nhaplo_accuracy <- cal_accuracy(nhaplo_model)
para_rank[[4,2]] <- c(nhaplo_accuracy)

##--------parameter5: BAN
para_rank[[5,1]] <- c('BAN')
ban <- hemo_table$BAN
R_ban <- ban[which(final_Hbf >= 15)]
R_ban_pdf <- density(R_ban, kernel = "gaussian", from = 0, to = 2)
plot(R_ban_pdf, ylim=c(0.0,0.8), col='lawngreen', xlab='BAN', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_ban <- ban[which(final_Hbf < 15)]
NR_ban_pdf <- density(NR_ban, kernel = "gaussian", from = 0, to = 2)
plot(NR_ban_pdf, ylim=c(0.0,0.8), col='tomato', xlab='BAN', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_ban <- NR_ban_pdf$x[which(diff((NR_ban_pdf$y - R_ban_pdf$y) >0) !=0) +1]
threshold_ban <- inter_ban[1]
abline(v=threshold_ban, col='royalblue')

ban_model <- c()
for (i in ban) {
  if(i <= threshold_ban) {ban_model <- c(ban_model, 'R')}
  else {ban_model <- c(ban_model, 'NR')}
}

ban_accuracy <- cal_accuracy(ban_model)
para_rank[[5,2]] <- c(ban_accuracy)

##--------parameter6: BEN
para_rank[[6,1]] <- c('BEN')
ben <- hemo_table$BEN
R_ben <- ben[which(final_Hbf >= 15)]
R_ben_pdf <- density(R_ben, kernel = "gaussian", from = 0, to = 2)
plot(R_ben_pdf, ylim=c(0.0,0.7), col='lawngreen', xlab='BEN', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_ben <- ben[which(final_Hbf < 15)]
NR_ben_pdf <- density(NR_ben, kernel = "gaussian", from = 0, to = 2)
plot(NR_ben_pdf, ylim=c(0.0,0.7), col='tomato', xlab='BEN', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

threshold_ben <- NR_ben_pdf$x[which(diff((NR_ben_pdf$y - R_ben_pdf$y) >0) !=0) +1]
abline(v=threshold_ben, col='royalblue')

ben_model <- c()
for (i in ben) {
  if(i >= threshold_ben) {ben_model <- c(ben_model, 'R')}
  else {ben_model <- c(ben_model, 'NR')}
}

ben_accuracy <- cal_accuracy(ben_model)
para_rank[[6,2]] <- c(ben_accuracy)

##--------parameter7: CAM
para_rank[[7,1]] <- c('CAM')
cam <- hemo_table$CAM
R_cam <- cam[which(final_Hbf >= 15)]
R_cam_pdf <- density(R_cam, kernel = "gaussian", from = 0, to = 1)
plot(R_cam_pdf, ylim=c(0.0,3.5), col='lawngreen', xlab='CAM', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_cam <- cam[which(final_Hbf < 15)]
NR_cam_pdf <- density(NR_cam, kernel = "gaussian", from = 0, to = 1)
plot(NR_cam_pdf, ylim=c(0.0,3.5), col='tomato', xlab='CAM', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

threshold_cam <- NR_cam_pdf$x[which(diff((NR_cam_pdf$y - R_cam_pdf$y) >0) !=0) +1]
abline(v=threshold_cam, col='royalblue')

cam_model <- c()
for (i in cam) {
  if(i <= threshold_cam) {cam_model <- c(cam_model, 'R')}
  else {cam_model <- c(cam_model, 'NR')}
}

cam_accuracy <- cal_accuracy(cam_model)
para_rank[[7,2]] <- c(cam_accuracy)

##--------parameter8: SEN
para_rank[[8,1]] <- c('SEN')
sen <- hemo_table$SEN
R_sen <- sen[which(final_Hbf >= 15)]
R_sen_pdf <- density(R_sen, kernel = "gaussian", from = 0, to = 2)
plot(R_sen_pdf, ylim=c(0.0,3.5), col='lawngreen', xlab='SEN', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_sen <- sen[which(final_Hbf < 15)]
NR_sen_pdf <- density(NR_sen, kernel = "gaussian", from = 0, to = 2)
plot(NR_sen_pdf, ylim=c(0.0,3.5), col='tomato', xlab='SEN', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

threshold_sen <- NR_sen_pdf$x[which(diff((NR_sen_pdf$y - R_sen_pdf$y) >0) !=0) +1]
abline(v=threshold_sen, col='royalblue')

sen_model <- c()
for (i in sen) {
  if(i >= threshold_sen) {sen_model <- c(sen_model, 'R')}
  else {sen_model <- c(sen_model, 'NR')}
}

sen_accuracy <- cal_accuracy(sen_model)
para_rank[[8,2]] <- c(sen_accuracy)

##--------parameter9: Weight
para_rank[[9,1]] <- c('Weight')
wgt <- hemo_table$Weight
R_wgt <- wgt[which(final_Hbf >= 15)]
R_wgt_pdf <- density(R_wgt, kernel = "gaussian", from = 30, to = 90)
plot(R_wgt_pdf, ylim=c(0.00,0.04), col='lawngreen', xlab='Weight', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_wgt <- wgt[which(final_Hbf < 15)]
NR_wgt_pdf <- density(NR_wgt, kernel = "gaussian", from = 30, to = 90)
plot(NR_wgt_pdf, ylim=c(0.00,0.04), col='tomato', xlab='Weight', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_wgt <- NR_wgt_pdf$x[which(diff((NR_wgt_pdf$y - R_wgt_pdf$y) >0) !=0) +1]
threshold_wgt <- inter_wgt[2]
abline(v=threshold_wgt, col='royalblue')

wgt_model <- c()
for (i in wgt) {
  if(i <= threshold_wgt) {wgt_model <- c(wgt_model, 'R')}
  else {wgt_model <- c(wgt_model, 'NR')}
}

wgt_accuracy <- cal_accuracy(wgt_model)
para_rank[[9,2]] <- c(wgt_accuracy)

##--------parameter10: Hbf
para_rank[[10,1]] <- c('Hbf')
hbf <- hemo_table$HbF
R_hbf <- hbf[which(final_Hbf >= 15)]
R_hbf_pdf <- density(R_hbf, kernel = "gaussian", from = 0, to = 25)
plot(R_hbf_pdf, ylim=c(0.00,0.25), col='lawngreen', xlab='Hbf', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_hbf <- hbf[which(final_Hbf < 15)]
NR_hbf_pdf <- density(NR_hbf, kernel = "gaussian", from = 0, to = 25)
plot(NR_hbf_pdf, ylim=c(0.00,0.25), col='tomato', xlab='Hbf', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

threshold_hbf <- NR_hbf_pdf$x[which(diff((NR_hbf_pdf$y - R_hbf_pdf$y) >0) !=0) +1]
abline(v=threshold_hbf, col='royalblue')

hbf_model <- c()
for (i in hbf) {
  if(i >= threshold_hbf) {hbf_model <- c(hbf_model, 'R')}
  else {hbf_model <- c(hbf_model, 'NR')}
}

hbf_accuracy <- cal_accuracy(hbf_model)
para_rank[[10,2]] <- c(hbf_accuracy)

##--------parameter11: Hb
para_rank[[11,1]] <- c('Hb')
hb <- hemo_table$Hb
R_hb <- hb[which(final_Hbf >= 15)]
R_hb_pdf <- density(R_hb, kernel = "gaussian", from = 5, to = 15)
plot(R_hb_pdf, ylim=c(0.0,0.5), col='lawngreen', xlab='Hb', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_hb <- hb[which(final_Hbf < 15)]
NR_hb_pdf <- density(NR_hb, kernel = "gaussian", from = 5, to = 15)
plot(NR_hb_pdf, ylim=c(0.0,0.5), col='tomato', xlab='Hb', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_hb <- NR_hb_pdf$x[which(diff((NR_hb_pdf$y - R_hb_pdf$y) >0) !=0) +1]
threshold_hb <- inter_hb[1]
abline(v=threshold_hb, col='royalblue')

hb_model <- c()
for (i in hb) {
  if(i <= threshold_hb) {hb_model <- c(hb_model, 'R')}
  else {hb_model <- c(hb_model, 'NR')}
}

hb_accuracy <- cal_accuracy(hb_model)
para_rank[[11,2]] <- c(hb_accuracy)

##--------parameter12: HbS
para_rank[[12,1]] <- c('HbS')
hbs <- hemo_table$HbS
R_hbs <- hbs[which(final_Hbf >= 15)]
R_hbs_pdf <- density(R_hbs, kernel = "gaussian", from = 50, to = 120)
plot(R_hbs_pdf, ylim=c(0.00,0.05), col='lawngreen', xlab='HbS', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_hbs <- hbs[which(final_Hbf < 15)]
NR_hbs_pdf <- density(NR_hbs, kernel = "gaussian", from = 50, to = 120)
plot(NR_hbs_pdf, ylim=c(0.00,0.05), col='tomato', xlab='HbS', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_hbs <- NR_hbs_pdf$x[which(diff((NR_hbs_pdf$y - R_hbs_pdf$y) >0) !=0) +1]
threshold_hbs <- inter_hbs[1]
abline(v=threshold_hbs, col='royalblue')

hbs_model <- c()
for (i in hbs) {
  if(i <= threshold_hbs) {hbs_model <- c(hbs_model, 'R')}
  else {hbs_model <- c(hbs_model, 'NR')}
}

hbs_accuracy <- cal_accuracy(hbs_model)
para_rank[[12,2]] <- c(hbs_accuracy)

##--------parameter13: RBC
para_rank[[13,1]] <- c('RBC')
rbc <- hemo_table$RBC
R_rbc <- rbc[which(final_Hbf >= 15)]
R_rbc_pdf <- density(R_rbc, kernel = "gaussian", from = 1, to = 5)
plot(R_rbc_pdf, ylim=c(0.0,0.8), col='lawngreen', xlab='RBC', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_rbc <- rbc[which(final_Hbf < 15)]
NR_rbc_pdf <- density(NR_rbc, kernel = "gaussian", from = 1, to = 5)
plot(NR_rbc_pdf, ylim=c(0.0,0.8), col='tomato', xlab='RBC', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_rbc <- NR_rbc_pdf$x[which(diff((NR_rbc_pdf$y - R_rbc_pdf$y) >0) !=0) +1]
threshold_rbc <- inter_rbc[2]
abline(v=threshold_rbc, col='royalblue')

rbc_model <- c()
for (i in rbc) {
  if(i >= threshold_rbc) {rbc_model <- c(rbc_model, 'R')}
  else {rbc_model <- c(rbc_model, 'NR')}
}

rbc_accuracy <- cal_accuracy(rbc_model)
para_rank[[13,2]] <- c(rbc_accuracy)

##--------parameter14: RDW
para_rank[[14,1]] <- c('RDW')
rdw <- hemo_table$RDW
R_rdw <- rdw[which(final_Hbf >= 15)]
R_rdw_pdf <- density(R_rdw, kernel = "gaussian", from = 0, to = 40)
plot(R_rdw_pdf, ylim=c(0.00,0.10), col='lawngreen', xlab='RDW', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_rdw <- rdw[which(final_Hbf < 15)]
NR_rdw_pdf <- density(NR_rdw, kernel = "gaussian", from = 0, to = 40)
plot(NR_rdw_pdf, ylim=c(0.00,0.10), col='tomato', xlab='RDW', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_rdw <- NR_rdw_pdf$x[which(diff((NR_rdw_pdf$y - R_rdw_pdf$y) >0) !=0) +1]
threshold_rdw <- inter_rdw[1]
abline(v=threshold_rdw, col='royalblue')

rdw_model <- c()
for (i in rdw) {
  if(i <= threshold_rdw) {rdw_model <- c(rdw_model, 'R')}
  else {rdw_model <- c(rdw_model, 'NR')}
}

rdw_accuracy <- cal_accuracy(rdw_model)
para_rank[[14,2]] <- c(rdw_accuracy)

##--------parameter15: PCV
para_rank[[15,1]] <- c('PCV')
pcv <- hemo_table$PCV
R_pcv <- pcv[which(final_Hbf >= 15)]
R_pcv_pdf <- density(R_pcv, kernel = "gaussian", from = 0.0, to = 0.4)
plot(R_pcv_pdf, ylim=c(0,10), col='lawngreen', xlab='PCV', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_pcv <- pcv[which(final_Hbf < 15)]
NR_pcv_pdf <- density(NR_pcv, kernel = "gaussian", from = 0.0, to = 0.4)
plot(NR_pcv_pdf, ylim=c(0,10), col='tomato', xlab='PCV', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_pcv <- NR_pcv_pdf$x[which(diff((NR_pcv_pdf$y - R_pcv_pdf$y) >0) !=0) +1]
threshold_pcv <- inter_pcv[3]
abline(v=threshold_pcv, col='royalblue')

pcv_model <- c()
for (i in pcv) {
  if(i >= threshold_pcv) {pcv_model <- c(pcv_model, 'R')}
  else {pcv_model <- c(pcv_model, 'NR')}
}

pcv_accuracy <- cal_accuracy(pcv_model)
para_rank[[15,2]] <- c(pcv_accuracy)

##--------parameter16: Retic
para_rank[[16,1]] <- c('Retic')
retic <- hemo_table$Retic
R_retic <- retic[which(final_Hbf >= 15)]
R_retic_pdf <- density(R_retic, kernel = "gaussian", from = 0, to = 600)
plot(R_retic_pdf, ylim=c(0.000,0.004), col='lawngreen', xlab='Retic', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_retic <- retic[which(final_Hbf < 15)]
NR_retic_pdf <- density(NR_retic, kernel = "gaussian", from = 0, to = 600)
plot(NR_retic_pdf, ylim=c(0.000,0.004), col='tomato', xlab='Retic', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_retic <- NR_retic_pdf$x[which(diff((NR_retic_pdf$y - R_retic_pdf$y) >0) !=0) +1]
threshold_retic <- inter_retic[2]
abline(v=threshold_retic, col='royalblue')

retic_model <- c()
for (i in retic) {
  if(i >= threshold_retic) {retic_model <- c(retic_model, 'R')}
  else {retic_model <- c(retic_model, 'NR')}
}

retic_accuracy <- cal_accuracy(retic_model)
para_rank[[16,2]] <- c(retic_accuracy)

##--------parameter17: MCV
para_rank[[17,1]] <- c('MCV')
mcv <- hemo_table$MCV
R_mcv <- mcv[which(final_Hbf >= 15)]
R_mcv_pdf <- density(R_mcv, kernel = "gaussian", from = 60, to = 130)
plot(R_mcv_pdf, ylim=c(0.00,0.06), col='lawngreen', xlab='MCV', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_mcv <- mcv[which(final_Hbf < 15)]
NR_mcv_pdf <- density(NR_mcv, kernel = "gaussian", from = 60, to = 130)
plot(NR_mcv_pdf, ylim=c(0.00,0.06), col='tomato', xlab='MCV', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_mcv <- NR_mcv_pdf$x[which(diff((NR_mcv_pdf$y - R_mcv_pdf$y) >0) !=0) +1]
threshold_mcv <- inter_mcv[4]
abline(v=threshold_mcv, col='royalblue')

mcv_model <- c()
for (i in mcv) {
  if(i >= threshold_mcv) {mcv_model <- c(mcv_model, 'R')}
  else {mcv_model <- c(mcv_model, 'NR')}
}

mcv_accuracy <- cal_accuracy(mcv_model)
para_rank[[17,2]] <- c(mcv_accuracy)

##--------parameter18: MCH
para_rank[[18,1]] <- c('MCH')
mch <- hemo_table$MCH
R_mch <- mch[which(final_Hbf >= 15)]
R_mch_pdf <- density(R_mch, kernel = "gaussian", from = 20, to = 50)
plot(R_mch_pdf, ylim=c(0.00,0.14), col='lawngreen', xlab='MCH', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_mch <- mch[which(final_Hbf < 15)]
NR_mch_pdf <- density(NR_mch, kernel = "gaussian", from = 20, to = 50)
plot(NR_mch_pdf, ylim=c(0.00,0.14), col='tomato', xlab='MCH', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_mch <- NR_mch_pdf$x[which(diff((NR_mch_pdf$y - R_mch_pdf$y) >0) !=0) +1]
threshold_mch <- inter_mch[2]
abline(v=threshold_mch, col='royalblue')

mch_model <- c()
for (i in mch) {
  if(i >= threshold_mch) {mch_model <- c(mch_model, 'R')}
  else {mch_model <- c(mch_model, 'NR')}
}

mch_accuracy <- cal_accuracy(mch_model)
para_rank[[18,2]] <- c(mch_accuracy)

##--------parameter19: WBC
para_rank[[19,1]] <- c('WBC')
wbc <- hemo_table$WBC
R_wbc <- wbc[which(final_Hbf >= 15)]
R_wbc_pdf <- density(R_wbc, kernel = "gaussian", from = 3, to = 25)
plot(R_wbc_pdf, ylim=c(0.00,0.14), col='lawngreen', xlab='WBC', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_wbc <- wbc[which(final_Hbf < 15)]
NR_wbc_pdf <- density(NR_wbc, kernel = "gaussian", from = 3, to = 25)
plot(NR_wbc_pdf, ylim=c(0.00,0.14), col='tomato', xlab='WBC', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_wbc <- NR_wbc_pdf$x[which(diff((NR_wbc_pdf$y - R_wbc_pdf$y) >0) !=0) +1]
threshold_wbc <- inter_wbc[5]
abline(v=threshold_wbc, col='royalblue')

wbc_model <- c()
for (i in wbc) {
  if(i >= threshold_wbc) {wbc_model <- c(wbc_model, 'R')}
  else {wbc_model <- c(wbc_model, 'NR')}
}

wbc_accuracy <- cal_accuracy(wbc_model)
para_rank[[19,2]] <- c(wbc_accuracy)

##--------parameter20: Polys
para_rank[[20,1]] <- c('Polys')
polys <- hemo_table$Polys
R_polys <- polys[which(final_Hbf >= 15)]
R_polys_pdf <- density(R_polys, kernel = "gaussian", from = 0, to = 20)
plot(R_polys_pdf, ylim=c(0.00,0.20), col='lawngreen', xlab='Polys', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_polys <- polys[which(final_Hbf < 15)]
NR_polys_pdf <- density(NR_polys, kernel = "gaussian", from = 0, to = 20)
plot(NR_polys_pdf, ylim=c(0.00,0.20), col='tomato', xlab='Polys', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_polys <- NR_polys_pdf$x[which(diff((NR_polys_pdf$y - R_polys_pdf$y) >0) !=0) +1]
threshold_polys <- inter_polys[3]
abline(v=threshold_polys, col='royalblue')

polys_model <- c()
for (i in polys) {
  if(i <= threshold_polys) {polys_model <- c(polys_model, 'R')}
  else {polys_model <- c(polys_model, 'NR')}
}

polys_accuracy <- cal_accuracy(polys_model)
para_rank[[20,2]] <- c(polys_accuracy)

##--------parameter21: Plats
para_rank[[21,1]] <- c('Plats')
plats <- hemo_table$Plats
R_plats <- plats[which(final_Hbf >= 15)]
R_plats_pdf <- density(R_plats, kernel = "gaussian", from = 0, to = 1500)
plot(R_plats_pdf, ylim=c(0.000,0.003), col='lawngreen', xlab='Plats', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_plats <- plats[which(final_Hbf < 15)]
NR_plats_pdf <- density(NR_plats, kernel = "gaussian", from = 0, to = 1500)
plot(NR_plats_pdf, ylim=c(0.000,0.003), col='tomato', xlab='Plats', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_plats <- NR_plats_pdf$x[which(diff((NR_plats_pdf$y - R_plats_pdf$y) >0) !=0) +1]
threshold_plats <- inter_plats[2]
abline(v=threshold_plats, col='royalblue')

plats_model <- c()
for (i in plats) {
  if(i <= threshold_plats) {plats_model <- c(plats_model, 'R')}
  else {plats_model <- c(plats_model, 'NR')}
}

plats_accuracy <- cal_accuracy(plats_model)
para_rank[[21,2]] <- c(plats_accuracy)

##--------parameter22: Bili
para_rank[[22,1]] <- c('Bili')
bili <- hemo_table$Bili
R_bili <- bili[which(final_Hbf >= 15)]
R_bili_pdf <- density(R_bili, kernel = "gaussian", from = 0, to = 12)
plot(R_bili_pdf, ylim=c(0.00,0.25), col='lawngreen', xlab='Bili', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_bili <- bili[which(final_Hbf < 15)]
NR_bili_pdf <- density(NR_bili, kernel = "gaussian", from = 0, to = 12)
plot(NR_bili_pdf, ylim=c(0.00,0.25), col='tomato', xlab='Bili', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_bili<- NR_bili_pdf$x[which(diff((NR_bili_pdf$y - R_bili_pdf$y) >0) !=0) +1]
threshold_bili <- inter_bili[1]
abline(v=threshold_bili, col='royalblue')

bili_model <- c()
for (i in bili) {
  if(i <= threshold_bili) {bili_model <- c(bili_model, 'R')}
  else {bili_model <- c(bili_model, 'NR')}
}

bili_accuracy <- cal_accuracy(bili_model)
para_rank[[22,2]] <- c(bili_accuracy)

##--------parameter23: NRBC
para_rank[[23,1]] <- c('NRBC')
nrbc <- hemo_table$NRBC
R_nrbc <- nrbc[which(final_Hbf >= 15)]
R_nrbc_pdf <- density(R_nrbc, kernel = "gaussian", from = 0, to = 50)
plot(R_nrbc_pdf, ylim=c(0.0,1.0), col='lawngreen', xlab='NRBC', main = "Conditional Probability Estiamtion")

par(new=TRUE)

NR_nrbc <- nrbc[which(final_Hbf < 15)]
NR_nrbc_pdf <- density(NR_nrbc, kernel = "gaussian", from = 0, to = 50)
plot(NR_nrbc_pdf, ylim=c(0.0,1.0), col='tomato', xlab='NRBC', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

inter_nrbc<- NR_nrbc_pdf$x[which(diff((NR_nrbc_pdf$y - R_nrbc_pdf$y) >0) !=0) +1]
threshold_nrbc <- inter_nrbc[1]
abline(v=threshold_nrbc, col='royalblue')

nrbc_model <- c()
for (i in nrbc) {
  if(i >= threshold_nrbc) {nrbc_model <- c(nrbc_model, 'R')}
  else {nrbc_model <- c(nrbc_model, 'NR')}
}

nrbc_accuracy <- cal_accuracy(nrbc_model)
para_rank[[23,2]] <- c(nrbc_accuracy)

### Sort the accuracies
sorted_para_rank <- para_rank[order(para_rank[,2], decreasing=TRUE),] 