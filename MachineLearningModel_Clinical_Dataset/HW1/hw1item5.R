##Question5

source("hw1item4.R", local = TRUE)

#Prediction function when k=1
k1_prediction <- function(v_par, k1_class) {
  k1_class <- c()
  for (i in 1:72) {
    test <- matrix(v_par[i], nrow=71, ncol=1)  #take one out as test data
    train <- matrix(v_par[-i], nrow=71, ncol=1)  #set the rest as training data
    fhbf <- matrix(final_Hbf[-i]) #build a matrix with the final hbf values of training set
    diffs <- abs(sweep(test,1,train,"-"))  #get absolute difference value between test data and each training data
    com_train_diffs <- cbind(train,diffs,fhbf)  #combine training data, absolute difference and final hbbf values into one matrix 
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]  #and sort from low to high according to absolute difference
    fhbf_nn <- sorted_com[1,3] #get the fhbf value from one nearest neighbour
    if(fhbf_nn >= 15) {k1_class <- c(k1_class, 'R')}  # classify test data using nearest neighbour based on 15% criteria
    else {k1_class <- c(k1_class, 'NR')}  
  }
  return(k1_class)  #R/NR prediction is stored and later will be used to calculate accuracy
}

#Prediction function when k=2
k2_prediction <- function(v_par, k2_class) {
  k2_class <- c()
  for (i in 1:72) {
    test <- matrix(v_par[i], nrow=71, ncol=1)
    train <- matrix(v_par[-i], nrow=71, ncol=1)
    fhbf <- matrix(final_Hbf[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,fhbf)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    fhbf_nn1 <- sorted_com[1,3]
    fhbf_nn2 <- sorted_com[2,3]
    all_fhbfs <- c(fhbf_nn1, fhbf_nn2)
    r_count <- 0
    nr_count <- 0
    for (i in all_fhbfs) {
      if(i>=15) {r_count=r_count+1}
      else {nr_count=nr_count+1}
    }
    if(r_count >= nr_count) {k2_class <- c(k2_class, 'R')} 
    else {k2_class <- c(k2_class, 'NR')}
  }
  return(k2_class)
}

#Prediction function when k=3
k3_prediction <- function(v_par, k3_class) {
  k3_class <- c()
  for (i in 1:72) {
    test <- matrix(v_par[i], nrow=71, ncol=1)
    train <- matrix(v_par[-i], nrow=71, ncol=1)
    fhbf <- matrix(final_Hbf[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,fhbf)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    fhbf_nn1 <- sorted_com[1,3]
    fhbf_nn2 <- sorted_com[2,3]
    fhbf_nn3 <- sorted_com[3,3]
    all_fhbfs <- c(fhbf_nn1, fhbf_nn2, fhbf_nn3)
    r_count <- 0
    nr_count <- 0
    for (i in all_fhbfs) {
      if(i>=15) {r_count=r_count+1}
      else {nr_count=nr_count+1}
    }
    if(r_count>=nr_count) {k3_class <- c(k3_class, 'R')} 
    else {k3_class <- c(k3_class, 'NR')}
  }
  return(k3_class)
}

#Prediction function when k=5
k5_prediction <- function(v_par, k5_class) {
  k5_class <- c()
  for (i in 1:72) {
    test <- matrix(v_par[i], nrow=71, ncol=1)
    train <- matrix(v_par[-i], nrow=71, ncol=1)
    fhbf <- matrix(final_Hbf[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,fhbf)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    fhbf_nn1 <- sorted_com[1,3]
    fhbf_nn2 <- sorted_com[2,3]
    fhbf_nn3 <- sorted_com[3,3]
    fhbf_nn4 <- sorted_com[4,3]
    fhbf_nn5 <- sorted_com[5,3]
    all_fhbfs <- c(fhbf_nn1, fhbf_nn2, fhbf_nn3, fhbf_nn4, fhbf_nn5)
    r_count <- 0
    nr_count <- 0
    for (i in all_fhbfs) {
      if(i>=15) {r_count=r_count+1}
      else {nr_count=nr_count+1}
    }
    if(r_count>=nr_count) {k5_class <- c(k5_class, 'R')} 
    else {k5_class <- c(k5_class, 'NR')}
  }
  return(k5_class)
}

#build four empty matrixes to hold accuracies for each parameter when k=1,2,3,5
k1_para_rank <- matrix(nrow = 23, ncol = 2)
colnames(k1_para_rank) <- c("parameter","k1_accuracy")
k2_para_rank <- matrix(nrow = 23, ncol = 2)
colnames(k2_para_rank) <- c("parameter","k2_accuracy")
k3_para_rank <- matrix(nrow = 23, ncol = 2)
colnames(k3_para_rank) <- c("parameter","k3_accuracy")
k5_para_rank <- matrix(nrow = 23, ncol = 2)
colnames(k5_para_rank) <- c("parameter","k5_accuracy")

##--------parameter1: Age
#calculate accuracy when k=1
k1_age_model <- k1_prediction(age, k1_class)
k1_age_accuracy <- cal_accuracy(k1_age_model)
k1_para_rank[[1,1]] <- c('Age')
k1_para_rank[[1,2]] <- c(k1_age_accuracy)
#calculate accuracy when k=2
k2_age_model <- k2_prediction(age, k2_class)
k2_age_accuracy <- cal_accuracy(k2_age_model)
k2_para_rank[[1,1]] <- c('Age')
k2_para_rank[[1,2]] <- c(k2_age_accuracy)
#calculate accuracy when k=3
k3_age_model <- k3_prediction(age, k3_class)
k3_age_accuracy <- cal_accuracy(k3_age_model)
k3_para_rank[[1,1]] <- c('Age')
k3_para_rank[[1,2]] <- c(k3_age_accuracy)
#calculate accuracy when k=5
k5_age_model <- k5_prediction(age, k5_class)
k5_age_accuracy <- cal_accuracy(k5_age_model)
k5_para_rank[[1,1]] <- c('Age')
k5_para_rank[[1,2]] <- c(k5_age_accuracy)

##--------parameter2: Nsex
#calculate accuracy when k=1
k1_nsex_model <- k1_prediction(nsex, k1_class)
k1_nsex_accuracy <- cal_accuracy(k1_nsex_model)
k1_para_rank[[2,1]] <- c('Nsex')
k1_para_rank[[2,2]] <- c(k1_nsex_accuracy)
#calculate accuracy when k=2
k2_nsex_model <- k2_prediction(nsex, k2_class)
k2_nsex_accuracy <- cal_accuracy(k2_nsex_model)
k2_para_rank[[2,1]] <- c('Nsex')
k2_para_rank[[2,2]] <- c(k2_nsex_accuracy)
#calculate accuracy when k=3
k3_nsex_model <- k3_prediction(nsex, k3_class)
k3_nsex_accuracy <- cal_accuracy(k3_nsex_model)
k3_para_rank[[2,1]] <- c('Nsex')
k3_para_rank[[2,2]] <- c(k3_nsex_accuracy)
#calculate accuracy when k=5
k5_nsex_model <- k5_prediction(nsex, k5_class)
k5_nsex_accuracy <- cal_accuracy(k5_nsex_model)
k5_para_rank[[2,1]] <- c('Nsex')
k5_para_rank[[2,2]] <- c(k5_nsex_accuracy)

##--------parameter3: NAGG
#calculate accuracy when k=1
k1_nagg_model <- k1_prediction(nagg, k1_class)
k1_nagg_accuracy <- cal_accuracy(k1_nagg_model)
k1_para_rank[[3,1]] <- c('NAGG')
k1_para_rank[[3,2]] <- c(k1_nagg_accuracy)
#calculate accuracy when k=2
k2_nagg_model <- k2_prediction(nagg, k2_class)
k2_nagg_accuracy <- cal_accuracy(k2_nagg_model)
k2_para_rank[[3,1]] <- c('NAGG')
k2_para_rank[[3,2]] <- c(k2_nagg_accuracy)
#calculate accuracy when k=3
k3_nagg_model <- k3_prediction(nagg, k3_class)
k3_nagg_accuracy <- cal_accuracy(k3_nagg_model)
k3_para_rank[[3,1]] <- c('NAGG')
k3_para_rank[[3,2]] <- c(k3_nagg_accuracy)
#calculate accuracy when k=5
k5_nagg_model <- k5_prediction(nagg, k5_class)
k5_nagg_accuracy <- cal_accuracy(k5_nagg_model)
k5_para_rank[[3,1]] <- c('NAGG')
k5_para_rank[[3,2]] <- c(k5_nagg_accuracy)

##--------parameter4: N_Haplo
#calculate accuracy when k=1
k1_nhaplo_model <- k1_prediction(nhaplo, k1_class)
k1_nhaplo_accuracy <- cal_accuracy(k1_nhaplo_model)
k1_para_rank[[4,1]] <- c('N_Haplo')
k1_para_rank[[4,2]] <- c(k1_nhaplo_accuracy)
#calculate accuracy when k=2
k2_nhaplo_model <- k2_prediction(nhaplo, k2_class)
k2_nhaplo_accuracy <- cal_accuracy(k2_nhaplo_model)
k2_para_rank[[4,1]] <- c('N_Haplo')
k2_para_rank[[4,2]] <- c(k2_nhaplo_accuracy)
#calculate accuracy when k=3
k3_nhaplo_model <- k3_prediction(nhaplo, k3_class)
k3_nhaplo_accuracy <- cal_accuracy(k3_nhaplo_model)
k3_para_rank[[4,1]] <- c('N_Haplo')
k3_para_rank[[4,2]] <- c(k3_nhaplo_accuracy)
#calculate accuracy when k=5
k5_nhaplo_model <- k5_prediction(nhaplo, k5_class)
k5_nhaplo_accuracy <- cal_accuracy(k5_nhaplo_model)
k5_para_rank[[4,1]] <- c('N_Haplo')
k5_para_rank[[4,2]] <- c(k5_nhaplo_accuracy)

##--------parameter5: BAN
#calculate accuracy when k=1
k1_ban_model <- k1_prediction(ban, k1_class)
k1_ban_accuracy <- cal_accuracy(k1_ban_model)
k1_para_rank[[5,1]] <- c('BAN')
k1_para_rank[[5,2]] <- c(k1_ban_accuracy)
#calculate accuracy when k=2
k2_ban_model <- k2_prediction(ban, k2_class)
k2_ban_accuracy <- cal_accuracy(k2_ban_model)
k2_para_rank[[5,1]] <- c('BAN')
k2_para_rank[[5,2]] <- c(k2_ban_accuracy)
#calculate accuracy when k=3
k3_ban_model <- k3_prediction(ban, k3_class)
k3_ban_accuracy <- cal_accuracy(k3_ban_model)
k3_para_rank[[5,1]] <- c('BAN')
k3_para_rank[[5,2]] <- c(k3_ban_accuracy)
#calculate accuracy when k=5
k5_ban_model <- k5_prediction(ban, k5_class)
k5_ban_accuracy <- cal_accuracy(k5_ban_model)
k5_para_rank[[5,1]] <- c('BAN')
k5_para_rank[[5,2]] <- c(k5_ban_accuracy)

##--------parameter6: BEN
#calculate accuracy when k=1
k1_ben_model <- k1_prediction(ben, k1_class)
k1_ben_accuracy <- cal_accuracy(k1_ben_model)
k1_para_rank[[6,1]] <- c('BEN')
k1_para_rank[[6,2]] <- c(k1_ben_accuracy)
#calculate accuracy when k=2
k2_ben_model <- k2_prediction(ben, k2_class)
k2_ben_accuracy <- cal_accuracy(k2_ben_model)
k2_para_rank[[6,1]] <- c('BEN')
k2_para_rank[[6,2]] <- c(k2_ben_accuracy)
#calculate accuracy when k=3
k3_ben_model <- k3_prediction(ben, k3_class)
k3_ben_accuracy <- cal_accuracy(k3_ben_model)
k3_para_rank[[6,1]] <- c('BEN')
k3_para_rank[[6,2]] <- c(k3_ben_accuracy)
#calculate accuracy when k=5
k5_ben_model <- k5_prediction(ben, k5_class)
k5_ben_accuracy <- cal_accuracy(k5_ben_model)
k5_para_rank[[6,1]] <- c('BEN')
k5_para_rank[[6,2]] <- c(k5_ben_accuracy)

##--------parameter7: CAM
#calculate accuracy when k=1
k1_cam_model <- k1_prediction(cam, k1_class)
k1_cam_accuracy <- cal_accuracy(k1_cam_model)
k1_para_rank[[7,1]] <- c('CAM')
k1_para_rank[[7,2]] <- c(k1_cam_accuracy)
#calculate accuracy when k=2
k2_cam_model <- k2_prediction(cam, k2_class)
k2_cam_accuracy <- cal_accuracy(k2_cam_model)
k2_para_rank[[7,1]] <- c('CAM')
k2_para_rank[[7,2]] <- c(k2_cam_accuracy)
#calculate accuracy when k=3
k3_cam_model <- k3_prediction(cam, k3_class)
k3_cam_accuracy <- cal_accuracy(k3_cam_model)
k3_para_rank[[7,1]] <- c('CAM')
k3_para_rank[[7,2]] <- c(k3_cam_accuracy)
#calculate accuracy when k=5
k5_cam_model <- k5_prediction(cam, k5_class)
k5_cam_accuracy <- cal_accuracy(k5_cam_model)
k5_para_rank[[7,1]] <- c('CAM')
k5_para_rank[[7,2]] <- c(k5_cam_accuracy)

##--------parameter8: SEN
#calculate accuracy when k=1
k1_sen_model <- k1_prediction(sen, k1_class)
k1_sen_accuracy <- cal_accuracy(k1_sen_model)
k1_para_rank[[8,1]] <- c('SEN')
k1_para_rank[[8,2]] <- c(k1_sen_accuracy)
#calculate accuracy when k=2
k2_sen_model <- k2_prediction(sen, k2_class)
k2_sen_accuracy <- cal_accuracy(k2_sen_model)
k2_para_rank[[8,1]] <- c('SEN')
k2_para_rank[[8,2]] <- c(k2_sen_accuracy)
#calculate accuracy when k=3
k3_sen_model <- k3_prediction(sen, k3_class)
k3_sen_accuracy <- cal_accuracy(k3_sen_model)
k3_para_rank[[8,1]] <- c('SEN')
k3_para_rank[[8,2]] <- c(k3_sen_accuracy)
#calculate accuracy when k=5
k5_sen_model <- k5_prediction(sen, k5_class)
k5_sen_accuracy <- cal_accuracy(k5_sen_model)
k5_para_rank[[8,1]] <- c('SEN')
k5_para_rank[[8,2]] <- c(k5_sen_accuracy)

##--------parameter9: Weight
#calculate accuracy when k=1
k1_wgt_model <- k1_prediction(wgt, k1_class)
k1_wgt_accuracy <- cal_accuracy(k1_wgt_model)
k1_para_rank[[9,1]] <- c('Weight')
k1_para_rank[[9,2]] <- c(k1_wgt_accuracy)
#calculate accuracy when k=2
k2_wgt_model <- k2_prediction(wgt, k2_class)
k2_wgt_accuracy <- cal_accuracy(k2_wgt_model)
k2_para_rank[[9,1]] <- c('Weight')
k2_para_rank[[9,2]] <- c(k2_wgt_accuracy)
#calculate accuracy when k=3
k3_wgt_model <- k3_prediction(wgt, k3_class)
k3_wgt_accuracy <- cal_accuracy(k3_wgt_model)
k3_para_rank[[9,1]] <- c('Weight')
k3_para_rank[[9,2]] <- c(k3_wgt_accuracy)
#calculate accuracy when k=5
k5_wgt_model <- k5_prediction(wgt, k5_class)
k5_wgt_accuracy <- cal_accuracy(k5_wgt_model)
k5_para_rank[[9,1]] <- c('Weight')
k5_para_rank[[9,2]] <- c(k5_wgt_accuracy)

##--------parameter10: Hbf
#calculate accuracy when k=1
k1_hbf_model <- k1_prediction(hbf, k1_class)
k1_hbf_accuracy <- cal_accuracy(k1_hbf_model)
k1_para_rank[[10,1]] <- c('Hbf')
k1_para_rank[[10,2]] <- c(k1_hbf_accuracy)
#calculate accuracy when k=2
k2_hbf_model <- k2_prediction(hbf, k2_class)
k2_hbf_accuracy <- cal_accuracy(k2_hbf_model)
k2_para_rank[[10,1]] <- c('Hbf')
k2_para_rank[[10,2]] <- c(k2_hbf_accuracy)
#calculate accuracy when k=3
k3_hbf_model <- k3_prediction(hbf, k3_class)
k3_hbf_accuracy <- cal_accuracy(k3_hbf_model)
k3_para_rank[[10,1]] <- c('Hbf')
k3_para_rank[[10,2]] <- c(k3_hbf_accuracy)
#calculate accuracy when k=5
k5_hbf_model <- k5_prediction(hbf, k5_class)
k5_hbf_accuracy <- cal_accuracy(k5_hbf_model)
k5_para_rank[[10,1]] <- c('Hbf')
k5_para_rank[[10,2]] <- c(k5_hbf_accuracy)

##--------parameter11: Hb
#calculate accuracy when k=1
k1_hb_model <- k1_prediction(hb, k1_class)
k1_hb_accuracy <- cal_accuracy(k1_hb_model)
k1_para_rank[[11,1]] <- c('Hb')
k1_para_rank[[11,2]] <- c(k1_hb_accuracy)
#calculate accuracy when k=2
k2_hb_model <- k2_prediction(hb, k2_class)
k2_hb_accuracy <- cal_accuracy(k2_hb_model)
k2_para_rank[[11,1]] <- c('Hb')
k2_para_rank[[11,2]] <- c(k2_hb_accuracy)
#calculate accuracy when k=3
k3_hb_model <- k3_prediction(hb, k3_class)
k3_hb_accuracy <- cal_accuracy(k3_hb_model)
k3_para_rank[[11,1]] <- c('Hb')
k3_para_rank[[11,2]] <- c(k3_hb_accuracy)
#calculate accuracy when k=5
k5_hb_model <- k5_prediction(hb, k5_class)
k5_hb_accuracy <- cal_accuracy(k5_hb_model)
k5_para_rank[[11,1]] <- c('Hb')
k5_para_rank[[11,2]] <- c(k5_hb_accuracy)

##--------parameter12: HbS
#calculate accuracy when k=1
k1_hbs_model <- k1_prediction(hbs, k1_class)
k1_hbs_accuracy <- cal_accuracy(k1_hbs_model)
k1_para_rank[[12,1]] <- c('HbS')
k1_para_rank[[12,2]] <- c(k1_hbs_accuracy)
#calculate accuracy when k=2
k2_hbs_model <- k2_prediction(hbs, k2_class)
k2_hbs_accuracy <- cal_accuracy(k2_hbs_model)
k2_para_rank[[12,1]] <- c('HbS')
k2_para_rank[[12,2]] <- c(k2_hbs_accuracy)
#calculate accuracy when k=3
k3_hbs_model <- k3_prediction(hbs, k3_class)
k3_hbs_accuracy <- cal_accuracy(k3_hbs_model)
k3_para_rank[[12,1]] <- c('HbS')
k3_para_rank[[12,2]] <- c(k3_hbs_accuracy)
#calculate accuracy when k=5
k5_hbs_model <- k5_prediction(hbs, k5_class)
k5_hbs_accuracy <- cal_accuracy(k5_hbs_model)
k5_para_rank[[12,1]] <- c('HbS')
k5_para_rank[[12,2]] <- c(k5_hbs_accuracy)

##--------parameter13: RBC
#calculate accuracy when k=1
k1_rbc_model <- k1_prediction(rbc, k1_class)
k1_rbc_accuracy <- cal_accuracy(k1_rbc_model)
k1_para_rank[[13,1]] <- c('RBC')
k1_para_rank[[13,2]] <- c(k1_rbc_accuracy)
#calculate accuracy when k=2
k2_rbc_model <- k2_prediction(rbc, k2_class)
k2_rbc_accuracy <- cal_accuracy(k2_rbc_model)
k2_para_rank[[13,1]] <- c('RBC')
k2_para_rank[[13,2]] <- c(k2_rbc_accuracy)
#calculate accuracy when k=3
k3_rbc_model <- k3_prediction(rbc, k3_class)
k3_rbc_accuracy <- cal_accuracy(k3_rbc_model)
k3_para_rank[[13,1]] <- c('RBC')
k3_para_rank[[13,2]] <- c(k3_rbc_accuracy)
#calculate accuracy when k=5
k5_rbc_model <- k5_prediction(rbc, k5_class)
k5_rbc_accuracy <- cal_accuracy(k5_rbc_model)
k5_para_rank[[13,1]] <- c('RBC')
k5_para_rank[[13,2]] <- c(k5_rbc_accuracy)

##--------parameter14: RDW
#calculate accuracy when k=1
k1_rdw_model <- k1_prediction(rdw, k1_class)
k1_rdw_accuracy <- cal_accuracy(k1_rdw_model)
k1_para_rank[[14,1]] <- c('RDW')
k1_para_rank[[14,2]] <- c(k1_rdw_accuracy)
#calculate accuracy when k=2
k2_rdw_model <- k2_prediction(rdw, k2_class)
k2_rdw_accuracy <- cal_accuracy(k2_rdw_model)
k2_para_rank[[14,1]] <- c('RDW')
k2_para_rank[[14,2]] <- c(k2_rdw_accuracy)
#calculate accuracy when k=3
k3_rdw_model <- k3_prediction(rdw, k3_class)
k3_rdw_accuracy <- cal_accuracy(k3_rdw_model)
k3_para_rank[[14,1]] <- c('RDW')
k3_para_rank[[14,2]] <- c(k3_rdw_accuracy)
#calculate accuracy when k=5
k5_rdw_model <- k5_prediction(rdw, k5_class)
k5_rdw_accuracy <- cal_accuracy(k5_rdw_model)
k5_para_rank[[14,1]] <- c('RDW')
k5_para_rank[[14,2]] <- c(k5_rdw_accuracy)

##--------parameter15: PCV
#calculate accuracy when k=1
k1_pcv_model <- k1_prediction(pcv, k1_class)
k1_pcv_accuracy <- cal_accuracy(k1_pcv_model)
k1_para_rank[[15,1]] <- c('PCV')
k1_para_rank[[15,2]] <- c(k1_pcv_accuracy)
#calculate accuracy when k=2
k2_pcv_model <- k2_prediction(pcv, k2_class)
k2_pcv_accuracy <- cal_accuracy(k2_pcv_model)
k2_para_rank[[15,1]] <- c('PCV')
k2_para_rank[[15,2]] <- c(k2_pcv_accuracy)
#calculate accuracy when k=3
k3_pcv_model <- k3_prediction(pcv, k3_class)
k3_pcv_accuracy <- cal_accuracy(k3_pcv_model)
k3_para_rank[[15,1]] <- c('PCV')
k3_para_rank[[15,2]] <- c(k3_pcv_accuracy)
#calculate accuracy when k=5
k5_pcv_model <- k5_prediction(pcv, k5_class)
k5_pcv_accuracy <- cal_accuracy(k5_pcv_model)
k5_para_rank[[15,1]] <- c('PCV')
k5_para_rank[[15,2]] <- c(k5_pcv_accuracy)

##--------parameter16: Retic
#calculate accuracy when k=1
k1_retic_model <- k1_prediction(retic, k1_class)
k1_retic_accuracy <- cal_accuracy(k1_retic_model)
k1_para_rank[[16,1]] <- c('Retic')
k1_para_rank[[16,2]] <- c(k1_retic_accuracy)
#calculate accuracy when k=2
k2_retic_model <- k2_prediction(retic, k2_class)
k2_retic_accuracy <- cal_accuracy(k2_retic_model)
k2_para_rank[[16,1]] <- c('Retic')
k2_para_rank[[16,2]] <- c(k2_retic_accuracy)
#calculate accuracy when k=3
k3_retic_model <- k3_prediction(retic, k3_class)
k3_retic_accuracy <- cal_accuracy(k3_retic_model)
k3_para_rank[[16,1]] <- c('Retic')
k3_para_rank[[16,2]] <- c(k3_retic_accuracy)
#calculate accuracy when k=5
k5_retic_model <- k5_prediction(retic, k5_class)
k5_retic_accuracy <- cal_accuracy(k5_retic_model)
k5_para_rank[[16,1]] <- c('Retic')
k5_para_rank[[16,2]] <- c(k5_retic_accuracy)

##--------parameter17: MCV
#calculate accuracy when k=1
k1_mcv_model <- k1_prediction(mcv, k1_class)
k1_mcv_accuracy <- cal_accuracy(k1_mcv_model)
k1_para_rank[[17,1]] <- c('MCV')
k1_para_rank[[17,2]] <- c(k1_mcv_accuracy)
#calculate accuracy when k=2
k2_mcv_model <- k2_prediction(mcv, k2_class)
k2_mcv_accuracy <- cal_accuracy(k2_mcv_model)
k2_para_rank[[17,1]] <- c('MCV')
k2_para_rank[[17,2]] <- c(k2_mcv_accuracy)
#calculate accuracy when k=3
k3_mcv_model <- k3_prediction(mcv, k3_class)
k3_mcv_accuracy <- cal_accuracy(k3_mcv_model)
k3_para_rank[[17,1]] <- c('MCV')
k3_para_rank[[17,2]] <- c(k3_mcv_accuracy)
#calculate accuracy when k=5
k5_mcv_model <- k5_prediction(mcv, k5_class)
k5_mcv_accuracy <- cal_accuracy(k5_mcv_model)
k5_para_rank[[17,1]] <- c('MCV')
k5_para_rank[[17,2]] <- c(k5_mcv_accuracy)

##--------parameter18: MCH
#calculate accuracy when k=1
k1_mch_model <- k1_prediction(mch, k1_class)
k1_mch_accuracy <- cal_accuracy(k1_mch_model)
k1_para_rank[[18,1]] <- c('MCH')
k1_para_rank[[18,2]] <- c(k1_mch_accuracy)
#calculate accuracy when k=2
k2_mch_model <- k2_prediction(mch, k2_class)
k2_mch_accuracy <- cal_accuracy(k2_mch_model)
k2_para_rank[[18,1]] <- c('MCH')
k2_para_rank[[18,2]] <- c(k2_mch_accuracy)
#calculate accuracy when k=3
k3_mch_model <- k3_prediction(mch, k3_class)
k3_mch_accuracy <- cal_accuracy(k3_mch_model)
k3_para_rank[[18,1]] <- c('MCH')
k3_para_rank[[18,2]] <- c(k3_mch_accuracy)
#calculate accuracy when k=5
k5_mch_model <- k5_prediction(mch, k5_class)
k5_mch_accuracy <- cal_accuracy(k5_mch_model)
k5_para_rank[[18,1]] <- c('MCH')
k5_para_rank[[18,2]] <- c(k5_mch_accuracy)

##--------parameter19: WBC
#calculate accuracy when k=1
k1_wbc_model <- k1_prediction(wbc, k1_class)
k1_wbc_accuracy <- cal_accuracy(k1_wbc_model)
k1_para_rank[[19,1]] <- c('WBC')
k1_para_rank[[19,2]] <- c(k1_wbc_accuracy)
#calculate accuracy when k=2
k2_wbc_model <- k2_prediction(wbc, k2_class)
k2_wbc_accuracy <- cal_accuracy(k2_wbc_model)
k2_para_rank[[19,1]] <- c('WBC')
k2_para_rank[[19,2]] <- c(k2_wbc_accuracy)
#calculate accuracy when k=3
k3_wbc_model <- k3_prediction(wbc, k3_class)
k3_wbc_accuracy <- cal_accuracy(k3_wbc_model)
k3_para_rank[[19,1]] <- c('WBC')
k3_para_rank[[19,2]] <- c(k3_wbc_accuracy)
#calculate accuracy when k=5
k5_wbc_model <- k5_prediction(wbc, k5_class)
k5_wbc_accuracy <- cal_accuracy(k5_wbc_model)
k5_para_rank[[19,1]] <- c('WBC')
k5_para_rank[[19,2]] <- c(k5_wbc_accuracy)

##--------parameter20: Polys
#calculate accuracy when k=1
k1_polys_model <- k1_prediction(polys, k1_class)
k1_polys_accuracy <- cal_accuracy(k1_polys_model)
k1_para_rank[[20,1]] <- c('Polys')
k1_para_rank[[20,2]] <- c(k1_polys_accuracy)
#calculate accuracy when k=2
k2_polys_model <- k2_prediction(polys, k2_class)
k2_polys_accuracy <- cal_accuracy(k2_polys_model)
k2_para_rank[[20,1]] <- c('Polys')
k2_para_rank[[20,2]] <- c(k2_polys_accuracy)
#calculate accuracy when k=3
k3_polys_model <- k3_prediction(polys, k3_class)
k3_polys_accuracy <- cal_accuracy(k3_polys_model)
k3_para_rank[[20,1]] <- c('Polys')
k3_para_rank[[20,2]] <- c(k3_polys_accuracy)
#calculate accuracy when k=5
k5_polys_model <- k5_prediction(polys, k5_class)
k5_polys_accuracy <- cal_accuracy(k5_polys_model)
k5_para_rank[[20,1]] <- c('Polys')
k5_para_rank[[20,2]] <- c(k5_polys_accuracy)

##--------parameter21: Plats
#calculate accuracy when k=1
k1_plats_model <- k1_prediction(plats, k1_class)
k1_plats_accuracy <- cal_accuracy(k1_plats_model)
k1_para_rank[[21,1]] <- c('Plats')
k1_para_rank[[21,2]] <- c(k1_plats_accuracy)
#calculate accuracy when k=2
k2_plats_model <- k2_prediction(plats, k2_class)
k2_plats_accuracy <- cal_accuracy(k2_plats_model)
k2_para_rank[[21,1]] <- c('Plats')
k2_para_rank[[21,2]] <- c(k2_plats_accuracy)
#calculate accuracy when k=3
k3_plats_model <- k3_prediction(plats, k3_class)
k3_plats_accuracy <- cal_accuracy(k3_plats_model)
k3_para_rank[[21,1]] <- c('Plats')
k3_para_rank[[21,2]] <- c(k3_plats_accuracy)
#calculate accuracy when k=5
k5_plats_model <- k5_prediction(plats, k5_class)
k5_plats_accuracy <- cal_accuracy(k5_plats_model)
k5_para_rank[[21,1]] <- c('Plats')
k5_para_rank[[21,2]] <- c(k5_plats_accuracy)

##--------parameter22: Bili
#calculate accuracy when k=1
k1_bili_model <- k1_prediction(bili, k1_class)
k1_bili_accuracy <- cal_accuracy(k1_bili_model)
k1_para_rank[[22,1]] <- c('Bili')
k1_para_rank[[22,2]] <- c(k1_bili_accuracy)
#calculate accuracy when k=2
k2_bili_model <- k2_prediction(bili, k2_class)
k2_bili_accuracy <- cal_accuracy(k2_bili_model)
k2_para_rank[[22,1]] <- c('Bili')
k2_para_rank[[22,2]] <- c(k2_bili_accuracy)
#calculate accuracy when k=3
k3_bili_model <- k3_prediction(bili, k3_class)
k3_bili_accuracy <- cal_accuracy(k3_bili_model)
k3_para_rank[[22,1]] <- c('Bili')
k3_para_rank[[22,2]] <- c(k3_bili_accuracy)
#calculate accuracy when k=5
k5_bili_model <- k5_prediction(bili, k5_class)
k5_bili_accuracy <- cal_accuracy(k5_bili_model)
k5_para_rank[[22,1]] <- c('Bili')
k5_para_rank[[22,2]] <- c(k5_bili_accuracy)

##--------parameter23: NRBC
#calculate accuracy when k=1
k1_nrbc_model <- k1_prediction(nrbc, k1_class)
k1_nrbc_accuracy <- cal_accuracy(k1_nrbc_model)
k1_para_rank[[23,1]] <- c('NRBC')
k1_para_rank[[23,2]] <- c(k1_nrbc_accuracy)
#calculate accuracy when k=2
k2_nrbc_model <- k2_prediction(nrbc, k2_class)
k2_nrbc_accuracy <- cal_accuracy(k2_nrbc_model)
k2_para_rank[[23,1]] <- c('NRBC')
k2_para_rank[[23,2]] <- c(k2_nrbc_accuracy)
#calculate accuracy when k=3
k3_nrbc_model <- k3_prediction(nrbc, k3_class)
k3_nrbc_accuracy <- cal_accuracy(k3_nrbc_model)
k3_para_rank[[23,1]] <- c('NRBC')
k3_para_rank[[23,2]] <- c(k3_nrbc_accuracy)
#calculate accuracy when k=5
k5_nrbc_model <- k5_prediction(nrbc, k5_class)
k5_nrbc_accuracy <- cal_accuracy(k5_nrbc_model)
k5_para_rank[[23,1]] <- c('NRBC')
k5_para_rank[[23,2]] <- c(k5_nrbc_accuracy)

### Sort the accuracies
sorted_k1_para_rank <- k1_para_rank[order(k1_para_rank[,2], decreasing=TRUE),] 
sorted_k2_para_rank <- k2_para_rank[order(k2_para_rank[,2], decreasing=TRUE),] 
sorted_k3_para_rank <- k3_para_rank[order(k3_para_rank[,2], decreasing=TRUE),] 
sorted_k5_para_rank <- k5_para_rank[order(k5_para_rank[,2], decreasing=TRUE),] 