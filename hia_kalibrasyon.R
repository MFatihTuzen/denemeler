library(dplyr)
library(data.table)
library(readr)
library(haven)
library(stringr)

ana.iter <- 4
alt.iter <- 8

setwd("D:/hia_cal/")
pers <- as.data.table(read_sas("fakisg_aralik_2018_il_pj.sas7bdat"))
pop_prop_age_gender_y <-  as.data.table(read_sas("proj_hia_ara2018_ag_il_pj.sas7bdat"))
# pop_prop_age_gender_y[, country := 1]
pop_prop_il <- as.data.table(read_sas("proj_hia_ara2018_il_pj.sas7bdat"))
# pop_prop_il[, country := 1]

pers[, fk_fkiml := (1:.N)]
setnames(pers,c("CINSIYET","BIRIMNO","YAS"),c("fk_cinsi","bulten","fk_yas"))
pers[fk_cinsi == 2, gender := "female"]
pers[fk_cinsi == 1, gender := "male"]
pers[fk_yas %between% c(0,4), age_g := "0-4"]
pers[fk_yas %between% c(5,11), age_g := "5-11"]
pers[fk_yas %between% c(12,14), age_g := "12-14"]
pers[fk_yas %between% c(15,17), age_g := "15-17"]
pers[fk_yas %between% c(18,20), age_g := "18-20"]
pers[fk_yas %between% c(21,24), age_g := "21-24"]
pers[fk_yas %between% c(25,29), age_g := "25-29"]
pers[fk_yas %between% c(30,34), age_g := "30-34"]
pers[fk_yas %between% c(35,39), age_g := "35-39"]
pers[fk_yas %between% c(40,44), age_g := "40-44"]
pers[fk_yas %between% c(45,49), age_g := "45-49"]
pers[fk_yas %between% c(50,54), age_g := "50-54"]
pers[fk_yas %between% c(55,59), age_g := "55-59"]
pers[fk_yas %between% c(60,64), age_g := "60-64"]
pers[fk_yas %between% c(65,74), age_g := "65-74"]
pers[fk_yas > 74, age_g := "75+"]
pers[, D_01 := ifelse(str_trim(ILKAYITNO) == "01",1,0)]
pers[, D_02 := ifelse(str_trim(ILKAYITNO) == "02",1,0)]
pers[, D_03 := ifelse(str_trim(ILKAYITNO) == "03",1,0)]
pers[, D_04 := ifelse(str_trim(ILKAYITNO) == "04",1,0)]
pers[, D_05 := ifelse(str_trim(ILKAYITNO) == "05",1,0)]
pers[, D_06 := ifelse(str_trim(ILKAYITNO) == "06",1,0)]
pers[, D_07 := ifelse(str_trim(ILKAYITNO) == "07",1,0)]
pers[, D_08 := ifelse(str_trim(ILKAYITNO) == "08",1,0)]
pers[, D_09 := ifelse(str_trim(ILKAYITNO) == "09",1,0)]
pers[, D_10 := ifelse(str_trim(ILKAYITNO) == "10",1,0)]
pers[, D_11 := ifelse(str_trim(ILKAYITNO) == "11",1,0)]
pers[, D_12 := ifelse(str_trim(ILKAYITNO) == "12",1,0)]
pers[, D_13 := ifelse(str_trim(ILKAYITNO) == "13",1,0)]
pers[, D_14 := ifelse(str_trim(ILKAYITNO) == "14",1,0)]
pers[, D_15 := ifelse(str_trim(ILKAYITNO) == "15",1,0)]
pers[, D_16 := ifelse(str_trim(ILKAYITNO) == "16",1,0)]
pers[, D_17 := ifelse(str_trim(ILKAYITNO) == "17",1,0)]
pers[, D_18 := ifelse(str_trim(ILKAYITNO) == "18",1,0)]
pers[, D_19 := ifelse(str_trim(ILKAYITNO) == "19",1,0)]
pers[, D_20 := ifelse(str_trim(ILKAYITNO) == "20",1,0)]
pers[, D_21 := ifelse(str_trim(ILKAYITNO) == "21",1,0)]
pers[, D_22 := ifelse(str_trim(ILKAYITNO) == "22",1,0)]
pers[, D_23 := ifelse(str_trim(ILKAYITNO) == "23",1,0)]
pers[, D_24 := ifelse(str_trim(ILKAYITNO) == "24",1,0)]
pers[, D_25 := ifelse(str_trim(ILKAYITNO) == "25",1,0)]
pers[, D_26 := ifelse(str_trim(ILKAYITNO) == "26",1,0)]
pers[, D_27 := ifelse(str_trim(ILKAYITNO) == "27",1,0)]
pers[, D_28 := ifelse(str_trim(ILKAYITNO) == "28",1,0)]
pers[, D_29 := ifelse(str_trim(ILKAYITNO) == "29",1,0)]
pers[, D_30 := ifelse(str_trim(ILKAYITNO) == "30",1,0)]
pers[, D_31 := ifelse(str_trim(ILKAYITNO) == "31",1,0)]
pers[, D_32 := ifelse(str_trim(ILKAYITNO) == "32",1,0)]
pers[, D_33 := ifelse(str_trim(ILKAYITNO) == "33",1,0)]
pers[, D_34 := ifelse(str_trim(ILKAYITNO) == "34",1,0)]
pers[, D_35 := ifelse(str_trim(ILKAYITNO) == "35",1,0)]
pers[, D_36 := ifelse(str_trim(ILKAYITNO) == "36",1,0)]
pers[, D_37 := ifelse(str_trim(ILKAYITNO) == "37",1,0)]
pers[, D_38 := ifelse(str_trim(ILKAYITNO) == "38",1,0)]
pers[, D_39 := ifelse(str_trim(ILKAYITNO) == "39",1,0)]
pers[, D_40 := ifelse(str_trim(ILKAYITNO) == "40",1,0)]
pers[, D_41 := ifelse(str_trim(ILKAYITNO) == "41",1,0)]
pers[, D_42 := ifelse(str_trim(ILKAYITNO) == "42",1,0)]
pers[, D_43 := ifelse(str_trim(ILKAYITNO) == "43",1,0)]
pers[, D_44 := ifelse(str_trim(ILKAYITNO) == "44",1,0)]
pers[, D_45 := ifelse(str_trim(ILKAYITNO) == "45",1,0)]
pers[, D_46 := ifelse(str_trim(ILKAYITNO) == "46",1,0)]
pers[, D_47 := ifelse(str_trim(ILKAYITNO) == "47",1,0)]
pers[, D_48 := ifelse(str_trim(ILKAYITNO) == "48",1,0)]
pers[, D_49 := ifelse(str_trim(ILKAYITNO) == "49",1,0)]
pers[, D_50 := ifelse(str_trim(ILKAYITNO) == "50",1,0)]
pers[, D_51 := ifelse(str_trim(ILKAYITNO) == "51",1,0)]
pers[, D_52 := ifelse(str_trim(ILKAYITNO) == "52",1,0)]
pers[, D_53 := ifelse(str_trim(ILKAYITNO) == "53",1,0)]
pers[, D_54 := ifelse(str_trim(ILKAYITNO) == "54",1,0)]
pers[, D_55 := ifelse(str_trim(ILKAYITNO) == "55",1,0)]
pers[, D_56 := ifelse(str_trim(ILKAYITNO) == "56",1,0)]
pers[, D_57 := ifelse(str_trim(ILKAYITNO) == "57",1,0)]
pers[, D_58 := ifelse(str_trim(ILKAYITNO) == "58",1,0)]
pers[, D_59 := ifelse(str_trim(ILKAYITNO) == "59",1,0)]
pers[, D_60 := ifelse(str_trim(ILKAYITNO) == "60",1,0)]
pers[, D_61 := ifelse(str_trim(ILKAYITNO) == "61",1,0)]
pers[, D_62 := ifelse(str_trim(ILKAYITNO) == "62",1,0)]
pers[, D_63 := ifelse(str_trim(ILKAYITNO) == "63",1,0)]
pers[, D_64 := ifelse(str_trim(ILKAYITNO) == "64",1,0)]
pers[, D_65 := ifelse(str_trim(ILKAYITNO) == "65",1,0)]
pers[, D_66 := ifelse(str_trim(ILKAYITNO) == "66",1,0)]
pers[, D_67 := ifelse(str_trim(ILKAYITNO) == "67",1,0)]
pers[, D_68 := ifelse(str_trim(ILKAYITNO) == "68",1,0)]
pers[, D_69 := ifelse(str_trim(ILKAYITNO) == "69",1,0)]
pers[, D_70 := ifelse(str_trim(ILKAYITNO) == "70",1,0)]
pers[, D_71 := ifelse(str_trim(ILKAYITNO) == "71",1,0)]
pers[, D_72 := ifelse(str_trim(ILKAYITNO) == "72",1,0)]
pers[, D_73 := ifelse(str_trim(ILKAYITNO) == "73",1,0)]
pers[, D_74 := ifelse(str_trim(ILKAYITNO) == "74",1,0)]
pers[, D_75 := ifelse(str_trim(ILKAYITNO) == "75",1,0)]
pers[, D_76 := ifelse(str_trim(ILKAYITNO) == "76",1,0)]
pers[, D_77 := ifelse(str_trim(ILKAYITNO) == "77",1,0)]
pers[, D_78 := ifelse(str_trim(ILKAYITNO) == "78",1,0)]
pers[, D_79 := ifelse(str_trim(ILKAYITNO) == "79",1,0)]
pers[, D_80 := ifelse(str_trim(ILKAYITNO) == "80",1,0)]
pers[, D_81 := ifelse(str_trim(ILKAYITNO) == "81",1,0)]
pers[, IL := (D_01+D_02+D_03+D_04+D_05+D_06+D_07+D_08+D_09+D_10+D_11+D_12+D_13+D_14+D_15+D_16+
       D_17+D_18+D_19+D_20+D_21+D_22+D_23+D_24+D_25+D_26+D_27+D_28+D_29+D_30+D_31+D_32+D_33+D_34+
       D_35+D_36+D_37+D_38+D_39+D_40+D_41+D_42+D_43+D_44+D_45+D_46+D_47+D_48+D_49+D_50+D_51+D_52+D_53+
       D_54+D_55+D_56+D_57+D_58+D_59+D_60+D_61+D_62+D_63+D_64+D_65+D_66+D_67+D_68+D_69+D_70+D_71+D_72+
       D_73+D_74+D_75+D_76+D_77+D_78+D_79+D_80+D_81)]
if(max(pers[,IL])- min(pers[,IL]) == 0) print("IL değişkenlerinde hata yok")

pers[, D_m_0_4 := ifelse(gender == "male" & age_g == "0-4",1,0)]
pers[, D_m_5_11:= ifelse(gender =='male' & age_g == '5-11' ,1,0)]
pers[, D_m_12_14:= ifelse(gender =='male' & age_g == '12-14' ,1,0)]
pers[, D_m_15_17:= ifelse(gender =='male' & age_g == '15-17' ,1,0)]
pers[, D_m_18_20:= ifelse(gender =='male' & age_g == '18-20' ,1,0)]
pers[, D_m_21_24:= ifelse(gender =='male' & age_g == '21-24' ,1,0)]
pers[, D_m_25_29:= ifelse(gender =='male' & age_g == '25-29' ,1,0)]
pers[, D_m_30_34:= ifelse(gender =='male' & age_g == '30-34' ,1,0)]
pers[, D_m_35_39:= ifelse(gender =='male' & age_g == '35-39' ,1,0)]
pers[, D_m_40_44:= ifelse(gender =='male' & age_g == '40-44' ,1,0)]
pers[, D_m_45_49:= ifelse(gender =='male' & age_g == '45-49' ,1,0)]
pers[, D_m_50_54:= ifelse(gender =='male' & age_g == '50-54' ,1,0)]
pers[, D_m_55_59:= ifelse(gender =='male' & age_g == '55-59' ,1,0)]
pers[, D_m_60_64:= ifelse(gender =='male' & age_g == '60-64' ,1,0)]
pers[, D_m_65_74:= ifelse(gender =='male' & age_g == '65-74' ,1,0)]
pers[, D_m_over75:= ifelse(gender =='male' & age_g == '75+' ,1,0)]
pers[, D_f_0_4:= ifelse(gender =='female' & age_g == '0-4' ,1,0)]
pers[, D_f_5_11:= ifelse(gender =='female' & age_g == '5-11' ,1,0)]
pers[, D_f_12_14:= ifelse(gender =='female' & age_g == '12-14' ,1,0)]
pers[, D_f_15_17:= ifelse(gender =='female' & age_g == '15-17' ,1,0)]
pers[, D_f_18_20:= ifelse(gender =='female' & age_g == '18-20' ,1,0)]
pers[, D_f_21_24:= ifelse(gender =='female' & age_g == '21-24' ,1,0)]
pers[, D_f_25_29:= ifelse(gender =='female' & age_g == '25-29' ,1,0)]
pers[, D_f_30_34:= ifelse(gender =='female' & age_g == '30-34' ,1,0)]
pers[, D_f_35_39:= ifelse(gender =='female' & age_g == '35-39' ,1,0)]
pers[, D_f_40_44:= ifelse(gender =='female' & age_g == '40-44' ,1,0)]
pers[, D_f_45_49:= ifelse(gender =='female' & age_g == '45-49' ,1,0)]
pers[, D_f_50_54:= ifelse(gender =='female' & age_g == '50-54' ,1,0)]
pers[, D_f_55_59:= ifelse(gender =='female' & age_g == '55-59' ,1,0)]
pers[, D_f_60_64:= ifelse(gender =='female' & age_g == '60-64' ,1,0)]
pers[, D_f_65_74:= ifelse(gender =='female' & age_g == '65-74' ,1,0)]
pers[, D_f_over75:= ifelse(gender =='female' & age_g == '75+' ,1,0)]
pers[, GENDAGE := (D_m_0_4+D_m_5_11+D_m_12_14+D_m_15_17+D_m_45_49+D_m_21_24+D_m_25_29+D_m_30_34+D_m_35_39+
       D_m_40_44+D_m_45_49+D_m_50_54+D_m_55_59+D_m_60_64+D_m_65_74+D_m_over75+D_f_0_4+D_f_5_11+D_f_12_14+D_f_15_17+D_f_45_49+D_f_21_24+D_f_25_29+D_f_30_34+D_f_35_39+
       D_f_40_44+D_f_45_49+D_f_50_54+D_f_55_59+D_f_60_64+ D_f_65_74+D_f_over75)]
if(max(pers[,GENDAGE])- min(pers[,GENDAGE]) == 0) print("GENDAGE değişkenlerinde hata yok")


mean <- pers[, lapply(.SD,mean), by = bulten, .SDcols = D_01:D_81]
count <- pers[, .(NUM_m_0_4 = sum(D_m_0_4), 
                  NUM_m_5_11 = sum(D_m_5_11),
                  NUM_m_12_14 = sum(D_m_12_14), 
                  NUM_m_15_17 = sum(D_m_15_17), 
                  NUM_m_18_20 = sum(D_m_18_20), 
                  NUM_m_21_24 = sum(D_m_21_24), 
                  NUM_m_25_29 = sum(D_m_25_29), 
                  NUM_m_30_34 = sum(D_m_30_34), 
                  NUM_m_35_39 = sum(D_m_35_39), 
                  NUM_m_40_44 = sum(D_m_40_44), 
                  NUM_m_45_49 = sum(D_m_45_49), 
                  NUM_m_50_54 = sum(D_m_50_54), 
                  NUM_m_55_59 = sum(D_m_55_59), 
                  NUM_m_60_64 = sum(D_m_60_64), 
                  NUM_m_65_74 = sum(D_m_65_74), 
                  NUM_m_over75 = sum(D_m_over75),
                  NUM_f_0_4 = sum(D_f_0_4), 
                  NUM_f_5_11 = sum(D_f_5_11),
                  NUM_f_12_14 = sum(D_f_12_14), 
                  NUM_f_15_17 = sum(D_f_15_17), 
                  NUM_f_18_20 = sum(D_f_18_20), 
                  NUM_f_21_24 = sum(D_f_21_24), 
                  NUM_f_25_29 = sum(D_f_25_29), 
                  NUM_f_30_34 = sum(D_f_30_34), 
                  NUM_f_35_39 = sum(D_f_35_39), 
                  NUM_f_40_44 = sum(D_f_40_44), 
                  NUM_f_45_49 = sum(D_f_45_49), 
                  NUM_f_50_54 = sum(D_f_50_54), 
                  NUM_f_55_59 = sum(D_f_55_59), 
                  NUM_f_60_64 = sum(D_f_60_64), 
                  NUM_f_65_74 = sum(D_f_65_74), 
                  NUM_f_over75 = sum(D_f_over75)), by = bulten]
setkey(mean, bulten)
setkey(count, bulten)
hfile <- mean[count, nomatch = 0]

hfile[, DH_m_0_4  := ifelse(NUM_m_0_4 == 0, 0,1)]
hfile[, DH_m_5_11 := ifelse(NUM_m_5_11 == 0, 0,1)]
hfile[, DH_m_12_14:= ifelse(NUM_m_12_14 == 0, 0,1)]
hfile[, DH_m_15_17:= ifelse(NUM_m_15_17 == 0, 0,1)]
hfile[, DH_m_18_20:= ifelse(NUM_m_18_20 == 0, 0,1)]
hfile[, DH_m_21_24:= ifelse(NUM_m_21_24 == 0, 0,1)]
hfile[, DH_m_25_29:= ifelse(NUM_m_25_29 == 0, 0,1)]
hfile[, DH_m_30_34:= ifelse(NUM_m_30_34 == 0, 0,1)]
hfile[, DH_m_35_39:= ifelse(NUM_m_35_39 == 0, 0,1)]
hfile[, DH_m_40_44:= ifelse(NUM_m_40_44 == 0, 0,1)]
hfile[, DH_m_45_49:= ifelse(NUM_m_45_49 == 0, 0,1)]
hfile[, DH_m_50_54:= ifelse(NUM_m_50_54 == 0, 0,1)]
hfile[, DH_m_55_59:= ifelse(NUM_m_55_59 == 0, 0,1)]
hfile[, DH_m_60_64:= ifelse(NUM_m_60_64 == 0, 0,1)]
hfile[, DH_m_65_74:= ifelse(NUM_m_65_74 == 0, 0,1)]
hfile[, DH_m_over75:= ifelse(NUM_m_over75 == 0, 0,1)]
hfile[, DH_f_0_4:= ifelse(NUM_f_0_4 == 0, 0,1)]
hfile[, DH_f_5_11:= ifelse(NUM_f_5_11 == 0, 0,1)]
hfile[, DH_f_12_14:= ifelse(NUM_f_12_14 == 0, 0,1)]
hfile[, DH_f_15_17:= ifelse(NUM_f_15_17 == 0, 0,1)]
hfile[, DH_f_18_20:= ifelse(NUM_f_18_20 == 0, 0,1)]
hfile[, DH_f_21_24:= ifelse(NUM_f_21_24 == 0, 0,1)]
hfile[, DH_f_25_29:= ifelse(NUM_f_25_29 == 0, 0,1)]
hfile[, DH_f_30_34:= ifelse(NUM_f_30_34 == 0, 0,1)]
hfile[, DH_f_35_39:= ifelse(NUM_f_35_39 == 0, 0,1)]
hfile[, DH_f_40_44:= ifelse(NUM_f_40_44 == 0, 0,1)]
hfile[, DH_f_45_49:= ifelse(NUM_f_45_49 == 0, 0,1)]
hfile[, DH_f_50_54:= ifelse(NUM_f_50_54 == 0, 0,1)]
hfile[, DH_f_55_59:= ifelse(NUM_f_55_59 == 0, 0,1)]
hfile[, DH_f_60_64:= ifelse(NUM_f_60_64 == 0, 0,1)]
hfile[, DH_f_65_74:= ifelse(NUM_f_65_74 == 0, 0,1)]
hfile[, DH_f_over75:= ifelse(NUM_f_over75 == 0, 0,1)]
hfile[, country := 1]

setkey(hfile, bulten)
setkey(pers, bulten)
pers_h0 <- pers[hfile, nomatch = 0]
colnames(hfile)

pers_h <- cbind(pers_h0, pop_prop_age_gender_y, pop_prop_il)
pers_h[, w := duzfakto]
pers_h[, w_old := w]

# p_yc <- pers_h[,lapply(.SD,weighted.mean,w=w),.SDcols=D_m_0_4: D_f_over75]
# colnames(p_yc) <- gsub("D","p",colnames(p_yc))
# p_dyc <- pers_h[,lapply(.SD,weighted.mean,w=w),.SDcols=DH_m_0_4: DH_f_over75]
# colnames(p_dyc) <- gsub("DH","a",colnames(p_dyc))
# p_il <- pers_h[,lapply(.SD,weighted.mean,w=w),.SDcols=D_01: D_81]
# colnames(p_il) <- gsub("D","p_il",colnames(p_il))
# a_il <- p_il
# colnames(a_il) <- gsub("p","a",colnames(a_il))

# pers_h <- cbind(pers_h, p_yc, p_dyc, p_il, a_il)

pers_h[, p_m_0_4 := weighted.mean(D_m_0_4, w = w)]
pers_h[, a_m_0_4 := weighted.mean(DH_m_0_4, w = w)]
pers_h[, w := ifelse(DH_m_0_4 == 1, w_old*(PF_m_0_4/p_m_0_4), w_old*((1-a_m_0_4*(PF_m_0_4/p_m_0_4))/(1-a_m_0_4)))]
pers_h[, w_old := w]

pers_h[, p_m_5_11 := weighted.mean(D_m_5_11, w = w)]
pers_h[, a_m_5_11 := weighted.mean(DH_m_5_11, w = w)]
pers_h[, w := ifelse(DH_m_5_11 == 1, w_old*(PF_m_5_11/p_m_5_11), w_old*((1-a_m_5_11*(PF_m_5_11/p_m_5_11))/(1-a_m_5_11)))]
pers_h[, w_old := w]

pers_h[, p_m_12_14 := weighted.mean(D_m_12_14, w = w)]
pers_h[, a_m_12_14 := weighted.mean(DH_m_12_14, w = w)]
pers_h[, w := ifelse(DH_m_12_14 == 1, w_old*(PF_m_12_14/p_m_12_14), w_old*((1-a_m_12_14*(PF_m_12_14/p_m_12_14))/(1-a_m_12_14)))]
pers_h[, w_old := w]

pers_h[, p_m_15_17 := weighted.mean(D_m_15_17, w = w)]
pers_h[, a_m_15_17 := weighted.mean(DH_m_15_17, w = w)]
pers_h[, w := ifelse(DH_m_15_17 == 1, w_old*(PF_m_15_17/p_m_15_17), w_old*((1-a_m_15_17*(PF_m_15_17/p_m_15_17))/(1-a_m_15_17)))]
pers_h[, w_old := w]

pers_h[, p_m_18_20 := weighted.mean(D_m_18_20, w = w)]
pers_h[, a_m_18_20 := weighted.mean(DH_m_18_20, w = w)]
pers_h[, w := ifelse(DH_m_18_20 == 1, w_old*(PF_m_18_20/p_m_18_20), w_old*((1-a_m_18_20*(PF_m_18_20/p_m_18_20))/(1-a_m_18_20)))]
pers_h[, w_old := w]

pers_h[, p_m_21_24 := weighted.mean(D_m_21_24, w = w)]
pers_h[, a_m_21_24 := weighted.mean(DH_m_21_24, w = w)]
pers_h[, w := ifelse(DH_m_21_24 == 1, w_old*(PF_m_21_24/p_m_21_24), w_old*((1-a_m_21_24*(PF_m_21_24/p_m_21_24))/(1-a_m_21_24)))]
pers_h[, w_old := w]

pers_h[, p_m_25_29 := weighted.mean(D_m_25_29, w = w)]
pers_h[, a_m_25_29 := weighted.mean(DH_m_25_29, w = w)]
pers_h[, w := ifelse(DH_m_25_29 == 1, w_old*(PF_m_25_29/p_m_25_29), w_old*((1-a_m_25_29*(PF_m_25_29/p_m_25_29))/(1-a_m_25_29)))]
pers_h[, w_old := w]

pers_h[, p_m_30_34 := weighted.mean(D_m_30_34, w = w)]
pers_h[, a_m_30_34 := weighted.mean(DH_m_30_34, w = w)]
pers_h[, w := ifelse(DH_m_30_34 == 1, w_old*(PF_m_30_34/p_m_30_34), w_old*((1-a_m_30_34*(PF_m_30_34/p_m_30_34))/(1-a_m_30_34)))]
pers_h[, w_old := w]

pers_h[, p_m_35_39 := weighted.mean(D_m_35_39, w = w)]
pers_h[, a_m_35_39:= weighted.mean(DH_m_35_39, w = w)]
pers_h[, w := ifelse(DH_m_35_39 == 1, w_old*(PF_m_35_39/p_m_35_39), w_old*((1-a_m_35_39*(PF_m_35_39/p_m_35_39))/(1-a_m_35_39)))]
pers_h[, w_old := w]

pers_h[, p_m_40_44 := weighted.mean(D_m_40_44, w = w)]
pers_h[, a_m_40_44 := weighted.mean(DH_m_40_44, w = w)]
pers_h[, w := ifelse(DH_m_40_44 == 1, w_old*(PF_m_40_44/p_m_40_44), w_old*((1-a_m_40_44*(PF_m_40_44/p_m_40_44))/(1-a_m_40_44)))]
pers_h[, w_old := w]

pers_h[, p_m_45_49 := weighted.mean(D_m_45_49, w = w)]
pers_h[, a_m_45_49 := weighted.mean(DH_m_45_49, w = w)]
pers_h[, w := ifelse(DH_m_45_49 == 1, w_old*(PF_m_45_49/p_m_45_49), w_old*((1-a_m_45_49*(PF_m_45_49/p_m_45_49))/(1-a_m_45_49)))]
pers_h[, w_old := w]

pers_h[, p_m_50_54 := weighted.mean(D_m_50_54, w = w)]
pers_h[, a_m_50_54 := weighted.mean(DH_m_50_54, w = w)]
pers_h[, w := ifelse(DH_m_50_54 == 1, w_old*(PF_m_50_54/p_m_50_54), w_old*((1-a_m_50_54*(PF_m_50_54/p_m_50_54))/(1-a_m_50_54)))]
pers_h[, w_old := w]

pers_h[, p_m_55_59:= weighted.mean(D_m_55_59, w = w)]
pers_h[, a_m_55_59 := weighted.mean(DH_m_55_59, w = w)]
pers_h[, w := ifelse(DH_m_55_59 == 1, w_old*(PF_m_55_59/p_m_55_59), w_old*((1-a_m_55_59*(PF_m_55_59/p_m_55_59))/(1-a_m_55_59)))]
pers_h[, w_old := w]

pers_h[, p_m_60_64 := weighted.mean(D_m_60_64, w = w)]
pers_h[, a_m_60_64 := weighted.mean(DH_m_60_64, w = w)]
pers_h[, w := ifelse(DH_m_60_64 == 1, w_old*(PF_m_60_64/p_m_60_64), w_old*((1-a_m_60_64*(PF_m_60_64/p_m_60_64))/(1-a_m_60_64)))]
pers_h[, w_old := w]

pers_h[, p_m_65_74 := weighted.mean(D_m_65_74, w = w)]
pers_h[, a_m_65_74 := weighted.mean(DH_m_65_74, w = w)]
pers_h[, w := ifelse(DH_m_65_74 == 1, w_old*(PF_m_65_74/p_m_65_74), w_old*((1-a_m_65_74*(PF_m_65_74/p_m_65_74))/(1-a_m_65_74)))]
pers_h[, w_old := w]

pers_h[, p_m_over75 := weighted.mean(D_m_over75, w = w)]
pers_h[, a_m_over75:= weighted.mean(DH_m_over75, w = w)]
pers_h[, w := ifelse(DH_m_over75 == 1, w_old*(PF_m_over75/p_m_over75), w_old*((1-a_m_over75*(PF_m_over75/p_m_over75))/(1-a_m_over75)))]
pers_h[, w_old := w]

pers_h[, p_f_0_4 := weighted.mean(D_f_0_4, w = w)]
pers_h[, a_f_0_4 := weighted.mean(DH_f_0_4, w = w)]
pers_h[, w := ifelse(DH_f_0_4 == 1, w_old*(PF_f_0_4/p_f_0_4), w_old*((1-a_f_0_4*(PF_f_0_4/p_f_0_4))/(1-a_f_0_4)))]
pers_h[, w_old := w]

pers_h[, p_f_5_11 := weighted.mean(D_f_5_11, w = w)]
pers_h[, a_f_5_11 := weighted.mean(DH_f_5_11, w = w)]
pers_h[, w := ifelse(DH_f_5_11 == 1, w_old*(PF_f_5_11/p_f_5_11), w_old*((1-a_f_5_11*(PF_f_5_11/p_f_5_11))/(1-a_f_5_11)))]
pers_h[, w_old := w]

pers_h[, p_f_12_14 := weighted.mean(D_f_12_14, w = w)]
pers_h[, a_f_12_14 := weighted.mean(DH_f_12_14, w = w)]
pers_h[, w := ifelse(DH_f_12_14 == 1, w_old*(PF_f_12_14/p_f_12_14), w_old*((1-a_f_12_14*(PF_f_12_14/p_f_12_14))/(1-a_f_12_14)))]
pers_h[, w_old := w]

pers_h[, p_f_15_17 := weighted.mean(D_f_15_17, w = w)]
pers_h[, a_f_15_17 := weighted.mean(DH_f_15_17, w = w)]
pers_h[, w := ifelse(DH_f_15_17 == 1, w_old*(PF_f_15_17/p_f_15_17), w_old*((1-a_f_15_17*(PF_f_15_17/p_f_15_17))/(1-a_f_15_17)))]
pers_h[, w_old := w]

pers_h[, p_f_18_20 := weighted.mean(D_f_18_20, w = w)]
pers_h[, a_f_18_20 := weighted.mean(DH_f_18_20, w = w)]
pers_h[, w := ifelse(DH_f_18_20 == 1, w_old*(PF_f_18_20/p_f_18_20), w_old*((1-a_f_18_20*(PF_f_18_20/p_f_18_20))/(1-a_f_18_20)))]
pers_h[, w_old := w]

pers_h[, p_f_21_24 := weighted.mean(D_f_21_24, w = w)]
pers_h[, a_f_21_24 := weighted.mean(DH_f_21_24, w = w)]
pers_h[, w := ifelse(DH_f_21_24 == 1, w_old*(PF_f_21_24/p_f_21_24), w_old*((1-a_f_21_24*(PF_f_21_24/p_f_21_24))/(1-a_f_21_24)))]
pers_h[, w_old := w]

pers_h[, p_f_25_29 := weighted.mean(D_f_25_29, w = w)]
pers_h[, a_f_25_29 := weighted.mean(DH_f_25_29, w = w)]
pers_h[, w := ifelse(DH_f_25_29 == 1, w_old*(PF_f_25_29/p_f_25_29), w_old*((1-a_f_25_29*(PF_f_25_29/p_f_25_29))/(1-a_f_25_29)))]
pers_h[, w_old := w]

pers_h[, p_f_30_34 := weighted.mean(D_f_30_34, w = w)]
pers_h[, a_f_30_34 := weighted.mean(DH_f_30_34, w = w)]
pers_h[, w := ifelse(DH_f_30_34 == 1, w_old*(PF_f_30_34/p_f_30_34), w_old*((1-a_f_30_34*(PF_f_30_34/p_f_30_34))/(1-a_f_30_34)))]
pers_h[, w_old := w]

pers_h[, p_f_35_39 := weighted.mean(D_f_35_39, w = w)]
pers_h[, a_f_35_39:= weighted.mean(DH_f_35_39, w = w)]
pers_h[, w := ifelse(DH_f_35_39 == 1, w_old*(PF_f_35_39/p_f_35_39), w_old*((1-a_f_35_39*(PF_f_35_39/p_f_35_39))/(1-a_f_35_39)))]
pers_h[, w_old := w]

pers_h[, p_f_40_44 := weighted.mean(D_f_40_44, w = w)]
pers_h[, a_f_40_44 := weighted.mean(DH_f_40_44, w = w)]
pers_h[, w := ifelse(DH_f_40_44 == 1, w_old*(PF_f_40_44/p_f_40_44), w_old*((1-a_f_40_44*(PF_f_40_44/p_f_40_44))/(1-a_f_40_44)))]
pers_h[, w_old := w]

pers_h[, p_f_45_49 := weighted.mean(D_f_45_49, w = w)]
pers_h[, a_f_45_49 := weighted.mean(DH_f_45_49, w = w)]
pers_h[, w := ifelse(DH_f_45_49 == 1, w_old*(PF_f_45_49/p_f_45_49), w_old*((1-a_f_45_49*(PF_f_45_49/p_f_45_49))/(1-a_f_45_49)))]
pers_h[, w_old := w]

pers_h[, p_f_50_54 := weighted.mean(D_f_50_54, w = w)]
pers_h[, a_f_50_54 := weighted.mean(DH_f_50_54, w = w)]
pers_h[, w := ifelse(DH_f_50_54 == 1, w_old*(PF_f_50_54/p_f_50_54), w_old*((1-a_f_50_54*(PF_f_50_54/p_f_50_54))/(1-a_f_50_54)))]
pers_h[, w_old := w]

pers_h[, p_f_55_59:= weighted.mean(D_f_55_59, w = w)]
pers_h[, a_f_55_59 := weighted.mean(DH_f_55_59, w = w)]
pers_h[, w := ifelse(DH_f_55_59 == 1, w_old*(PF_f_55_59/p_f_55_59), w_old*((1-a_f_55_59*(PF_f_55_59/p_f_55_59))/(1-a_f_55_59)))]
pers_h[, w_old := w]

pers_h[, p_f_60_64 := weighted.mean(D_f_60_64, w = w)]
pers_h[, a_f_60_64 := weighted.mean(DH_f_60_64, w = w)]
pers_h[, w := ifelse(DH_f_60_64 == 1, w_old*(PF_f_60_64/p_f_60_64), w_old*((1-a_f_60_64*(PF_f_60_64/p_f_60_64))/(1-a_f_60_64)))]
pers_h[, w_old := w]

pers_h[, p_f_65_74 := weighted.mean(D_f_65_74, w = w)]
pers_h[, a_f_65_74 := weighted.mean(DH_f_65_74, w = w)]
pers_h[, w := ifelse(DH_f_65_74 == 1, w_old*(PF_f_65_74/p_f_65_74), w_old*((1-a_f_65_74*(PF_f_65_74/p_f_65_74))/(1-a_f_65_74)))]
pers_h[, w_old := w]

pers_h[, p_f_over75 := weighted.mean(D_f_over75, w = w)]
pers_h[, a_f_over75:= weighted.mean(DH_f_over75, w = w)]
pers_h[, w := ifelse(DH_f_over75 == 1, w_old*(PF_f_over75/p_f_over75), w_old*((1-a_f_over75*(PF_f_over75/p_f_over75))/(1-a_f_over75)))]
pers_h[, w_old := w]  



pers_h[, p_il_01 := weighted.mean(D_01, w = w)]
pers_h[, w := ifelse(D_01 == 1, w_old*(PF_il_01/p_il_01), w_old*((1-PF_il_01)/(1-p_il_01)))]
pers_h[, w_old := w]

pers_h[, p_il_02 := weighted.mean(D_02, w = w)]
pers_h[, w := ifelse(D_02 == 1, w_old*(PF_il_02/p_il_02), w_old*((1-PF_il_02)/(1-p_il_02)))]
pers_h[, w_old := w]

pers_h[, p_il_03 := weighted.mean(D_03, w = w)]
pers_h[, w := ifelse(D_03 == 1, w_old*(PF_il_03/p_il_03), w_old*((1-PF_il_03)/(1-p_il_03)))]
pers_h[, w_old := w]

pers_h[, p_il_04 := weighted.mean(D_04, w = w)]
pers_h[, w := ifelse(D_04 == 1, w_old*(PF_il_04/p_il_04), w_old*((1-PF_il_04)/(1-p_il_04)))]
pers_h[, w_old := w]

pers_h[, p_il_05 := weighted.mean(D_05, w = w)]
pers_h[, w := ifelse(D_05 == 1, w_old*(PF_il_05/p_il_05), w_old*((1-PF_il_05)/(1-p_il_05)))]
pers_h[, w_old := w]

pers_h[, p_il_06 := weighted.mean(D_06, w = w)]
pers_h[, w := ifelse(D_06 == 1, w_old*(PF_il_06/p_il_06), w_old*((1-PF_il_06)/(1-p_il_06)))]
pers_h[, w_old := w]

pers_h[, p_il_07 := weighted.mean(D_07, w = w)]
pers_h[, w := ifelse(D_07 == 1, w_old*(PF_il_07/p_il_07), w_old*((1-PF_il_07)/(1-p_il_07)))]
pers_h[, w_old := w]

pers_h[, p_il_08 := weighted.mean(D_08, w = w)]
pers_h[, w := ifelse(D_08 == 1, w_old*(PF_il_08/p_il_08), w_old*((1-PF_il_08)/(1-p_il_08)))]
pers_h[, w_old := w]

pers_h[, p_il_09 := weighted.mean(D_09, w = w)]
pers_h[, w := ifelse(D_09 == 1, w_old*(PF_il_09/p_il_09), w_old*((1-PF_il_09)/(1-p_il_09)))]
pers_h[, w_old := w]

pers_h[, p_il_10 := weighted.mean(D_10, w = w)]
pers_h[, w := ifelse(D_10 == 1, w_old*(PF_il_10/p_il_10), w_old*((1-PF_il_10)/(1-p_il_10)))]
pers_h[, w_old := w]

pers_h[, p_il_11 := weighted.mean(D_11, w = w)]
pers_h[, w := ifelse(D_11 == 1, w_old*(PF_il_11/p_il_11), w_old*((1-PF_il_11)/(1-p_il_11)))]
pers_h[, w_old := w]

pers_h[, p_il_12 := weighted.mean(D_12, w = w)]
pers_h[, w := ifelse(D_12 == 1, w_old*(PF_il_12/p_il_12), w_old*((1-PF_il_12)/(1-p_il_12)))]
pers_h[, w_old := w]

pers_h[, p_il_13 := weighted.mean(D_13, w = w)]
pers_h[, w := ifelse(D_13 == 1, w_old*(PF_il_13/p_il_13), w_old*((1-PF_il_13)/(1-p_il_13)))]
pers_h[, w_old := w]

pers_h[, p_il_14 := weighted.mean(D_14, w = w)]
pers_h[, w := ifelse(D_14 == 1, w_old*(PF_il_14/p_il_14), w_old*((1-PF_il_14)/(1-p_il_14)))]
pers_h[, w_old := w]

pers_h[, p_il_15 := weighted.mean(D_15, w = w)]
pers_h[, w := ifelse(D_15 == 1, w_old*(PF_il_15/p_il_15), w_old*((1-PF_il_15)/(1-p_il_15)))]
pers_h[, w_old := w]

pers_h[, p_il_16 := weighted.mean(D_16, w = w)]
pers_h[, w := ifelse(D_16 == 1, w_old*(PF_il_16/p_il_16), w_old*((1-PF_il_16)/(1-p_il_16)))]
pers_h[, w_old := w]

pers_h[, p_il_17 := weighted.mean(D_17, w = w)]
pers_h[, w := ifelse(D_17 == 1, w_old*(PF_il_17/p_il_17), w_old*((1-PF_il_17)/(1-p_il_17)))]
pers_h[, w_old := w]

pers_h[, p_il_18 := weighted.mean(D_18, w = w)]
pers_h[, w := ifelse(D_18 == 1, w_old*(PF_il_18/p_il_18), w_old*((1-PF_il_18)/(1-p_il_18)))]
pers_h[, w_old := w]

pers_h[, p_il_19 := weighted.mean(D_19, w = w)]
pers_h[, w := ifelse(D_19 == 1, w_old*(PF_il_19/p_il_19), w_old*((1-PF_il_19)/(1-p_il_19)))]
pers_h[, w_old := w]

pers_h[, p_il_20 := weighted.mean(D_20, w = w)]
pers_h[, w := ifelse(D_20 == 1, w_old*(PF_il_20/p_il_20), w_old*((1-PF_il_20)/(1-p_il_20)))]
pers_h[, w_old := w]

pers_h[, p_il_21 := weighted.mean(D_21, w = w)]
pers_h[, w := ifelse(D_21 == 1, w_old*(PF_il_21/p_il_21), w_old*((1-PF_il_21)/(1-p_il_21)))]
pers_h[, w_old := w]

pers_h[, p_il_22 := weighted.mean(D_22, w = w)]
pers_h[, w := ifelse(D_22 == 1, w_old*(PF_il_22/p_il_22), w_old*((1-PF_il_22)/(1-p_il_22)))]
pers_h[, w_old := w]

pers_h[, p_il_23 := weighted.mean(D_23, w = w)]
pers_h[, w := ifelse(D_23 == 1, w_old*(PF_il_23/p_il_23), w_old*((1-PF_il_23)/(1-p_il_23)))]
pers_h[, w_old := w]

pers_h[, p_il_24 := weighted.mean(D_24, w = w)]
pers_h[, w := ifelse(D_24 == 1, w_old*(PF_il_24/p_il_24), w_old*((1-PF_il_24)/(1-p_il_24)))]
pers_h[, w_old := w]

pers_h[, p_il_25 := weighted.mean(D_25, w = w)]
pers_h[, w := ifelse(D_25 == 1, w_old*(PF_il_25/p_il_25), w_old*((1-PF_il_25)/(1-p_il_25)))]
pers_h[, w_old := w]

pers_h[, p_il_26 := weighted.mean(D_26, w = w)]
pers_h[, w := ifelse(D_26 == 1, w_old*(PF_il_26/p_il_26), w_old*((1-PF_il_26)/(1-p_il_26)))]
pers_h[, w_old := w]

pers_h[, p_il_27 := weighted.mean(D_27, w = w)]
pers_h[, w := ifelse(D_27 == 1, w_old*(PF_il_27/p_il_27), w_old*((1-PF_il_27)/(1-p_il_27)))]
pers_h[, w_old := w]

pers_h[, p_il_28 := weighted.mean(D_28, w = w)]
pers_h[, w := ifelse(D_28 == 1, w_old*(PF_il_28/p_il_28), w_old*((1-PF_il_28)/(1-p_il_28)))]
pers_h[, w_old := w]

pers_h[, p_il_29 := weighted.mean(D_29, w = w)]
pers_h[, w := ifelse(D_29 == 1, w_old*(PF_il_29/p_il_29), w_old*((1-PF_il_29)/(1-p_il_29)))]
pers_h[, w_old := w]

pers_h[, p_il_30 := weighted.mean(D_30, w = w)]
pers_h[, w := ifelse(D_30 == 1, w_old*(PF_il_30/p_il_30), w_old*((1-PF_il_30)/(1-p_il_30)))]
pers_h[, w_old := w]

pers_h[, p_il_31 := weighted.mean(D_31, w = w)]
pers_h[, w := ifelse(D_31 == 1, w_old*(PF_il_31/p_il_31), w_old*((1-PF_il_31)/(1-p_il_31)))]
pers_h[, w_old := w]

pers_h[, p_il_32 := weighted.mean(D_32, w = w)]
pers_h[, w := ifelse(D_32 == 1, w_old*(PF_il_32/p_il_32), w_old*((1-PF_il_32)/(1-p_il_32)))]
pers_h[, w_old := w]

pers_h[, p_il_33 := weighted.mean(D_33, w = w)]
pers_h[, w := ifelse(D_33 == 1, w_old*(PF_il_33/p_il_33), w_old*((1-PF_il_33)/(1-p_il_33)))]
pers_h[, w_old := w]

pers_h[, p_il_34 := weighted.mean(D_34, w = w)]
pers_h[, w := ifelse(D_34 == 1, w_old*(PF_il_34/p_il_34), w_old*((1-PF_il_34)/(1-p_il_34)))]
pers_h[, w_old := w]

pers_h[, p_il_35 := weighted.mean(D_35, w = w)]
pers_h[, w := ifelse(D_35 == 1, w_old*(PF_il_35/p_il_35), w_old*((1-PF_il_35)/(1-p_il_35)))]
pers_h[, w_old := w]

pers_h[, p_il_36 := weighted.mean(D_36, w = w)]
pers_h[, w := ifelse(D_36 == 1, w_old*(PF_il_36/p_il_36), w_old*((1-PF_il_36)/(1-p_il_36)))]
pers_h[, w_old := w]

pers_h[, p_il_37 := weighted.mean(D_37, w = w)]
pers_h[, w := ifelse(D_37 == 1, w_old*(PF_il_37/p_il_37), w_old*((1-PF_il_37)/(1-p_il_37)))]
pers_h[, w_old := w]

pers_h[, p_il_38 := weighted.mean(D_38, w = w)]
pers_h[, w := ifelse(D_38 == 1, w_old*(PF_il_38/p_il_38), w_old*((1-PF_il_38)/(1-p_il_38)))]
pers_h[, w_old := w]

pers_h[, p_il_39 := weighted.mean(D_39, w = w)]
pers_h[, w := ifelse(D_39 == 1, w_old*(PF_il_39/p_il_39), w_old*((1-PF_il_39)/(1-p_il_39)))]
pers_h[, w_old := w]

pers_h[, p_il_40 := weighted.mean(D_40, w = w)]
pers_h[, w := ifelse(D_40 == 1, w_old*(PF_il_40/p_il_40), w_old*((1-PF_il_40)/(1-p_il_40)))]
pers_h[, w_old := w]

pers_h[, p_il_41 := weighted.mean(D_41, w = w)]
pers_h[, w := ifelse(D_41 == 1, w_old*(PF_il_41/p_il_41), w_old*((1-PF_il_41)/(1-p_il_41)))]
pers_h[, w_old := w]

pers_h[, p_il_42 := weighted.mean(D_42, w = w)]
pers_h[, w := ifelse(D_42 == 1, w_old*(PF_il_42/p_il_42), w_old*((1-PF_il_42)/(1-p_il_42)))]
pers_h[, w_old := w]

pers_h[, p_il_43 := weighted.mean(D_43, w = w)]
pers_h[, w := ifelse(D_43 == 1, w_old*(PF_il_43/p_il_43), w_old*((1-PF_il_43)/(1-p_il_43)))]
pers_h[, w_old := w]

pers_h[, p_il_44 := weighted.mean(D_44, w = w)]
pers_h[, w := ifelse(D_44 == 1, w_old*(PF_il_44/p_il_44), w_old*((1-PF_il_44)/(1-p_il_44)))]
pers_h[, w_old := w]

pers_h[, p_il_45 := weighted.mean(D_45, w = w)]
pers_h[, w := ifelse(D_45 == 1, w_old*(PF_il_45/p_il_45), w_old*((1-PF_il_45)/(1-p_il_45)))]
pers_h[, w_old := w]

pers_h[, p_il_46 := weighted.mean(D_46, w = w)]
pers_h[, w := ifelse(D_46 == 1, w_old*(PF_il_46/p_il_46), w_old*((1-PF_il_46)/(1-p_il_46)))]
pers_h[, w_old := w]

pers_h[, p_il_47 := weighted.mean(D_47, w = w)]
pers_h[, w := ifelse(D_47 == 1, w_old*(PF_il_47/p_il_47), w_old*((1-PF_il_47)/(1-p_il_47)))]
pers_h[, w_old := w]

pers_h[, p_il_48 := weighted.mean(D_48, w = w)]
pers_h[, w := ifelse(D_48 == 1, w_old*(PF_il_48/p_il_48), w_old*((1-PF_il_48)/(1-p_il_48)))]
pers_h[, w_old := w]

pers_h[, p_il_49 := weighted.mean(D_49, w = w)]
pers_h[, w := ifelse(D_49 == 1, w_old*(PF_il_49/p_il_49), w_old*((1-PF_il_49)/(1-p_il_49)))]
pers_h[, w_old := w]

pers_h[, p_il_50 := weighted.mean(D_50, w = w)]
pers_h[, w := ifelse(D_50 == 1, w_old*(PF_il_50/p_il_50), w_old*((1-PF_il_50)/(1-p_il_50)))]
pers_h[, w_old := w]

pers_h[, p_il_51 := weighted.mean(D_51, w = w)]
pers_h[, w := ifelse(D_51 == 1, w_old*(PF_il_51/p_il_51), w_old*((1-PF_il_51)/(1-p_il_51)))]
pers_h[, w_old := w]

pers_h[, p_il_52 := weighted.mean(D_52, w = w)]
pers_h[, w := ifelse(D_52 == 1, w_old*(PF_il_52/p_il_52), w_old*((1-PF_il_52)/(1-p_il_52)))]
pers_h[, w_old := w]

pers_h[, p_il_53 := weighted.mean(D_53, w = w)]
pers_h[, w := ifelse(D_53 == 1, w_old*(PF_il_53/p_il_53), w_old*((1-PF_il_53)/(1-p_il_53)))]
pers_h[, w_old := w]

pers_h[, p_il_54 := weighted.mean(D_54, w = w)]
pers_h[, w := ifelse(D_54 == 1, w_old*(PF_il_54/p_il_54), w_old*((1-PF_il_54)/(1-p_il_54)))]
pers_h[, w_old := w]

pers_h[, p_il_55 := weighted.mean(D_55, w = w)]
pers_h[, w := ifelse(D_55 == 1, w_old*(PF_il_55/p_il_55), w_old*((1-PF_il_55)/(1-p_il_55)))]
pers_h[, w_old := w]

pers_h[, p_il_56 := weighted.mean(D_56, w = w)]
pers_h[, w := ifelse(D_56 == 1, w_old*(PF_il_56/p_il_56), w_old*((1-PF_il_56)/(1-p_il_56)))]
pers_h[, w_old := w]

pers_h[, p_il_57 := weighted.mean(D_57, w = w)]
pers_h[, w := ifelse(D_57 == 1, w_old*(PF_il_57/p_il_57), w_old*((1-PF_il_57)/(1-p_il_57)))]
pers_h[, w_old := w]

pers_h[, p_il_58 := weighted.mean(D_58, w = w)]
pers_h[, w := ifelse(D_58 == 1, w_old*(PF_il_58/p_il_58), w_old*((1-PF_il_58)/(1-p_il_58)))]
pers_h[, w_old := w]

pers_h[, p_il_59 := weighted.mean(D_59, w = w)]
pers_h[, w := ifelse(D_59 == 1, w_old*(PF_il_59/p_il_59), w_old*((1-PF_il_59)/(1-p_il_59)))]
pers_h[, w_old := w]

pers_h[, p_il_60 := weighted.mean(D_60, w = w)]
pers_h[, w := ifelse(D_60 == 1, w_old*(PF_il_60/p_il_60), w_old*((1-PF_il_60)/(1-p_il_60)))]
pers_h[, w_old := w]

pers_h[, p_il_61 := weighted.mean(D_61, w = w)]
pers_h[, w := ifelse(D_61 == 1, w_old*(PF_il_61/p_il_61), w_old*((1-PF_il_61)/(1-p_il_61)))]
pers_h[, w_old := w]

pers_h[, p_il_62 := weighted.mean(D_62, w = w)]
pers_h[, w := ifelse(D_62 == 1, w_old*(PF_il_62/p_il_62), w_old*((1-PF_il_62)/(1-p_il_62)))]
pers_h[, w_old := w]

pers_h[, p_il_63 := weighted.mean(D_63, w = w)]
pers_h[, w := ifelse(D_63 == 1, w_old*(PF_il_63/p_il_63), w_old*((1-PF_il_63)/(1-p_il_63)))]
pers_h[, w_old := w]

pers_h[, p_il_64 := weighted.mean(D_64, w = w)]
pers_h[, w := ifelse(D_64 == 1, w_old*(PF_il_64/p_il_64), w_old*((1-PF_il_64)/(1-p_il_64)))]
pers_h[, w_old := w]

pers_h[, p_il_65 := weighted.mean(D_65, w = w)]
pers_h[, w := ifelse(D_65 == 1, w_old*(PF_il_65/p_il_65), w_old*((1-PF_il_65)/(1-p_il_65)))]
pers_h[, w_old := w]

pers_h[, p_il_66 := weighted.mean(D_66, w = w)]
pers_h[, w := ifelse(D_66 == 1, w_old*(PF_il_66/p_il_66), w_old*((1-PF_il_66)/(1-p_il_66)))]
pers_h[, w_old := w]

pers_h[, p_il_67 := weighted.mean(D_67, w = w)]
pers_h[, w := ifelse(D_67 == 1, w_old*(PF_il_67/p_il_67), w_old*((1-PF_il_67)/(1-p_il_67)))]
pers_h[, w_old := w]

pers_h[, p_il_68 := weighted.mean(D_68, w = w)]
pers_h[, w := ifelse(D_68 == 1, w_old*(PF_il_68/p_il_68), w_old*((1-PF_il_68)/(1-p_il_68)))]
pers_h[, w_old := w]

pers_h[, p_il_69 := weighted.mean(D_69, w = w)]
pers_h[, w := ifelse(D_69 == 1, w_old*(PF_il_69/p_il_69), w_old*((1-PF_il_69)/(1-p_il_69)))]
pers_h[, w_old := w]

pers_h[, p_il_70 := weighted.mean(D_70, w = w)]
pers_h[, w := ifelse(D_70 == 1, w_old*(PF_il_70/p_il_70), w_old*((1-PF_il_70)/(1-p_il_70)))]
pers_h[, w_old := w]

pers_h[, p_il_71 := weighted.mean(D_71, w = w)]
pers_h[, w := ifelse(D_71 == 1, w_old*(PF_il_71/p_il_71), w_old*((1-PF_il_71)/(1-p_il_71)))]
pers_h[, w_old := w]

pers_h[, p_il_72 := weighted.mean(D_72, w = w)]
pers_h[, w := ifelse(D_72 == 1, w_old*(PF_il_72/p_il_72), w_old*((1-PF_il_72)/(1-p_il_72)))]
pers_h[, w_old := w]

pers_h[, p_il_73 := weighted.mean(D_73, w = w)]
pers_h[, w := ifelse(D_73 == 1, w_old*(PF_il_73/p_il_73), w_old*((1-PF_il_73)/(1-p_il_73)))]
pers_h[, w_old := w]

pers_h[, p_il_74 := weighted.mean(D_74, w = w)]
pers_h[, w := ifelse(D_74 == 1, w_old*(PF_il_74/p_il_74), w_old*((1-PF_il_74)/(1-p_il_74)))]
pers_h[, w_old := w]

pers_h[, p_il_75 := weighted.mean(D_75, w = w)]
pers_h[, w := ifelse(D_75 == 1, w_old*(PF_il_75/p_il_75), w_old*((1-PF_il_75)/(1-p_il_75)))]
pers_h[, w_old := w]

pers_h[, p_il_76 := weighted.mean(D_76, w = w)]
pers_h[, w := ifelse(D_76 == 1, w_old*(PF_il_76/p_il_76), w_old*((1-PF_il_76)/(1-p_il_76)))]
pers_h[, w_old := w]

pers_h[, p_il_77 := weighted.mean(D_77, w = w)]
pers_h[, w := ifelse(D_77 == 1, w_old*(PF_il_77/p_il_77), w_old*((1-PF_il_77)/(1-p_il_77)))]
pers_h[, w_old := w]

pers_h[, p_il_78 := weighted.mean(D_78, w = w)]
pers_h[, w := ifelse(D_78 == 1, w_old*(PF_il_78/p_il_78), w_old*((1-PF_il_78)/(1-p_il_78)))]
pers_h[, w_old := w]

pers_h[, p_il_79 := weighted.mean(D_79, w = w)]
pers_h[, w := ifelse(D_79 == 1, w_old*(PF_il_79/p_il_79), w_old*((1-PF_il_79)/(1-p_il_79)))]
pers_h[, w_old := w]

pers_h[, p_il_80 := weighted.mean(D_80, w = w)]
pers_h[, w := ifelse(D_80 == 1, w_old*(PF_il_80/p_il_80), w_old*((1-PF_il_80)/(1-p_il_80)))]
pers_h[, w_old := w]

pers_h[, p_il_81 := weighted.mean(D_81, w = w)]
pers_h[, w := ifelse(D_81 == 1, w_old*(PF_il_81/p_il_81), w_old*((1-PF_il_81)/(1-p_il_81)))]
pers_h[, w_old := w]
pers_h <- pers_h[, -c(167:247)]

weights_ind <- pers_h[,c("fk_fkiml", "w", "w_old")]
pers_h2 <- pers_h[,-c(347:491)]

for (i in 1:ana.iter){
  
  setkey(weights_ind, fk_fkiml)
  setkey(pers_h2, fk_fkiml)
  pers_h2 <- weights_ind[pers_h2, nomatch = 0]
  pers_h2[, i.w := NULL]
  pers_h2[, i.w_old := NULL]
  
  for (j in 1:alt.iter){
    print(paste0("Ana iterasyon: ",i," alt iterasyon:",j))
    pers_h2[, p_m_0_4 := weighted.mean(D_m_0_4, w = w)]
    pers_h2[, a_m_0_4 := weighted.mean(DH_m_0_4, w = w)]
    pers_h2[, w := ifelse(DH_m_0_4 == 1, w_old*(PF_m_0_4/p_m_0_4), w_old*((1-a_m_0_4*(PF_m_0_4/p_m_0_4))/(1-a_m_0_4)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_5_11 := weighted.mean(D_m_5_11, w = w)]
    pers_h2[, a_m_5_11 := weighted.mean(DH_m_5_11, w = w)]
    pers_h2[, w := ifelse(DH_m_5_11 == 1, w_old*(PF_m_5_11/p_m_5_11), w_old*((1-a_m_5_11*(PF_m_5_11/p_m_5_11))/(1-a_m_5_11)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_12_14 := weighted.mean(D_m_12_14, w = w)]
    pers_h2[, a_m_12_14 := weighted.mean(DH_m_12_14, w = w)]
    pers_h2[, w := ifelse(DH_m_12_14 == 1, w_old*(PF_m_12_14/p_m_12_14), w_old*((1-a_m_12_14*(PF_m_12_14/p_m_12_14))/(1-a_m_12_14)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_15_17 := weighted.mean(D_m_15_17, w = w)]
    pers_h2[, a_m_15_17 := weighted.mean(DH_m_15_17, w = w)]
    pers_h2[, w := ifelse(DH_m_15_17 == 1, w_old*(PF_m_15_17/p_m_15_17), w_old*((1-a_m_15_17*(PF_m_15_17/p_m_15_17))/(1-a_m_15_17)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_18_20 := weighted.mean(D_m_18_20, w = w)]
    pers_h2[, a_m_18_20 := weighted.mean(DH_m_18_20, w = w)]
    pers_h2[, w := ifelse(DH_m_18_20 == 1, w_old*(PF_m_18_20/p_m_18_20), w_old*((1-a_m_18_20*(PF_m_18_20/p_m_18_20))/(1-a_m_18_20)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_21_24 := weighted.mean(D_m_21_24, w = w)]
    pers_h2[, a_m_21_24 := weighted.mean(DH_m_21_24, w = w)]
    pers_h2[, w := ifelse(DH_m_21_24 == 1, w_old*(PF_m_21_24/p_m_21_24), w_old*((1-a_m_21_24*(PF_m_21_24/p_m_21_24))/(1-a_m_21_24)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_25_29 := weighted.mean(D_m_25_29, w = w)]
    pers_h2[, a_m_25_29 := weighted.mean(DH_m_25_29, w = w)]
    pers_h2[, w := ifelse(DH_m_25_29 == 1, w_old*(PF_m_25_29/p_m_25_29), w_old*((1-a_m_25_29*(PF_m_25_29/p_m_25_29))/(1-a_m_25_29)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_30_34 := weighted.mean(D_m_30_34, w = w)]
    pers_h2[, a_m_30_34 := weighted.mean(DH_m_30_34, w = w)]
    pers_h2[, w := ifelse(DH_m_30_34 == 1, w_old*(PF_m_30_34/p_m_30_34), w_old*((1-a_m_30_34*(PF_m_30_34/p_m_30_34))/(1-a_m_30_34)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_35_39 := weighted.mean(D_m_35_39, w = w)]
    pers_h2[, a_m_35_39:= weighted.mean(DH_m_35_39, w = w)]
    pers_h2[, w := ifelse(DH_m_35_39 == 1, w_old*(PF_m_35_39/p_m_35_39), w_old*((1-a_m_35_39*(PF_m_35_39/p_m_35_39))/(1-a_m_35_39)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_40_44 := weighted.mean(D_m_40_44, w = w)]
    pers_h2[, a_m_40_44 := weighted.mean(DH_m_40_44, w = w)]
    pers_h2[, w := ifelse(DH_m_40_44 == 1, w_old*(PF_m_40_44/p_m_40_44), w_old*((1-a_m_40_44*(PF_m_40_44/p_m_40_44))/(1-a_m_40_44)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_45_49 := weighted.mean(D_m_45_49, w = w)]
    pers_h2[, a_m_45_49 := weighted.mean(DH_m_45_49, w = w)]
    pers_h2[, w := ifelse(DH_m_45_49 == 1, w_old*(PF_m_45_49/p_m_45_49), w_old*((1-a_m_45_49*(PF_m_45_49/p_m_45_49))/(1-a_m_45_49)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_50_54 := weighted.mean(D_m_50_54, w = w)]
    pers_h2[, a_m_50_54 := weighted.mean(DH_m_50_54, w = w)]
    pers_h2[, w := ifelse(DH_m_50_54 == 1, w_old*(PF_m_50_54/p_m_50_54), w_old*((1-a_m_50_54*(PF_m_50_54/p_m_50_54))/(1-a_m_50_54)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_55_59:= weighted.mean(D_m_55_59, w = w)]
    pers_h2[, a_m_55_59 := weighted.mean(DH_m_55_59, w = w)]
    pers_h2[, w := ifelse(DH_m_55_59 == 1, w_old*(PF_m_55_59/p_m_55_59), w_old*((1-a_m_55_59*(PF_m_55_59/p_m_55_59))/(1-a_m_55_59)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_60_64 := weighted.mean(D_m_60_64, w = w)]
    pers_h2[, a_m_60_64 := weighted.mean(DH_m_60_64, w = w)]
    pers_h2[, w := ifelse(DH_m_60_64 == 1, w_old*(PF_m_60_64/p_m_60_64), w_old*((1-a_m_60_64*(PF_m_60_64/p_m_60_64))/(1-a_m_60_64)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_65_74 := weighted.mean(D_m_65_74, w = w)]
    pers_h2[, a_m_65_74 := weighted.mean(DH_m_65_74, w = w)]
    pers_h2[, w := ifelse(DH_m_65_74 == 1, w_old*(PF_m_65_74/p_m_65_74), w_old*((1-a_m_65_74*(PF_m_65_74/p_m_65_74))/(1-a_m_65_74)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_m_over75 := weighted.mean(D_m_over75, w = w)]
    pers_h2[, a_m_over75:= weighted.mean(DH_m_over75, w = w)]
    pers_h2[, w := ifelse(DH_m_over75 == 1, w_old*(PF_m_over75/p_m_over75), w_old*((1-a_m_over75*(PF_m_over75/p_m_over75))/(1-a_m_over75)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_0_4 := weighted.mean(D_f_0_4, w = w)]
    pers_h2[, a_f_0_4 := weighted.mean(DH_f_0_4, w = w)]
    pers_h2[, w := ifelse(DH_f_0_4 == 1, w_old*(PF_f_0_4/p_f_0_4), w_old*((1-a_f_0_4*(PF_f_0_4/p_f_0_4))/(1-a_f_0_4)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_5_11 := weighted.mean(D_f_5_11, w = w)]
    pers_h2[, a_f_5_11 := weighted.mean(DH_f_5_11, w = w)]
    pers_h2[, w := ifelse(DH_f_5_11 == 1, w_old*(PF_f_5_11/p_f_5_11), w_old*((1-a_f_5_11*(PF_f_5_11/p_f_5_11))/(1-a_f_5_11)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_12_14 := weighted.mean(D_f_12_14, w = w)]
    pers_h2[, a_f_12_14 := weighted.mean(DH_f_12_14, w = w)]
    pers_h2[, w := ifelse(DH_f_12_14 == 1, w_old*(PF_f_12_14/p_f_12_14), w_old*((1-a_f_12_14*(PF_f_12_14/p_f_12_14))/(1-a_f_12_14)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_15_17 := weighted.mean(D_f_15_17, w = w)]
    pers_h2[, a_f_15_17 := weighted.mean(DH_f_15_17, w = w)]
    pers_h2[, w := ifelse(DH_f_15_17 == 1, w_old*(PF_f_15_17/p_f_15_17), w_old*((1-a_f_15_17*(PF_f_15_17/p_f_15_17))/(1-a_f_15_17)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_18_20 := weighted.mean(D_f_18_20, w = w)]
    pers_h2[, a_f_18_20 := weighted.mean(DH_f_18_20, w = w)]
    pers_h2[, w := ifelse(DH_f_18_20 == 1, w_old*(PF_f_18_20/p_f_18_20), w_old*((1-a_f_18_20*(PF_f_18_20/p_f_18_20))/(1-a_f_18_20)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_21_24 := weighted.mean(D_f_21_24, w = w)]
    pers_h2[, a_f_21_24 := weighted.mean(DH_f_21_24, w = w)]
    pers_h2[, w := ifelse(DH_f_21_24 == 1, w_old*(PF_f_21_24/p_f_21_24), w_old*((1-a_f_21_24*(PF_f_21_24/p_f_21_24))/(1-a_f_21_24)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_25_29 := weighted.mean(D_f_25_29, w = w)]
    pers_h2[, a_f_25_29 := weighted.mean(DH_f_25_29, w = w)]
    pers_h2[, w := ifelse(DH_f_25_29 == 1, w_old*(PF_f_25_29/p_f_25_29), w_old*((1-a_f_25_29*(PF_f_25_29/p_f_25_29))/(1-a_f_25_29)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_30_34 := weighted.mean(D_f_30_34, w = w)]
    pers_h2[, a_f_30_34 := weighted.mean(DH_f_30_34, w = w)]
    pers_h2[, w := ifelse(DH_f_30_34 == 1, w_old*(PF_f_30_34/p_f_30_34), w_old*((1-a_f_30_34*(PF_f_30_34/p_f_30_34))/(1-a_f_30_34)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_35_39 := weighted.mean(D_f_35_39, w = w)]
    pers_h2[, a_f_35_39:= weighted.mean(DH_f_35_39, w = w)]
    pers_h2[, w := ifelse(DH_f_35_39 == 1, w_old*(PF_f_35_39/p_f_35_39), w_old*((1-a_f_35_39*(PF_f_35_39/p_f_35_39))/(1-a_f_35_39)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_40_44 := weighted.mean(D_f_40_44, w = w)]
    pers_h2[, a_f_40_44 := weighted.mean(DH_f_40_44, w = w)]
    pers_h2[, w := ifelse(DH_f_40_44 == 1, w_old*(PF_f_40_44/p_f_40_44), w_old*((1-a_f_40_44*(PF_f_40_44/p_f_40_44))/(1-a_f_40_44)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_45_49 := weighted.mean(D_f_45_49, w = w)]
    pers_h2[, a_f_45_49 := weighted.mean(DH_f_45_49, w = w)]
    pers_h2[, w := ifelse(DH_f_45_49 == 1, w_old*(PF_f_45_49/p_f_45_49), w_old*((1-a_f_45_49*(PF_f_45_49/p_f_45_49))/(1-a_f_45_49)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_50_54 := weighted.mean(D_f_50_54, w = w)]
    pers_h2[, a_f_50_54 := weighted.mean(DH_f_50_54, w = w)]
    pers_h2[, w := ifelse(DH_f_50_54 == 1, w_old*(PF_f_50_54/p_f_50_54), w_old*((1-a_f_50_54*(PF_f_50_54/p_f_50_54))/(1-a_f_50_54)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_55_59:= weighted.mean(D_f_55_59, w = w)]
    pers_h2[, a_f_55_59 := weighted.mean(DH_f_55_59, w = w)]
    pers_h2[, w := ifelse(DH_f_55_59 == 1, w_old*(PF_f_55_59/p_f_55_59), w_old*((1-a_f_55_59*(PF_f_55_59/p_f_55_59))/(1-a_f_55_59)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_60_64 := weighted.mean(D_f_60_64, w = w)]
    pers_h2[, a_f_60_64 := weighted.mean(DH_f_60_64, w = w)]
    pers_h2[, w := ifelse(DH_f_60_64 == 1, w_old*(PF_f_60_64/p_f_60_64), w_old*((1-a_f_60_64*(PF_f_60_64/p_f_60_64))/(1-a_f_60_64)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_65_74 := weighted.mean(D_f_65_74, w = w)]
    pers_h2[, a_f_65_74 := weighted.mean(DH_f_65_74, w = w)]
    pers_h2[, w := ifelse(DH_f_65_74 == 1, w_old*(PF_f_65_74/p_f_65_74), w_old*((1-a_f_65_74*(PF_f_65_74/p_f_65_74))/(1-a_f_65_74)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_f_over75 := weighted.mean(D_f_over75, w = w)]
    pers_h2[, a_f_over75:= weighted.mean(DH_f_over75, w = w)]
    pers_h2[, w := ifelse(DH_f_over75 == 1, w_old*(PF_f_over75/p_f_over75), w_old*((1-a_f_over75*(PF_f_over75/p_f_over75))/(1-a_f_over75)))]
    pers_h2[, w_old := w]  
    
    
    
    pers_h2[, p_il_01 := weighted.mean(D_01, w = w)]
    pers_h2[, w := ifelse(D_01 == 1, w_old*(PF_il_01/p_il_01), w_old*((1-PF_il_01)/(1-p_il_01)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_02 := weighted.mean(D_02, w = w)]
    pers_h2[, w := ifelse(D_02 == 1, w_old*(PF_il_02/p_il_02), w_old*((1-PF_il_02)/(1-p_il_02)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_03 := weighted.mean(D_03, w = w)]
    pers_h2[, w := ifelse(D_03 == 1, w_old*(PF_il_03/p_il_03), w_old*((1-PF_il_03)/(1-p_il_03)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_04 := weighted.mean(D_04, w = w)]
    pers_h2[, w := ifelse(D_04 == 1, w_old*(PF_il_04/p_il_04), w_old*((1-PF_il_04)/(1-p_il_04)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_05 := weighted.mean(D_05, w = w)]
    pers_h2[, w := ifelse(D_05 == 1, w_old*(PF_il_05/p_il_05), w_old*((1-PF_il_05)/(1-p_il_05)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_06 := weighted.mean(D_06, w = w)]
    pers_h2[, w := ifelse(D_06 == 1, w_old*(PF_il_06/p_il_06), w_old*((1-PF_il_06)/(1-p_il_06)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_07 := weighted.mean(D_07, w = w)]
    pers_h2[, w := ifelse(D_07 == 1, w_old*(PF_il_07/p_il_07), w_old*((1-PF_il_07)/(1-p_il_07)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_08 := weighted.mean(D_08, w = w)]
    pers_h2[, w := ifelse(D_08 == 1, w_old*(PF_il_08/p_il_08), w_old*((1-PF_il_08)/(1-p_il_08)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_09 := weighted.mean(D_09, w = w)]
    pers_h2[, w := ifelse(D_09 == 1, w_old*(PF_il_09/p_il_09), w_old*((1-PF_il_09)/(1-p_il_09)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_10 := weighted.mean(D_10, w = w)]
    pers_h2[, w := ifelse(D_10 == 1, w_old*(PF_il_10/p_il_10), w_old*((1-PF_il_10)/(1-p_il_10)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_11 := weighted.mean(D_11, w = w)]
    pers_h2[, w := ifelse(D_11 == 1, w_old*(PF_il_11/p_il_11), w_old*((1-PF_il_11)/(1-p_il_11)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_12 := weighted.mean(D_12, w = w)]
    pers_h2[, w := ifelse(D_12 == 1, w_old*(PF_il_12/p_il_12), w_old*((1-PF_il_12)/(1-p_il_12)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_13 := weighted.mean(D_13, w = w)]
    pers_h2[, w := ifelse(D_13 == 1, w_old*(PF_il_13/p_il_13), w_old*((1-PF_il_13)/(1-p_il_13)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_14 := weighted.mean(D_14, w = w)]
    pers_h2[, w := ifelse(D_14 == 1, w_old*(PF_il_14/p_il_14), w_old*((1-PF_il_14)/(1-p_il_14)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_15 := weighted.mean(D_15, w = w)]
    pers_h2[, w := ifelse(D_15 == 1, w_old*(PF_il_15/p_il_15), w_old*((1-PF_il_15)/(1-p_il_15)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_16 := weighted.mean(D_16, w = w)]
    pers_h2[, w := ifelse(D_16 == 1, w_old*(PF_il_16/p_il_16), w_old*((1-PF_il_16)/(1-p_il_16)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_17 := weighted.mean(D_17, w = w)]
    pers_h2[, w := ifelse(D_17 == 1, w_old*(PF_il_17/p_il_17), w_old*((1-PF_il_17)/(1-p_il_17)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_18 := weighted.mean(D_18, w = w)]
    pers_h2[, w := ifelse(D_18 == 1, w_old*(PF_il_18/p_il_18), w_old*((1-PF_il_18)/(1-p_il_18)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_19 := weighted.mean(D_19, w = w)]
    pers_h2[, w := ifelse(D_19 == 1, w_old*(PF_il_19/p_il_19), w_old*((1-PF_il_19)/(1-p_il_19)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_20 := weighted.mean(D_20, w = w)]
    pers_h2[, w := ifelse(D_20 == 1, w_old*(PF_il_20/p_il_20), w_old*((1-PF_il_20)/(1-p_il_20)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_21 := weighted.mean(D_21, w = w)]
    pers_h2[, w := ifelse(D_21 == 1, w_old*(PF_il_21/p_il_21), w_old*((1-PF_il_21)/(1-p_il_21)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_22 := weighted.mean(D_22, w = w)]
    pers_h2[, w := ifelse(D_22 == 1, w_old*(PF_il_22/p_il_22), w_old*((1-PF_il_22)/(1-p_il_22)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_23 := weighted.mean(D_23, w = w)]
    pers_h2[, w := ifelse(D_23 == 1, w_old*(PF_il_23/p_il_23), w_old*((1-PF_il_23)/(1-p_il_23)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_24 := weighted.mean(D_24, w = w)]
    pers_h2[, w := ifelse(D_24 == 1, w_old*(PF_il_24/p_il_24), w_old*((1-PF_il_24)/(1-p_il_24)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_25 := weighted.mean(D_25, w = w)]
    pers_h2[, w := ifelse(D_25 == 1, w_old*(PF_il_25/p_il_25), w_old*((1-PF_il_25)/(1-p_il_25)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_26 := weighted.mean(D_26, w = w)]
    pers_h2[, w := ifelse(D_26 == 1, w_old*(PF_il_26/p_il_26), w_old*((1-PF_il_26)/(1-p_il_26)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_27 := weighted.mean(D_27, w = w)]
    pers_h2[, w := ifelse(D_27 == 1, w_old*(PF_il_27/p_il_27), w_old*((1-PF_il_27)/(1-p_il_27)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_28 := weighted.mean(D_28, w = w)]
    pers_h2[, w := ifelse(D_28 == 1, w_old*(PF_il_28/p_il_28), w_old*((1-PF_il_28)/(1-p_il_28)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_29 := weighted.mean(D_29, w = w)]
    pers_h2[, w := ifelse(D_29 == 1, w_old*(PF_il_29/p_il_29), w_old*((1-PF_il_29)/(1-p_il_29)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_30 := weighted.mean(D_30, w = w)]
    pers_h2[, w := ifelse(D_30 == 1, w_old*(PF_il_30/p_il_30), w_old*((1-PF_il_30)/(1-p_il_30)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_31 := weighted.mean(D_31, w = w)]
    pers_h2[, w := ifelse(D_31 == 1, w_old*(PF_il_31/p_il_31), w_old*((1-PF_il_31)/(1-p_il_31)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_32 := weighted.mean(D_32, w = w)]
    pers_h2[, w := ifelse(D_32 == 1, w_old*(PF_il_32/p_il_32), w_old*((1-PF_il_32)/(1-p_il_32)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_33 := weighted.mean(D_33, w = w)]
    pers_h2[, w := ifelse(D_33 == 1, w_old*(PF_il_33/p_il_33), w_old*((1-PF_il_33)/(1-p_il_33)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_34 := weighted.mean(D_34, w = w)]
    pers_h2[, w := ifelse(D_34 == 1, w_old*(PF_il_34/p_il_34), w_old*((1-PF_il_34)/(1-p_il_34)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_35 := weighted.mean(D_35, w = w)]
    pers_h2[, w := ifelse(D_35 == 1, w_old*(PF_il_35/p_il_35), w_old*((1-PF_il_35)/(1-p_il_35)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_36 := weighted.mean(D_36, w = w)]
    pers_h2[, w := ifelse(D_36 == 1, w_old*(PF_il_36/p_il_36), w_old*((1-PF_il_36)/(1-p_il_36)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_37 := weighted.mean(D_37, w = w)]
    pers_h2[, w := ifelse(D_37 == 1, w_old*(PF_il_37/p_il_37), w_old*((1-PF_il_37)/(1-p_il_37)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_38 := weighted.mean(D_38, w = w)]
    pers_h2[, w := ifelse(D_38 == 1, w_old*(PF_il_38/p_il_38), w_old*((1-PF_il_38)/(1-p_il_38)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_39 := weighted.mean(D_39, w = w)]
    pers_h2[, w := ifelse(D_39 == 1, w_old*(PF_il_39/p_il_39), w_old*((1-PF_il_39)/(1-p_il_39)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_40 := weighted.mean(D_40, w = w)]
    pers_h2[, w := ifelse(D_40 == 1, w_old*(PF_il_40/p_il_40), w_old*((1-PF_il_40)/(1-p_il_40)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_41 := weighted.mean(D_41, w = w)]
    pers_h2[, w := ifelse(D_41 == 1, w_old*(PF_il_41/p_il_41), w_old*((1-PF_il_41)/(1-p_il_41)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_42 := weighted.mean(D_42, w = w)]
    pers_h2[, w := ifelse(D_42 == 1, w_old*(PF_il_42/p_il_42), w_old*((1-PF_il_42)/(1-p_il_42)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_43 := weighted.mean(D_43, w = w)]
    pers_h2[, w := ifelse(D_43 == 1, w_old*(PF_il_43/p_il_43), w_old*((1-PF_il_43)/(1-p_il_43)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_44 := weighted.mean(D_44, w = w)]
    pers_h2[, w := ifelse(D_44 == 1, w_old*(PF_il_44/p_il_44), w_old*((1-PF_il_44)/(1-p_il_44)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_45 := weighted.mean(D_45, w = w)]
    pers_h2[, w := ifelse(D_45 == 1, w_old*(PF_il_45/p_il_45), w_old*((1-PF_il_45)/(1-p_il_45)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_46 := weighted.mean(D_46, w = w)]
    pers_h2[, w := ifelse(D_46 == 1, w_old*(PF_il_46/p_il_46), w_old*((1-PF_il_46)/(1-p_il_46)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_47 := weighted.mean(D_47, w = w)]
    pers_h2[, w := ifelse(D_47 == 1, w_old*(PF_il_47/p_il_47), w_old*((1-PF_il_47)/(1-p_il_47)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_48 := weighted.mean(D_48, w = w)]
    pers_h2[, w := ifelse(D_48 == 1, w_old*(PF_il_48/p_il_48), w_old*((1-PF_il_48)/(1-p_il_48)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_49 := weighted.mean(D_49, w = w)]
    pers_h2[, w := ifelse(D_49 == 1, w_old*(PF_il_49/p_il_49), w_old*((1-PF_il_49)/(1-p_il_49)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_50 := weighted.mean(D_50, w = w)]
    pers_h2[, w := ifelse(D_50 == 1, w_old*(PF_il_50/p_il_50), w_old*((1-PF_il_50)/(1-p_il_50)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_51 := weighted.mean(D_51, w = w)]
    pers_h2[, w := ifelse(D_51 == 1, w_old*(PF_il_51/p_il_51), w_old*((1-PF_il_51)/(1-p_il_51)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_52 := weighted.mean(D_52, w = w)]
    pers_h2[, w := ifelse(D_52 == 1, w_old*(PF_il_52/p_il_52), w_old*((1-PF_il_52)/(1-p_il_52)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_53 := weighted.mean(D_53, w = w)]
    pers_h2[, w := ifelse(D_53 == 1, w_old*(PF_il_53/p_il_53), w_old*((1-PF_il_53)/(1-p_il_53)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_54 := weighted.mean(D_54, w = w)]
    pers_h2[, w := ifelse(D_54 == 1, w_old*(PF_il_54/p_il_54), w_old*((1-PF_il_54)/(1-p_il_54)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_55 := weighted.mean(D_55, w = w)]
    pers_h2[, w := ifelse(D_55 == 1, w_old*(PF_il_55/p_il_55), w_old*((1-PF_il_55)/(1-p_il_55)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_56 := weighted.mean(D_56, w = w)]
    pers_h2[, w := ifelse(D_56 == 1, w_old*(PF_il_56/p_il_56), w_old*((1-PF_il_56)/(1-p_il_56)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_57 := weighted.mean(D_57, w = w)]
    pers_h2[, w := ifelse(D_57 == 1, w_old*(PF_il_57/p_il_57), w_old*((1-PF_il_57)/(1-p_il_57)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_58 := weighted.mean(D_58, w = w)]
    pers_h2[, w := ifelse(D_58 == 1, w_old*(PF_il_58/p_il_58), w_old*((1-PF_il_58)/(1-p_il_58)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_59 := weighted.mean(D_59, w = w)]
    pers_h2[, w := ifelse(D_59 == 1, w_old*(PF_il_59/p_il_59), w_old*((1-PF_il_59)/(1-p_il_59)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_60 := weighted.mean(D_60, w = w)]
    pers_h2[, w := ifelse(D_60 == 1, w_old*(PF_il_60/p_il_60), w_old*((1-PF_il_60)/(1-p_il_60)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_61 := weighted.mean(D_61, w = w)]
    pers_h2[, w := ifelse(D_61 == 1, w_old*(PF_il_61/p_il_61), w_old*((1-PF_il_61)/(1-p_il_61)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_62 := weighted.mean(D_62, w = w)]
    pers_h2[, w := ifelse(D_62 == 1, w_old*(PF_il_62/p_il_62), w_old*((1-PF_il_62)/(1-p_il_62)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_63 := weighted.mean(D_63, w = w)]
    pers_h2[, w := ifelse(D_63 == 1, w_old*(PF_il_63/p_il_63), w_old*((1-PF_il_63)/(1-p_il_63)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_64 := weighted.mean(D_64, w = w)]
    pers_h2[, w := ifelse(D_64 == 1, w_old*(PF_il_64/p_il_64), w_old*((1-PF_il_64)/(1-p_il_64)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_65 := weighted.mean(D_65, w = w)]
    pers_h2[, w := ifelse(D_65 == 1, w_old*(PF_il_65/p_il_65), w_old*((1-PF_il_65)/(1-p_il_65)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_66 := weighted.mean(D_66, w = w)]
    pers_h2[, w := ifelse(D_66 == 1, w_old*(PF_il_66/p_il_66), w_old*((1-PF_il_66)/(1-p_il_66)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_67 := weighted.mean(D_67, w = w)]
    pers_h2[, w := ifelse(D_67 == 1, w_old*(PF_il_67/p_il_67), w_old*((1-PF_il_67)/(1-p_il_67)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_68 := weighted.mean(D_68, w = w)]
    pers_h2[, w := ifelse(D_68 == 1, w_old*(PF_il_68/p_il_68), w_old*((1-PF_il_68)/(1-p_il_68)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_69 := weighted.mean(D_69, w = w)]
    pers_h2[, w := ifelse(D_69 == 1, w_old*(PF_il_69/p_il_69), w_old*((1-PF_il_69)/(1-p_il_69)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_70 := weighted.mean(D_70, w = w)]
    pers_h2[, w := ifelse(D_70 == 1, w_old*(PF_il_70/p_il_70), w_old*((1-PF_il_70)/(1-p_il_70)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_71 := weighted.mean(D_71, w = w)]
    pers_h2[, w := ifelse(D_71 == 1, w_old*(PF_il_71/p_il_71), w_old*((1-PF_il_71)/(1-p_il_71)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_72 := weighted.mean(D_72, w = w)]
    pers_h2[, w := ifelse(D_72 == 1, w_old*(PF_il_72/p_il_72), w_old*((1-PF_il_72)/(1-p_il_72)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_73 := weighted.mean(D_73, w = w)]
    pers_h2[, w := ifelse(D_73 == 1, w_old*(PF_il_73/p_il_73), w_old*((1-PF_il_73)/(1-p_il_73)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_74 := weighted.mean(D_74, w = w)]
    pers_h2[, w := ifelse(D_74 == 1, w_old*(PF_il_74/p_il_74), w_old*((1-PF_il_74)/(1-p_il_74)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_75 := weighted.mean(D_75, w = w)]
    pers_h2[, w := ifelse(D_75 == 1, w_old*(PF_il_75/p_il_75), w_old*((1-PF_il_75)/(1-p_il_75)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_76 := weighted.mean(D_76, w = w)]
    pers_h2[, w := ifelse(D_76 == 1, w_old*(PF_il_76/p_il_76), w_old*((1-PF_il_76)/(1-p_il_76)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_77 := weighted.mean(D_77, w = w)]
    pers_h2[, w := ifelse(D_77 == 1, w_old*(PF_il_77/p_il_77), w_old*((1-PF_il_77)/(1-p_il_77)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_78 := weighted.mean(D_78, w = w)]
    pers_h2[, w := ifelse(D_78 == 1, w_old*(PF_il_78/p_il_78), w_old*((1-PF_il_78)/(1-p_il_78)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_79 := weighted.mean(D_79, w = w)]
    pers_h2[, w := ifelse(D_79 == 1, w_old*(PF_il_79/p_il_79), w_old*((1-PF_il_79)/(1-p_il_79)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_80 := weighted.mean(D_80, w = w)]
    pers_h2[, w := ifelse(D_80 == 1, w_old*(PF_il_80/p_il_80), w_old*((1-PF_il_80)/(1-p_il_80)))]
    pers_h2[, w_old := w]
    
    pers_h2[, p_il_81 := weighted.mean(D_81, w = w)]
    pers_h2[, w := ifelse(D_81 == 1, w_old*(PF_il_81/p_il_81), w_old*((1-PF_il_81)/(1-p_il_81)))]
    pers_h2[, w_old := w]
    
    control <- pers_h2[, .(c_m_0_4=PF_m_0_4/p_m_0_4,
                           c_m_5_11=PF_m_5_11/p_m_5_11,
                           c_m_12_14=PF_m_12_14/p_m_12_14,
                           c_m_15_17=PF_m_15_17/p_m_15_17,
                           c_m_18_20=PF_m_18_20/p_m_18_20,
                           c_m_21_24=PF_m_21_24/p_m_21_24,
                           c_m_25_29=PF_m_25_29/p_m_25_29,
                           c_m_30_34=PF_m_30_34/p_m_30_34,
                           c_m_35_39=PF_m_35_39/p_m_35_39,
                           c_m_40_44=PF_m_40_44/p_m_40_44,
                           c_m_45_49=PF_m_45_49/p_m_45_49,
                           c_m_50_54=PF_m_50_54/p_m_50_54,
                           c_m_55_59=PF_m_55_59/p_m_55_59,
                           c_m_60_64=PF_m_60_64/p_m_60_64,
                           c_m_65_74=PF_m_65_74/p_m_65_74,
                           c_m_over75=PF_m_over75/p_m_over75,
                           
                           
                           c_f_0_4=PF_f_0_4/p_f_0_4,
                           c_f_5_11=PF_f_5_11/p_f_5_11,
                           c_f_12_14=PF_f_12_14/p_f_12_14,
                           c_f_15_17=PF_f_15_17/p_f_15_17,
                           c_f_18_20=PF_f_18_20/p_f_18_20,
                           c_f_21_24=PF_f_21_24/p_f_21_24,
                           c_f_25_29=PF_f_25_29/p_f_25_29,
                           c_f_30_34=PF_f_30_34/p_f_30_34,
                           c_f_35_39=PF_f_35_39/p_f_35_39,
                           c_f_40_44=PF_f_40_44/p_f_40_44,
                           c_f_45_49=PF_f_45_49/p_f_45_49,
                           c_f_50_54=PF_f_50_54/p_f_50_54,
                           c_f_55_59=PF_f_55_59/p_f_55_59,
                           c_f_60_64=PF_f_60_64/p_f_60_64,
                           c_f_65_74=PF_f_65_74/p_f_65_74,
                           c_f_over75=PF_f_over75/p_f_over75,
                           
                           
                           c_il_01=PF_il_01/p_il_01,
                           c_il_02=PF_il_02/p_il_02,
                           c_il_03=PF_il_03/p_il_03,
                           c_il_04=PF_il_04/p_il_04,
                           c_il_05=PF_il_05/p_il_05,
                           c_il_06=PF_il_06/p_il_06,
                           c_il_07=PF_il_07/p_il_07,
                           c_il_08=PF_il_08/p_il_08,
                           c_il_09=PF_il_09/p_il_09,
                           c_il_10=PF_il_10/p_il_10,
                           c_il_11=PF_il_11/p_il_11,
                           c_il_12=PF_il_12/p_il_12,
                           c_il_13=PF_il_13/p_il_13,
                           c_il_14=PF_il_14/p_il_14,
                           c_il_15=PF_il_15/p_il_15,
                           c_il_16=PF_il_16/p_il_16,
                           c_il_17=PF_il_17/p_il_17,
                           c_il_18=PF_il_18/p_il_18,
                           c_il_19=PF_il_19/p_il_19,
                           c_il_20=PF_il_20/p_il_20,
                           c_il_21=PF_il_21/p_il_21,
                           c_il_22=PF_il_22/p_il_22,
                           c_il_23=PF_il_23/p_il_23,
                           c_il_24=PF_il_24/p_il_24,
                           c_il_25=PF_il_25/p_il_25,
                           c_il_26=PF_il_26/p_il_26,
                           c_il_27=PF_il_27/p_il_27,
                           c_il_28=PF_il_28/p_il_28,
                           c_il_29=PF_il_29/p_il_29,
                           c_il_30=PF_il_30/p_il_30,
                           c_il_31=PF_il_31/p_il_31,
                           c_il_32=PF_il_32/p_il_32,
                           c_il_33=PF_il_33/p_il_33,
                           c_il_34=PF_il_34/p_il_34,
                           c_il_35=PF_il_35/p_il_35,
                           c_il_36=PF_il_36/p_il_36,
                           c_il_37=PF_il_37/p_il_37,
                           c_il_38=PF_il_38/p_il_38,
                           c_il_39=PF_il_39/p_il_39,
                           c_il_40=PF_il_40/p_il_40,
                           c_il_41=PF_il_41/p_il_41,
                           c_il_42=PF_il_42/p_il_42,
                           c_il_43=PF_il_43/p_il_43,
                           c_il_44=PF_il_44/p_il_44,
                           c_il_45=PF_il_45/p_il_45,
                           c_il_46=PF_il_46/p_il_46,
                           c_il_47=PF_il_47/p_il_47,
                           c_il_48=PF_il_48/p_il_48,
                           c_il_49=PF_il_49/p_il_49,
                           c_il_50=PF_il_50/p_il_50,
                           c_il_51=PF_il_51/p_il_51,
                           c_il_52=PF_il_52/p_il_52,
                           c_il_53=PF_il_53/p_il_53,
                           c_il_54=PF_il_54/p_il_54,
                           c_il_55=PF_il_55/p_il_55,
                           c_il_56=PF_il_56/p_il_56,
                           c_il_57=PF_il_57/p_il_57,
                           c_il_58=PF_il_58/p_il_58,
                           c_il_59=PF_il_59/p_il_59,
                           c_il_60=PF_il_60/p_il_60,
                           c_il_61=PF_il_61/p_il_61,
                           c_il_62=PF_il_62/p_il_62,
                           c_il_63=PF_il_63/p_il_63,
                           c_il_64=PF_il_64/p_il_64,
                           c_il_65=PF_il_65/p_il_65,
                           c_il_66=PF_il_66/p_il_66,
                           c_il_67=PF_il_67/p_il_67,
                           c_il_68=PF_il_68/p_il_68,
                           c_il_69=PF_il_69/p_il_69,
                           c_il_70=PF_il_70/p_il_70,
                           c_il_71=PF_il_71/p_il_71,
                           c_il_72=PF_il_72/p_il_72,
                           c_il_73=PF_il_73/p_il_73,
                           c_il_74=PF_il_74/p_il_74,
                           c_il_75=PF_il_75/p_il_75,
                           c_il_76=PF_il_76/p_il_76,
                           c_il_77=PF_il_77/p_il_77,
                           c_il_78=PF_il_78/p_il_78,
                           c_il_79=PF_il_79/p_il_79,
                           c_il_80=PF_il_80/p_il_80,
                           c_il_81=PF_il_81/p_il_81)]
    
  assign(paste0("control_",i,"_",j),value = control[,lapply(.SD,mean),.SDcols=c_m_0_4: c_il_81])
  Sys.sleep(3)  
  }

  w_mean_value <- mean(pers_h2$w)
  weights_ind <- pers_h2[, w_mean := w_mean_value]
  weights_ind[, w_scale := w/w_mean]
  weights_ind[w_scale < 0.3, w:= 0.3*w_mean]
  weights_ind[w_scale > 3, w:= 3*w_mean]
  weights_ind[, c("fk_fkiml","w")]
  weights_ind[, w_old:= w]
  gc()
    
}

control_final <- control[, lapply(.SD,mean),.SDcols=c_m_0_4: c_il_81]
















