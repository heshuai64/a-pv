#select entry_date,asin,sku,pv,buy_box_per,sessions,units_ordered,orders_placed,price,cat_level_name_3,amazon_cat_3 
#into outfile '/tmp/34.csv' from orders_flow_v4 where supplier = '中泰国际u' and 
#(entry_date between '02-09' and '02-22')

z_pv <- read.table("/tmp/34.csv", sep=";", quote="\"")
colnames(z_pv) <- c("date","asin","sku","pv","buy_box_per","sessions","sold","orders","price","i_cat","a_cat")
z_pv$date <- substr(z_pv$date, 6, 10)

qplot(date, pv, data=z_pv, color=i_cat, alpha=0.3)

z_pv_1 <- z_pv[z_pv$date %in% c("01-05","01-06","01-07","01-08","01-09","01-10","01-11"),]
z_pv_2 <- z_pv[z_pv$date %in% c("01-12","01-13","01-14","01-15","01-16","01-17","01-18"),]
z_pv_3 <- z_pv[z_pv$date %in% c("01-19","01-20","01-21","01-22","01-23","01-24","01-25"),]
z_pv_4 <- z_pv[z_pv$date %in% c("01-26","01-27","01-28","01-29","01-30","01-31","02-01"),]
z_pv_5 <- z_pv[z_pv$date %in% c("02-02","02-03","02-04","02-05","02-06","02-07","02-08"),]
z_pv_6 <- z_pv[z_pv$date %in% c("02-09","02-10","02-11","02-12","02-13","02-14","02-15"),]
z_pv_7 <- z_pv[z_pv$date %in% c("02-16","02-17","02-18","02-19","02-20","02-21","02-22"),]

z_pv_1  <- z_pv_1[order(-z_pv_1$pv, z_pv_1$sku),]
z_pv_2  <- z_pv_2[order(-z_pv_2$pv, z_pv_2$sku),]
z_pv_3  <- z_pv_3[order(-z_pv_3$pv, z_pv_3$sku),]
z_pv_4  <- z_pv_4[order(-z_pv_4$pv, z_pv_4$sku),]
z_pv_5  <- z_pv_5[order(-z_pv_5$pv, z_pv_5$sku),]
z_pv_6  <- z_pv_6[order(-z_pv_6$pv, z_pv_6$sku),]
z_pv_7  <- z_pv_7[order(-z_pv_7$pv, z_pv_7$sku),]

z_pv_1_sum <- sum(z_pv_1$pv)
z_pv_2_sum <- sum(z_pv_2$pv)
z_pv_3_sum <- sum(z_pv_3$pv)
z_pv_4_sum <- sum(z_pv_4$pv)
z_pv_5_sum <- sum(z_pv_5$pv)
z_pv_6_sum <- sum(z_pv_6$pv)
z_pv_7_sum <- sum(z_pv_7$pv)

#intersect
pv_same1  <- intersect(z_pv_1$sku, z_pv_2$sku)
pv_same2  <- intersect(z_pv_1$sku, z_pv_3$sku)
pv_same3  <- intersect(z_pv_1$sku, z_pv_4$sku)
pv_same4  <- intersect(z_pv_1$sku, z_pv_5$sku)
pv_same5  <- intersect(z_pv_1$sku, z_pv_6$sku)
pv_same6  <- intersect(z_pv_1$sku, z_pv_7$sku)

s2  <- sum(z_pv_2[z_pv_2$sku  %in% pv_same1, 4])
s3  <- sum(z_pv_3[z_pv_3$sku  %in% pv_same2, 4])
s4  <- sum(z_pv_4[z_pv_4$sku  %in% pv_same3, 4])
s5  <- sum(z_pv_5[z_pv_5$sku  %in% pv_same4, 4])
s6  <- sum(z_pv_6[z_pv_6$sku  %in% pv_same5, 4])
s7  <- sum(z_pv_7[z_pv_7$sku  %in% pv_same6, 4])

#----- pv in x not in 1 ----------
pv_diff1 <- setdiff(z_pv_2$sku, z_pv_1$sku)
pv_diff2 <- setdiff(z_pv_3$sku, z_pv_1$sku)
pv_diff3 <- setdiff(z_pv_4$sku, z_pv_1$sku)
pv_diff4 <- setdiff(z_pv_5$sku, z_pv_1$sku)
pv_diff5 <- setdiff(z_pv_6$sku, z_pv_1$sku)
pv_diff6 <- setdiff(z_pv_7$sku, z_pv_1$sku)

z_pv_d2 <- z_pv_2[z_pv_2$sku  %in% pv_diff1, ]
z_pv_d3 <- z_pv_3[z_pv_3$sku  %in% pv_diff2, ]
z_pv_d4 <- z_pv_4[z_pv_4$sku  %in% pv_diff3, ]
z_pv_d5 <- z_pv_5[z_pv_5$sku  %in% pv_diff4, ]
z_pv_d6 <- z_pv_6[z_pv_6$sku  %in% pv_diff5, ]
z_pv_d7 <- z_pv_7[z_pv_7$sku  %in% pv_diff6, ]

d2 <- sum(z_pv_2[z_pv_2$sku  %in% pv_diff1, 4])
d3 <- sum(z_pv_3[z_pv_3$sku  %in% pv_diff2, 4])
d4 <- sum(z_pv_4[z_pv_4$sku  %in% pv_diff3, 4])
d5 <- sum(z_pv_5[z_pv_5$sku  %in% pv_diff4, 4])
d6 <- sum(z_pv_6[z_pv_6$sku  %in% pv_diff5, 4])
d7 <- sum(z_pv_7[z_pv_7$sku  %in% pv_diff6, 4])

#-------- pv in 1 not in x ---------
pv_diff_n1 <- setdiff(z_pv_1$sku, z_pv_2$sku)
pv_diff_n2 <- setdiff(z_pv_1$sku, z_pv_3$sku)
pv_diff_n3 <- setdiff(z_pv_1$sku, z_pv_4$sku)
pv_diff_n4 <- setdiff(z_pv_1$sku, z_pv_5$sku)
pv_diff_n5 <- setdiff(z_pv_1$sku, z_pv_6$sku)
pv_diff_n6 <- setdiff(z_pv_1$sku, z_pv_7$sku)

z_pv_dn2 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n1, ]
z_pv_dn3 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n2, ]
z_pv_dn4 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n3, ]
z_pv_dn5 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n4, ]
z_pv_dn6 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n5, ]
z_pv_dn7 <- z_pv_1[z_pv_1$sku  %in% pv_diff_n6, ]

dn2 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n1, 4])
dn3 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n2, 4])
dn4 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n3, 4])
dn5 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n4, 4])
dn6 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n5, 4])
dn7 <- sum(z_pv_1[z_pv_1$sku  %in% pv_diff_n6, 4])

plot(c(1,6), range(z_pv_2_sum), xlab="Week", ylab="PV", type="n")
lines(c(z_pv_2_sum, z_pv_3_sum, z_pv_4_sum, z_pv_5_sum, z_pv_6_sum ,z_pv_7_sum))
lines(c(s2, s3, s4, s5, s6, s7))

t_vs_s <- list(total = c(z_pv_2_sum, z_pv_3_sum, z_pv_4_sum, z_pv_5_sum, z_pv_6_sum ,z_pv_7_sum),
            same = c(s2, s3, s4, s5, s6, s7))
qplot(c(1:6), pv, data=t_vs_s, geom="point", main="")

lines(c(d2, d3, d4, d5, d6, d7), col="#FF0000FF", lty=1, lwd=1.5)
lines(c(dn2, dn3, dn4, dn5, dn6, dn7), col="#00FFFFFF", lty=2, lwd=1.5)

title("Add/Reduce PV")
legend(1, 20000, c("Add", "Reduce"), cex=0.8, col=c("#FF0000FF", "#00FFFFFF"), lty=c(1,2))

tmp_01  <- aggregate(z_pv_d2$pv, by=list(z_pv_d2$sku),FUN=sum)
tmp_02 <- aggregate(z_pv_dn2$pv, by=list(z_pv_dn2$sku),FUN=sum)
colnames(tmp_01) <- c("sku", "pv")
colnames(tmp_02) <- c("sku", "pv")
tmp_01 <- tmp_01[order(-tmp_01$pv),]
tmp_02 <- tmp_02[order(-tmp_02$pv),]

tmp_11  <- aggregate(z_pv_d3$pv, by=list(z_pv_d3$sku),FUN=sum)
tmp_12 <- aggregate(z_pv_dn3$pv, by=list(z_pv_dn3$sku),FUN=sum)
colnames(tmp_11) <- c("sku", "pv")
colnames(tmp_12) <- c("sku", "pv")
tmp_11 <- tmp_11[order(-tmp_11$pv),]
tmp_12 <- tmp_12[order(-tmp_12$pv),]

tmp_21  <- aggregate(z_pv_d4$pv, by=list(z_pv_d4$sku),FUN=sum)
tmp_22 <- aggregate(z_pv_dn4$pv, by=list(z_pv_dn4$sku),FUN=sum)
colnames(tmp_21) <- c("sku", "pv")
colnames(tmp_22) <- c("sku", "pv")
tmp_21 <- tmp_21[order(-tmp_21$pv),]
tmp_22 <- tmp_22[order(-tmp_22$pv),]

tmp_31  <- aggregate(z_pv_d5$pv, by=list(z_pv_d5$sku),FUN=sum)
tmp_32 <- aggregate(z_pv_dn5$pv, by=list(z_pv_dn5$sku),FUN=sum)
colnames(tmp_31) <- c("sku", "pv")
colnames(tmp_32) <- c("sku", "pv")
tmp_31 <- tmp_31[order(-tmp_31$pv),]
tmp_32 <- tmp_32[order(-tmp_32$pv),]

tmp_41  <- aggregate(z_pv_d6$pv, by=list(z_pv_d6$sku),FUN=sum)
tmp_42 <- aggregate(z_pv_dn6$pv, by=list(z_pv_dn6$sku),FUN=sum)
colnames(tmp_41) <- c("sku", "pv")
colnames(tmp_42) <- c("sku", "pv")
tmp_41 <- tmp_41[order(-tmp_41$pv),]
tmp_42 <- tmp_42[order(-tmp_42$pv),]

z_pv[z_pv$sku=="a13073000ux1325",c(1,4)]
#--------------------------------- PV Add/Reduce Plot -----------------------------
g13062000ux0235 <- z_pv[z_pv$sku == "a13062000ux0665" | z_pv$sku == "a13101200ux0292" | z_pv$sku == "a13101200ux0293" | z_pv$sku == "a13101200ux0294" | z_pv$sku == "a13101200ux0295", c(1,3,4,7)]
g13062000ux0235$skux <- factor(g13062000ux0235$sku)
qplot(date, pv, data=g13062000ux0235, facets = skux~ ., geom="point")


#reduce
g12121000ux0721 <- z_pv[z_pv$sku == "a12031400ux0256" | z_pv$sku == "a12071900ux0508" | z_pv$sku == "a12071900ux0509" | z_pv$sku == "a12121800ux0191" | z_pv$sku == "a13061100ux0395" | z_pv$sku == "a14071100ux2278", c(1,3,4,7)]
g12121000ux0721$skux <- factor(g12121000ux0721$sku)
qplot(date, pv, data=g12121000ux0721, facets = skux~ ., geom="point", main="g12121000ux0721")

#select entry_date,sum(pv) from orders_flow_v4 where entry_date > '01-01'  and sku in ("a12030500ux0299","a12061400ux0109","a12061400ux0110","a12092000ux0412") group by entry_date order by entry_date;
#select entry_date,sum(pv) from orders_flow_v4 where entry_date > '01-01' and sku in ("a12052500ux0268","a13031500ux0139","a13031500ux0140","a13092000ux1231","a13092000ux1232","a13092000ux1233","a13092000ux1234") group by entry_date order by entry_date;

g12120900ux0027 <- z_pv[z_pv$sku == "a11112300ux0115" | z_pv$sku == "a11112300ux0116" | z_pv$sku == "a11112300ux0117" | z_pv$sku == "a13061000ux0192" | z_pv$sku == "a13061000ux0194" | z_pv$sku == "a13061000ux0196" | z_pv$sku == "a13061000ux0198" | z_pv$sku == "a13083000ux0416" | z_pv$sku == "a13083000ux0417" | z_pv$sku == "a13083000ux0418" | z_pv$sku == "a13083000ux0419" | z_pv$sku == "a13083000ux0420" | z_pv$sku == "a13083000ux0421" | z_pv$sku == "a13083000ux0422" | z_pv$sku == "a13083000ux0423", c(1,3,4,7)]
g12120900ux0027$skux <- factor(g12120900ux0027$sku)
qplot(date, pv, data=g12120900ux0027, facets = skux~ ., geom="point")
