z_sold_1 <- z_sold[z_sold$week == 1,]
z_sold_2 <- z_sold[z_sold$week == 2,]
z_sold_3 <- z_sold[z_sold$week == 3,]
z_sold_4 <- z_sold[z_sold$week == 4,]
z_sold_5 <- z_sold[z_sold$week == 5,]
z_sold_6 <- z_sold[z_sold$week == 6,]
z_sold_7 <- z_sold[z_sold$week == 7,]
z_sold_8 <- z_sold[z_sold$week == 8,]

z_sold_1 <- z_sold_1[order(-z_sold_1$sold), ]
z_sold_2 <- z_sold_2[order(-z_sold_2$sold), ]
z_sold_3 <- z_sold_3[order(-z_sold_3$sold), ]
z_sold_4 <- z_sold_4[order(-z_sold_4$sold), ]
z_sold_5 <- z_sold_5[order(-z_sold_5$sold), ]
z_sold_6 <- z_sold_6[order(-z_sold_6$sold), ]
z_sold_7 <- z_sold_7[order(-z_sold_7$sold), ]
z_sold_8 <- z_sold_8[order(-z_sold_8$sold), ]

z_sold_1_sum <- sum(z_sold_1$sold)
z_sold_2_sum <- sum(z_sold_2$sold)
z_sold_3_sum <- sum(z_sold_3$sold)
z_sold_4_sum <- sum(z_sold_4$sold)
z_sold_5_sum <- sum(z_sold_5$sold)
z_sold_6_sum <- sum(z_sold_6$sold)
z_sold_7_sum <- sum(z_sold_7$sold)

#intersect
sold_same1  <- intersect(z_sold_1$sku, z_sold_2$sku)
sold_same2  <- intersect(z_sold_1$sku, z_sold_3$sku)
sold_same3  <- intersect(z_sold_1$sku, z_sold_4$sku)
sold_same4  <- intersect(z_sold_1$sku, z_sold_5$sku)
sold_same5  <- intersect(z_sold_1$sku, z_sold_6$sku)
sold_same6  <- intersect(z_sold_1$sku, z_sold_7$sku)
sold_same7  <- intersect(z_sold_1$sku, z_sold_8$sku)

c(z_sold_2_sum, z_sold_3_sum, z_sold_4_sum, z_sold_5_sum, z_sold_6_sum, z_sold_7_sum)

sold_s2  <- sum(z_sold_2[z_sold_2$sku  %in% sold_same2, 4])
sold_s3  <- sum(z_sold_3[z_sold_3$sku  %in% sold_same3, 4])
sold_s4  <- sum(z_sold_4[z_sold_4$sku  %in% sold_same4, 4])
sold_s5  <- sum(z_sold_5[z_sold_5$sku  %in% sold_same5, 4])
sold_s6  <- sum(z_sold_6[z_sold_6$sku  %in% sold_same6, 4])
sold_s7  <- sum(z_sold_7[z_sold_7$sku  %in% sold_same7, 4])

plot(c(2,6), c(0:1), xlab="Week", ylab="Repetition Rate", type="n")
lines(c(2,3,4,5,6,7), c(sold_s2, sold_s3, sold_s4, sold_s5, sold_s6, sold_s7) / c(z_sold_2_sum, z_sold_3_sum, z_sold_4_sum, z_sold_5_sum, z_sold_6_sum, z_sold_7_sum))
title("Repetition VS Week 1")

c(sold_s2, sold_s3, sold_s4, sold_s5, sold_s6, sold_s7)

#----- sold in x not in 1 ----------

sold_diff_d2 <- z_sold_2[z_sold_2$sku  %in% setdiff(z_sold_2$sku, z_sold_1$sku), ]
sold_diff_d3 <- z_sold_3[z_sold_3$sku  %in% setdiff(z_sold_3$sku, z_sold_1$sku), ]
sold_diff_d4 <- z_sold_4[z_sold_4$sku  %in% setdiff(z_sold_4$sku, z_sold_1$sku), ]
sold_diff_d5 <- z_sold_5[z_sold_5$sku  %in% setdiff(z_sold_5$sku, z_sold_1$sku), ]
sold_diff_d6 <- z_sold_6[z_sold_6$sku  %in% setdiff(z_sold_6$sku, z_sold_1$sku), ]
sold_diff_d7 <- z_sold_7[z_sold_7$sku  %in% setdiff(z_sold_7$sku, z_sold_1$sku), ]

sold_d2 <- sum(z_sold_2[z_sold_2$sku  %in% sold_diff_d2$sku, 4])
sold_d3 <- sum(z_sold_3[z_sold_3$sku  %in% sold_diff_d3$sku, 4])
sold_d4 <- sum(z_sold_4[z_sold_4$sku  %in% sold_diff_d4$sku, 4])
sold_d5 <- sum(z_sold_5[z_sold_5$sku  %in% sold_diff_d5$sku, 4])
sold_d6 <- sum(z_sold_6[z_sold_6$sku  %in% sold_diff_d6$sku, 4])
sold_d7 <- sum(z_sold_7[z_sold_7$sku  %in% sold_diff_d7$sku, 4])

#-------- sold in 1 not in x ---------

sold_diff_dn2 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_2$sku), ]
sold_diff_dn3 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_3$sku), ]
sold_diff_dn4 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_4$sku), ]
sold_diff_dn5 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_5$sku), ]
sold_diff_dn6 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_6$sku), ]
sold_diff_dn7 <- z_sold_1[z_sold_1$sku  %in% setdiff(z_sold_1$sku, z_sold_7$sku), ]

sold_dn2 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn2$sku, 4])
sold_dn3 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn3$sku, 4])
sold_dn4 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn4$sku, 4])
sold_dn5 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn5$sku, 4])
sold_dn6 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn6$sku, 4])
sold_dn7 <- sum(z_sold_1[z_sold_1$sku  %in% sold_diff_dn7$sku, 4])

#---------- Sold Add/Reduce Plot -------------
plot(c(1,6), c(6,1500), xlab="Week", ylab="Sold", type="n")
lines(c(1,2,3,4,5,6), c(sold_d2, sold_d3, sold_d4, sold_d5, sold_d6, sold_d7), col="#FF0000FF", lty=1, lwd=1.5)
lines(c(1,2,3,4,5,6), c(sold_dn2, sold_dn3, sold_dn4, sold_dn5, sold_dn6, sold_dn7), col="#00FFFFFF", lty=2, lwd=1.5)

title("Sold +/- VS Week 1")
legend(1, 1500, c("+", "-"), cex=0.8, col=c("#FF0000FF", "#00FFFFFF"), lty=c(1,2))
