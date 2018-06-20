shooting <- readRDS("./shootingComparison.rds")

compare_stats <- function(name1,year1,name2,year2) {
    layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(6, 1))
    par(mai=c(0.5,0.5,1.0,0.25))

    s.perc.1 <- as.numeric(shooting[[name1]][which(shooting[[name1]]$year == year1),10:14])
    s.perc.2 <- as.numeric(shooting[[name2]][which(shooting[[name2]]$year == year2),10:14])
    s.perc <- rbind(s.perc.1,s.perc.2)
    pps <- cbind(2*s.perc[,1:4],3*s.perc[,5])
    pps[is.na(pps)] <- 0

    perc.s.1 <- as.numeric(shooting[[name1]][which(shooting[[name1]]$year == year1),5:9])
    perc.s.2 <- as.numeric(shooting[[name2]][which(shooting[[name2]]$year == year2),5:9])
    perc.s <- rbind(perc.s.1,perc.s.2)*100
    perc.s[is.na(perc.s)] <- 0

    ylim <- max(pps)*1.1
    colnames(pps) <- c("0-3","3-10","10-16","16<3","3")
    bp <- barplot(pps, col=c('#e41a1c','#377eb8'), beside=TRUE,
                  cex.axis=1.5, cex.names=1.5, font=2, ylim=c(0,ylim))
    text(x=bp,y=pps,labels=round(s.perc*100,1),pos=3,cex=1.4)
    mtext("Shot Distance",side=1,line=3)
    mtext("Points Per Shot",side=2,line=3)
    mtext("Points Per Shot by Location",side=3,line=1,cex=1.5)
    #abline(h=1.028,col='Black',lty=2,lwd=2)
    box()
    ylim <- max(perc.s)*1.1
    colnames(perc.s) <- c("0-3","3-10","10-16","16<3","3")
    bp <- barplot(perc.s, col=c('#e41a1c','#377eb8'), beside=TRUE,
                  cex.axis=1.5, cex.names=1.5, font=2, ylim=c(0,ylim))
    text(x=bp,y=perc.s,labels=perc.s,pos=3,cex=1.4)
    mtext("Shot Distance",side=1,line=3)
    mtext("Percent of Shots",side=2,line=3)
    mtext("Percent of Shots by Location",side=3,line=1,cex=1.5)
    box()
    par(mai=c(0,0,0,0))
    plot.new()
    legend(x="center", ncol=2, legend=c(name1,name2),
           col=c('#e41a1c','#377eb8'), lty=c(1,1), lwd=c(7,7), cex=1.25)
}

## compare_stats <- function(name1,year1,name2,year2) {
##     layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(6, 1))
##     par(mai=c(0.5,0.5,1.0,0.25))

##     advCols <- c(2,3,5,4)
##     ppsCols <- 11:15
##     percCols <- 6:10

##     adv.1 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name1),advCols])
##     s.perc.1 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name1),ppsCols])
##     perc.s.1 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name1),percCols])

##     adv.2 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name2),advCols])
##     adv <- t(data.frame(adv.1,adv.2))
##     adv[,c(1,3,4)] <- 100*adv[,c(1,3,4)]
##     adv[is.na(adv)] <- 0
##     colnames(adv) <- c("TS%","USG%","FTR","FT%")
##     ylim <- max(adv)*1.1
##     bp <- barplot(adv, col=c('#e41a1c','#377eb8'), beside=TRUE,
##                   ylab="", ylim=c(0,ylim), cex.axis=1.5, cex.names=1.5, font=2)
##     text(x=bp,y=adv,labels=adv,pos=3,cex=1.5)
##     mtext("Advanced Stats",side=3,line=1,cex=1.5)
##     box()
##     league.pps <- 1.028
##     s.perc.2 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name2),ppsCols])
##     s.perc <- t(data.frame(s.perc.1,s.perc.2))
##     pps <- cbind(2*s.perc[,1:4],3*s.perc[,5])
##     pps <- pps
##     pps[is.na(pps)] <- 0
##     ylim <- max(pps)*1.1
##     colnames(pps) <- c("0-3","3-10","10-16","16<3","3")
##     bp <- barplot(pps, col=c('#e41a1c','#377eb8'), beside=TRUE,
##                   cex.axis=1.5, cex.names=1.5, font=2, ylim=c(0,ylim))
##     text(x=bp,y=pps,labels=round(s.perc*100,1),pos=3,cex=1.4)
##     mtext("Shot Distance",side=1,line=3)
##     mtext("Points Per Shot",side=2,line=3)
##     mtext("Points Per Shot by Location",side=3,line=1,cex=1.5)
##     #abline(h=1.028,col='Black',lty=2,lwd=2)
##     box()
##     perc.s.2 <- as.numeric(stats18[tolower(stats18$Player) == tolower(name2),percCols])
##     perc.s <- t(data.frame(perc.s.1,perc.s.2))*100
##     perc.s[is.na(perc.s)] <- 0
##     ylim <- max(perc.s)*1.1
##     colnames(perc.s) <- c("0-3","3-10","10-16","16<3","3")
##     bp <- barplot(perc.s, col=c('#e41a1c','#377eb8'), beside=TRUE,
##                   cex.axis=1.5, cex.names=1.5, font=2, ylim=c(0,ylim))
##     text(x=bp,y=perc.s,labels=perc.s,pos=3,cex=1.4)
##     mtext("Shot Distance",side=1,line=3)
##     mtext("Percent of Shots",side=2,line=3)
##     mtext("Percent of Shots by Location",side=3,line=1,cex=1.5)
##     box()
##     par(mai=c(0,0,0,0))
##     plot.new()
##     legend(x="center", ncol=2, legend=c(name1,name2),
##            col=c('#e41a1c','#377eb8'), lty=c(1,1), lwd=c(7,7), cex=2)
## }
