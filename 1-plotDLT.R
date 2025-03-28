#############################################################
#                                                           #
#   Swimming performance escape response:from DLT to R (1)  #
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)

setwd("D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT")
#####################################################################
#  to change every new video !!!!!! 
vid <- "vid228"
Length <- 5.7 # from miseurment in lab
Ref1cm_pixel <- 23  # from ImageJ calculation on Ref photo
Start <- "C-start"
fps <- 1000 # camera setup
BY <- 2; TIME <- 0.002  # 2 - I decided to have a data point every 2 frames
#####################################################################

vidplot  <- read_csv(paste0("digitalized_DLTprj_excel/DLTdv8_data_",vid,"flippedxypts.csv")) %>% na.omit()
#vidplot<-vidplot %>% dplyr::select(-pt6_cam1_Y,-pt6_cam1_X)   %>% na.omit()
attach(vidplot)
summary(vidplot)

#jpeg(paste0("trj_",vid,  "_bodypart.jpeg"),width = 300, height = 170, units = "mm", res = 300)
par(mfcol = c(3, 2))
 plot(pt1_cam1_X, pt1_cam1_Y, main=paste0("Head ",vid),
                xlab="X", ylab="Y ", pch=19, type = "b")
 plot(pt2_cam1_X, pt2_cam1_Y, main=paste0("Throat ",vid),
                xlab="X", ylab="Y ", pch=19, type = "b")
 plot(pt3_cam1_X, pt3_cam1_Y, main=paste0("Anal ",vid),
                xlab="X", ylab="Y ", pch=19, type = "b")
 plot(pt4_cam1_X, pt4_cam1_Y, main=paste0("Tail ped ",vid),
                xlab="X", ylab="Y ", pch=19, type = "b")
 plot(pt5_cam1_X, pt5_cam1_Y, main=paste0("Tail ",vid),
     xlab="X", ylab="Y ", pch=19, type = "b")
# dev.off()
 

# vidplot$frame <- paste0("frame" , 1:length(vidplot$pt1_cam1_X))
 vidplot$frame <-  1:length(vidplot$pt1_cam1_X)
 vidplot.temp <- as.data.frame(vidplot %>% pivot_longer(cols = 1:10 ,values_to = "Value", names_to = "Coord"))
 vidplot.temp <- as.data.frame(vidplot.temp %>% dplyr::mutate(Coord = str_sub(vidplot.temp$Coord,  -1)))
 vidplot.pl <- vidplot.temp %>% filter(Coord == "X") %>% dplyr::select(-Coord) %>% rename(X = Value)
 vidplot.tempY <- vidplot.temp %>% filter(Coord == "Y") %>% dplyr::select(-Coord) %>% rename(Y = Value)
 vidplot.pl$Y <- vidplot.tempY$Y  # - se usi original file (not "flipped")
 vidplot.pl$Body_part <- c("Head","Throat","Anal","Tail_ped","Tail")
 vidplot.pl$time <- TIME*(vidplot.pl$frame-1)  # 0.001 from Matlab DLT video (by 1 frame), 0.005 by 5 frames
 vidplot.pl$vid_ID <- vid
 vidplot.pl$Start_type <- Start 
 vidplot.pl$Length <- Length 
 vidplot.pl$Ref1cm_pixel <- Ref1cm_pixel 
 vidplot.pl$fps <- fps 
 vidplot.pl$BY <- BY 
 head(vidplot.pl)
 
 #plot to chek bad smoother
 ggplot(vidplot.pl, aes(x = X, y = Y, group = frame, color=as.factor(frame) )) +geom_point()+geom_path(method = "loess",se = F, size= 1)+facet_wrap(~as.factor(frame))+  theme(legend.position = "none")
 # frame to be removed
 #DELETE <- c(50,51,52,53)
 #DELETE <- 15
 
 # plot tot fish (the five points) by frame
p1 <- ggplot(vidplot.pl   , aes(x = X, y = Y, group = frame,label = frame, color=as.factor(frame) )) +geom_point() + geom_path(size= 0.4) +
# geom_smooth(method = "loess",se = F, size= 0.8)+
  geom_point(size=1.1) +  ggtitle(paste(vid, "fish")) +  theme(legend.position = "none")+geom_text(size= 2.8, hjust=1.1, vjust=1.1); p1


##############################################################################
#  to change every new video !!!!!! 
# Filter by area of interest
# filtered_df <- c(1:30) # based on video/personal judgment! see Di Santo 2021
##############################################################################


 kin.db <- vidplot.pl  # %>% dplyr::filter((frame %in% filtered_df))
# kin.db$totalframes <- length(unique(kin.db$frame))*BY
 
p2 <-ggplot(kin.db , aes(x = X, y = Y, group=frame, label = frame, color=as.factor(frame) )) +geom_point(size=1.1) + geom_path(size= 0.4) +
  #geom_smooth(method = "loess",se = F, size= 0.8) +
  ggtitle(paste(vid, "selection")) +  theme(legend.position = "none")+geom_text(size= 2.8, hjust=1.2, vjust=1.2) ;p2
 
 #plot and csv to be saved
 #jpeg(paste0("trj_",vid,  "_fish.jpeg"),width = 350, height = 170, units = "mm", res = 300)
gridExtra::grid.arrange(p1,p2, ncol = 2)
 #dev.off()
 dev.off()
 # save db
 write.csv(kin.db, paste0(vid , "_dig.csv"),row.names=FALSE)
 
 