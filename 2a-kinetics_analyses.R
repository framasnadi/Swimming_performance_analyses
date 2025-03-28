#############################################################
#                                                           #
# Swimming performance escape response:Kinematic Analyses(2a)#
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################

# video from the RIGHT to the LEFT
library(readr)
library(ggplot2)
library(dplyr)
library(grid)
library("gridExtra")

setwd("D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT")
dir <- "D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT"
#####################################################################
#  to change every new video !!!!!! 
#vid <- "vid228"
TIME <- 0.002  # if BY = 1... otherwise change accordingly
kin.db  <- read_csv(paste0(vid,"_dig.csv")) ; head(kin.db)
plotdir <-  paste0(dir ,"/plots_",vid )
plotDBs <- paste0(dir ,"/kinematics_DBs" )   
dir.create(plotdir,recursive = F)
#####################################################################

# trasform from pixel to cm and to BL
kin.db$X <- (kin.db$X/  kin.db$Ref1cm_pixel)   /kin.db$Length
kin.db$Y <- (kin.db$Y/  kin.db$Ref1cm_pixel)   /kin.db$Length

# trasposizione sul asse delle X
LM <-lm(Y~X,kin.db  %>% dplyr::filter(frame %in% 1:100 ))
#LM <-lm(Y~X,kin.db  )
coef(LM)
angle_rg <- atan(coef(LM)[2]) #* (180 / pi)

# Define a function to transform the points
transform_points <- function(x, y, angle ) {
  new_x <- x * cos(angle) + y * sin(angle)
  new_y <- (x * -sin(angle) + y * cos(angle))-coef(LM)[1]
  return(data.frame(X = new_x, Y = new_y))
}
# Apply the transformation
kin.db.orig <- kin.db #%>% filter(Body_part == "Head")

transformed_points <- transform_points(kin.db.orig$X, kin.db.orig$Y, angle_rg );transformed_points
LM2 <-lm(Y~X,transformed_points ); LM2
plot(kin.db.orig$X,  kin.db.orig$Y , col = "blue", xlim = c(min(transformed_points$X) , max(kin.db.orig$X)), ylim =c(min(transformed_points$Y) , max(kin.db.orig$Y) ))#
points(transformed_points, col = "red")
abline(0,0); abline(v=0, col="black"); abline( coef(LM)[1],coef(LM)[2], col = "blue"); abline(coef(LM2)[1],coef(LM2)[2], col = "red")

# overwrite the kin.db 
kin.db$X <- transformed_points$X 
kin.db$Y <- transformed_points$Y 

###################################################
kin.db.full  <- kin.db 
jpeg(paste0(plotdir,"/head_ang",vid,  "_plot.jpeg"),width = 400, height = 200, units = "mm", res = 300)
ggplot(kin.db %>% dplyr::filter((Body_part %in% c("Head","Throat"))), aes(x = X, y = Y, group=frame, label = frame, color=as.factor(frame) )) +geom_point(size=1.1) +geom_line(size=0.4)+geom_text( hjust=1.2, vjust=1.2)+  theme(legend.position = "none")  +coord_fixed(ratio = 1) 
dev.off()

# Create an empty vector to store the results
angles <- numeric()
angles.dir <- numeric()
COM_displ <- numeric()
Tail_displ <- numeric()
Curvs <- numeric()
HT_amplitude <- numeric()
# Unique frames in the data
unique_frames <- unique(kin.db$frame)
# Loop through frames
for (n in unique_frames) {
  # Subset the data for the current frame
  frame_data_orig <- kin.db[kin.db$frame == unique_frames[1], ]
  frame_data <- kin.db[kin.db$frame == n, ]
  frame_data_post <- kin.db[kin.db$frame == n+1, ]
  
  # Extract coordinates for "head" and "Throat" for the current frame
  x_head.or <- frame_data_orig$X[frame_data_orig$Body_part == "Head"]
  y_head.or <- frame_data_orig$Y[frame_data_orig$Body_part == "Head"]
  
  x_tail.or <- frame_data_orig$X[frame_data_orig$Body_part == "Throat"]
  y_tail.or <- frame_data_orig$Y[frame_data_orig$Body_part == "Throat"]
  
  
  x_head <- frame_data$X[frame_data$Body_part == "Head"]
  y_head <- frame_data$Y[frame_data$Body_part == "Head"]
  
  x_tail <- frame_data$X[frame_data$Body_part == "Throat"]
  y_tail <- frame_data$Y[frame_data$Body_part == "Throat"]
  
  x_head_post <- frame_data_post$X[frame_data_post$Body_part == "Head"]
  y_head_post <- frame_data_post$Y[frame_data_post$Body_part == "Head"]
 
  x_tail_post <- frame_data_post$X[frame_data_post$Body_part == "Throat"]
  y_tail_post <- frame_data_post$Y[frame_data_post$Body_part == "Throat"]
  
  x_coda.or <- frame_data_orig$X[frame_data_orig$Body_part == "Tail"]
  y_coda.or <- frame_data_orig$Y[frame_data_orig$Body_part == "Tail"]
  
  x_coda_post <- frame_data_post$X[frame_data_post$Body_part == "Tail"]
  y_coda_post <- frame_data_post$Y[frame_data_post$Body_part == "Tail"]
  
  # for body curvature
  x_coda <- frame_data$X[frame_data$Body_part == "Tail"]
  y_coda <- frame_data$Y[frame_data$Body_part == "Tail"]
  x_anal <- frame_data$X[frame_data$Body_part == "Anal"]
  y_anal  <- frame_data$Y[frame_data$Body_part == "Anal"]
  x_throat <- frame_data$X[frame_data$Body_part == "Throat"]
  y_throat  <- frame_data$Y[frame_data$Body_part == "Throat"]
  x_ped <- frame_data$X[frame_data$Body_part == "Tail_ped"]
  y_ped  <- frame_data$Y[frame_data$Body_part == "Tail_ped"]
  
  # Calculate the angle and store the result
 # angle <- angle_between_segments(x_head, y_head, x_tail, y_tail, x_head_post, y_head_post, x_tail_post, y_tail_post)
  uv <- (x_head-x_tail)*(x_head_post-x_tail_post)+(y_head-y_tail)*(y_head_post-y_tail_post)
  sqrtuv<-sqrt((x_head-x_tail)^2+(y_head-y_tail)^2)*sqrt((x_head_post-x_tail_post)^2+(y_head_post-y_tail_post)^2)
  cosangle = uv/sqrtuv
  angle <- (acos(cosangle)*180/pi)
  angles <- c(angles, angle)
  
  uv.dir <- (x_head.or-x_tail.or)*(x_head_post-x_tail_post)+(y_head.or-y_tail.or)*(y_head_post-y_tail_post)
  sqrtuv.dir<-sqrt((x_head.or-x_tail.or)^2+(y_head.or-y_tail.or)^2)*sqrt((x_head_post-x_tail_post)^2+(y_head_post-y_tail_post)^2)
  cosangle.dir = uv.dir/sqrtuv.dir
  angle.dir <- (acos(cosangle.dir)*180/pi)
#  angle.dir  <-  abs(angle.dir)/angle.dir
  angles.dir <- c(angles.dir, angle.dir)
  
  COMdispl <- (sqrt((x_tail.or - x_tail_post)^2 + (y_tail.or - y_tail_post)^2 )) #/(unique(kin.db$Ref1cm_pixel)/10)
  COM_displ <- c(COM_displ, COMdispl)

  TAILdispl <- (sqrt((x_coda.or - x_coda_post)^2 + (y_coda.or - y_coda_post)^2 )) #/(unique(kin.db$Ref1cm_pixel)/10)
  Tail_displ <- c(Tail_displ, TAILdispl)
  
  # Curvature
   A=((x_anal-x_ped)^2+(y_anal-y_ped)^2)^0.5 # distance Ped-ANal 
   B=((x_throat-x_anal)^2+(y_throat-y_anal)^2)^0.5 # distance ANal- Throat
   C=((x_throat-x_ped)^2+(y_throat-y_ped)^2)^0.5 # distance Ped-Throat
    D=(A+B+C)/2
    Radius=(A*B*C)/(4*(D*(D-A)*(D-B)*(D-C))^0.5)
    Curv=1/(Radius )  # / (unique(kin.db$Ref1cm_pixel)/1) 
    Curvs <- c(Curvs, Curv)
    
    # Head-Tail amplitude
    HTamplitude <- abs(y_coda - y_head)
    HT_amplitude <- c(HT_amplitude, HTamplitude)
}

# Print the results
turn_angle.db <- as.data.frame(angles) 
turn_angle.dir.db <- as.data.frame(angles.dir) ; new_row <- data.frame( angles.dir = 0) 
turn_angle.db$angles_to_OR <- turn_angle.dir.db$angles.dir
turn_angle.db$COM_displ <- COM_displ
turn_angle.db$Tail_displ <- Tail_displ

#;turn_angle.dir.db <- rbind(new_row,turn_angle.dir.db)

ZZ <- data.frame(turn_angle.dir.db$angles.dir - rbind(new_row,turn_angle.dir.db)); ZZ <- ZZ[-nrow(ZZ), ]
turn_angle.dir.db$angles <- ZZ; turn_angle.dir.db$direction <-  abs(turn_angle.dir.db$angles)/turn_angle.dir.db$angles
turn_angle.db$angles  <- turn_angle.db$angles*turn_angle.dir.db$direction;
new_row <- data.frame( angles = 0, angles_to_OR = 0, Tail_displ= 0, COM_displ= 0);turn_angle.db <- rbind(new_row,turn_angle.db);turn_angle.db$Curvs <- Curvs; turn_angle.db$HT_amplitude <- HT_amplitude

ang_vel <- turn_angle.db$angles/TIME  # dθ divide it by the instantaneous change in time
anglular_vel.db <- as.data.frame(ang_vel)
anglular_vel.db$time_ms <- (TIME*(unique(1:length(anglular_vel.db$ang_vel))-1)) *1000  # 0.005 from Matlab DLT video 
anglular_vel.db$ang_acc <- abs(anglular_vel.db$ang_vel)/TIME   # If we divide ang_vel by dt, we get the instantaneous angular acceleration 
anglular_vel.db$frame <- unique_frames; anglular_vel.db
anglular_vel.db$turn_angle <- turn_angle.db$angles_to_OR
anglular_vel.db$Tail_displ <- turn_angle.db$Tail_displ
anglular_vel.db$COM_displ <- turn_angle.db$COM_displ
anglular_vel.db$Curvs <- turn_angle.db$Curvs
anglular_vel.db$HT_amplitude <- turn_angle.db$HT_amplitude
anglular_vel.db

ggplot(kin.db %>% dplyr::filter((Body_part %in% c("Head","Throat"))), aes(x = X, y = Y, group=frame, label = frame, color=as.factor(frame) )) +geom_point(size=1.1) +geom_line(size=0.4)+geom_text( hjust=1.2, vjust=1.2)+  theme(legend.position = "none")  +coord_fixed(ratio = 1) 
# MANUALLY determine the END of St2 !!!!!!!!!
# usually cutting the db at the first frame after the ang_vel is positive again should be enough, but visually check with the "head_ang..._plot" for a confirmation (sometimes the direction of the first segment could produce errors)

# # # # # # # #
END_Stage1 <- 10  #
# # # # # # # # 

# # # # # # #
STOP <-  20 #
# # # # # # # 

anglular_vel.db <- anglular_vel.db %>% filter(frame <= STOP)
anglular_vel.db$Stage <- ifelse(anglular_vel.db$frame <=  END_Stage1, "St1", "St2")
anglular_vel.db[anglular_vel.db$frame == STOP,]$Stage <- NA
anglular_vel.db$ang_vel <- ifelse(anglular_vel.db$Stage ==  "St2", -abs(ang_vel) , abs(ang_vel))
#lastpoint <- data.frame(ang_vel=0 ,time_ms= max(anglular_vel.db$time_ms) + (5.0003/2)   ,ang_acc=0 , frame=NA,  Stage = NA); anglular_vel.db <- rbind(anglular_vel.db,lastpoint)


# join the dbs
kin.db.full <- left_join(kin.db , anglular_vel.db, by = "frame")
kin.db <- left_join(kin.db , anglular_vel.db, by = "frame") %>% filter(frame <= STOP)
# COM and TAIL velocity
kin.db$COM_vel <- kin.db$COM_displ / kin.db$time
kin.db$Tail_vel <- kin.db$Tail_displ / kin.db$time
summary(kin.db)
# check Curvature
with(kin.db,plot(Curvs~as.factor(Stage)))
#kin.db %>% na.omit() %>% group_by(Stage) %>% summarise(   min(Curvs), max(Curvs),mean(Curvs), )
plot(kin.db$frame, kin.db$Curvs); mean(kin.db$Curvs)

# tryBY 3 frames
#kin.db.by3 <-kin.db %>% slice(which(frame %% 2 == 1))

################################
#          PLOT
################################
# plot istant angular velocity
#jpeg(paste0("fish",vid,  "_angular_velocity.jpeg"),width = 300, height = 170, units = "mm", res = 300)
angpl <-ggplot(anglular_vel.db , aes(x = time_ms, y = ang_vel ,label= frame)) +geom_point(aes(x = time_ms, y = ang_vel, color = Stage ),size=0) +geom_line(linetype = "dotted", size=0.4)+geom_line(aes(x = time_ms, y = ang_vel, color = Stage ))+ xlab("Time (ms)") + ylab("Angular vel (deg/s)") + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = (((anglular_vel.db  %>% filter(Stage == "St1")) %>% filter(frame == max(frame)))$time_ms + (0.5)) , linetype = "longdash") +geom_text( aes(x = time_ms, y = ang_vel, color = Stage ),hjust=1.2, vjust=1.2)+
  labs( title = expression("Angular velocity of the head"),
       subtitle = expression(italic("St1 began in the frame of the first movement of the fish’s snout and ended when angular velocity first returned to zero. \nSt2 ended when angular velocity returned to zero for the second time. (Danos & Lauder, 2012)")))+
  theme(legend.position="NULL",   plot.subtitle = element_text(size = 10,margin = margin(t = 10))); angpl
#dev.off()

# plot COM and TAIL displacement
COMdisplplot <-ggplot(kin.db, aes(x = time_ms, y = COM_vel ,label= frame)) +geom_point(aes(x = time_ms, y = COM_vel, color = Stage ),size=0) +geom_line(linetype = "dotted", size=0.4)+geom_line(aes(x = time_ms, y = COM_vel, color = Stage ))+ xlab("Time (ms)") + ylab("COM velocity (BL/s)") + geom_hline(yintercept = 0, linetype = "dashed")  +geom_text( aes(x = time_ms, y = COM_vel, color = Stage ),hjust=1.2, vjust=1.2)+
  labs( title = expression("COM velocity"))+
  theme(legend.position="NULL",   plot.subtitle = element_text(size = 10,margin = margin(t = 10))); COMdisplplot
TAILdisplplot <-ggplot(kin.db, aes(x = time_ms, y = Tail_vel ,label= frame)) +geom_point(aes(x = time_ms, y = Tail_vel, color = Stage ),size=0) +geom_line(linetype = "dotted", size=0.4)+geom_line(aes(x = time_ms, y = Tail_vel, color = Stage ))+ xlab("Time (ms)") + ylab("Tail velocity (BL/s)") + geom_hline(yintercept = 0, linetype = "dashed")  +geom_text( aes(x = time_ms, y = Tail_vel, color = Stage ),hjust=1.2, vjust=1.2)+
  labs( title = expression("Tail velocity"))+
  theme(legend.position="NULL",   plot.subtitle = element_text(size = 10,margin = margin(t = 10))); TAILdisplplot

# plot the fish midlines
#jpeg(paste0("fish",vid,  "_midlines.jpeg"),width = 300, height = 170, units = "mm", res = 300)
fishpl <-ggplot(kin.db , aes(x = X, y = Y, group=frame, label = frame, color = Stage )) +geom_point(size=1) +
  geom_path() +
  # geom_smooth(method = "loess",se = F, size= 0.5) +
  geom_text(size= 3.3, hjust=1.2, vjust=1.2,show.legend = FALSE) + 
 #  coord_fixed(ratio = 1)  +
  ggtitle(paste("Midlines displacement"))+ 
  theme(legend.position="bottom",legend.text = element_text(size=13),legend.title = element_text(size=13) )+ 
  guides(color = guide_legend(override.aes = list(size = 3)))+ xlab("X (BL)") + ylab("Y (BL)"); fishpl
#dev.off()

#plot trajactoríes body part
#jpeg(paste0("trj_",vid,  "_bodypart.jpeg"),width = 300, height = 170, units = "mm", res = 300)
# Convert X to a factor with desired order
kin.db.full$Body_part <- factor(kin.db.full$Body_part , levels = c("Head","Throat","Anal","Tail_ped", "Tail"))
vline_data <- data.frame( X = ((kin.db.full  %>% filter(Stage == "St1")) %>% filter(frame == max(frame)))$X)
vline_data$Body_part <- unique(kin.db.full$Body_part)
trjpl <-ggplot(kin.db.full, aes(x = X, y = Y ,label= frame)) +geom_point(aes(x = X, y = Y, color = Stage ),size=1.1) +geom_path(color= "#00AFBB")+geom_path(aes(x = X, y = Y, color = Stage )) +  facet_wrap(~ Body_part ,ncol=1)+geom_text( aes(x = X, y = Y, color = Stage ),size= 2.3,hjust=1.2, vjust=1.2)+
 # geom_vline(data= vline_data , aes(xintercept  = X), linetype = "longdash", color = "black")+
  ggtitle(paste("Body part displacment (all frames available)")) + theme(legend.position="NULL")+ xlab("X (BL)") + ylab("Y (BL)") ; trjpl#+coord_fixed(ratio = 1)
#dev.off()
dev.off()
######################################################################################################
jpeg(paste0(plotdir,"/Final_",vid,  "_plot.jpeg"),width = 400, height = 200, units = "mm", res = 300)
grid.arrange(top = grid::textGrob(paste("Stage Explorative plots",vid, "►", kin.db$Start_type),gp=gpar(fontsize=17,font=3)) ,arrangeGrob( angpl, fishpl, ncol = 1),     
             trjpl, # Second row with 2 plots in 2 different columns
             nrow = 1)                       # Number of rows
dev.off()
######################################################################################################

#################################################################################
# save db fish
write.csv(kin.db, paste0(plotDBs,"/db_kin_",vid , ".csv"),row.names=FALSE)
#################################################################################

####################################
# move to GIF_kinetics_analyses_Danos_part.R script for GIF animation !!!
####################################
