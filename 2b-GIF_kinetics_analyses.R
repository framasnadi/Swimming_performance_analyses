################################################################
#                                                              #
# Swimming performance escape response:Kinematic Analyses(2b)  #
#    Create a moving plot using ggplot2 and gganimate          #
#   Author: Francesco Masnadi (DEEP, Stockholm University)     #
#                                                              #
################################################################


library(transformr)
library(ggplot2)
library(gganimate)
library(ggpubr)
library(magick)

setwd("D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT")
dir <- "D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT"
plotDBs <- paste0(dir ,"/kinematics_DBs" ) 
vid <- "vid228"

kin.db.full  <- read_csv(paste0(plotDBs, "/db_kin_",vid, ".csv")) ; head(kin.db.full)
plotdir <-  paste0(dir ,"/plots_",vid )
  
gifTime <- round(max(kin.db.full$time) *100 )/1; gifTime # The length of the animation in seconds
gifEnd <- 0


# # #  Angular vel GIF
angpl <-ggplot(kin.db.full) +
  geom_line( aes(x = time_ms, y = ang_vel  )  ,linetype = "dotted", size=0.3)+ 
  geom_line( aes(x = time_ms, y = ang_vel ,color = factor(Stage) ))+ 
  geom_point(aes(x = time_ms, y = ang_vel ,color = factor(Stage) ))+
  geom_text( aes(x = time_ms, y = ang_vel ,label= as.factor(frame),color = factor(Stage) ),  hjust=1.6, vjust=1.6) +
  xlab("Time (ms)") + ylab("Angular vel (deg/s)") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = (((kin.db.full  %>% filter(Stage == "St1")) %>% filter(frame == max(frame)))$time_ms + (0.5)) , linetype = "longdash") +
  transition_reveal(frame)+
  labs( title = expression("Angular velocity of the head"))+
  theme(legend.position="bottom",  legend.text = element_text(size=13),legend.title = element_text(size=13), plot.subtitle = element_text(size = 7,margin = margin(t = 10)))+guides(color = guide_legend(title="Stage",override.aes = list(size = 3)))#; angpl
anim_save(paste0(plotdir, "/Moving_Ang_vel_",vid,  ".gif"),angpl, duration = round(gifTime), end_pause = gifEnd, width = 150,height = 85, units = "mm", res = 300 )

# # #  FISH trj GIF
fishpl <-kin.db.full %>% 
  ggplot() +
  aes(x = X, y = Y, group=frame, color = Stage,label = as.factor(frame) ) +
  geom_point(size=1.25) +
  geom_path() +
  #geom_smooth(method = "loess",se = F, size =0.5 ) +
  geom_text(  size= 3.3, hjust=1.2, vjust=1.2,show.legend = FALSE) + 
#  transition_reveal(time_ms)+
  transition_states(frame, transition_length = 1, state_length = 1, wrap = F) +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = 0.3, size =0.75)+
  #coord_fixed(ratio = 1)  +
  ggtitle(paste("Midlines displacement"))+ 
  theme(legend.position="NULL",legend.text = element_text(size=13),legend.title = element_text(size=13) )+ 
  xlab("X (pixel)") + ylab("Y (pixel)")
# Save the animation as a gif
anim_save(paste0(plotdir, "/Moving_trj_fish_",vid,  ".gif"),fishpl, duration = round(gifTime), end_pause = gifEnd,width = 150,height = 85, units = "mm", res = 300 )


# # #  COM displ plot GIF
COMdisplplot <-ggplot(kin.db.full) +
  geom_line( aes(x = time_ms, y = COM_displ  )  ,linetype = "dotted", size=0.3)+ 
  geom_line( aes(x = time_ms, y = COM_displ ,color = factor(Stage) ))+ 
  geom_point(aes(x = time_ms, y = COM_displ ,color = factor(Stage) ))+
  geom_text( aes(x = time_ms, y = COM_displ ,label= as.factor(frame),color = factor(Stage) ),  hjust=1.6, vjust=1.6) +
  xlab("Time (ms)") + ylab("COM displacement (BL/s)") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = (((kin.db.full  %>% filter(Stage == "St1")) %>% filter(frame == max(frame)))$time_ms + (0.5)) , linetype = "longdash") +
  transition_reveal(frame)+
  labs( title = expression("COM displacement (BL/s)"))+
  theme(legend.position="bottom",  legend.text = element_text(size=13),legend.title = element_text(size=13), plot.subtitle = element_text(size = 7,margin = margin(t = 10)))+guides(color = guide_legend(title="Stage",override.aes = list(size = 3)))
anim_save(paste0(plotdir, "/Moving_COM_displ_",vid,  ".gif"),COMdisplplot, duration = round(gifTime), end_pause = gifEnd, width = 150,height = 85, units = "mm", res = 300 )



# # #  BODY PART trj GIF
kin.db.full$Body_part <- factor(kin.db.full$Body_part , levels = c("Head","Throat","Anal","Tail_ped", "Tail"))
trjpl <-ggplot(kin.db.full, aes(x = X, y = Y ,label= as.factor(frame))) +geom_point(aes(x = X, y = Y, color = Stage ))+ geom_path()  +geom_point(color= "#00AFBB")+geom_point(aes(x = X, y = Y, color = Stage )) +
 # transition_reveal(time_ms)+
  transition_states(frame, transition_length = 1, state_length = 1) +
  shadow_mark()+
  enter_fade() +
  exit_fade() +
 # coord_fixed(ratio = 1)+  
  facet_wrap(~ Body_part ,nrow =1)+
 # geom_text( aes(x = X, y = Y, color = Stage ),size= 2.3,hjust=1.2, vjust=1.2)+
  # geom_vline(data= vline_data , aes(xintercept  = X), linetype = "longdash", color = "black")+
  #ggtitle(paste("Body part displacement"))
 theme(legend.position="NULL")+ xlab("X (pixel)") + ylab("Y (pixel)") 
anim_save(paste0(plotdir, "/Moving_trj_bodypart_",vid,  ".gif"),trjpl, duration = round(gifTime), end_pause = gifEnd,width = 150,height = 85, units = "mm", res = 300 )



