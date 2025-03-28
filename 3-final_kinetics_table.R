#############################################################
#                                                           #
# Swimming performance escape response:create final db(3)   #
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################


library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(ggrepel)
# ........merge DBs
# Set the path to the folder containing the CSV files
dir <- "D:/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT"
plotDBs <- paste0(dir ,"/kinematics_DBs" ) 

# Get a list of all CSV files in the folder
library(plyr)
csv_files <- list.files(plotDBs, pattern = "\\.csv$", full.names = TRUE);csv_files
# Read all CSV files and bind them into a single data frame
kin.db.merge <- ldply(csv_files, read.csv, header=TRUE)  
detach("package:plyr", unload = TRUE)
kin.db.merge <- kin.db.merge   %>% dplyr::select(-totalframes)
unique(kin.db.merge$vid_ID)
# kin.db.merge <-kin.db.merge %>% slice(which(frame %% 2 == 1))


# Stage duration and final turn angle
Stage_Dur_turnang <- kin.db.merge %>% na.omit()%>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(time_ms == max(time_ms))  %>% summarise(Duration = max(time_ms),
                                                 End_St_turn_angle = max(turn_angle)
  );Stage_Dur_turnang$Type <- "Duration & End Stage Turn Ang";Stage_Dur_turnang  <- Stage_Dur_turnang %>% pivot_longer(cols= 5:6 , names_to = "Indicators", values_to = "value")

# Maximum
Max <- kin.db.merge  %>% na.omit() %>% group_by(vid_ID, Stage, Start_type, Length)  %>%  mutate(Stage = "Max") %>% 
  summarise(Max_turn_angle = max(turn_angle),
            Max_ang_vel = max(abs(ang_vel)),
            Max_ang_acc = max(ang_acc),
            Max_COM_vel = max(COM_vel),
            Max_Tail_vel = max(Tail_vel),
            Max_Curvature = max(Curvs)
            );Max$Type<- "Maximum";Max  <- Max %>% pivot_longer(cols= 5:10 , names_to = "Indicators", values_to = "value")

# Maximum by STAGE
Max_st <- kin.db.merge  %>% na.omit() %>% group_by(vid_ID, Stage, Start_type, Length)  %>%
  summarise(Max_ang_vel = max(abs(ang_vel)),
            Max_ang_acc = max(ang_acc)
  );Max_st$Type<- "Maximum";Max_st  <- Max_st %>% pivot_longer(cols= 5:6 , names_to = "Indicators", values_to = "value")

# Average
Average <- kin.db.merge  %>% na.omit() %>% group_by(vid_ID, Stage, Start_type, Length)  %>%  mutate(Stage = "Average") %>% 
  summarise(Avg_turn_angle = mean(turn_angle),
            Avg_ang_vel = mean(abs(ang_vel)),
            Avg_ang_acc = mean(ang_acc),
            Avg_COM_vel = mean(COM_vel),
            Avg_Tail_vel = mean(Tail_vel),
            Avg_Curvature = mean(Curvs)
  );Average$Type<- "Average";Average  <- Average %>% pivot_longer(cols= 5:10 , names_to = "Indicators", values_to = "value")

# Average by STAGE
Average_st <- kin.db.merge  %>% na.omit() %>% group_by(vid_ID, Stage, Start_type, Length)  %>%
  summarise(Avg_ang_vel = mean(abs(ang_vel)),
            Avg_ang_acc = mean(ang_acc)
  );Average_st$Type<- "Average";Average_st  <- Average_st %>% pivot_longer(cols= 5:6 , names_to = "Indicators", values_to = "value")


# Displecement
Displ_End_St <- kin.db.merge %>% na.omit()%>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(time_ms == max(time_ms))  %>% summarise(COM_displ = max(COM_displ),
                                                 Tail_displ = max(Tail_displ) , 
  );Displ_End_St$Type <- "Displacement";Displ_End_St  <- Displ_End_St %>% pivot_longer(cols= 5:6 , names_to = "Indicators", values_to = "value")

at30ms <- kin.db.merge %>% na.omit()%>% mutate(Stage = "at 30ms") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(time_ms == 30)  %>% summarise(COM_displ = max(COM_displ) ,  
                                      Tail_displ = max(Tail_displ) );at30ms$Type <- "Displacement";at30ms<- at30ms %>% pivot_longer(cols= 5:6 , names_to = "Indicators", values_to = "value")


# Time to Max 
Time_Max_turn <- kin.db.merge %>% na.omit()%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(turn_angle == max(turn_angle))  %>% summarise(Time_Max_turn = max(time_ms)  );Time_Max_turn$Type <- "Time to Max";Time_Max_turn<- Time_Max_turn %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_ang_vel <- kin.db.merge %>% na.omit()%>% mutate(ang_vel = abs(ang_vel))%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(ang_vel == max(ang_vel))  %>% summarise(Time_Max_ang_vel = max(time_ms)  );Time_Max_ang_vel$Type <- "Time to Max";Time_Max_ang_vel<- Time_Max_ang_vel %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_ang_acc <- kin.db.merge %>% na.omit()%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(ang_acc == max(ang_acc))  %>% summarise(Time_Max_ang_acc = max(time_ms)  );Time_Max_ang_acc$Type <- "Time to Max";Time_Max_ang_acc<- Time_Max_ang_acc %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_COM_vel <- kin.db.merge %>% na.omit()%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(COM_vel == max(COM_vel))  %>% summarise(Time_Max_COM_vel = max(time_ms)  );Time_Max_COM_vel$Type <- "Time to Max";Time_Max_COM_vel<- Time_Max_COM_vel %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_Tail_vel <- kin.db.merge %>% na.omit()%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(Tail_vel == max(Tail_vel))  %>% summarise(Time_Max_Tail_vel = max(time_ms)  );Time_Max_Tail_vel$Type <- "Time to Max";Time_Max_Tail_vel <- Time_Max_Tail_vel %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_Curvature <- kin.db.merge %>% na.omit()%>% mutate(Stage = "Max") %>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(Curvs == max(Curvs))  %>% summarise(Time_Max_Curvature = max(time_ms)  );Time_Max_Curvature$Type <- "Time to Max";Time_Max_Curvature <- Time_Max_Curvature %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

# Time to Max by STAGE
Time_Max_ang_vel_St <- kin.db.merge %>% na.omit()%>% mutate(ang_vel = abs(ang_vel))%>%
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(ang_vel == max(ang_vel))  %>% summarise(Time_Max_ang_vel = max(time_ms)  );Time_Max_ang_vel_St$Type <- "Time to Max";Time_Max_ang_vel_St <- Time_Max_ang_vel_St %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")

Time_Max_ang_acc_St <- kin.db.merge %>% na.omit()%>% 
  group_by(vid_ID, Stage, Start_type, Length) %>%
  filter(ang_acc == max(ang_acc))  %>% summarise(Time_Max_ang_acc = max(time_ms)  );Time_Max_ang_acc_St$Type <- "Time to Max";Time_Max_ang_acc_St <- Time_Max_ang_acc_St %>% pivot_longer(cols= 5 , names_to = "Indicators", values_to = "value")
 

### merge results to get final table for analyses
db.kin.var <- rbind(Stage_Dur_turnang, Max, Max_st, Average, Average_st , Displ_End_St, at30ms, Time_Max_turn,Time_Max_ang_vel,Time_Max_ang_acc,Time_Max_COM_vel,Time_Max_Tail_vel,Time_Max_Curvature, Time_Max_ang_vel_St,Time_Max_ang_acc_St)
unique(db.kin.var$vid_ID)
#db.kin.var <- cbind(Max_st,End_St[5:7],Time_Max_turn[5],Time_Max_ang_vel[5], Time_Max_ang_acc[5],Time_Max_COM_vel[5],Time_Max_Tail_vel[5])

#################################################################################
# save final kin table
write.csv(db.kin.var, paste0(plotDBs,"/Kineticts_table.csv"),row.names=FALSE)
#################################################################################


####################################################################
db.kin.var <- read_csv("C:/Users/frma6502/Desktop/SU_Postdoc/DO_STCKBCK_project/VideoAskö/VIDEO/ResultDLT/kinematics_DBs/ALLtreatment_Final_KinDB/Kineticts_table.csv")

# Exploratory plot 

####################################################################
#db.plot <- db.kin.var %>% pivot_longer(cols= 5:19 , names_to = "Indicators", values_to = "value")
#db.kin.var.plot <- db.kin.var %>% filter(Indicators != c("Max_ang_acc"))
plotDBs <- paste(plotDBs , "/", sep="")
for (i in unique(db.kin.var$Type)   )   {
    jpeg(paste( plotDBs , "Explor_plot" ,i,  ".jpeg", sep = " "),width = 400, height = 200, units = "mm", res = 600)
  #  my_palette <- c("red","green")
  CC <- ggplot(db.kin.var %>% filter(Type == i), aes(Stage , value, label = vid_ID, color =  vid_ID, fill = vid_ID))  + geom_point() + facet_wrap(~ Indicators , scales = "free")+ geom_text_repel()+ ggtitle(paste( i))+theme(legend.position=""); print(CC)
    dev.off()
}


############
#  PLOT finale con Endpoint e Treatment
############
# import info from the captivity excel sheet
library(readxl)
db.video <- read_excel("C:/Users/frma6502/Desktop/SU_Postdoc/DO_STCKBCK_project/Data_Asko_2023_FIN.xlsx", 
                      sheet = "Video")
db.video$Endpoint <- (str_sub(db.video$Endpoint, start = -1  )) 

# join dbs
db.kin.var.info <- left_join(db.kin.var, db.video )
str(db.kin.var.info); unique(db.kin.var.info$vid_ID)
# 
db.kin.var.info <-db.kin.var.info %>% dplyr::filter(Fish_ID != "T6" ) %>% dplyr::filter(vid_ID != "vid251" )
db.kin.var.info <- db.kin.var.info %>% group_by(Stage, Start_type,Length ,Type,Indicators,Endpoint,Fish_ID,TankN, Treatment, Length_mm, Weight) %>% dplyr::summarise(value = mean(value))


# boxplot with original value
for (i in unique(db.kin.var.info$Type)   )   {
  jpeg(paste( plotDBs , "ALLtreatment_plot" ,i,  ".jpeg", sep = " "),width = 400, height = 200, units = "mm", res = 600)
  CC <- ggplot(db.kin.var.info %>% filter(Type == i), aes(Endpoint , value, label = Fish_ID, color =  Treatment))  + geom_boxplot() + facet_wrap(~Indicators+Stage , scales = "free")+ geom_text_repel()+ geom_smooth(aes(x = as.numeric(Endpoint),y = value,color=Treatment) ,size=0.7,linetype="dashed",method = "loess", se = F); print(CC) #+ geom_text_repel()
  dev.off()
}

# Trasform data
ggplot(db.kin.var.info) + geom_histogram(aes(x =value, stat = "bin")) + facet_wrap(~ Indicators, scale="free")
db.kin.var.info$sqrt_value  <- sqrt(db.kin.var.info$value)
db.kin.var.info$log_value  <- log(db.kin.var.info$value)
ggplot(db.kin.var.info) + geom_histogram(aes(x =sqrt_value, stat = "bin")) + facet_wrap(~ Indicators, scale="free")
ggplot(db.kin.var.info) + geom_histogram(aes(x =log_value, stat = "bin")) + facet_wrap(~ Indicators, scale="free")


# Boxplot with % compare to respective treatment baseline (endpoint 1)
mean.End1 <- db.kin.var.info %>% filter(Endpoint == "1") %>% group_by(Treatment,Indicators,Stage,Type)  %>% dplyr::summarise(value_end1 = mean(value, na.rm = T), sqrt_value_end1 = mean(sqrt_value, na.rm = T) )
db.kin.var.info <- left_join(db.kin.var.info, mean.End1)
db.kin.var.info$level <- ((db.kin.var.info$value / db.kin.var.info$value_end1)-1)*100
db.kin.var.info$sqrt_level <- ((db.kin.var.info$sqrt_value / db.kin.var.info$sqrt_value_end1)-1)*100
hist(db.kin.var.info$level)
hist(db.kin.var.info$sqrt_level)

for (i in unique(db.kin.var.info$Type)   )   {
  jpeg(paste( plotDBs , "LEVEL_ALLtreatment_plot" ,i,  ".jpeg", sep = " "),width = 400, height = 200, units = "mm", res = 600)
  CC <-  ggplot(db.kin.var.info %>% filter(Type == i), aes(x = Endpoint, label = Fish_ID, fill=Treatment,color = Treatment,y = level) ) + geom_boxplot(alpha = 0.3)+
    stat_summary(fun=mean, geom="point", shape=21, size=3)+theme_bw() + facet_wrap(~Indicators+Stage , scales = "free")+ geom_smooth(aes(x = as.numeric(Endpoint),y = level,color=Treatment) ,size=0.9,linetype="dashed",method = "loess", se = F)  + theme(legend.position = "right")+geom_hline(yintercept = 0, size=0.6,linetype="dotted") + ylab("Value % compare to baseline"); print(CC)
 #+ geom_text_repel()
  dev.off()
}

# sqrt value
for (i in unique(db.kin.var.info$Type)   )   {
  jpeg(paste( plotDBs , "sqrtLEVEL_ALLtreatment_plot" ,i,  ".jpeg", sep = " "),width = 400, height = 200, units = "mm", res = 600)
  CC <-  ggplot(db.kin.var.info %>% filter(Type == i), aes(x = Endpoint, label = Fish_ID, fill=Treatment,color = Treatment,y = sqrt_level) ) + geom_boxplot(alpha = 0.3)+
    stat_summary(fun=mean, geom="point", shape=21, size=3)+theme_bw() + facet_wrap(~Indicators+Stage , scales = "free")+ geom_smooth(aes(x = as.numeric(Endpoint),y = sqrt_level,color=Treatment) ,size=0.9,linetype="dashed",method = "loess", se = F)  + theme(legend.position = "right")+geom_hline(yintercept = 0, size=0.6,linetype="dotted") + ylab("sqrt Value % compare to baseline"); print(CC)
  #+ geom_text_repel()
  dev.off()
}

#################################################################################
# save kin db
write.csv(db.kin.var.info, paste0(plotDBs,"/db_kin_ALLtreatment.csv"),row.names=FALSE)
####





