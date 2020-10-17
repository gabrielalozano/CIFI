df = read.csv("CIFI-data.csv")
library(ggplot2)
library(dplyr)

rad = read.csv("rad.csv")
imag = read.csv("imag.csv")
trans = read.csv("trans.csv")
eclipse = read.csv("eclipse.csv")
micro=read.csv("micro.csv")
pulsar = read.csv("pulsar.csv")
ast = read.csv("ast.csv")
puls = read.csv("puls.csv")
orb =read.csv("orb.csv")
disk = read.csv("disk.csv")
ss = read.csv("ss.csv")


newdf = merge(rad,imag, all=TRUE)
newdf = merge(newdf, trans, all=TRUE)
newdf = merge(newdf, eclipse, all=TRUE)
newdf = merge(newdf, micro, all=TRUE)
newdf = merge(newdf, pulsar, all=TRUE)
newdf = merge(newdf, ast, all=TRUE)
newdf = merge(newdf, puls, all=TRUE)
newdf = merge(newdf, orb, all=TRUE)
newdf = merge(newdf, disk, all=TRUE)
newdf = merge(newdf, ss, all=TRUE)

df=newdf

write.csv(df, "DataCIFI.csv", row.names=FALSE)

df = read.csv("DataCIFI.csv")

# Radio Orbital vs Masa del planeta
ggplot(df, aes(log10(pl_orbsmax), log10(pl_bmassj), shape=discoverymethod, color=discoverymethod))+ 
  geom_point() +
  scale_shape_manual(values=c(1:11, 19))+
  scale_color_manual(values=c("plum", "pink", "aquamarine", "darkorange",
                              "gold", "purple4", "sienna2", "slateblue4", "green",
                              "seagreen4","blue", "black"))+
  geom_text(data=df %>% filter(discoverymethod == "SS"), # Filter data first
            aes(label=pl_name), col="black")+ # , position=position_jitter(1,1)
  ggtitle("Orbit Semi-Major Axis vs Mass of Planet") +
  xlab("Orbit Semi-Major Axis (UA)") + ylab("Mass of Planet (Jupiter's Mass)")


ggplot(df, aes((pl_orbsmax), (pl_bmassj), shape=discoverymethod, color=discoverymethod))+ 
  geom_point() +
  scale_y_log10()+
  scale_x_log10()+
  scale_shape_manual(values=c(1:11, 19))+
  scale_color_manual(values=c("plum", "pink", "aquamarine", "darkorange",
                              "gold", "purple4", "sienna2", "slateblue4", "green",
                              "seagreen4","blue", "black"))+
  geom_text(data=df %>% filter(discoverymethod == "SS"), # Filter data first
            aes(label=pl_name), col="black" , position=position_jitter(.5,.25))+
  ggtitle("Orbit Semi-Major Axis vs Mass of Planet") +
  xlab("Orbit Semi-Major Axis (UA)") + ylab("Mass of Planet (Jupiter's Mass)")


ggplot(df, aes((pl_bmasse), (st_mass), shape=discoverymethod, color=discoverymethod))+ 
  geom_point() +
  scale_y_log10()+
  scale_x_log10()+
  scale_shape_manual(values=c(1:11, 19))+
  scale_color_manual(values=c("plum", "pink", "aquamarine", "darkorange",
                              "gold", "purple4", "sienna2", "slateblue4", "green",
                              "seagreen4","blue", "black"))+
  geom_text(data=df %>% filter(discoverymethod == "SS"), # Filter data first
            aes(label=pl_name), col="black", position=position_jitter(.25,.25))+
  ggtitle("Mass of Planet vs Mass of Star") +
  xlab("Mass of Planet (Earth's Mass)") + ylab("Mass of Star (Sun's Mass)")


#Masa del planeta (masa Tierra) vs Masa de la estrella (masa del sol)
ggplot(df, aes(log10(pl_bmasse), log10(st_mass), shape=discoverymethod, color=discoverymethod))+ 
  geom_point() +
  scale_shape_manual(values=c(1:12))+
  geom_text(data=df %>% filter(discoverymethod == "SS"), # Filter data first
            aes(label=pl_name), col="black", position=position_jitter(1,1))+
  ggtitle("Masa del planeta vs Masa de la estrella") +
  xlab("Masa del planeta (Masa Tierra)") + ylab("Masa de la estrella (Masa del Sol)")
  
#df[df$discoverymethod == "Transit", "discoverymethod"] <- "1Transit"

x=c("Transit", "Radial Velocity", "Transit Timing Variations", "Imaging", "Microlensing","Pulsar Timing",
    "Pulsation Timing Variations", "Eclipse Timing Variations", "Orbital Brightness Modulation", 
    "Disk Kinematics", "Astrometry", "SS")
df %>%
  slice(match(x, discoverymethod))

df %>%
  mutate(discoverymethod =  factor(discoverymethod, levels = x)) %>%
  arrange(discoverymethod)

plot(df["pl_orbsmax"], df["pl_bmassj"])

