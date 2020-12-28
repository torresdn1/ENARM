#ENARM
library(tidyverse)
library(readxl)

datos <- read_excel("enarm_procedencia.xlsx")

#Los datos fueron obtenidos a partir de una solicitud de información.
#Contiene el puntaje individual y la universidad de procedencia de cada uno de los
#aspirantes seleccionados en el examen, y su especialidad correspondiente. 

datos %>% 
  group_by(universidad) %>% 
  mutate(mean= mean(puntaje), min= min(puntaje), max= max(puntaje), n= n()) %>% 
  ungroup() %>% group_by(especialidad) %>% mutate(nes= n()) %>% ungroup() %>% 
  filter(n>=30, nes>=400) %>% 
  ggplot(aes(x= reorder(universidad, mean), y= puntaje))+
  geom_boxplot(aes(fill=mean),alpha=0.6, outlier.shape = NA)+ 
  geom_point(aes(color=mean), size = 1.5, position="jitter", alpha=0.5)+ 
  coord_flip()+
  labs(title= "\n¿Cómo varían los puntajes de los aspirantes seleccionados dentro de cada universidad?: ENARM 2018" ,
       subtitle= "La gráfica muestra los puntajes de los aspirantes seleccionados por universidad de procedencia (cada punto es un aspirante) y por especialidad médica\nLas cajas agrupan el 50% de los aspirantes centrales y las líneas dentro de ellas indican el valor mediano de cada distribución\n",
       caption = "\nNota: Para mejorar la visualización, fueron eliminadas las especialidades con menos de 400 aspirantes seleccionados y las universidades con menos de 30\nFuente: Secretaría de Salud del Gobierno Federal\nElaboración Propia: Donovan Torres (@torres_dn1)",
       x= "",y= "Puntaje", fill= NULL)+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  theme_minimal()+
  theme(legend.position = "none",
        text= element_text(family="Times New Roman", size= 20),
        plot.title = element_text(face = "bold", size=45, hjust = 0.5),
        plot.subtitle = element_text(size=35, hjust = 0.5),
        strip.background = element_rect(colour = "black", fill = "white"),
        plot.caption = element_text(face= "italic", size=25), 
        plot.title.position = "plot")+
  facet_wrap(~especialidad, nrow = 2, scales = 'free_x') + 
  scale_y_continuous(limits = c(55,90))

