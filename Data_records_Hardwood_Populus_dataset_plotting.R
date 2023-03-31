library(readxl)
Hardwood_and_Populus_datasets <- read_excel("D:/STOMATA/Stomata pictures from Heidi/copied_from_all_folders/Scientific_data/Hardwood_and_Populus_datasets.xlsx", 
                                            sheet = "Sheet1")
# View(Hardwood_and_Populus_datasets)

summary(Hardwood_and_Populus_datasets)

unique(Hardwood_and_Populus_datasets$Scientific_name)

sum(table(Hardwood_and_Populus_datasets$Scientific_name))


Hardwood_unique <- Hardwood_and_Populus_datasets[!duplicated(Hardwood_and_Populus_datasets$Scientific_name),]

writexl::write_xlsx(Hardwood_unique, "D:/STOMATA/Stomata pictures from Heidi/copied_from_all_folders/Scientific_data/Manuscript/Hardwood_unique.xlsx")


hist(Hardwood_and_Populus_datasets$Mean_WST_No[Hardwood_and_Populus_datasets$Scientific_name=="Populus L."], breaks = 30, xlab = "Number of Stomata", ylab = "Frequency", main =NA,  col = "Sea Green")

hist(Hardwood_and_Populus_datasets$Mean_WST_No[!Hardwood_and_Populus_datasets$Scientific_name=="Populus L."], breaks = 100, xlab = "Number of Stomata", ylab = "Frequency", main =NA, col = "Tan")

Hardwood <- Hardwood_and_Populus_datasets[!Hardwood_and_Populus_datasets$Scientific_name=="Populus L.",]
Hardwood$hardwood_populus <- "Hardwoods"

Populus <- Hardwood_and_Populus_datasets[Hardwood_and_Populus_datasets$Scientific_name=="Populus L.",]
Populus$hardwood_populus <- "Populus L."

combined_data <- rbind.data.frame(Hardwood,Populus)


library(tidyverse)



p <- combined_data %>%
  ggplot(aes(x=as.numeric(Mean_WST_No), fill=hardwood_populus)) +
  geom_histogram(alpha=0.4, position = 'identity') +
  geom_vline(xintercept=quantile(as.numeric(combined_data$Mean_WST_No)), linetype="dashed", linewidth=0.25, color = "blue")+
  scale_fill_manual(values=c("#008F9D", "#F08721")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dashed',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.05, linetype = 'dashed',
                                        colour = "gray"))+
  stat_bin(bins = 30, alpha=0.6)+
  labs(fill="")+
  xlab(expression(Number ~of ~Stomata))+
  ylab(expression(Count))

p



hist(Hardwood_and_Populus_datasets$Mean_WST_No[Hardwood_and_Populus_datasets$Scientific_name=="Ulmus americana Planch" ], breaks = 40, xlab = "Number of Stomata", ylab = "Frequency", main =NA, xlim = c(0,70), col = "green")

hist(Hardwood_and_Populus_datasets$Mean_WST_No[Hardwood_and_Populus_datasets$Scientific_name=="Ilex opaca Aiton" ], breaks = 30, xlab = "Number of Stomata", ylab = "Frequency", main =NA, xlim = c(0,70), col = "orange")

library(tidyverse)

mean_stm <- Hardwood_and_Populus_datasets %>%
  group_by(Scientific_name) %>%
  summarise(mean_stm = mean(Mean_WST_No))

ggplot()+
  geom_line(aes(Scientific_name, Mean_WST_No,  color = Scientific_name),data = Hardwood_and_Populus_datasets)+
  geom_point(aes(Scientific_name, mean_stm, color = Scientific_name),data = mean_stm)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.15, linetype = 'dashed',
                                        colour = "gray"),
        legend.position="none")+
  xlab("Scientific name")+
  ylab("Mean Number of Stomata")+
  coord_flip()
