
# Create byperson data set, selecting the largest gain in case of muliple gains
byperson_first <- create_byperson(data = sgdata,
                sg_crit1_cutoff = 7,
                id_var_name = "id",
                tx_start_var_name = "bdi_s1",
                tx_end_var_name = "bdi_s12",
                sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
                                "bdi_s4", "bdi_s5", "bdi_s6",
                                "bdi_s10", "bdi_s11", "bdi_s12"),
                sg_measure_name = "bdi",
                multiple_sg_select = "first")

byperson_last <- create_byperson(data = sgdata,
                            sg_crit1_cutoff = 7,
                            id_var_name = "id",
                            tx_start_var_name = "bdi_s1",
                            tx_end_var_name = "bdi_s12",
                            sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
                                            "bdi_s4", "bdi_s5", "bdi_s6",
                                            "bdi_s7", "bdi_s8", "bdi_s9",
                                            "bdi_s10", "bdi_s11", "bdi_s12"),
                            sg_measure_name = "bdi",
                            multiple_sg_select = "last")

byperson_largest <- create_byperson(data = sgdata,
                            sg_crit1_cutoff = 7,
                            id_var_name = "id",
                            tx_start_var_name = "bdi_s1",
                            tx_end_var_name = "bdi_s12",
                            sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
                                            "bdi_s4", "bdi_s5", "bdi_s6",
                                            "bdi_s7", "bdi_s8", "bdi_s9",
                                            "bdi_s10", "bdi_s11", "bdi_s12"),
                            sg_measure_name = "bdi",
                            multiple_sg_select = "largest")


describe_sg(byperson)$total_n %>% cat()

lalala <- byperson %>% 
  select(sg_session_n) %>% 
  ggplot(aes(sg_session_n)) + 
  geom_bar() +
  labs(x = "Pregain Time Point", y = "Number of Sudden Gains", fill = "") +
  scale_x_continuous(breaks = seq(2, max(byperson$sg_session_n, na.rm = T), by = 1)) +
  # scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  theme(text = element_text(size = 12))

lalala

# lalala + coord_polar()



# First create a bysg (or byperson) dataset
bysg <- create_bysg(data = sgdata,
                    sg_crit1_cutoff = 7,
                    id_var_name = "id",
                    tx_start_var_name = "bdi_s1",
                    tx_end_var_name = "bdi_s12",
                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
                                    "bdi_s4", "bdi_s5", "bdi_s6",
                                    "bdi_s7", "bdi_s8", "bdi_s9",
                                    "bdi_s10", "bdi_s11", "bdi_s12"),
                    sg_measure_name = "bdi")

# Plot average change of BDI values around the period of the sudden gain
plot_sg(data = byperson_first,
        id_var_name = "id",
        tx_start_var_name = "bdi_s1",
        tx_end_var_name = "bdi_s12",
        sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                 "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
        ylab = "BDI", xlab = "Session")

plot_sg(data = byperson_last,
        id_var_name = "id",
        tx_start_var_name = "bdi_s1",
        tx_end_var_name = "bdi_s12",
        sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                 "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
        ylab = "BDI", xlab = "Session")

plot_sg(data = byperson_largest,
        id_var_name = "id",
        tx_start_var_name = "bdi_s1",
        tx_end_var_name = "bdi_s12",
        sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                 "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
        ylab = "BDI", xlab = "Session")

plot_sg(data = bysg,
        id_var_name = "id",
        tx_start_var_name = "bdi_s1",
        tx_end_var_name = "bdi_s12",
        sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                 "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
        ylab = "BDI", xlab = "Session")

library(tidyr)
byperson %>% 
  select(sg_session_n) %>% 
  group_by(sg_session_n) %>% 
  count() %>% 
  drop_na() %>% 
  max(.$n)
  
  
