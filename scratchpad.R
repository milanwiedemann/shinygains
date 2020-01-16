
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


describe_sg(byperson_largest, sg_data_structure = "byperson")

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

describe_sg(bysg, sg_data_structure = "bysg")

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
  
  
#' Plot average change in variables around the sudden gain
#'
#' @description Generates a plot of the mean values around the sudden gain using \code{\link[ggplot2]{ggplot}}.
#' This can be used to plot the primary outcome or secondary measures.
#' The parameters starting with "group" allow to plot the average gain magnitude by group.
#' Further ggplot2 components can be added using + following this function.
#' @param data A \code{bysg} or \code{byperson} dataset created using the function \code{\link{create_bysg}} or \code{\link{create_byperson}}.
#' @param id_var_name String, specifying the name of the ID variable.
#' @param tx_start_var_name String, specifying the variable name of the first measurement point of the intervention.
#' @param tx_end_var_name String, specifying the variable name of the last measurement point of the intervention.
#' @param sg_pre_post_var_list Vector, specifying the variable names of the 3 measurement points before,
#' and the 3 after the sudden gain, for the measure being plotted.
#' @param colour_single String, specifying the colour of the plot for one group.
#' @param colour_group String, specifying the discrete colour palette to be used for the groups.
#' @param viridis_option String specifying the colour option for discrete viridis palette, see \code{\link[ggplot2]{scale_fill_viridis_d}}.
#' @param viridis_begin Numeric, specifying hue between 0 and 1 at which the viridis colormap begins, see \code{\link[ggplot2]{scale_fill_viridis_d}}.
#' @param viridis_end Numeric, specifying hue between 0 and 1 at which the viridis colormap ends, see \code{\link[ggplot2]{scale_fill_viridis_d}}.
#' @param group_var_name String, specifying the variable name of the group variable.
#' @param group_levels Vector, specifying the levels as numeric for the groups in \code{group_var_name}.
#' @param group_labels Vector, specifying the label names as strings for the groups in \code{group_var_name}.
#' @param group_title String, specifying the title that will be used for the groups specified in \code{group_labels}.
#' @param ylab String, specifying the label for the y axis i.e. the name of the measure being plotted.
#' @param xlab String, specifying the label for the x axis, e.g. \code{"Session"}.
#' @param apaish Logical, make plot APA publishable.
#' @return A plot of the mean values around the sudden gain, for the measure specified.
#' @export
#' @examples # First create a bysg (or byperson) dataset
#' bysg <- create_bysg(data = sgdata,
#'                     sg_crit1_cutoff = 7,
#'                     id_var_name = "id",
#'                     tx_start_var_name = "bdi_s1",
#'                     tx_end_var_name = "bdi_s12",
#'                     sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
#'                                     "bdi_s4", "bdi_s5", "bdi_s6",
#'                                     "bdi_s7", "bdi_s8", "bdi_s9",
#'                                     "bdi_s10", "bdi_s11", "bdi_s12"),
#'                     sg_measure_name = "bdi")
#'
#' # Plot average change of BDI values around the period of the sudden gain
#' plot_sg(data = bysg,
#'         id_var_name = "id",
#'         tx_start_var_name = "bdi_s1",
#'         tx_end_var_name = "bdi_s12",
#'         sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
#'                                  "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
#'         ylab = "BDI", xlab = "Session")

