library(ggplot2)
library(RevGadgets)

DATASET <- "NC"

# read in and process the ancestral states
bisse_anc_file <- paste0("output/SSE_Models/primates_BiSSE_",DATASET,"_anc_states_results.tree")
p_anc <- processAncStates(bisse_anc_file)

# plot the ancestral states
plot <- plotAncStatesMAP(p_anc,
                         tree_layout = "rect",
                         tip_labels_size = 1) +
  # modify legend location using ggplot2
  theme(legend.position = c(0.1,0.85),
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=4))

ggsave(paste0("output/SSE_Models/BiSSE_anc_states_",DATASET,".png"),plot, width=8, height=8)


# read in and process the log file
bisse_log_file <- paste0("output/SSE_Models/primates_BiSSE_",DATASET,".log")
pdata <- processSSE(bisse_log_file)

# plot the rates
plot <- plotMuSSE(pdata) +
  theme(legend.position = c(0.875,0.915),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6))

ggsave(paste0("output/SSE_Models/BiSSE_div_rates_",DATASET,".png"),plot, width=5, height=5)
