# Plot functions for HowDirty

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
theme_hd <-
  theme_classic(base_size = 10) +
  theme(plot.margin = margin(4,4,4,4),
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
scale_fill_risk <- function(..., option = "plasma", direction = 1){
  # options = "RdOrBlu", "plasma"
  if(option ==  "RdOrBlu"){colors_risk = c('#d73027','#f46d43','#fdae61', '#fee090','#abd9e9','#74add1','#4575b4')} #https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=8
  if(option ==  "plasma"){colors_risk = c("#0D0887FF", "#5002A2FF", "#8405A7FF", "#B12A90FF",  "#D35171FF", "#ED7953FF", "#FCA636FF")}  #c(scales::viridis_pal(end=0.8, option =  "plasma", direction = 1)(7))
  if(!option %in% c("RdOrBlu", "plasma")) stop("option must be = c(RdYlBlu, plasma)")
  if(direction == 1){colors_risk = colors_risk}
  if(direction == 1){colors_risk = rev(colors_risk)}
  if(!direction %in% c(-1,1))stop("risk must be = c(0,1)")

  ggplot2:::manual_scale(
    'fill',
    values = setNames(colors_risk,
                      c(0, 1, 2, 3, 4, 5, 6)),
    ...
  )
}
#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
scale_fill_risk_level <- function(..., option = "plasma", direction = 1){
  # options = "RdOrBlu", "plasma"
  if(option ==  "RdOrBlu"){colors_risk = c('#d73027','#f46d43','#fdae61', '#fee090','#abd9e9','#74add1','#4575b4')} #https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=8
  if(option ==  "plasma"){colors_risk = c("#0D0887FF", "#5002A2FF", "#8405A7FF", "#B12A90FF",  "#D35171FF", "#ED7953FF", "#FCA636FF")}  #c(scales::viridis_pal(end=0.8, option =  "plasma", direction = 1)(7))
  if(!option %in% c("RdOrBlu", "plasma")) stop("option must be = c(RdYlBlu, plasma)")
  if(direction == 1){colors_risk = colors_risk}
  if(direction == 1){colors_risk = rev(colors_risk)}
  if(!direction %in% c(-1,1))stop("risk must be = c(0,1)")

  ggplot2:::manual_scale(
    'fill',
    values = setNames(colors_risk,
                      c("1) Very Low (OK)", "2) Low (OK)", "3) Medium (Warning)", "4) High (Warning)", "5) Very High (DO NOT PROCEED)", "6) No theshold in reference")),
    ...
  )
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
scale_color_risk <- function(..., option = "plasma", direction = 1, verbose = FALSE){
  # options = "RdOrBlu", "plasma"
  if(option ==  "RdOrBlu"){colors_risk = c('#d73027','#f46d43','#fdae61', '#fee090','#abd9e9','#74add1','#4575b4')} #https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=8
  if(option ==  "plasma"){colors_risk = c("#0D0887FF", "#5002A2FF", "#8405A7FF", "#B12A90FF",  "#D35171FF", "#ED7953FF", "#FCA636FF")}  #c(scales::viridis_pal(end=0.8, option =  "plasma", direction = 1)(7))
  if(!option %in% c("RdOrBlu", "plasma")) stop("option must be = c(RdOrBlu, plasma)")
  if(direction == 1){colors_risk = colors_risk}
  if(direction == 1){colors_risk = rev(colors_risk)}
  if(!direction %in% c(-1,1))stop("risk must be = c(0,1)")
  if(verbose == FALSE) names_levels = c(0, 1, 2, 3, 4, 5, 6)
  if(verbose == TRUE) names_levels = c("0) Not Detected",
                                       "1) Very Low",
                                       "2) Low",
                                       "3) Medium",
                                       "4) High",
                                       "5) Very High",
                                       "6) No threshold in reference")


  ggplot2:::manual_scale(
    'color',
    values = setNames(colors_risk,
                      names_levels),
    ...
  )
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
# plot_abundance <- function(input_conta, scale = "linear"){
#   # plot the abundance
#   # scale: changes the scale to linear or log10; options = c("linear", "log10")
#   output <-
#     ggplot(input_conta, aes(x = Contaminant, y = Abundance)) +
#     geom_boxplot( alpha = 0.4, width = 0.5, size = 0.2, outlier.shape = NA, outlier.size = 0, outlier.alpha = 0, outlier.color = NA, outlier.fill = NA) +
#     geom_point(aes(color=Risk, text = paste("Replicate: ", ReplicateName, "\nSample: ", Sample)) , alpha = 0.5, size = 1) +
#     scale_color_risk(verbose = TRUE) +
#     facet_wrap(~ContaminantGroup, scales = "free", nrow = 1) +
#     scale_y_continuous(n.breaks = 5) +
#     ylab("Abundance = Area/TICA") +
#     xlab("Contaminant")+
#     theme_hd +
#     ggtitle("Abundance of contaminants") +
#     rotate()
#
#   if(scale == "linear"){return(output)}
#   if(scale == "log10"){
#     output <- output +
#       scale_y_log10(n.breaks = 5) +
#       ylab("Abundance = log10(Area/TICA)")
#     return(output)}
# }
plot_abundance <- function(input_conta, level, variable, scale = "linear"){
  # plot the abundance
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  output <-
    ggplot(input_conta, aes(x = {{ level }} , y = {{ variable }})) +
    geom_boxplot( alpha = 0.4, width = 0.5, size = 0.2, outlier.shape = NA, outlier.size = 0, outlier.alpha = 0, outlier.color = NA, outlier.fill = NA) +
    geom_point(aes(color=Risk, text = paste("Replicate: ", ReplicateName, "\nSample: ", Sample)) , alpha = 0.5, size = 1) +
    scale_color_risk(verbose = TRUE) +
    # facet_wrap(~ContaminantGroup, scales = "free", nrow = 1) +
    scale_y_continuous(n.breaks = 5) +
    ylab("Abundance = Area/TICA") +
    xlab("Contaminant")+
    theme_hd +
    # ggtitle("Abundance of contaminants") +
    rotate() #needed to rotate the boxplot

  if(scale == "linear"){return(output)}
  if(scale == "log10"){
    output <- output +
      scale_y_log10(n.breaks = 5) +
      ylab("Abundance = log10(Area/TICA)")
    return(output)}
}



#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_pseudochromatogram <- function(input_conta, scale = "linear"){
  # plot the abundance
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  output <-
    ggplot(input_conta, aes(x = PeptideRetentionTime, y = Abundance, color = Sample)) +
    geom_point(aes(text = paste("Replicate: ", ReplicateName, "\nSample: ", Sample,
                                "\nContaminant: ", Contaminant, "\nRisk: ", Risk)), alpha = 0.8, size = 1)  +
    scale_color_viridis_d(end=0.9, option =  "viridis", direction = 1) +
    facet_wrap(~ContaminantGroup, scales = "free", ncol = 1) +
    scale_x_continuous(n.breaks = 10,
                       limits = c(0,round(max(input_conta$PeptideRetentionTime, na.rm = TRUE)*1.1, 0))) +
    scale_y_continuous(n.breaks = 5) +
    ylab("Abundance") +
    xlab("Retention time (min)")+
    theme_hd
    # ggtitle("abundance vs. RT")

  if(scale == "linear"){return(output)}
  if(scale == "log10"){
    output <- output +
      scale_y_log10(n.breaks = 5) +
      ylab("log10(Abundance)")
    return(output)}
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_sample_risk_total <- function(input_conta_summ_sample, order_x = "Sample", scale = "linear"){
  # plot Total Abundance vs Replicate Name
  # order = c("Sample", "Abundance")
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  # arrange in function of Sample or Abundance_total, !!input$order_sample_total didn't work
  if(order_x == "Sample"){
    input_conta_summ_sample <-
      input_conta_summ_sample %>%
      arrange(Sample)
  }else{
    if(order_x == "Abundance"){
      input_conta_summ_sample <-
        input_conta_summ_sample %>%
        arrange(desc(Abundance_total))
    }else{
      stop("order_x must be Sample or Abundance")}
  }

  input_conta_summ_sample <-
    input_conta_summ_sample %>%
    mutate(Sample = factor(Sample, levels = unique(Sample)))

  output <- input_conta_summ_sample %>%
    # arrange(!! order_x) %>%
    # mutate(Sample = factor(Sample, levels = unique(Sample))) %>%
    mutate(ContaminantGroup = "Total") %>%
    ggplot(aes(y = Abundance_total, x = Sample, color = RiskLevel, size = Abundance_total)) +
    geom_point(alpha = 1) +
    scale_y_continuous(n.breaks = 10) +
    scale_color_risk() +
    ylab("Total Abundance") +
    xlab("Sample")+
    theme_hd +
    rotate_x_text(angle=90)
  # adapt scale
  if(scale == "linear"){return(output)}
  if(scale == "log10"){
    output <- output +
      scale_y_log10(n.breaks = 10) +
      ylab("log10(Total Abundance)")
    return(output)}
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_sample_risk_contaminant <- function(input_conta_summ_sample_risk,
                                     order_x = "Sample", order_y = "Abundance",
                                     show_zeros = FALSE){
  # plot Anaylte vs Sample, with color in function of risk and size in function of Abundance_median
  # order = c("Sample", "Abundance")
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  # remove all zero values
  if(show_zeros == FALSE){
    # remove zero values
    ContaminantGroup2Plot <-
      input_conta_summ_sample_risk %>%
      group_by(ContaminantGroup, Abundance_median) %>%
      summarise(Keep = all(Abundance_median > 0), .groups = "drop") %>%
      filter(Keep == TRUE) %>%
      pull(ContaminantGroup) %>%
      unique()
    input_conta_summ_sample_risk <-
      filter(input_conta_summ_sample_risk, ContaminantGroup %in% ContaminantGroup2Plot)
  }
  # arrange in function of Sample or Abundance, !!input$order_sample_total didn't work
  if(order_x == "Sample"){
    input_conta_summ_sample_risk <-
      input_conta_summ_sample_risk %>%
      arrange(Sample)
  }else{
    if(order_x == "Abundance"){
      input_conta_summ_sample_risk <-
        input_conta_summ_sample_risk %>%
        group_by(Sample) %>%
        arrange(desc(median(Abundance_median))) %>%
        ungroup()
    }else{
      stop("order_x must be Sample or Abundance")}
  }
  input_conta_summ_sample_risk <-
    input_conta_summ_sample_risk %>%
    mutate(Sample = factor(Sample, levels = unique(Sample)))
  # arrange in function of ContaminantGroup or Abundance_median, !!input$order_sample_total didn't work
  if(order_y == "Abundance"){
    input_conta_summ_sample_risk <-
      input_conta_summ_sample_risk %>%
      arrange(Abundance_median)
  }else{
    if(order_y == "ContaminantGroup"){
      input_conta_summ_sample_risk <-
        input_conta_summ_sample_risk %>%
        arrange(ContaminantGroup)
    }else{
      stop("order_y must be ContaminantGroup or Abundance")}
  }

  input_conta_summ_sample_risk <-
    input_conta_summ_sample_risk %>%
    mutate(ContaminantGroup = factor(ContaminantGroup, levels = unique(ContaminantGroup)))


  output <-
    input_conta_summ_sample_risk %>%
    ggplot(aes(y = ContaminantGroup, x = Sample, color = RiskLevel, size = Abundance_median)) +
    geom_point() +
    scale_color_risk() +
    ylab("Contaminant Group") +
    xlab("Sample")+
    theme_hd +
    theme(plot.margin = margin(4,4,4,10)) +
    rotate_x_text(angle = 90)
  return(output)
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_condition_risk_contaminant <- function(input_conta_summ_sample_risk,
                                        order_x = "Condition", order_y = "Abundance",
                                        show_zeros = FALSE){
  # plot Anaylte vs Condition, with color in function of risk and size in function of Abundance_median
  # order = c("Condition", "Abundance")
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  # remove all zero values
  if(show_zeros == FALSE){
    # remove zero values
    ContaminantGroup2Plot <-
      input_conta_summ_sample_risk %>%
      group_by(ContaminantGroup, Abundance_median) %>%
      summarise(Keep = all(Abundance_median > 0), .groups = "drop") %>%
      filter(Keep == TRUE) %>%
      pull(ContaminantGroup) %>%
      unique()
    input_conta_summ_sample_risk <-
      filter(input_conta_summ_sample_risk, ContaminantGroup %in% ContaminantGroup2Plot)
  }
  # arrange in function of Condition or Abundance, !!input$order_sample_total didn't work
  if(order_x == "Condition"){
    input_conta_summ_sample_risk <-
      input_conta_summ_sample_risk %>%
      arrange(Condition)
  }else{
    if(order_x == "Abundance"){
      input_conta_summ_sample_risk <-
        input_conta_summ_sample_risk %>%
        group_by(Condition) %>%
        arrange(desc(median(Abundance_median))) %>%
        ungroup()
    }else{
      stop("order_x must be Condition or Abundance")}
  }
  input_conta_summ_sample_risk <-
    input_conta_summ_sample_risk %>%
    mutate(Condition = factor(Condition, levels = unique(Condition)))
  # arrange in function of ContaminantGroup or Abundance_median, !!input$order_sample_total didn't work
  if(order_y == "Abundance"){
    input_conta_summ_sample_risk <-
      input_conta_summ_sample_risk %>%
      arrange(Abundance_median)
  }else{
    if(order_y == "ContaminantGroup"){
      input_conta_summ_sample_risk <-
        input_conta_summ_sample_risk %>%
        arrange(ContaminantGroup)
    }else{
      stop("order_y must be ContaminantGroup or Abundance")}
  }

  input_conta_summ_sample_risk <-
    input_conta_summ_sample_risk %>%
    mutate(ContaminantGroup = factor(ContaminantGroup, levels = unique(ContaminantGroup)))


  output <-
    input_conta_summ_sample_risk %>%
    ggplot(aes(y = ContaminantGroup, x = Condition, color = RiskLevel, size = Abundance_median)) +
    geom_point() +
    scale_color_risk() +
    ylab("Contaminant Group") +
    xlab("Condition")+
    theme_hd +
    theme(plot.margin = margin(4,4,4,10)) +
    rotate_x_text(angle = 90)
  return(output)
}


#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
layout_ggplotly_label_margin <- function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}

#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_conta_summ_sampleset <- function(df_conta){
  df_conta %>%
    arrange(desc(Risk)) %>%
    mutate(ypos = cumsum(Freq) - 0.5*Freq) %>%
    ggplot(aes(x = "", y= Freq , fill= Risk)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar("y", start=0) +
    geom_text(aes(y = ypos, label = Perc), color = "white", size=4) +
    theme_void()  +
    scale_fill_risk_level()
}


#' Plot functions for HowDirty
#'
#' diverse plot functions
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' bmi3(bmi.vals)
#'
#' @export
plot_condition_risk_total_boxplot <- function(input_conta_summ_sample,  scale = "linear", compare_means = TRUE, method ="wilcox.test",  ...){
  # plot the abundance
  # scale: changes the scale to linear or log10; options = c("linear", "log10")
  require(ggpubr)
  output <-
    ggplot(input_conta_summ_sample, aes(x = Condition, y = Abundance_total)) +
    geom_boxplot(alpha = 0.4, width = 0.5, size = 0.2,
                 outlier.shape = NA, outlier.size = 0, outlier.alpha = 0,
                 outlier.color = NA, outlier.fill = NA) +
    geom_point(aes(group = Condition, color = RiskLevel,
                   text = paste("Replicate: ", ReplicateName, "\nSample: ", Sample)),
               alpha = 0.5, size = 1) +
    scale_color_risk() +
    scale_y_continuous(n.breaks = 5) +
    xlab("Contaminant")+
    theme_hd


  if(compare_means == TRUE){
    # add stats
    if(any(method %in% c("anova", "kruskal.test"))){
      output <- output +
        ggpubr::stat_compare_means(method = method, size = 2.5, ...)
    }
    if(any(method %in% c("t.test", "wilcox.test"))){
      # get conditions to compare
      conditions_to_compare <-
        levels(input_conta_summ_sample$Condition) %>%
        combn(m = 2) %>%
        t() %>%
        split(.,seq(nrow(.)))
      output <- output +
        ggpubr::stat_compare_means(method = method,
                                   comparisons = conditions_to_compare,
                                   size = 2.5, ...)
    }
  }
  if(scale == "linear"){return(output)}
  if(scale == "log10"){
    output <- output +
      scale_y_log10(n.breaks = 5) +
      ylab("Abundance = log10(Area/TICA)")
    return(output)}
}
