


#' plot_state_curves
#'
#' @param vacc_data 
#' @param states 
#' @param nstates_sample 
#'
#' @return
#' 
#' @import ggplot2
#' @export
#'
#' @examples
plot_state_curves <- function(vacc_data = vacc_data_10yr, 
                              states = NULL, 
                              nstates_sample = 12){
    
    if (is.null(states)){
        usps_samp <- sample(unique(vacc_data$USPS), nstates_sample, replace = FALSE)
    } else {
        usps_samp <- states
    }
    print(vacc_data %>% dplyr::filter(USPS %in% usps_samp) %>%
        dplyr::arrange(prop_vacc_age) %>%
        dplyr::mutate(USPS = forcats::fct_reorder(USPS, (prop_vacc_age))) %>%
        ggplot2::ggplot(aes(x=date, y=prop_vacc_age, color=age_group, group=age_group)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(aes(shape=source, fill=source)) +
        ggplot2::scale_shape_manual("Source", values = c(21, 23)) +
        ggplot2::scale_fill_manual("Source", values=c("orange","lightblue4")) +
        ggplot2::scale_color_discrete("Age group, yrs") +
        ggplot2::xlab("State") + 
        ggplot2::ylab("proportion vaccinated") +
        ggplot2::ggtitle("Proportion Vaccinated, By State and Age") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::facet_wrap(~USPS))

}


#' save_pdf_state_curves
#'
#' @param vacc_data 
#' @param pdf_filename 
#'
#' @return
#' @export
#'
#' @examples
save_pdf_state_curves <- function(vacc_data = vacc_data_10yr, 
                                  pdf_filename = paste0("figures/prop_vacc_by_age10yr_", Sys.Date(), ".pdf")){
    
    states_ <- sort(unique(vacc_data$USPS))
    pages_ <- ceiling(length(states_)/9)
    state_groups <- sort(rep(1:pages_, 9))[1:length(states_)]
    
    pdf(pdf_filename, height=11, width = 8.5)
    for (i in 1:pages_){
        state_tmp <- states_[state_groups==i]
        plot_state_curves(vacc_data = vacc_data, 
                                      states = state_tmp, 
                                      nstates_sample = NULL)
    }
    dev.off()
}