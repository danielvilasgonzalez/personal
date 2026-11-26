# Four-panel figure: (A, B) GA convergence of NLL; (C, D) contribution of data and parameter types to the fall in NLL.
# Needs, from the run script: p_panel1, p_panel2 (convergence panels), PLOTS; and contrib_panels (A, B, lo, hi, brk) created by "fig contribution and parameters phase6.R".
suppressMessages({library(ggplot2); library(patchwork)})
stopifnot(exists("p_panel1"), exists("p_panel2"), exists("contrib_panels"), exists("PLOTS"))

SQUARE <- FALSE  # TRUE: all four panels square (aspect ratio 1); FALSE: panels fill the grid
ar  <- if(SQUARE) theme(aspect.ratio = 1) else theme(aspect.ratio = NULL)
BORD <- "grey20"; BW <- 0.55   # same colour and stroke for every panel contour, tick and colour-bar frame
same <- theme(panel.border = element_rect(colour = BORD, fill = NA, linewidth = BW),
              axis.ticks = element_line(colour = BORD, linewidth = BW * 0.6),
              axis.title.y = element_text(margin = margin(r = 6)), axis.title.y.right = element_text(margin = margin(l = 6)),
              axis.title.x = element_text(margin = margin(t = 6)))
tag_th <- theme(plot.tag = element_text(face = "bold", size = 23), plot.tag.position = c(0.01, 0.99))
mg  <- theme(plot.margin = margin(14, 5.5, 5.5, 8))
# one text size for every component: tick labels, axis titles, legend text and in-plot labels
TXT <- 10.8; TTL <- 12; BASE_TXT <- 8.5
fix_vec <- function(p){ el <- p$theme$axis.text.y; if(!is.null(el) && length(el$size) > 1) el$size <- ifelse(el$size > 8, TXT, BASE_TXT) else if(!is.null(el)) el$size <- TXT; p + theme(axis.text.y = el) }
set_text <- function(p, pt){ for(i in seq_along(p$layers)) if(inherits(p$layers[[i]]$geom, "GeomText")) p$layers[[i]]$aes_params$size <- pt / ggplot2::.pt; p }
txt_th <- theme(axis.text = element_text(size = TXT), axis.text.x = element_text(size = TXT), axis.text.y.right = element_text(size = TXT, colour = "black"),
                axis.title = element_text(size = TTL), legend.text = element_text(size = TXT))
conv1 <- fix_vec(p_panel1) + ar + mg + coord_cartesian(clip = "off")   # clip off: the frame is not half-hidden by the panel
conv2 <- fix_vec(p_panel2) + ar + mg + coord_cartesian(clip = "off")
pC <- set_text(contrib_panels$A, TXT) + ar
pD <- set_text(contrib_panels$B, TXT) + ar

BAR_GAP <- 0.03; BAR_W <- 0.06   # bar position and width, as fractions of the width of panel D
if(is.finite(contrib_panels$lo)){   # vertical NLL colour bar of C and D, on the right of the C/D row
  lo <- contrib_panels$lo; hi <- contrib_panels$hi
  bar_v <- ggplot(data.frame(y = seq(lo, hi, length.out = 300)), aes(1, y, fill = y)) + geom_raster() +
    scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(lo, hi), guide = "none") +
    scale_y_continuous(breaks = contrib_panels$brk, labels = function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE),
                       expand = c(0, 0), position = "right") +
    scale_x_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    labs(x = NULL, y = "NLL") +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 9.6), axis.title.y.right = element_text(face = "bold", angle = 90, margin = margin(l = 6)),
          axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0), plot.background = element_blank())
  # the colour bar is drawn inside panel D's right margin (data coordinates, clip off), so B and D stay in the same grid column
  # and keep exactly the same width and position
  pp <- ggplot_build(pD)$layout$panel_params[[1]]
  xr <- pp$x.range; yr <- pp$y.range; xs <- diff(xr)
  nb <- 200; cols_b <- viridisLite::viridis(nb, option = "plasma", direction = -1)
  yb <- seq(yr[1], yr[2], length.out = nb)
  x0 <- xr[2] + BAR_GAP * xs; x1 <- x0 + BAR_W * xs
  brk <- contrib_panels$brk; ybrk <- yr[1] + (brk - lo) / (hi - lo) * diff(yr)
  pD <- pD + coord_cartesian(xlim = xr, ylim = yr, expand = FALSE, clip = "off") +
    annotate("rect", xmin = x0, xmax = x1, ymin = yb[-nb], ymax = yb[-1], fill = cols_b[-nb], colour = NA) +
    annotate("rect", xmin = x0, xmax = x1, ymin = yr[1], ymax = yr[2], fill = NA, colour = BORD, linewidth = BW) +
    annotate("segment", x = x1, xend = x1 + 0.012 * xs, y = ybrk, yend = ybrk, colour = BORD, linewidth = BW * 0.6) +
    annotate("text", x = x1 + 0.02 * xs, y = ybrk, label = format(round(brk), big.mark = ",", trim = TRUE, scientific = FALSE),
             hjust = 0, size = TXT / .pt, colour = "black") +
    annotate("text", x = x1 + 0.27 * xs, y = mean(yr), label = "NLL", angle = 90, vjust = 0.5, fontface = "bold", size = TTL / .pt) +
    theme(plot.margin = margin(14, 78, 5.5, 8))
  # give the panels of the same column the same space on the left and on the right of the panel, so A-C and B-D have identical width and position
  ext <- function(p){ g <- ggplotGrob(p); k <- which(g$layout$name == "panel")[1]
    w <- function(i) if(length(i)) sum(sapply(g$widths[i], function(x) grid::convertWidth(x, "pt", valueOnly = TRUE))) else 0
    c(l = w(seq_len(g$layout$l[k] - 1)), r = w(seq(g$layout$r[k] + 1, length(g$widths)))) }
  padto <- function(p, e, target){ m <- p$theme$plot.margin; if(is.null(m)) m <- margin(5.5, 5.5, 5.5, 5.5)
    p + theme(plot.margin = margin(as.numeric(m[1]), as.numeric(m[2]) + target["r"] - e["r"], as.numeric(m[3]), as.numeric(m[4]) + target["l"] - e["l"])) }
  eA <- ext(conv1); eB <- ext(conv2); eC <- ext(pC); eD <- ext(pD)
  tL <- pmax(eA, eC); tR <- pmax(eB, eD)
  conv1 <- padto(conv1, eA, tL); pC <- padto(pC, eC, tL); conv2 <- padto(conv2, eB, tR); pD <- padto(pD, eD, tR)
  fig4 <- (conv1 + conv2) / (pC + pD) + plot_annotation(tag_levels = list(c("A", "B", "C", "D"))) & tag_th & same & txt_th
} else {
  fig4 <- (conv1 + conv2) / (pC + pD) + plot_annotation(tag_levels = list(c("A", "B", "C", "D"))) & tag_th & same & txt_th
}
f4 <- file.path(PLOTS, "ts", "Fig_NLL_convergence_and_contribution")
ggsave(paste0(f4, ".pdf"), fig4, width = 11.6, height = 7.6, device = cairo_pdf)
ggsave(paste0(f4, ".png"), fig4, width = 11.6, height = 7.6, dpi = 300, bg = "white")
message("saved ", basename(f4), ".png / .pdf")
