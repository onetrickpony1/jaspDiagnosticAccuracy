#
# Copyright (C) 2026 onetrickpony1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Plots ----
.rocPlot <- function(jaspResults, options, ready) {
  if (!options[["rocPlot"]])
    return()
  if (!is.null(jaspResults[["rocPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("ROC Curve"), width = 520, height = 420)
  plot$dependOn(c(.rocDataDependencies, .rocCutoffDependencies, "cutoffTable", "rocPlot"))
  plot$position <- 5
  jaspResults[["rocPlot"]] <- plot

  if (!ready)
    return()

  results <- .rocGetFits(jaspResults)
  curves  <- list()
  points  <- list()
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem))
      next
    label  <- decodeColNames(marker)
    counts <- .rocAllCounts(info)
    se <- counts$tp / info$nPositive
    sp <- counts$tn / info$nNegative
    ord <- order(1 - sp, se)
    curves[[marker]] <- data.frame(marker = label, x = (1 - sp)[ord], y = se[ord])

    # The chosen cut-off is marked only when the cut-off table is on (its criterion is set there).
    if (isTRUE(options[["cutoffTable"]])) {
      chosen <- .rocSelectCutoff(info, options)
      if (nrow(chosen) > 0)
        points[[marker]] <- data.frame(marker = label, x = 1 - chosen$tn / info$nNegative,
                                       y = chosen$tp / info$nPositive)
    }
  }

  if (length(curves) == 0) {
    plot$setError(gettext("No marker has an ROC curve. See the footnotes of the AUC table."))
    return()
  }

  curveData <- do.call(rbind, curves)
  labels <- unique(curveData$marker)
  curveData$marker <- factor(curveData$marker, levels = labels)
  breaks <- seq(0, 1, by = 0.2)

  # The chance line is a segment from (0, 0) to (1, 1), so it stays inside the axes.
  p <- ggplot2::ggplot(curveData, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], colour = .data[["marker"]])) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_path(linewidth = 1)
  notes <- character(0)
  if (length(points) > 0) {
    pointData <- do.call(rbind, points)
    pointData$marker <- factor(pointData$marker, levels = labels)
    p <- p + ggplot2::geom_point(data = pointData, size = 3, shape = 21, fill = "white", stroke = 1.5)
    notes <- .rocCutoffPointNote(options)
  }
  notes <- c(notes, .rocNoCurveNote(results))
  p <- p +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Marker")) +
    jaspGraphs::scale_x_continuous(name = gettext("1 - Specificity"), breaks = breaks, limits = c(0, 1)) +
    jaspGraphs::scale_y_continuous(name = gettext("Sensitivity"), breaks = breaks, limits = c(0, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (length(labels) > 1) "right" else "none")
  if (length(labels) > 1) {
    room <- .rocLegendRoom(c(labels, gettext("Marker")))
    plot$width <- room$width
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(r = room$margin))
  }
  if (length(notes) > 0)
    p <- p + .rocPlotNote(notes)

  plot$plotObject <- p
}

# Room for the legend at the right of the ROC plot, for the legend title and labels in texts. JASP lays a plot out
# before it draws it: jaspBase's decodeplot builds the ggplot on a png() device, and writeImageJaspResults then draws
# it on a ragg device, whose text is wider (about 5% in the Linux Flatpak). A legend at the right edge is then cut off
# at the right. A right margin of 15% of the longest text, plus 6 points, takes up that difference. The plot also
# grows by the extra width of a text longer than 7 characters, so the panel keeps the size it has with short names.
# One plot pixel is one point (writeImageJaspResults draws width * ppi / 96 pixels at 72 * ppi / 96 per inch). A
# character is taken as 0.62 of the font size wide, about the mean of DejaVu Sans, the plot font in the Linux Flatpak.
# nchar(type = "width") counts wide characters (as in Chinese) twice.
# Returns list(width, margin): the plot width in pixels and the right margin in points.
.rocLegendRoom <- function(texts) {
  charWidth <- 0.62 * jaspGraphs::getGraphOption("fontsize")
  textWidth <- charWidth * max(7, nchar(texts, type = "width"), na.rm = TRUE)
  margin    <- ceiling(0.15 * textWidth) + 6
  extra     <- ceiling(1.15 * (textWidth - 7 * charWidth))
  list(width = 520 + extra, margin = margin)
}

# A number as typed: up to 15 significant digits, without trailing zeros (1234.5678 stays 1234.5678).
.rocTypedText <- function(x) {
  sprintf("%.15g", x)
}

# What the circles on the ROC plot mark.
.rocCutoffPointNote <- function(options) {
  if (options[["cutoffCriterion"]] == "threshold")
    return(gettextf("Circles mark each marker's point at the threshold %s.",
                    .rocTypedText(options[["thresholdValue"]])))
  # Only the Youden index and closest to (0, 1) can tie; the fixed criteria break ties (.rocSelectCutoff).
  if (options[["cutoffCriterion"]] %in% c("youden", "closestTopLeft"))
    return(gettextf("Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (%s). Tied cut-offs each get a circle.",
                    .rocCriterionLabel(options)))
  gettextf("Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (%s).",
           .rocCriterionLabel(options))
}

# Step curves of sensitivity and specificity against the threshold, for one marker.
# With pROC's rule (roc_utils_perfs in pROC/R/roc.utils.R), the counts at a threshold t depend only on which
# observed values lie on each side of t, so both curves are flat between neighbouring observed values and
# change only at an observed value. Each flat piece is evaluated at one threshold that gives its counts
# (.rocPieceThresholds: pROC's midpoint thresholds, and -Inf and Inf for the outer pieces). The outer pieces
# run to the ends of the x axis (.rocThresholdAxis).
# Returns a data frame with columns marker, threshold, value and measure, one path per measure. The threshold
# column is on the axis scale: the threshold minus the axis offset. The axis is in attr(, "axis").
.rocThresholdSteps <- function(fitInfo, options, label) {
  v <- sort(unique(c(fitInfo$cases, fitInfo$controls)))
  k <- length(v)
  probes <- .rocPieceThresholds(v, options[["direction"]])
  counts <- .rocCountsAtThreshold(fitInfo, probes, options)
  se <- counts$tp / fitInfo$nPositive
  sp <- counts$tn / fitInfo$nNegative

  # Path: start of the first piece, then at each observed value the level before and after the jump,
  # then the end of the last piece. The pieces start and end at the axis limits: the first and last axis break,
  # or a value beyond them (.rocThresholdAxis).
  axis <- .rocThresholdAxis(v)
  ends <- axis$limits
  x <- c(ends[1], rep(v - axis$offset, each = 2), ends[2])
  piece <- c(1, as.vector(rbind(seq_len(k), seq_len(k) + 1)), k + 1)
  steps <- rbind(
    data.frame(marker = label, threshold = x, value = se[piece], measure = gettext("Sensitivity")),
    data.frame(marker = label, threshold = x, value = sp[piece], measure = gettext("Specificity"))
  )
  attr(steps, "axis") <- axis
  steps
}

# The x axis of the threshold plot for the sorted unique values v, as in the test characteristics plot of
# jaspLearnBayes (R/LSbinaryclassification.R, .bcFillPlotTestCharacteristics, commit 700b1e7): breaks from
# jaspGraphs::getPrettyAxisBreaks on the range of the data, and the axis runs from the first to the last break.
# When the values span less than 1e-5 of their size, tick labels would need more than about 7 significant
# digits, and ggplot2 would show them alike (1.7e+12 for timestamps) or, below about 1e-13, collapse the axis
# to a line (scales::zero_range). The axis then shows the threshold minus a round offset, and says so.
# Returns list(offset, breaks, limits), on the axis scale (threshold minus offset). The limits are the range
# of the breaks, widened to the values when the cleaned breaks (.rocPrettyBreaks) stop just inside them.
.rocThresholdAxis <- function(v) {
  lim <- range(v)
  offset <- 0
  span <- lim[2] - lim[1]
  size <- max(abs(lim))
  if (is.finite(span) && span > 0 && span < 1e-5 * size) {
    # A multiple of a power of ten at least ten times the span, nearest the middle: the values then lie within
    # a few spans of it. Values this close lie within a factor 2 of each other and of the offset, so the
    # subtraction is exact.
    step <- 10^(ceiling(log10(span)) + 1)
    mid <- lim[1] / 2 + lim[2] / 2
    offset <- round(mid / step) * step
    # Near the largest double that multiple can round past it to Inf. Then round toward zero instead, and if
    # that also fails, use the smallest value.
    if (!is.finite(offset))
      offset <- trunc(mid / step) * step
    if (!is.finite(offset))
      offset <- lim[1]
    # The axis title states the offset with 15 significant digits (.rocOffsetText). The number subtracted is
    # that stated number, so the title is exact. Rounding to 15 digits can pass the largest double; rounding
    # to 14 cannot, because the largest double, 1.7976931348623157e308, rounds down at 14 digits.
    stated <- as.numeric(format(offset, digits = 15))
    offset <- if (is.finite(stated)) stated else as.numeric(format(offset, digits = 14))
  }
  breaks <- .rocPrettyBreaks(lim - offset)
  list(offset = offset, breaks = breaks, limits = range(breaks, lim - offset))
}

# The axis title or panel note for an offset: "Threshold \u2212 1e+15" (\u2212 is a minus sign). format() picks
# the shorter of the fixed and the scientific form, so 1.7e12 shows as 1.7e+12.
.rocOffsetText <- function(offset) {
  if (offset > 0)
    gettextf("Threshold \u2212 %s", format(offset, digits = 15))
  else
    gettextf("Threshold + %s", format(-offset, digits = 15))
}

# One threshold for each of the k + 1 pieces cut by the sorted unique values v: below v[1], between neighbours,
# above v[k]. Every threshold gives the counts of the open interval between its two neighbouring values.
# This follows pROC's roc_utils_thresholds (R/roc.utils.R): the midpoint, computed as a / 2 + b / 2 when
# (a + b) / 2 is above 1e100 in size or overflows. When two values are a few units in the last place apart, the
# midpoint can round onto one of them. pROC then moves it to the value that gives the same counts as the open
# interval: for "higher" (case positive when x >= t) the upper value, for "lower" (case positive when x <= t)
# the lower value. Here the same fix applies to any midpoint that is not strictly between its neighbours.
.rocPieceThresholds <- function(v, direction) {
  k <- length(v)
  if (k < 2)
    return(c(-Inf, Inf))
  a <- v[-k]
  b <- v[-1]
  mid <- (a + b) / 2
  huge <- !is.finite(mid) | abs(mid) > 1e100
  mid[huge] <- a[huge] / 2 + b[huge] / 2
  off <- !(mid > a & mid < b)
  mid[off] <- if (direction == "lower") a[off] else b[off]
  c(-Inf, mid, Inf)
}

.rocThresholdPlot <- function(jaspResults, options, ready) {
  if (!options[["thresholdPlot"]])
    return()
  if (!is.null(jaspResults[["thresholdPlot"]]))
    return()

  nMarkers <- max(1, length(.rocMarkers(options)))
  plot <- createJaspPlot(title = gettext("Sensitivity and Specificity by Threshold"), width = 520,
                         height = 80 + 300 * nMarkers)
  plot$dependOn(c(.rocDataDependencies, "thresholdPlot"))
  plot$position <- 6
  jaspResults[["thresholdPlot"]] <- plot

  if (!ready)
    return()

  results <- .rocGetFits(jaspResults)
  parts <- list()
  axes  <- list()
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem))
      next
    parts[[marker]] <- .rocThresholdSteps(info, options, decodeColNames(marker))
    axes[[marker]]  <- attr(parts[[marker]], "axis")
  }

  if (length(parts) == 0) {
    plot$setError(gettext("No marker has an ROC curve. See the footnotes of the AUC table."))
    return()
  }

  # With more than one marker each gets a full-width panel, one below the other (free x scales), so every panel
  # has room for the same pretty breaks as a single plot. A panel with an offset says so in its title.
  faceted <- length(parts) > 1
  panelLabels <- vapply(names(parts), function(marker) {
    label <- decodeColNames(marker)
    if (faceted && axes[[marker]]$offset != 0)
      label <- gettextf("%1$s (%2$s)", label, .rocOffsetText(axes[[marker]]$offset))
    label
  }, "")
  for (marker in names(parts))
    parts[[marker]]$marker <- panelLabels[[marker]]
  plotData <- do.call(rbind, parts)
  plotData$marker <- factor(plotData$marker, levels = unname(panelLabels))
  breaksList <- lapply(axes, `[[`, "breaks")

  # Each curve is a path with flat pieces between observed values and vertical jumps at them. The x axis of each
  # panel runs from its first to its last pretty break, or to a value just beyond (.rocThresholdAxis), and the
  # paths end there. The legend sits below the plot with a right margin on each label, and geom_rangeframe and
  # themeJaspRaw follow the test characteristics plot of jaspLearnBayes (R/LSbinaryclassification.R,
  # .bcPlotTestCharacteristics and .bcFillPlotTestCharacteristics, commit 700b1e7). A right plot margin leaves
  # room for half of the last tick label, at the right edge of the panel.
  xTitle <- gettext("Threshold")
  if (!faceted && axes[[1]]$offset != 0)
    xTitle <- .rocOffsetText(axes[[1]]$offset)
  xScale <- if (faceted) {
    # With free x scales, ggplot2 asks for the breaks of each panel by its range, which is the range of its
    # breaks, so .rocBreaksFor finds the panel's own breaks again.
    ggplot2::scale_x_continuous(name = xTitle, breaks = function(lim) .rocBreaksFor(lim, breaksList))
  } else {
    ggplot2::scale_x_continuous(name = xTitle, breaks = breaksList[[1]], limits = axes[[1]]$limits)
  }

  p <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[["threshold"]], y = .data[["value"]],
                                              colour = .data[["measure"]])) +
    ggplot2::geom_path(linewidth = 1) +
    jaspGraphs::scale_JASPcolor_discrete(name = NULL) +
    xScale +
    jaspGraphs::scale_y_continuous(name = gettext("Proportion"), breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "bottom") +
    ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")),
                   plot.margin = ggplot2::margin(r = 0.75, unit = "cm"))

  if (faceted)
    p <- p + ggplot2::facet_wrap(~ marker, ncol = 1, scales = "free_x")
  else
    p <- p + ggplot2::ggtitle(levels(plotData$marker))

  note <- .rocNoCurveNote(results)
  if (length(note) > 0)
    p <- p + .rocPlotNote(note)

  plot$plotObject <- p
}

# Pretty axis breaks (jaspGraphs::getPrettyAxisBreaks, which calls pretty(), with its default n = 5). When the
# tick labels would not fit side by side in a panel 520 pixels wide (about 48 characters, with two characters
# of space per label; values such as 1e300 give labels like -3e+300), fewer breaks are asked for. For a range
# near the largest double, pretty() can give infinite breaks; the ends of the range are used then.
.rocPrettyBreaks <- function(lim) {
  for (n in 5:2) {
    breaks <- try(jaspGraphs::getPrettyAxisBreaks(lim, n = n), silent = TRUE)
    if (isTryError(breaks) || length(breaks) < 2 || !all(is.finite(breaks)))
      return(lim)
    breaks <- .rocCleanBreaks(breaks)
    if (sum(nchar(format(breaks, trim = TRUE))) + 2 * length(breaks) <= 48)
      break
  }
  breaks
}

# pretty() can return breaks with rounding noise: for a range from -1e-14 to 1 it gives -1e-14 and
# 0.19999999999999202, which print as "-1e-14" and "2e-01". A break within a millionth of a step of zero
# becomes 0, and the breaks are rounded to the decimals of the step (0.2: one decimal), so they print as plain
# numbers. Breaks with a step of 1 or more are left as they are, apart from the zero.
.rocCleanBreaks <- function(breaks) {
  step <- stats::median(diff(breaks))
  if (!is.finite(step) || step <= 0)
    return(breaks)
  breaks[abs(breaks) < 1e-6 * step] <- 0
  decimals <- -floor(log10(step))
  if (decimals > 0)
    breaks <- round(breaks, decimals)
  breaks + 0  # turns -0 into 0
}

# The breaks, out of a list of break vectors, whose first and last values lie closest to the ends of lim.
.rocBreaksFor <- function(lim, breaksList) {
  distance <- vapply(breaksList, function(b) abs(b[1] - lim[1]) + abs(b[length(b)] - lim[2]), numeric(1))
  breaksList[[which.min(distance)]]
}

# One sentence naming the markers that have no ROC curve, or character(0).
.rocNoCurveNote <- function(results) {
  missing <- Filter(function(marker) !is.null(results$fits[[marker]]$problem), results$markers)
  if (length(missing) == 0)
    return(character(0))
  labels <- paste(vapply(missing, decodeColNames, ""), collapse = ", ")
  sprintf(ngettext(length(missing),
                   "No curve for %s: see the AUC table.",
                   "No curves for %s: see the AUC table."),
          labels)
}

# A note under a plot, left-aligned, as a ggplot2 caption. A plot has no footnotes, so notes go in the caption,
# as in the raincloud plots of jaspDescriptives (R/raincloudPlots.R, commit e3595b5).
.rocPlotNote <- function(lines) {
  text <- paste(vapply(lines, function(x) paste(strwrap(x, width = 60), collapse = "\n"), ""), collapse = "\n")
  list(ggplot2::labs(caption = text),
       ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)))
}
