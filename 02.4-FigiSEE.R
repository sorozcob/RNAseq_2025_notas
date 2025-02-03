## The following list of commands will generate the plots created in iSEE
## Copy them into a script or an R session containing your SingleCellExperiment.
## All commands below refer to your SingleCellExperiment object as `se`.

se <- sce_layer
colormap <- ExperimentColorMap()
se <- iSEE::cleanDataset(se)
colormap <- synchronizeAssays(colormap, se)
all_contents <- list()

################################################################################
# Defining brushes
################################################################################

all_active <- list()
all_active[['ReducedDimensionPlot1']] <- list()
all_active[['FeatureAssayPlot1']] <- list()
all_active[['ColumnDataPlot1']] <- list()
all_active[['RowDataPlot1']] <- list()
all_active[['SampleAssayPlot1']] <- list()

################################################################################
## Reduced dimension plot 1
################################################################################


red.dim <- reducedDim(se, "PCA");
plot.data <- data.frame(X=red.dim[, 1], Y=red.dim[, 2], row.names=colnames(se));

plot.data$ColorBy <- colData(se)[, "layer_guess_reordered_short"];

# Avoid visual biases from default ordering by shuffling the points
set.seed(76);
plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];

dot.plot <- ggplot() +
  geom_point(aes(x=X, y=Y, color=ColorBy), alpha=0.6, plot.data, size=3) +
  labs(x="Dimension 1", y="Dimension 2", color="layer_guess_reordered_short", title="PCA") +
  coord_cartesian(xlim=range(plot.data$X, na.rm=TRUE),
                  ylim=range(plot.data$Y, na.rm=TRUE), expand=TRUE) +
  scale_color_manual(values=colDataColorMap(colormap, "layer_guess_reordered_short", discrete=TRUE)(7), na.value='grey50', drop=FALSE) +
  scale_fill_manual(values=colDataColorMap(colormap, "layer_guess_reordered_short", discrete=TRUE)(7), na.value='grey50', drop=FALSE) +
  guides(colour = guide_legend(override.aes = list(size=1)), fill = guide_legend(override.aes = list(size=1))) +
  theme_bw() +
  theme(legend.position='bottom', legend.box='vertical', legend.text=element_text(size=9), legend.title=element_text(size=11),
        axis.text=element_text(size=10), axis.title=element_text(size=12), title=element_text(size=12))

# Saving data for transmission
all_contents[['ReducedDimensionPlot1']] <- plot.data

################################################################################
## Row data table 1
################################################################################


tab <- as.data.frame(rowData(se));

# Saving data for transmission
all_contents[['RowDataTable1']] <- tab

################################################################################
## Feature assay plot 1
################################################################################


plot.data <- data.frame(Y=assay(se, "logcounts")["ENSG00000243485", ], row.names=colnames(se))
plot.data$X <- factor(character(ncol(se)))

plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- iSEE::jitterViolinPoints(plot.data$X, plot.data$Y,
                                                width=0.4, varwidth=FALSE, adjust=1,
                                                method='quasirandom', nbins=NULL);

# Avoid visual biases from default ordering by shuffling the points
set.seed(76);
plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];

dot.plot <- ggplot() +
  geom_violin(aes(x=X, y=Y, group=GroupBy), alpha=0.2, data=plot.data, scale='width', width=0.8) +
  geom_point(aes(y=Y, x=jitteredX), alpha=1, plot.data, color='#000000', size=1) +
  labs(x="", y="ENSG00000243485 (logcounts)", title="ENSG00000243485") +
  coord_cartesian(ylim=range(plot.data$Y, na.rm=TRUE), expand=TRUE) +
  scale_x_discrete(drop=FALSE) +
  theme_bw() +
  theme(legend.position='bottom', legend.text=element_text(size=9),
        legend.title=element_text(size=11), legend.box='vertical',
        axis.text.x=element_text(angle=90, size=10, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=12), title=element_text(size=12))

# Saving data for transmission
all_contents[['FeatureAssayPlot1']] <- plot.data

################################################################################
## Column data plot 1
################################################################################


plot.data <- data.frame(Y=colData(se)[, "sample_name"], row.names=colnames(se));
plot.data$X <- factor(character(ncol(se)))

plot.data[["Y"]] <- factor(plot.data[["Y"]]);

set.seed(100);
j.out <- iSEE:::jitterSquarePoints(plot.data$X, plot.data$Y);
summary.data <- j.out$summary;
plot.data$jitteredX <- j.out$X;
plot.data$jitteredY <- j.out$Y;

# Avoid visual biases from default ordering by shuffling the points
set.seed(76);
plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];

dot.plot <- ggplot(plot.data) +
  geom_tile(aes(x=X, y=Y, height=2*YWidth, width=2*XWidth, group=interaction(X, Y)),
            summary.data, color='black', alpha=0, size=0.5) +
  geom_point(aes(x=jitteredX, y=jitteredY), alpha=1, plot.data, color='#000000', size=1) +
  labs(x="", y="sample_name", title="sample_name ") +
  scale_x_discrete(drop=FALSE) +
  scale_y_discrete(drop=FALSE) +
  theme_bw() +
  theme(legend.position='bottom', legend.text=element_text(size=9),
        legend.title=element_text(size=11), legend.box='vertical',
        axis.text.x=element_text(angle=90, size=10, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=12), title=element_text(size=12))

# Saving data for transmission
all_contents[['ColumnDataPlot1']] <- plot.data

################################################################################
## Row data plot 1
################################################################################


plot.data <- data.frame(Y=rowData(se)[, "source"], row.names=rownames(se));
plot.data$X <- factor(character(nrow(se)))

set.seed(100);
j.out <- iSEE:::jitterSquarePoints(plot.data$X, plot.data$Y);
summary.data <- j.out$summary;
plot.data$jitteredX <- j.out$X;
plot.data$jitteredY <- j.out$Y;

# Avoid visual biases from default ordering by shuffling the points
set.seed(22331);
plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];

dot.plot <- ggplot(plot.data) +
  geom_tile(aes(x=X, y=Y, height=2*YWidth, width=2*XWidth, group=interaction(X, Y)),
            summary.data, color='black', alpha=0, size=0.5) +
  geom_point(aes(x=jitteredX, y=jitteredY), alpha=1, plot.data, color='#000000', size=1) +
  labs(x="", y="source", title="source ") +
  scale_x_discrete(drop=FALSE) +
  scale_y_discrete(drop=FALSE) +
  theme_bw() +
  theme(legend.position='bottom', legend.text=element_text(size=9),
        legend.title=element_text(size=11), legend.box='vertical',
        axis.text.x=element_text(angle=90, size=10, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=12), title=element_text(size=12))

# Saving data for transmission
all_contents[['RowDataPlot1']] <- plot.data

################################################################################
## Sample assay plot 1
################################################################################


plot.data <- data.frame(Y=assay(se, "logcounts")[,"151507_Layer1"], row.names=rownames(se));
plot.data$X <- factor(character(nrow(se)));

plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- iSEE::jitterViolinPoints(plot.data$X, plot.data$Y,
                                                width=0.4, varwidth=FALSE, adjust=1,
                                                method='quasirandom', nbins=NULL);

# Avoid visual biases from default ordering by shuffling the points
set.seed(22331);
plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];

dot.plot <- ggplot() +
  geom_violin(aes(x=X, y=Y, group=GroupBy), alpha=0.2, data=plot.data, scale='width', width=0.8) +
  geom_point(aes(y=Y, x=jitteredX), alpha=1, plot.data, color='#000000', size=1) +
  labs(x="", y="151507_Layer1 (logcounts)", title="151507_Layer1") +
  coord_cartesian(ylim=range(plot.data$Y, na.rm=TRUE), expand=TRUE) +
  scale_x_discrete(drop=FALSE) +
  theme_bw() +
  theme(legend.position='bottom', legend.text=element_text(size=9),
        legend.title=element_text(size=11), legend.box='vertical',
        axis.text.x=element_text(angle=90, size=10, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=12), title=element_text(size=12))

# Saving data for transmission
all_contents[['SampleAssayPlot1']] <- plot.data

################################################################################
## Column data table 1
################################################################################


tab <- as.data.frame(colData(se));

# Saving data for transmission
all_contents[['ColumnDataTable1']] <- tab

################################################################################
## Complex heatmap 1
################################################################################


.chosen.rows <- c("ENSG00000168314", "ENSG00000183036", "ENSG00000197971");
.chosen.columns <- colnames(se);
plot.data <- assay(se, "logcounts")[.chosen.rows, .chosen.columns, drop=FALSE]
plot.data <- as.matrix(plot.data);

plot.data <- plot.data - rowMeans(plot.data)
plot.data <- plot.data / apply(plot.data, 1, sd)

.assay_colors <- c("blue", "white", "orange")
.assay_colors <- circlize::colorRamp2(breaks = c(-2.17978728210008, 0, 2.66762445840324), colors = .assay_colors)

# Keep all samples to compute the full range of continuous annotations
.column_data <- colData(se)[, "layer_guess_reordered_short", drop=FALSE]
.column_data[["Selected points"]] <- iSEE::multiSelectionToFactor(list(), colnames(se))

.column_col <- list()

.color_values <- .column_data[["layer_guess_reordered_short"]]
.color_values <- setdiff(unique(.color_values), NA)
.col_colors <- colDataColorMap(colormap, "layer_guess_reordered_short", discrete=TRUE)(length(.color_values))
if (is.null(names(.col_colors))) names(.col_colors) <- levels(factor(.color_values))
.column_col[["layer_guess_reordered_short"]] <- .col_colors

.column_col[["Selected points"]] <- iSEE::columnSelectionColorMap(colormap, levels(.column_data[["Selected points"]]))

.column_data <- .column_data[colnames(plot.data), , drop=FALSE]
.column_data <- as.data.frame(.column_data, optional=TRUE)
.column_annot_order <- order(.column_data[["Selected points"]], .column_data[["layer_guess_reordered_short"]])
.column_data <- .column_data[.column_annot_order, , drop=FALSE]
plot.data <- plot.data[, .column_annot_order, drop=FALSE]
.column_annot <- ComplexHeatmap::columnAnnotation(df=.column_data, col=.column_col, annotation_legend_param=list(direction="horizontal", nrow=10))

hm <- ComplexHeatmap::Heatmap(matrix=plot.data, col=.assay_colors,
                              top_annotation=.column_annot, cluster_rows=TRUE,
                              clustering_distance_rows="spearman", clustering_method_rows="ward.D2",
                              cluster_columns=FALSE, name="logcounts (centered, scaled)",
                              show_row_names=TRUE, show_column_names=TRUE,
                              row_names_gp=grid::gpar(fontsize=10),
                              column_names_gp=grid::gpar(fontsize=5),
                              heatmap_legend_param=list(direction="horizontal"))

ComplexHeatmap::draw(hm, heatmap_legend_side="bottom", annotation_legend_side="bottom")

################################################################################
## To guarantee the reproducibility of your code, you should also
## record the output of sessionInfo()
sessionInfo()
