nM<-M/rowSums(M)


hM<-Heatmap(M,
  name = "Mixture test", # Name of the heatmap legend
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  show_row_names = FALSE,
  show_column_names = FALSE,
  heatmap_legend_param = list(title = "Value"), # Customize legend title
  col = colorRamp2::colorRamp2(c(0, 0, max(M)),
  c("white", "white", "red")), # Optional: custom color scheme
  column_title = "mixture"
)



friends<-friends.test::friends.test.bic(M,prior.to.have.friends = 0.001)


hmask<-Heatmap(mask,
             name = "mask", # Name of the heatmap legend
             cluster_rows = FALSE,
             cluster_columns = FALSE,
             show_row_names = FALSE,
             show_column_names = FALSE,
             col = colorRamp2::colorRamp2(c(0, 0, 1),
                                          c("lightgrey", "lightgrey", "red")), # Optional: custom color scheme
             column_title = "mask",
             show_heatmap_legend = FALSE
)


friends.mat<-matrix(0,nrow=nrow(M),ncol=ncol(M))
rownames(friends.mat)<-rownames(M)
colnames(friends.mat)<-colnames(M)

for(r in seq(nrow(friends))){
  friends.mat[friends[r,"marker"],friends[r,"friend"]]=1
}

hfb<-Heatmap(friends.mat,
            name = "Friends", # Name of the heatmap legend
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            show_row_names = FALSE,
            show_column_names = FALSE,
            col = colorRamp2::colorRamp2(c(0, 0, 1),
                          c("white", "white", "orange")), # Optional: custom color scheme
            column_title = "friends.test.bic",
            show_heatmap_legend = FALSE
)

pdf("mixture_and_bic.pdf")
plot(hM+hmask+hfb)
dev.off()



