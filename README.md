# supFTSVD Vignette


# Introduction

This R package is built for a supervised low-rank representation of
high-dimensional multivariate functional data. The methodology is
proposed in Alam et. al. (2024+). Note that the package is still in
developing stage.

# Installation

To install the package, run the following codes

``` r
devtools::install_github("https://github.com/msalam14/supFTSVD")
```

- This vignette analyzes the food and resulting microbial metabolite
  (FARMM) data (Tanes et al. 2021). The data set used in this
  illustration can be downloaded from
  https://github.com/syma-research/microTensor/tree/main/data/FARMM.

- Calling necessary R-packages

<!-- -->

- Loading the data files

``` r
data_dir<-"data/FARMM/"
load(paste(data_dir,"mat_count.RData",sep="")) # meta data
load(paste(data_dir,"df_samples.RData",sep="")) # OTU data 
count_all<-t(mat_count)
meta_all<-df_samples
```

- Extracting the meta data

``` r
meta_uni<-meta_all %>%
  dplyr::select(SubjectID,study_group,BMI,Age) %>%
  distinct_all() %>%
  mutate(Vegan=as.numeric(study_group=="Vegan"),
         EEN=as.numeric(study_group=="EEN"),
         Omnivore=as.numeric(study_group=="Omnivore"))
```

- Data preparation

``` r
datlist_all <- format_ftsvd(count_all, meta_all$study_day,meta_all$SubjectID,threshold=0.95, pseudo_count=0.5,transform='clr')
```

- Preparation of baseline covariates to supervised the low-rank
  decomposition

``` r
resVEC<-model.matrix(~.-1,data = meta_uni[,-c(1,2)])
resVEC_age<-model.matrix(~.-1,data = meta_uni[,-c(1,2,3)])
resVEC_bmi<-model.matrix(~.-1,data = meta_uni[,-c(1,2,4)])
resVEC_diet<-model.matrix(~.-1,data = meta_uni[,-c(1,2,5,6,7)])
```

- Fitting Supervised FTSVD

``` r
pt<-proc.time()
set.seed(75)
sup_res_ftsvd <- supFTSVD(datlist = svd_all$datlist,response = as.matrix(resVEC), r = 6, resolution = 51,CVPhi = TRUE,K=5,smooth=exp(seq(-5,1,length.out=10)),maxiter = 150)
proc.time()-pt
```

- Accumulated $R^2$ for different low-rank components

``` r
sup_res_ftsvd$accum.r.square
```

    [1] 0.2490332 0.3129319 0.3512570 0.3761869 0.4082605 0.4323625

- Different model components

  - Subject loading function

``` r
slplot1<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==1) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 1")

slplot2<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==2) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 2")

slplot3<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==3) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 3")

slplot4<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==4) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 4")
slplot5<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==5) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 5")

slplot6<-data.frame(sup_res_ftsvd$A.hat,meta_uni) %>%
  pivot_longer(Component.1:Component.6,names_to = "Component",names_prefix = "Component.",values_to = "Loadings") %>%
  filter(Component==6) %>%
  ggplot(aes(study_group,Loadings)) +
  geom_boxplot()+
  xlab("Diet group")+
  ylab("Subject loadings") +
  ggtitle("Component 6")
```

``` r
ggarrange(slplot1,slplot2,slplot3,slplot4,slplot5,slplot6,ncol=3)
```

    $`1`

![Subject loading corresponding to the six
components](README_files/figure-commonmark/unnamed-chunk-12-1.png)


    $`2`

![Subject loading corresponding to the six
components](README_files/figure-commonmark/unnamed-chunk-12-2.png)


    attr(,"class")
    [1] "list"      "ggarrange"

- Singular functions

``` r
ISF<-sapply(1:ncol(sup_res_ftsvd$A.hat), function(u){
  as.numeric(t(outer(sup_res_ftsvd$A.hat[,u],sup_res_ftsvd$Phi.hat[,u])))
})

colnames(ISF)<-paste("Comp",seq_len(ncol(ISF)),sep = "")
ind_singF<-data.frame(Subject=rep(1:nrow(sup_res_ftsvd$A.hat),each=length(sup_res_ftsvd$time)),Days=rep(sup_res_ftsvd$time,times=nrow(sup_res_ftsvd$A.hat)),Group=rep(meta_uni$study_group,each=length(sup_res_ftsvd$time)),ISF)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

grp_singF<-ind_singF %>%
  pivot_longer(Comp1:Comp6,names_to = "Component",values_to = "IndF") %>%
  group_by(Component,Days,Group) %>%
  summarise(Mval=mean(IndF),Sdval=sd(IndF),Num=n()) %>%
  mutate(SEval=Sdval/sqrt(Num)) %>%
  ungroup() 
```

    `summarise()` has grouped output by 'Component', 'Days'. You can override using
    the `.groups` argument.

``` r
singF1<-grp_singF %>%
  filter(Component=="Comp1") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.80,0.80),legend.background = element_rect(fill = "transparent"),legend.direction = "vertical",legend.title = element_blank())
```

    Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()` instead.

``` r
  ggtitle("Component 1")
```

    $title
    [1] "Component 1"

    attr(,"class")
    [1] "labels"

``` r
singF2<-grp_singF %>%
  filter(Component=="Comp2") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.8,0.30),legend.background = element_rect(fill = "transparent"),legend.direction = "vertical",legend.title = element_blank())+
  ggtitle("")
singF3<-grp_singF %>%
  filter(Component=="Comp3") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.8,0.30),legend.background = element_rect(fill = "transparent"),legend.direction = "vertical",legend.title = element_blank())+
  ggtitle("Component 3")
singF4<-grp_singF %>%
  filter(Component=="Comp4") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.35,0.15),legend.background = element_rect(fill = "transparent"),legend.direction = "horizontal",legend.title = element_blank())+
  ggtitle("")
singF5<-grp_singF %>%
  filter(Component=="Comp5") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.8,0.30),legend.background = element_rect(fill = "transparent"),legend.direction = "vertical",legend.title = element_blank())+
  ggtitle("Component 5")
singF6<-grp_singF %>%
  filter(Component=="Comp6") %>%
  ggplot(aes(x=Days,y=Mval,group=Group,color=Group)) +
  geom_errorbar(aes(ymin=Mval-(2*SEval),ymax=Mval+(2*SEval)),colour="gray", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Study day") +
  ylab("Group average loading") +
  theme(legend.position = c(0.8,0.78),legend.background = element_rect(fill = "transparent"),legend.direction = "vertical",legend.title = element_blank())+
  ggtitle("")
```

``` r
ggarrange(singF1,singF2,singF3,singF4,singF5,singF6,ncol=3)
```

    $`1`

![Subject loading corresponding to the six
components](README_files/figure-commonmark/unnamed-chunk-14-1.png)


    $`2`

![Subject loading corresponding to the six
components](README_files/figure-commonmark/unnamed-chunk-14-2.png)


    attr(,"class")
    [1] "list"      "ggarrange"

- Feature loading

``` r
source("miscellaneous/tax_feat_name_sep.R")
pattern_vec<-c("k","p","c","o","f","g","s")
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,1])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,1])),1][1:10]))
taxa1<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("")+
  ggtitle("Component 1")
```

``` r
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,2])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,2])),2][1:10]))
taxa2<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("") +
  ggtitle("")
```

``` r
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,3])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,3])),3][1:10]))
taxa3<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("") +
  ggtitle("Component 3")
```

``` r
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,4])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,4])),4][1:10]))
taxa4<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("") +
  ggtitle("")
```

``` r
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,5])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,5])),5][1:10]))
taxa5<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("") +
  ggtitle("Component 5")
```

``` r
tax_dat<-do.call(rbind,lapply(rownames(sup_res_ftsvd$B.hat), function(u){tax_feat_sep(u,pattern_vec=pattern_vec)}))
top_feat<-sapply(str_split(tax_dat[order(-abs(sup_res_ftsvd$B.hat[,6])),7][1:10],"_"),function(u){paste(u[1],u[2])})
tf_dat<-data.frame(TFname=top_feat,TFloadings=
as.numeric(sup_res_ftsvd$B.hat[order(-abs(sup_res_ftsvd$B.hat[,6])),6][1:10]))
taxa6<-tf_dat %>%
  ggplot(aes(x=TFloadings,y=reorder(TFname,TFloadings))) + 
  geom_bar(stat = "identity") +
  xlab("Feature loading") +
  ylab("") +
  ggtitle("")
```

``` r
ggpubr::ggarrange(slplot2,taxa2,singF2,
                  slplot4,taxa4,singF4,
                  slplot6,taxa6,singF6,nrow=3,ncol=3)
```

![](README_files/figure-commonmark/unnamed-chunk-21-1.png)

``` r
ggsave("farm_res.pdf",height = 10,width = 12,units = "in")
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-tanes2021role" class="csl-entry">

Tanes, Ceylan, Kyle Bittinger, Yuan Gao, Elliot S Friedman, Lisa Nessel,
Unmesha Roy Paladhi, Lillian Chau, et al. 2021. “Role of Dietary Fiber
in the Recovery of the Human Gut Microbiome and Its Metabolome.” *Cell
Host & Microbe* 29 (3): 394–407.

</div>

</div>
