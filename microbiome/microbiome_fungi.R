library(phyloseq)
library(vegan)
library(dplyr)
library(ggplot2)
library(microbiome)
library(ggpubr)
library(metagMisc)
library(RColorBrewer)
library(tidyverse)
library(tibble)
library(rcompanion)
library(multcompView)
library(ggpattern)
library(DESeq2)
library(DAtest)
library(Maaslin2)
library(MASS)
library(gvlma)




############ Import Data and Filtering ################
## Imported otu table 
OTU_dust <- read.table("otu_table_dust.txt", header=TRUE, row.names="OTUID"); OTU_dust
OTU_dust <- otu_table(OTU_dust, taxa_are_rows = TRUE); OTU_dust
## Imported taxonomy
Tax_dust <- read.table("/taxonomy_dust.txt", header=TRUE, row.names="OTUID")
Tax_dust <- as.matrix(Tax_dust)
Tax_dust <- tax_table(Tax_dust)
## Imported metadata file
metadata_dust <- read.csv("metadata_dust.csv")
rownames(metadata_dust) <- metadata_dust[,1]
metadata_dust <- sample_data(metadata_dust); metadata_dust
#Convert to phyloseq object
ps_dust <- merge_phyloseq(OTU_dust,Tax_dust, metadata_dust);ps_dust

#Subset taxa if needed 
ps_dust = subset_taxa(ps_dust, Kingdom=="Fungi"); ps_dust
ps_dust <- prune_taxa(taxa_sums(ps_dust)>0, ps_dust); ps_dust

sums_dust<- sample_sums(ps_dust); sums_dust
sums_dust <- data.frame(sums_dust)
#sums_matrix_ITS <- as.matrix(sums_ITS)
colSums(sums_dust) #      
max(sums_dust) #
min(sums_dust) #
sums_dust

sums_dust%>%
  arrange(sums_dust)


############ Beta diversity analysis #####
ps_dust_CSS <- phyloseq_transform_css(ps_dust, norm = TRUE, log = TRUE)
matrixdistance_ps_dust_CSS_PCoA <- ordinate(physeq = ps_dust_CSS, method = "PCoA", distance = "bray", trymax = 1000)
matrixdistance_ps_dust_CSS_NMDS <- ordinate(physeq = ps_dust_CSS, method = "NMDS", distance = "bray", trymax = 1000)

PCoA_bray_dust<-plot_ordination(physeq = ps_dust_CSS, ordination = matrixdistance_ps_dust_CSS_NMDS) + 
  #geom_text(aes(label=SampleID), size = 5)+
  geom_point(aes(color = Health_status, shape=""), size = 4)+ 
  theme_bw()+
  stat_ellipse(aes(colour= Health_status,group=Health_status))#+


dist.matrix_CSS_dust <- t(data.frame(otu_table(ps_dust_CSS)))
sampledf_CSS_dust <- data.frame(sample_data(ps_dust_CSS))
phyloseq_bray_CSS_dust <- vegan::vegdist(dist.matrix_CSS_dust, method = "bray")
vegan::adonis2(phyloseq_bray_CSS_dust ~ Health_status*Sequencing_Run, data = sampledf_CSS_dust)


dispersion_ITS_dust<- vegan::betadisper(phyloseq_bray_CSS_dust, sampledf_CSS_dust$Health_status, bias.adjust = TRUE, sqrt.dist = TRUE)
anova(dispersion_ITS_dust) #
vegan::permutest(dispersion_ITS_dust, pairwise=TRUE, permutations=999)
plot(dispersion_ITS_dust)
boxplot(dispersion_ITS_dust, main = "", xlab = "") #visulaize the data dispersion


############ Alpha diversity analysis ######

sums_ps_dust <- sample_sums(ps_dust); sums_ps_dust
sums_ps_dust <- data.frame(sums_ps_dust)
sums_ps_dust%>%
  arrange(sums_ps_dust)


subsamp_dust_min <- rarefy_even_depth(ps_dust, sample.size=min(ps_dust), rngseed=5163, replace=FALSE, trimOTUs=TRUE); subsamp_dust_min 
ggrare(subsamp_dust_min, step= 50, color="Health_status", se=FALSE) 

ITS_richness_dust_min = estimate_richness(subsamp_dust_min , measures=c("Observed","Shannon", "Chao1", "InvSimpson"))


meta_ps_dust_min = as(meta(ps_dust_min), "matrix")
meta_ps_dust_min = as.data.frame(meta_ps_dust_min)
rownames(meta_ps_dust_min) <- meta_ps_dust_min[,1]

ITS_richness_dust_min$SampleID<- rownames(ITS_richness_dust_min)
ITS_richness_dust_min$SampleID<-rownames(meta_ps_dust_min)
ITS_richness_dust_min<-full_join(ITS_richness_dust_min, meta_ps_dust_min, by="SampleID");ITS_richness_dust_min


shapiro.test(ITS_richness_dust_min$Shannon) #
# -> data is normally distributed!!!
hist(ITS_richness_dust_min$Shannon, col='steelblue')
qqnorm(ITS_richness_dust_min$Shannon)
qqline(ITS_richness_dust_min$Shannon)

one.way.Shannon <- aov(Shannon ~ Health_status, data = ITS_richness_dust_min)
kruskal.test(Shannon ~ Health_status, data = ITS_richness_dust_min)

Shannon_all_Healthy_PW <- pairwise.wilcox.test(ITS_richness_dust_min$Shannon, ITS_richness_dust_min$Health_status,p.adjust.method="fdr") #for exact numbers
Shannon_all_Healthy_PW = Shannon_all_Healthy_PW$p.value
Shannon_all_Healthy_PW = fullPTable(Shannon_all_Healthy_PW)
multcompLetters(Shannon_all_Healthy_PW, compare="<", threshold=0.05,Letters=letters,reversed = FALSE) #if you prefer letters instead of exact numbers


shannon_country_field<-ggboxplot(ITS_richness_dust_min, x = "Health_status", y = "Shannon", fill= "Health_status", ylab = "Shannon Index", xlab="", add = "dotplot", legend="") +  
  #theme(axis.text.x=element_text(angle=90)) + 
  theme() + 
  #facet_wrap(~Health_status, scales="free", labeller=as_labeller(plot_names), nrow=1)+  
  scale_x_discrete(labels=c("Healthy", "Sick"))#+
#add_pvalue(sha_p_val, label="p.adj")


shapiro.test(ITS_richness_dust_min$Observed) #
hist(ITS_richness_dust_min$Observed, col='steelblue')
qqnorm(ITS_richness_dust_min$Observed)
qqline(ITS_richness_dust_min$Observed)

one.way.Observed <- aov(Observed ~ Health_status, data = ITS_richness_country)
kruskal.test(Observed ~ Health_status, data = ITS_richness_country)

Observed_all_Healthy_PW <- pairwise.wilcox.test(ITS_richness_dust_min$Observed, ITS_richness_dust_min$Health_status,p.adjust.method="fdr") #for exact numbers
Observed_all_Healthy_PW = Observed_all_Healthy_PW$p.value
Observed_all_Healthy_PW = fullPTable(Observed_all_Healthy_PW)
multcompLetters(Observed_all_Healthy_PW, compare="<", threshold=0.05,Letters=letters,reversed = FALSE) #if you prefer letters instead of exact numbers

Observed_country_field<-ggboxplot(ITS_richness_dust_min, x = "Health_status", y = "Observed", fill= "Health_status", ylab = "Number of ASVs", xlab="", add = "dotplot", legend="") +  
  #theme(axis.text.x=element_text(angle=90)) + 
  theme() + 
  #facet_wrap(~Health_status, scales="free", labeller=as_labeller(plot_names), nrow=1)+  
  scale_x_discrete(labels=c("Healthy", "Sick"))#+
#add_pvalue(sha_p_val, label="p.adj")


############ relative abundance ######

ps_dust_merged_health= merge_samples(ps_dust, "Health_status")
ps_dust_merged_rel <- microbiome::transform(ps_dust_merged_health, "compositional")

### Phylum
ps_dust_merged_rel_glom <- tax_glom(ps_dust_merged_rel, taxrank = 'Phylum', NArm = FALSE)
ps.melt.ps_dust_merged_rel <- psmelt(ps_dust_merged_rel_glom)

ps.melt.ps_dust_merged_rel$Phylum <- as.character(ps.melt.ps_dust_merged_rel$Phylum)
ps.melt.ps_dust_merged_rel <- ps.melt.ps_dust_merged_rel %>%
  group_by(Sample, Phylum) %>%
  mutate(median=median(Abundance))

keep <- unique(ps.melt.ps_dust_merged_rel$Phylum[ps.melt.ps_dust_merged_rel$median > 0.01])
ps.melt.ps_dust_merged_rel$Phylum[!(ps.melt.ps_dust_merged_rel$Phylum %in% keep)] <- "< 1%"

ps.melt.ps_dust_merged_rel <- ps.melt.ps_dust_merged_rel %>%
  group_by(Sample, Health_status, Phylum) %>%
  summarise(Abundance=sum(Abundance))

#write.csv(ps.melt.ps_dust_merged_rel, file = "abundance_dust_phylum.csv")

#visulaize relative abundance
nb.cols <- 25
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

abundance_dust_phylum<- ggplot(ps.melt.ps_dust_merged_rel, aes(x = Sample, y = Abundance, fill = Phylum)) + 
  geom_bar(stat = "identity", aes(fill=Phylum)) + 
  labs(x= NULL, y="Percentage Relative Abundance [%]") + 
  #facet_grid(~Health_status, scales="free", space="free_x")+ 
  theme_classic() +
  theme(strip.background = element_blank(), 
        axis.text.x.bottom = element_text(angle = 90))+ 
  scale_fill_manual(values = mycolors)+ theme(text = element_text(size = 15))#+



###differential abundance analysis
ps_dust_trimmed <- preDA(ps_dust, min.reads= 10) #trim low count ASVs to reduce 0 overflation
ps_dust_trimmed_glom <- tax_glom(ps_dust_trimmed, taxrank = 'Phylum', NArm = FALSE)
ps_dust_trimmed_glom_ds <- DA.ds2(ps_dust_trimmed_glom, predictor = "Health_status")

ps_dust_trimmed_glom_merged = merge_samples(ps_dust_trimmed_glom, "Health_status")
ps_dust_trimmed_glom_merged_rel <- microbiome::transform(ps_dust_trimmed_glom_merged, "compositional")
otu_ps_dust_trimmed_glom_merged_rel <- as.data.frame(t(otu_table(ps_dust_trimmed_glom_merged_rel)))
otu_ps_dust_trimmed_glom_merged_rel <- cbind(rownames(ps_dust_trimmed_glom_merged_rel), otu_ps_dust_trimmed_glom_merged_rel)
rownames(otu_ps_dust_trimmed_glom_merged_rel)<- NULL
colnames(otu_ps_dust_trimmed_glom_merged_rel)<- c("OTUID", "sick", "healthy")

otu_ps_dust_trimmed_glom_merged_rel_mean<-data.frame(OTUID=otu_ps_dust_trimmed_glom_merged_rel[,1], Means=rowMeans(otu_ps_dust_trimmed_glom_merged_rel[,-1]))
ps_dust_ds_merged<-merge(ps_roots_A_SE_ds, otu_ps_dust_trimmed_glom_merged_rel_mean, by.x = "Feature", by.y = "OTUID") 

ps_dust_ds_merged$diffexpressed <- "NO"
ps_dust_ds_merged$diffexpressed[ps_dust_ds_merged$log2FoldChange > 0.58 & ps_dust_ds_merged$pval.adj < 0.05] <- "sick" #0.58 represents a fold change of 1.5
ps_dust_ds_merged$diffexpressed[ps_dust_ds_merged$log2FoldChange < -0.58 & ps_dust_ds_merged$pval.adj < 0.05] <- "healthy"

ps_dust_ds_merged_sig <- subset(ps_dust_ds_merged, diffexpressed =="UP")
nrow(ps_dust_ds_merged_sig) # number of significant phyla
sum(ps_dust_ds_merged$Means)#relative abundance of significant phyla
ps_dust_ds_merged_unsig <- subset(ps_dust_ds_merged, diffexpressed =="NO")
nrow(ps_dust_ds_merged_unsig)
sum(ps_dust_ds_merged_unsig$Means)#0


 ###visualisation of differential abundance analysis using volcano plot
p <- ggplot(data=ps_dust_ds_merged, aes(x=log2FoldChange, y=-log10(pval.adj))) + geom_point(aes(size=Means))+ theme_minimal();p
p_ds_dust <- p + geom_vline(xintercept=c(-1, 1), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")


############ core ######

Health_status <- unique(as.character(meta(ps_dust)$Health_status))
print(Health_status) # healthy # sick

ps_dust_bh <- format_to_besthit(ps_dust)
taxa_names(ps_dust_bh)[1:5]
list_core <- c()
for (n in Health_status){ # for each variable n in DiseaseState
  #print(paste0("Identifying Core Taxa for ", n))
  ps.sub <- subset_samples(ps_dust_bh, Health_status == n) # Choose sample from DiseaseState by n
  core_m <- core_members(ps.sub, # ps.sub is phyloseq selected with only samples from g 
                         detection = 0.001, # 0.001 in atleast 90% samples 
                         prevalence = 0.75)
  print(paste0("No. of core taxa in ", n, " : ", length(core_m))) # print core taxa identified in each DiseaseState.
  list_core[[n]] <- core_m # add to a list core taxa for each group.
  #print(list_core)
}
plot(venn(list_core))
print(list_core)

pseq.core_dust <- core(ps_dust, detection = 0.001, prevalence = .75)

############ generalized linear model for alpha div #############

alpha_dust_par$Health_status <- as.factor(alpha_dust_par$Health_status)
cormat<-cor(na.omit(alpha_dust_par),  method = c("pearson"))

#for all parameters, remove strong correlating ones
CO2<-boxcox(lm(alpha_dust_par$CO2 ~ 1)) 
CO2.lambda <- CO2$x[which.max(CO2$y)];CO2.lambda # -0.6666667 --> 1/sqrt(x) transformation

plot(alpha_dust_par_transf$Shannon, alpha_dust_par_transf$CO2)


alpha_dust_par_transf <- alpha_dust_par

alpha_dust_par_transf$CO2<-log((alpha_dust_par$CO2)+1)

plot(alpha_dust_par_transf$Shannon, alpha_dust_par_transf$CO2)


glm.full.model <- glm(Shannon ~., data = na.omit(alpha_dust_par_transf), family=Gamma)
summary(glm.full.model)
anova(glm.full.model)
plot(glm.full.model)
# Stepwise regression model
glm.step.model <- stepAIC(glm.full.model, direction = "both", trace=FALSE)
summary(glm.step.model)
glm.step.model$anova
plot(glm.step.model)
anova(glm.step.model, test="F")
adjR2(glm.step.model)
car::vif(glm.step.model)

reduced.model <- glm(Shannon ~ X, data = alpha_dust_par_transf)#x represents the significant ones from anova


