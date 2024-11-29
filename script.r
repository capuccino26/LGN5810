library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(multcomp)
library(dplyr)
library(tidyr)
library(emmeans)
library(ExpDes.pt)
library(ggpubr)
library(dunn.test)

#Pereira de Castro A, Breseghello F, Furtini IV, Utumi MM, Pereira JA, Cao T-V, Bartholomé J. 2023. Population improvement via recurrent selection drives genetic gain in upland rice breeding. Heredity. 131(3):201–210.
path<-"./"
dataf <- read.csv2(sprintf("%sCNA6_phenotypic_data.csv",path), header = TRUE,sep=",")
dataf$pht = as.numeric(as.character(dataf$pht))

rodar_analise <- function(dados, resposta) {
	print(resposta)
	dados$gid <- gsub(".*CNA.*", "CNA6", dados$gid)
	dadosf <- dados
	dados <- na.omit(dados)
	modelo <- lmer(paste(resposta, "~ (1|gid) + (1|bloc)", sep = ""), data = dados)
	summary_output <- capture.output(summary(modelo))
	writeLines(summary_output, sprintf("%s%s_resumo_modelo.txt",path,resposta))
	fixed_effects <- summary(modelo)$coefficients
	fixed_df <- data.frame(
	  Term = rownames(fixed_effects),
	  Estimate = fixed_effects[, 1],
	  Std_Error = fixed_effects[, 2],
	  t_value = fixed_effects[, 3]
	)
	write.csv(fixed_df, sprintf("%s%s_efeitos_fixos.csv",path,resposta), row.names = FALSE)
	random_effects <- ranef(modelo)
	random_sd <- summary(modelo)$varcor
	random_df <- data.frame(
	  Group = rep(names(random_effects), each = sapply(random_effects, length)),
	  Effect = unlist(random_effects)
	)
	random_df$Std_Dev <- rep(unlist(lapply(random_sd, function(x) sqrt(x[1]))), times = sapply(random_effects, length))
	write.csv(random_df, sprintf("%s%s_efeitos_aleatorios.csv",path,resposta), row.names = FALSE)
	variancias <- as.data.frame(VarCorr(modelo))
	variancia_genetica <- variancias$vcov[1]
	variancia_residual <- variancias$vcov[2]
	herdabilidade <- variancia_genetica / (variancia_genetica + variancia_residual)
	resultado_variancias <- data.frame(
	  Variavel = c("Variância Genética", "Variância Residual", "Herdabilidade"),
	  Valor = c(variancia_genetica, variancia_residual, herdabilidade)
	)
	write.table(resultado_variancias, sprintf("%s%s_var_herd.txt",path,resposta), sep = ";", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
	modelo_simples <- lm(as.formula(paste(resposta, "~ gid + bloc")), data = dados)
	summary_output <- capture.output(summary(modelo_simples))
	writeLines(summary_output, sprintf("%s%s_resumo_modelo_simples.txt",path,resposta))
	fixed_effects <- summary(modelo_simples)$coefficients
	fixed_df <- data.frame(
	  Term = rownames(fixed_effects),
	  Estimate = fixed_effects[, 1],
	  Std_Error = fixed_effects[, 2],
	  t_value = fixed_effects[, 3],
	  p_value = fixed_effects[, 4]
	)
	write.csv(fixed_df, sprintf("%s%s_simples_efeitos_fixos.csv",path,resposta), row.names = FALSE)
	residuos <- residuals(modelo_simples)
	residuos_df <- data.frame(Residuo = residuos)
	write.table(residuos_df, sprintf("%s%s_simples_residuos.txt",path,resposta), sep = ";", row.names = TRUE, col.names = TRUE, fileEncoding = "UTF-8")
	estatisticas_modelo <- summary(modelo_simples)$r.squared
	p_valor_fstat <- summary(modelo_simples)$fstatistic[1]
	estatisticas_df <- data.frame(
		R_squared = estatisticas_modelo,
		F_statistic_p_value = p_valor_fstat
	)
	valores_preditos <- fitted(modelo_simples)
	valores_observados <- modelo_simples$model[[1]]
	dados_residuos <- data.frame(
	  Observados = valores_observados,
	  Preditos = valores_preditos,
	  Residuos = residuos
	)
	resumo_residuos <- summary(residuos)
	resumo_residuos_file <- sprintf("%s%s_ResumoResiduos.txt", path, resposta)
	capture.output(print("Resumo Estatístico dos Resíduos:"), resumo_residuos, file = resumo_residuos_file)
	teste_normalidade <- shapiro.test(residuos)
	teste_normalidade_file <- sprintf("%s%s_TesteNormalidade.txt", path, resposta)
	capture.output(print("Teste de Normalidade dos Resíduos (Shapiro-Wilk):"), teste_normalidade, file = teste_normalidade_file)
	limite_superior <- mean(residuos) + 3 * sd(residuos)
	limite_inferior <- mean(residuos) - 3 * sd(residuos)
	outliers <- dados_residuos[dados_residuos$Residuos < limite_inferior | dados_residuos$Residuos > limite_superior, ]
	outliers_file <- sprintf("%s%s_Outliers.txt", path, resposta)
	capture.output(print("Possíveis Outliers:"), outliers, file = outliers_file)
	residuos_file <- sprintf("%s%s_Residuos.txt", path, resposta)
	write.table(dados_residuos, file = residuos_file, row.names = FALSE, sep = "\t", quote = FALSE)
	grafico_residuos_preditos <- ggplot(dados_residuos, aes(x = Preditos, y = Residuos)) +
	  geom_point(color = "blue", alpha = 0.6) +
	  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
	  labs(title = "Gráfico de Resíduos vs Valores Preditos",
		   x = "Valores Preditos",
		   y = "Resíduos") +
	  theme_minimal()
	ggsave(sprintf("%s%s_%s.png", path, resposta, "ResiduosVsPreditos"), plot = grafico_residuos_preditos, width = 8, height = 6)
	grafico_hist_residuos <- ggplot(dados_residuos, aes(x = Residuos)) +
	  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
	  labs(title = "Histograma dos Resíduos",
		   x = "Resíduos",
		   y = "Frequência") +
	  theme_minimal()
	ggsave(sprintf("%s%s_%s.png", path, resposta, "HistogramaResiduos"), plot = grafico_hist_residuos, width = 8, height = 6)
	qqplot_file <- sprintf("%s%s_QQPlot.png", path, resposta)
	png(qqplot_file, width = 800, height = 600)
	qqnorm(residuos, main = "Q-Q Plot dos Resíduos")
	qqline(residuos, col = "red", lwd = 2)
	dev.off()
	grafico_residuos_observados <- ggplot(dados_residuos, aes(x = Observados, y = Residuos)) +
	  geom_point(color = "purple", alpha = 0.6) +
	  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
	  labs(title = "Gráfico de Resíduos vs Valores Observados",
		   x = "Valores Observados",
		   y = "Resíduos") +
	  theme_minimal()
	ggsave(sprintf("%s%s_%s.png", path, resposta, "ResiduosVsObservados"), plot = grafico_residuos_observados, width = 8, height = 6)
	grafico_observados_preditos <- ggplot(dados_residuos, aes(x = Observados, y = Preditos)) +
	  geom_point(color = "darkgreen", alpha = 0.6) +
	  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
	  labs(title = "Comparação: Valores Observados vs Preditos",
		   x = "Valores Observados",
		   y = "Valores Preditos") +
	  theme_minimal()
	ggsave(sprintf("%s%s_%s.png", path, resposta, "ObservadosVsPreditos"), plot = grafico_observados_preditos, width = 8, height = 6)
	write.csv(estatisticas_df, sprintf("%s%s_simples_stats.csv",path,resposta), row.names = TRUE, fileEncoding = "UTF-8")
	anovam<-anova(modelo_simples)
	anova_df <- as.data.frame(anovam)
	write.csv(anova_df, sprintf("%s%s_anova.csv",path,resposta), row.names = TRUE, fileEncoding = "UTF-8")
	summary_anovam <- summary(anovam)
	summary_df <- as.data.frame(summary_anovam)
	write.csv(summary_df, sprintf("%s%s_anova_sum.csv",path,resposta), row.names = TRUE, fileEncoding = "UTF-8")
	res_stud<-rstandard(modelo_simples)
	res_stud_df <- data.frame(
		Observacao = 1:length(res_stud),
		Residuos_Padronizados = res_stud
	)
	write.csv(res_stud_df, sprintf("%s%s_stdres.csv",path,resposta), row.names = FALSE, fileEncoding = "UTF-8")
	res_shap<-shapiro.test(res_stud)
	shap_output <- capture.output(res_shap)
	writeLines(shap_output, sprintf("%s%s_shapiro.txt",path,resposta))
	png(sprintf("%s%s_resb.png", path,resposta), width=800,height=600, res = 100)
	layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
	plot(modelo_simples, which = 2)
	boxplot(res_stud, ylab = "Studentized Residuals")
	plot(modelo_simples, which = 1)
	dev.off()
	pr<-ggplot(dados,aes(x=resposta,y=res_stud))+geom_point()+geom_hline(yintercept=0,color="red")+xlab(resposta)+ylab("Studentized Residuals")
	ggsave(pr, file=sprintf("%s%s_rem.png",path,resposta),limitsize=FALSE)
	prt<-ggplot(dados,aes(x=fitted(modelo_simples),y=res_stud,color=resposta))+geom_point()+geom_hline(yintercept=0,color="red")+xlab(paste(resposta))+ylab("Studentized Residuals")
	ggsave(prt, file=sprintf("%s%s_rtem.png",path,resposta),limitsize=FALSE)
	p <- ggplot(dadosf, aes(x = as.factor(bloc), y = dadosf[[resposta]], fill = as.factor(gid))) +
		geom_boxplot() +
		stat_compare_means(method = "kruskal.test", label.y = max(dadosf[[resposta]]) * 1.1) +
		stat_compare_means(comparisons = combn(levels(as.factor(dadosf$bloc)), 2, simplify = FALSE), 
						   method = "wilcox.test", label = "p.signif", hide.ns = TRUE) +
		xlab("Blocks") + 
		ylab(paste(resposta)) +
		ggtitle(sprintf("Genotypic comparison of %s per block", resposta)) +
		theme_minimal() +
		scale_fill_brewer(palette = "Dark2") +
		labs(fill = "Genotypes") +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))
	ggsave(p, file=sprintf("%s%s_boxfull.png",path,resposta),width=10,height=20,limitsize=FALSE)
	pg<-ggplot(dados, aes(x = as.factor(bloc), y = dados[[resposta]], fill = as.factor(gid))) +
		geom_boxplot() +
		xlab("Bloc") + 
		ylab(paste(resposta)) +
		ggtitle(sprintf("Boxplot of %s per block", resposta)) +
		theme_minimal() +
		scale_fill_brewer(palette = "Dark2")+
		stat_compare_means(aes(group = as.factor(gid)), method = "kruskal.test", label = "p.signif")
	ggsave(pg, file=sprintf("%s%s_boxblock.png",path,resposta),width=10,height=20,limitsize=FALSE)
	pbf<-ggplot(dadosf, aes(x = as.factor(gid), y = dadosf[[resposta]], fill = as.factor(bloc))) +
		geom_boxplot() +
		facet_wrap(~bloc, scales = "free") +
		stat_compare_means(method = "kruskal.test", label = "p.signif") +
		xlab("Genotype") + 
		ylab(paste(resposta)) +
		ggtitle(sprintf("Genotypic comparison of %s per block", resposta)) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
		labs(fill = "Blocks")
		scale_fill_brewer(palette = "Paired")
	ggsave(pbf, file=sprintf("%s%s_boxblockf.png",path,resposta),width=10,height=20,limitsize=FALSE)
	kruskal_bloc <- kruskal.test(as.formula(paste(resposta, "~ bloc")), data = dados)
	kruskal_genotipo <- kruskal.test(as.formula(paste(resposta, "~ gid")), data = dados)
	dunn_result <- dunn.test(dadosf[[resposta]], dadosf$gid, method = "bonferroni")
	dunn_data <- data.frame(
		Comparison = dunn_result$comparisons,
		Z = dunn_result$Z,
		P = dunn_result$P,
		P_adjusted = dunn_result$P.adjusted
	)
	write.csv(dunn_data, sprintf("%s%s_dunn.csv", path, resposta), row.names = FALSE)
	dados$bloc <- as.factor(dados$bloc)
	comparacoes_genotipos <- list()
	for (bloco in levels(dados$bloc)) {
	  subset_bloco <- subset(dados, bloc == bloco)
	  kruskal_result <- kruskal.test(as.formula(paste(resposta, "~ gid")), data = subset_bloco)
	  comparacoes_genotipos[[bloco]] <- kruskal_result
	}
	resultados <- data.frame(
	  Bloco = character(),
	  P_value = numeric(),
	  Test_statistic = numeric(),
	  Df = numeric(),
	  stringsAsFactors = FALSE
	)
	for (bloco in names(comparacoes_genotipos)) {
	  kruskal_result <- comparacoes_genotipos[[bloco]]
	  resultados <- rbind(resultados, data.frame(
		Bloco = bloco,
		P_value = kruskal_result$p.value,
		Test_statistic = kruskal_result$statistic,
		Df = kruskal_result$parameter
	  ))
	}
	write.table(resultados, file = sprintf("%s%s_kruskal_blocs.tsv",path,resposta), sep = "\t", row.names = FALSE, quote = FALSE)


	return(list(variancia_genetica = variancia_genetica, 
				variancia_residual = variancia_residual, 
				herdabilidade = herdabilidade,
				anovam = anovam,
				modelo = modelo,
				modelo_simples = modelo_simples,
				res_stud=res_stud,
				res_shap=res_shap,
				kruskal_bloc = kruskal_bloc,
				kruskal_genotipo = kruskal_genotipo,
				kruskal_genotipo = kruskal_genotipo,
				comparacoes_genotipos=comparacoes_genotipos))
}

resultados_yld <- rodar_analise(dataf, "yld")
resultados_pht <- rodar_analise(dataf, "pht")
resultados_flw <- rodar_analise(dataf, "flw")