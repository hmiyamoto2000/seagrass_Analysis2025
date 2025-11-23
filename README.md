# seagrass_Analysis2025

This website only shows the codes, and the csvfile for analysis is assumed to be stored on the related site of the submitted Journal, except for the csv files (public data derived from the Japan Meteorological Agency, https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/engan.html) for Fig.1.

1. Fig.1 (including the informations for Fig.S1)
   (raw_data)
   area306_Sagamiwan_1982.csv / area319_Notokita_1982.csv / area508_Akiiyonada_2016.csv / area509_Seouchichuou_2016.csv / area518_Bungosuidounanbu_1982.csv / area602_Genkainada_1982.csv
   (code)
   Time_course_graph_final.py
   
2. Fig.2 (including the information for Table S3)
 a) Bacteria
   (raw_data stored on the related site)
   Bacteria_diversity.csv / List.csv
   (code)
   NMDS_Bacteria.R
   (raw_data for plot, stored on the related site)
   NMDS_scores_bacteria.csv / data.scores_seagrassD4_cnd_env_new.csv
   (code for plot)
   NMDS_graph.py
 b) Eukaryotes
   (raw_data files stored on the related site)
   Eukaryota_diversity.csv / List.csv
   (code)
   NMDS_Eukaryota.R
   (raw_data for plot, stored on the related site)
   data.scores_seagrassD4_Eukaryota_cnd_env_new.csv
   (code for plot)
   NMDS_graph.py
   
3. Fig.3 
 a) Bacteria
   (raw_data for plot,stored on the related site)
   Bacteria_100_sgr_morethan1new.csv
   (code for plot)
   Graph_100%Relative_abundance_bacteria_26color.py
 b) Eukaryotes
   (raw_data for plot,stored on the related site)
   Eukaryota_seki100_morethan1.csv
   (code for plot)
   Graph_100%Relative_abundance_eukaryote_26color.py
   
4. Fig.4 (including the information for Fig.S13)
   (raw_data stored on the related site)
   AA_Seagrass_D4.csv / AA_selected_MLraws.csv
   (code)
   AA.R / SHAP_Bubble_Helvetica.py
   
5. Fig.5 (including the information for Figs.S16, S17, and S20, and Table S6)
   (raw_data stored on the related site)
   causal_learning_data_seagrass.csv / EFA_seagrass.csv 
   (code)
   causal_learning_and_analysis_seagrass.py / ROC_seagrass.R / EFA_seagrass.R / SEM_seagrass.R / CMA_seagrass.R / BayesLingam_seagrass.R
   
6. Fig.6 (including the information for Figs. S18, S19, and S21,and Table S7)
   (raw_data stored on the related site)
   Pathway_whole.csv and Group_list.csv / Seagrass_path_selected.csv / EFA_seagrass.csv 
   (code)
   Log_FC_seagrass_volcano_plot_stat.R / Pathway_clr_transform.py / causal_learning_and_analysis_seagrass.py / SEM_seagrass_path.R / CMA_seagrass_path.R / BayesLingam_seagrass_path.R

7. supplementary information
   Figs.S4 and S5, and Table S2
   (raw_data for plot,stored on the related site)
   Bacteria_alpha_diversity_result_stat.csv 
   Eukaryota_alpha_diversity_result_stat.csv
   List_feature_alpha_diversity_group_new.csv
   (code)
   alpha_diversity_plot_cnd.py 
   stat_alpha_diversity.py

   Figs.S6,S7, and S8
   (raw_data for plot,stored on the related site)
   Bacteria_seagrass_D4_family.csv
   Bacteria_seagrass_D5_genus.csv
   Seagrass_list_cnd.csv
   significant_results_dex.tsv
   (code)
   masilin3_commands.R


   Figs.S9,S10, and S11
   (raw_data for plot,stored on the related site)
   Bacteria_seagrass_D4_family.csv
   Bacteria_seagrass_D5_genus.csv
   Seagrass_list_cnd.csv
   significant_results_D4.tsv
   (code)
   masilin3_commands.R

   Fig.S12
   (raw_data for plot,stored on the related site)
   Chemical_seagrass.csv
   Seagrass_list_cnd.csv
   (code)
   masilin3_commands.R
   
   
   
 
   
