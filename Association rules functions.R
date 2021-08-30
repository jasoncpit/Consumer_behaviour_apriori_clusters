#List of functions ----------
segment_trans_cluster = function(trans_sequence,data,cluster_label){
  trans_sequence = data.frame(trans_sequence)
  #determine the number of clusters 
  n_cluster = length(levels(data[,cluster_label])) 
  #determine the levels of clusters 
  n_levels = levels(data[,cluster_label])
  #create empty list based on the number of clusters
  cluster_group <- vector(mode = "list", length = n_cluster)
  #Segment transaction sequence based on cluster and household ID 
  for(i in 1:n_cluster){
    #Retrive household ID from each cluster 
    cluster_household = data[data[,cluster_label] == n_levels[i],"households"]
    cluster_group[[i]] =data.frame(itemList = trans_sequence[trans_sequence[,"households"] %in% cluster_household,"all_transaction"])
  }
  return(cluster_group)
}
saving_reading_trans_sequence = function(trans_sequence,mainDir, subDir){
  library(arules)
  ## Provide the dir name(i.e sub dir) that you want to create under main dir:
  #Set directory 
  output_dir <- file.path(mainDir, subDir)
  #Check whether the director exist 
  if (!dir.exists(output_dir)){
    print("Dir does not exists, new path created")
    dir.create(output_dir)
    setwd(output_dir)
  } else {
    print("Dir already exists!")
    setwd(output_dir)
  }
  for(i in 1:length(trans_sequence)){
    write.csv(trans_sequence[[i]], paste0("cluster",i,".csv"),row.names = FALSE,quote = FALSE)
  }
  trans_sequence_read <-
    list.files(pattern = "*.csv") %>% 
    map(~read.transactions(file=., rm.duplicates= TRUE, format="basket",sep=","))
  return(trans_sequence_read)
}
association_rules_2_df = function(rule){
  #Redundant rules filtering 
  #i.e. school -> coffee, coffee -> school 
  #i.e. subseting problems 
  #Standardising rules for visualisation
  rule_filter = rule[!is.redundant(rule)]
  rule_filter_df = data.frame(
    lhs = labels(lhs(rule_filter)),
    rhs = labels(rhs(rule_filter)), 
    cluster_confidence_lift = paste0(formattable(rule_filter@quality$confidence,digits =2,format ="f"),
                                     "/",
                                     formattable(rule_filter@quality$lift,digits =2,format ="f")))
  return(rule_filter_df)
}
apriori_bulk = function(trans_sequence_list,supp = 0.01, conf = 0.5){
  no_dataframe =  length(trans_sequence_list)
  rules_df <- vector(mode = "list", length =no_dataframe) 
  for(i in 1:no_dataframe){
    rules <-apriori(trans_sequence_list[[i]],parameter =list(supp = supp, conf = conf, target = "rules"))
    rules_df[[i]] <- association_rules_2_df(rules)
  }
  return(rules_df)
}
apriori_vis_table = function(rule_df,new_names = c("rules","0_1 km","1_5 km","5_10 km",">10 km")){
  all_rules = rule_df %>% 
    reduce(full_join,by = c("lhs","rhs"))  
  all_rules$rules = paste0(all_rules$lhs,"=>",all_rules$rhs)
  #Old columns names 
  all_rules = all_rules%>%
    select(rules, everything())%>% 
    select(-c("lhs","rhs"))
  colnames(all_rules) = new_names
  return(all_rules)
}
jason_apriori_bulk = function(trans_sequence,data,cluster_label,mainDir, subDir, supp = 0.01, conf = 0.5){
  library("formattable")
  library(purrr)
  #Segement transaction sequence based on clusters and make list 
  trans_sequence_cluster_list = segment_trans_cluster(trans_sequence,data,cluster_label)
  #Saving, reading, and conversting transaction sequence to transaction database 
  trans_sequence_cluster_sequence = saving_reading_trans_sequence(trans_sequence_cluster_list,mainDir,subDir)
  #Readings transaction lists based on user paramaters 
  trans_sequence_rules_df = apriori_bulk(trans_sequence_cluster_sequence,supp = supp, conf = conf)
  #Combing all sequence 
  #Rename 
  all_levels = levels(data[,cluster_label])
  length <- length(all_levels)
  colnames <- rep(NA, length)
  for(i in 1:length(all_levels)){
    colnames[i] = all_levels[i]
  }
  colnames= paste(c("rules",colnames))
  trans_sequence_rules_df_all = apriori_vis_table(trans_sequence_rules_df,new_names = colnames)
  #Return all sequence 
  return(trans_sequence_rules_df_all)
}

# Define color_tile_mean function
color_tile_mean <- function (...) {
  #Define_colors
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = csscolor(gradient(as.numeric(substr(x,1,3)),customGreen0, customGreen)))
  })}


