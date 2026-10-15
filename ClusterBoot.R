hierarchical_bootstrap <- function(df, cluster_columns, id_in_cluster, level_begin = 1) {

  clustiddat<-apply(df[,c(cluster_columns,id_in_cluster)],2, function(dvec) trimws(as.character(dvec)))
  df$clustid<-apply(clustiddat,1,function(yvec) paste(yvec,collapse="_"))
  samp_clustids<-list()
  for (level in level_begin:length(cluster_columns)) {
       if (level>1) { 
           lclustlev1<-apply(clustiddat[,1:level],1,function(yvec) paste(yvec,collapse="_"))
           lclustiddat<-data.frame(lclustlev1,clustiddat[,level+1])
           names(lclustiddat)<-c("clev1",cluster_columns[level+1])
           lvl_clust_df<-as.data.frame(unique(lclustiddat))
           sampids<-unlist(sapply(scids,function(lclustid) grep(lclustid,lvl_clust_df$clev1)))
           lvl_clust_df<-lvl_clust_df[sampids,]
       } else {
           lclustiddat<-clustiddat[,1:2]
           lvl_clust_df<-as.data.frame(unique(lclustiddat))
           names(lvl_clust_df)[1]<-"clev1"
       }
       nperlevelcat<-table(lvl_clust_df[,"clev1"])
       clust_levs<-names(nperlevelcat)
       scids<-c()
       for (clidx in 1:length(clust_levs)) {
            tscids<-sample(lvl_clust_df[lvl_clust_df$clev1==clust_levs[clidx],2],nperlevelcat[clidx],replace=T)
            tscids<-sapply(tscids,function(uid) paste(c(clust_levs[clidx],uid),collapse="_"))
            scids<-c(scids,tscids)
       }
       samp_clustids[[level]]<-scids
  }
  sampids<-unlist(sapply(scids,function(lclustid) which(df$clustid==lclustid)))
  return(sampids)
}  
  
#### Not Run
#  lvl_clustids<-apply(df[,cluster_columns],1,function(yvec) paste(yvec,collapse="_"))
#  unique_lvl_clustids<-unique(lvl_clustids)
#  
#  nperlevelcat<-table(df[lvl_clustids[[sample(unique_lvl_clustids,length(unique_lvl_clustids),replace=T)]],cluster_columns[level]])  
#  current_cluster <- cluster_columns[level]
#  unique_clusters <- unique(df[[current_cluster]])
#  
#  # Sample clusters with replacement
#  sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
#  
  # Initialize result data frame
#  bootstrap_data <- data.frame()
# 
  # For each sampled cluster, get the data and recurse to next level
#  
#  for (cidx in 1:length(sampled_clusters))  {
#    cluster_id <- sampled_clusters
#    # Get all rows for this cluster
#    cluster_data <- df[df[[current_cluster]] == cluster_id, , drop = FALSE]
#    
#    # If there are more levels, recurse
#    if (level < length(cluster_columns)) {
#      cluster_data <- hierarchical_bootstrap(cluster_data, cluster_columns, level + 1)
#    }
#    
#    # Create new cluster ID to avoid duplicates
#    new_cluster_id <- paste0(cluster_id, "_boot_", cidx)
#    cluster_data[[current_cluster]] <- new_cluster_id
#    
#    # Bind to result
#    bootstrap_data <- rbind(bootstrap_data, cluster_data)
#    cluster_counter <- cluster_counter + 1
#  }
#  
#  return(bootstrap_data)
#}
# Clustered Bootstrap Function with Arbitrary Number of Levels
#clustered_bootstrap <- function(data, cluster_vars, n_bootstrap = 100, seed = NULL) {
  # Set seed for reproducibility
#  if (!is.null(seed)) {
#    set.seed(seed)
#  } else { set.seed(Sys.time()) }
#  
  # Validate inputs
#  if (!is.data.frame(data)) {
#    stop("data must be a data frame")
#  }
#  
#  if (!all(cluster_vars %in% colnames(data))) {
#    missing_vars <- cluster_vars[!cluster_vars %in% colnames(data)]
#    stop(paste("Clustering variables not found in data:", paste(missing_vars, collapse = ", ")))
#  }
#  
#  if (length(cluster_vars) == 0) {
#    stop("At least one clustering variable must be specified")
#  }
#  
  # Function to perform hierarchical clustering bootstrap
#  
  # Generate bootstrap samples
#  bootstrap_samples <- vector("list", n_bootstrap)
  
#  cat("Generating", n_bootstrap, "bootstrap samples with", length(cluster_vars), "clustering levels...\n")
#  cat("Clustering variables:", paste(cluster_vars, collapse = ", "), "\n")
  
#  for (i in 1:n_bootstrap) {
#    if (i %% 10 == 0 || i == 1) {
#      cat("Bootstrap sample", i, "of", n_bootstrap, "\n")
#    }
#    
#    bootstrap_samples[[i]] <- hierarchical_bootstrap(data, cluster_vars)
#    
#    # Add bootstrap sample identifier
#    bootstrap_samples[[i]]$bootstrap_sample <- i
#  }
#  
#  cat("Bootstrap sampling complete!\n")
#  
#  # Add metadata
#  attr(bootstrap_samples, "cluster_vars") <- cluster_vars
#  attr(bootstrap_samples, "n_bootstrap") <- n_bootstrap
#  attr(bootstrap_samples, "original_n") <- nrow(data)
#  attr(bootstrap_samples, "seed") <- seed
#  
#  return(bootstrap_samples)
#}

# Helper function to summarize bootstrap results
#summarize_bootstrap <- function(bootstrap_list) {
#  if (length(bootstrap_list) == 0) {
#    stop("Bootstrap list is empty")
#  }
#  
#  cluster_vars <- attr(bootstrap_list, "cluster_vars")
#  n_bootstrap <- attr(bootstrap_list, "n_bootstrap")
#  original_n <- attr(bootstrap_list, "original_n")
#  
#  cat("=== Bootstrap Summary ===\n")
#  cat("Number of bootstrap samples:", n_bootstrap, "\n")
#  cat("Original dataset size:", original_n, "rows\n")
#  cat("Clustering variables:", paste(cluster_vars, collapse = ", "), "\n")
#  
#  # Sample size distribution
#  sample_sizes <- sapply(bootstrap_list, nrow)
#  cat("Bootstrap sample sizes:\n")
#  cat("  Mean:", round(mean(sample_sizes), 2), "\n")
#  cat("  Range:", min(sample_sizes), "-", max(sample_sizes), "\n")
#  
#  # Cluster count distributions for each level
#  for (var in cluster_vars) {
#    cluster_counts <- sapply(bootstrap_list, function(x) length(unique(x[[var]])))
#    cat("Unique", var, "clusters per sample:\n")
#    cat("  Mean:", round(mean(cluster_counts), 2), "\n")
#    cat("  Range:", min(cluster_counts), "-", max(cluster_counts), "\n")
#  }
#}

# Function to apply a statistic to all bootstrap samples
#bootstrap_statistic <- function(bootstrap_list, stat_func, ...) {
#  results <- lapply(bootstrap_list, function(sample_data) {
#    stat_func(sample_data, ...)
#  })
#  
#  return(results)
#}

# Example usage functions

# Create example dataset
#create_example_data <- function() {
#  set.seed(123)
  
  # Create hierarchical data: schools -> classes -> students
#  n_schools <- 5
#  n_classes_per_school <- 3
#  n_students_per_class <- 20
  
#  data <- data.frame()
  
#  for (school in 1:n_schools) {
#    for (class in 1:n_classes_per_school) {
#      n_students <- rpois(1, n_students_per_class) + 10  # Variable class sizes
#      
#      class_data <- data.frame(
#        school_id = paste0("School_", school),
#        class_id = paste0("Class_", school, "_", class),
#        student_id = paste0("Student_", school, "_", class, "_", 1:n_students),
#        math_score = rnorm(n_students, mean = 70 + school*2 + class, sd = 15),
#        reading_score = rnorm(n_students, mean = 75 + school*1.5, sd = 12),
#        grade_level = sample(c("3rd", "4th", "5th"), n_students, replace = TRUE)
#      )
#      
#      data <- rbind(data, class_data)
#    }
#  }
#  
#  return(data)
#}

# Example 1: Two-level clustering (school -> class)
#example_two_level <- function() {
#  cat("=== Example 1: Two-level Clustering ===\n")
#  
#  # Create example data
#  example_data <- create_example_data()
#  cat("Original data dimensions:", nrow(example_data), "rows,", ncol(example_data), "columns\n")
#  cat("Schools:", length(unique(example_data$school_id)), "\n")
#  cat("Classes:", length(unique(example_data$class_id)), "\n")
#  
#  # Perform bootstrap
#  bootstrap_results <- clustered_bootstrap(
#    data = example_data,
#    cluster_vars = c("school_id", "class_id"),
#    n_bootstrap = 20,
#    seed = 456
#  )
  
  # Summarize results
#summarize_bootstrap(bootstrap_results)
  
#  return(bootstrap_results)
#}

# Example 2: Single-level clustering (school only)
#example_single_level <- function() {
#  cat("\n=== Example 2: Single-level Clustering ===\n")
#  
#  example_data <- create_example_data()
#  
#  bootstrap_results <- clustered_bootstrap(
#    data = example_data,
#    cluster_vars = c("school_id"),
#    n_bootstrap = 15,
#    seed = 789
#  )
#  
#  summarize_bootstrap(bootstrap_results)
#  
#  return(bootstrap_results)
#}

# Example 3: Computing statistics on bootstrap samples
#example_bootstrap_statistics <- function() {
#  cat("\n=== Example 3: Bootstrap Statistics ===\n")
#  
#  example_data <- create_example_data()
#  
#  bootstrap_results <- clustered_bootstrap(
#    data = example_data,
#    cluster_vars = c("school_id", "class_id"),
#    n_bootstrap = 50,
#    seed = 101
#  )
#  
  # Calculate mean math scores for each bootstrap sample
#  mean_math_scores <- bootstrap_statistic(bootstrap_results, function(x) mean(x$math_score))
  
  # Calculate correlation between math and reading scores
#  #correlations <- bootstrap_statistic(bootstrap_results, function(x) cor(x$math_score, x$reading_score))
  
#  cat("Bootstrap distribution of mean math scores:\n")
#  cat("  Mean:", round(mean(unlist(mean_math_scores)), 2), "\n")
#  cat("  SD:", round(sd(unlist(mean_math_scores)), 2), "\n")
#  cat("  95% CI:", round(quantile(unlist(mean_math_scores), c(0.025, 0.975)), 2), "\n")
  
#  cat("Bootstrap distribution of math-reading correlations:\n")
#  cat("  Mean:", round(mean(unlist(correlations)), 3), "\n")
#  cat("  SD:", round(sd(unlist(correlations)), 3), "\n")
#  cat("  95% CI:", round(quantile(unlist(correlations), c(0.025, 0.975)), 3), "\n")
  
#  return(list(mean_scores = mean_math_scores, correlations = correlations))
#}

# Function to run all examples
#run_bootstrap_examples <- function() {
#  example_two_level()
#  example_single_level()
#  example_bootstrap_statistics()
#}

# Uncomment to run examples
# run_bootstrap_examples()