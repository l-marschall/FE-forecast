csir <- function(alpha_pr, alpha_wt, u){
  
  P = length(alpha_pr)
  
  alpha_up = zeros(P,1)  # updated vector
  
  # Now sort weights and particles
  alpha_wt = alpha_wt / sum(alpha_wt)   # element-wise division
  
  # alpha_pr
  alpha_PR = cbind(1:length(alpha_pr), alpha_pr)
  alpha_PR_ordered = alpha_PR[order(alpha_PR[,"a"]),]
  alpha_idx = alpha_PR_ordered[,1]
  alpha_pr = alpha_PR_ordered[,2]
  ### 
  
  alpha_WT = cbind(alpha_idx, alpha_wt)     # get the sorted weights by order of sorted alphas
  alpha_WT_ordered = alpha_WT[order(alpha_WT[,"alpha_idx"]),]
  alpha_wt = alpha_WT_ordered[,2]
  alpha_cwt = apply(alpha_wt, 2, cumsum)    # if matrix. if vector no apply function needed

  ##### This next line differentiate CSIR from SIR #####
  alpha_pr = rbind(alpha_pr[1,], alpha_pr)   # I THINK this is binding the rows
  #############################################################
  
  j = 1
  for (i in 1:P){
    while(alpha_cwt[i,] < u[j] && u[j] <= alpha_cwt[i+1,]){
      alpha_up[j,] = alpha_pr[i,] + (( alpha_pr[i+1,]-alpha_pr[i,] )/( alpha_cwt[i+1,]-alpha_cwt[i,] )) * (u[j,]-alpha_cwt[i,]) 
      if (j<P){
        j = j+1
      }
      else 
        break
    }
  }
  return(alpha_up)
}