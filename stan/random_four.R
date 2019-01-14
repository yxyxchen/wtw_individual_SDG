random_four="
  data{
    int<lower=1> L;  //person or level
    int<lower=1> Num;   // observation
    int<lower=1> id[Num]; // ID variable
    int other_invest[Num];
    int self_invest[Num];
    int <lower=0> endowment[Num];
    matrix[6,5] s_paymatrix;
    matrix[5,6] w_paymatrix;
    vector[11] AI;
  }

parameters{
  real mu_p[4];  
  real<lower=0> sigma[4];
  
  vector[L] delta_pr;   
  vector[L] rho_pr; 
  vector[L] N_ini_pr;   
  vector[L] lambda_pr;   

}


transformed parameters{
  
  vector<lower=0,upper=1>[L] delta;
  vector<lower=0,upper=1>[L] rho;
  vector<lower=0,upper=10>[L] N_ini;
  vector<lower=0,upper=10>[L] lambda;
  
  matrix[6,80*L] option;
  
  for (k in 1:L) {
    delta[k]  = Phi_approx( mu_p[1] + sigma[1] * delta_pr[k] );
    rho[k]  = Phi_approx( mu_p[2] + sigma[2] * rho_pr[k] );
    lambda[k]  = Phi_approx( mu_p[3] + sigma[3] * lambda_pr[k] )*10;
    N_ini[k]  = Phi_approx( mu_p[4] + sigma[4] * N_ini_pr[k] )*10;
    
  }
  
  for (i in 1:(L)) {
    int trial_ind;
    int cond;
    vector[81] N;
    matrix[6,81] A; // include A0[0]-A6[0];
    
    cond=endowment[(i-1)*80+1];
    
    if (cond==5){
      A[1:6,1]=AI[1:6];
    }else{  
      A[1:5,1]=AI[7:11];
    }
    
    N[1]=N_ini[id[(i-1)*80+1]];
    
    for (x in 1:80){
      vector[cond+1] U_down;
      vector[cond+1] payment;
      
      int other_this;
      int self_this;
      
      trial_ind=80*(i-1)+x;
      
      other_this=other_invest[trial_ind];
      self_this=self_invest[trial_ind];
      
      N[x+1]=rho[id[trial_ind]]*N[x]+1;
      
      if (cond==5){
        payment=s_paymatrix[1:(cond+1),other_this+1];
        option[1:6,trial_ind]=A[1:6,x];
      }else{  
        payment=w_paymatrix[1:(cond+1),other_this+1];
        option[1:5,trial_ind]=A[1:5,x];
      } 
      
      for (m in 1:(cond+1)){
        if (self_this==(m-1)){
          A[m,x+1]=A[m,x]+(payment[m]-A[m,x])/N[x+1];
        }else{
          A[m,x+1]=A[m,x]+(delta[id[trial_ind]]*payment[m]-A[m,x])/N[x+1];
        }
      }
    }
  }
}


model{
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  

  rho_pr  ~ normal(0,1);
  delta_pr  ~ normal(0,1);
  lambda_pr ~ normal(0,1);
  N_ini_pr ~ normal(0,1);
  
  for (n in 1:Num) {
    int self_trans;
    self_trans=self_invest[n]+1;
    self_trans ~ categorical_logit(lambda[id[n]]*option[1:(endowment[n]+1),n]);
  }
  
}

generated quantities {
  vector[Num] log_lik;
  real      LL_all;

  for (j in 1:Num){
    int self_trans;
    self_trans=self_invest[j]+1;
    log_lik[j] =categorical_logit_lpmf(self_trans | (lambda[id[j]]*option[1:(endowment[j]+1),j]));

  }
  LL_all=sum(log_lik);
} 

"


