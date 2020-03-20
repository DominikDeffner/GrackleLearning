// Individual learning only

data{
   int N;
   int N_id;
   int id[N];
   int Trial[N];
   int Choice[N];
   int Correct[N];
}

parameters{
  vector<lower=0, upper=1>[N_id] phi_ID;
  vector<lower=0>[N_id] L_ID;
  vector<lower=0>[2] sigma_ID;
  corr_matrix[2] Rho_ID;
  real<lower=0, upper=1> phi;
  real<lower=0> L;
}

model{

matrix[N_id,2] A; // attraction matrix
vector[2] YY[N_id];
vector[2] MU;

// varying effects
sigma_ID ~ exponential(1);
Rho_ID ~ lkj_corr(4);
phi ~ beta(2,2);
L ~ exponential(1);

 MU = [ L , phi ]';
 for ( j in 1:N_id ) YY[j] = [ L_ID[j] , phi_ID[j] ]';
 YY ~ multi_normal( MU , quad_form_diag(Rho_ID , sigma_ID) );


// initialize attraction scores
for ( i in 1:N_id ) A[i,1:2] = rep_vector(0,2)';

// loop over Choices

for ( i in 1:N ) {
vector[2] pay;
vector[2] p;


// first, what is log-prob of observed choice

p = softmax(L_ID[id[i]]  *A[id[i],1:2]' );
Choice[i] ~ categorical( p );

// second, update attractions conditional on observed choice

pay[1:2] = rep_vector(0,2);
pay[ Choice[i] ] = Correct[i];
A[ id[i] , 1:2 ] = ( (1-phi_ID[id[i]])*to_vector(A[ id[i] , 1:2 ]) + phi_ID[id[i]]*pay)';

}//i
}
