# feature dimension 

pdim<-c(500)

# No of rank-1 components
lr<-2
r<-lr

# Basis dimension
lmd_val<-c(120,80) #lmd_min*(r:1)

# weight for reminder
sig<-0

# noise variance in tensor
Tau2<-1

# Parameters for supervised component
Eta2<-c(1,1.5) # error variance
gam<-as.matrix(cbind(c(1.5,3),c(2,3.4))) # partial slopes



# Grid of Time points
nres<-51
Time<-seq(0,1,length.out=nres)
#Pht<-PhiF[[1]](Tg)
set.seed(nres)
PhiFunc<-lapply(1:r, function(x){
  function(s){
    a<-t(sapply(s,function(j){
      c(1,sapply(2:10, function(i){sqrt(2)*cos((i-1)*(pi)*j)}))
    }))%*%matrix(sapply(1:10,function(j){runif(1,-1/j,1/j)}))
    a/norm(a,type="2")
  }
})
PhiF<-sapply(1:r,function(r){sqrt(length(Time))*PhiFunc[[r]](Time)})

set.seed(pdim)
Bval<<-sapply(1:r, function(b){runif(pdim)})
bval<<-Bval*outer(rep(1,pdim),1/apply(Bval,2,norm,type="2"))


# Grid scenario

Mi<-3:8

n<-50
m_i<-sample(Mi,n,replace = TRUE)
set.seed(n)
Vmat<<-cbind(round(runif(n),2),round(rbeta(n,1,1),2))


data<-data_gen_supFTSVD(m_i=m_i,Xmatrix = Vmat,Beta = gam,Xi = bval,
                        PsiF = PhiF,sing_val = lmd_val,SubE_Var = Eta2,
                        Data_Var = Tau2,Tgrid = Time)


set.seed(25) # it is necessary for reproducibility because of the use of random SVD
fit_supF<-supFTSVD(datlist = data$data,
                   response = Vmat,
                   interval = NULL,
                   r=2,
                   resolution = 50,
                   CVPhi = TRUE,
                   K = 5,
                   cvT = 2,
                   smooth = exp(seq(-2,2,length.out=10)),
                   maxiter = 100,
                   epsilon = 1e-6,
                   KInd = NULL)


# New data set for prediction

set.seed(n+5)
valid_Vmat<<-cbind(round(runif(n),2),round(rbeta(n,1,1),2))


valid_data<-data_gen_supFTSVD(m_i=m_i,Xmatrix = valid_Vmat,Beta = gam,Xi = bval,
                        PsiF = PhiF,sing_val = lmd_val,SubE_Var = Eta2,
                        Data_Var = Tau2,Tgrid = Time)

# Fitting of supFTSVD
set.seed(25) # it is necessary for reproducibility because of the use of random SVD
fit_supF<-supFTSVD(datlist = data$data,
                   response = Vmat,
                   interval = NULL,
                   r=2,
                   resolution = 50,
                   CVPhi = TRUE,
                   K = 5,
                   cvT = 2,
                   smooth = exp(seq(-2,2,length.out=10)),
                   maxiter = 100,
                   epsilon = 1e-6,
                   KInd = NULL)

# Prediction using the fitted supFTSVD


pred_dat<-predict.supFTSVD(obj = fit_supF,designM = valid_Vmat,new_dat = valid_data$data)
