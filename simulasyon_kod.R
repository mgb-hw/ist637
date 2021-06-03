# catR paketinin yüklenmesi
install.packages("catR")

# catR paketinin aktif edilmesi
library("catR")

# bu fonksiyon ikili IRT modelleri için bir öge parametreleri matrisi olusturur.
# Madde havuzu 3PL ile olusturulur. Yani a, b, c parametreleri belirtilmelidir.
itPar<-genDichoMatrix(items=300, model = "3PL", 
                      aPrior = c("unif", 0.5, 2), 
                      bPrior = c("norm", 0, 1), 
                      cPrior = c("beta", 4, 16), 
                      seed = 1)

# theta degerleri standart normal dagilimdan üretildi.
seed(1)
theta<-rnorm(2000,0,1)

#head(itPar,10)

start <- list(nrItems=1, theta = 0, startSelect="MFI", randomesque = 10)
test <- list(method = "EAP", itemSelect = "MFI", priorDist = "norm", 
             priorPar = c(0, 1), randomesque = 10)
stop <- list(rule ="length", thr = 45) 
final <- list(method = "EAP", priorDist = "norm", priorPar = c(0, 1))
catResults1<- simulateRespondents(thetas = theta, itemBank = itPar, 
                                  rmax = 0.2, 
                                  start = start, test = test, stop = stop,
                                  final = final, save.output = TRUE, 
                                  output = c("","catR","txt"))

# Simulasyona iliskin grafikleri çikti alma
par(mar=rep(2,4))
plot(catResults1, type = "all")

data<-matrix(NA, length(theta), nrow(itPar))
for (i in 1:length(theta)){
  data[i,]<-genPattern(th = theta[i], itPar)}

start <- list(nrItems=1, theta = 0, startSelect="MFI", randomesque = 10)
test <- list(method = "EAP", itemSelect = "MFI", priorDist = "norm", 
             priorPar = c(0, 1), randomesque = 10)

# Standart hata 0,3'e ulastiginda veya 45 madde uygulandiginda test durur. 
# Kriterlerden biri saglandiginda sonlandirilir.
stop <- list(rule =c("precision", "length"), thr = c(0.3, 45)) 
final <- list(method = "EAP", priorDist = "norm", priorPar = c(0, 1))

# Maksimum maruz kalma orani 0,2 ile sinirlandirildi
catResults2<-simulateRespondents(thetas = theta, itemBank = itPar, 
                                 responsesMatrix=data, rmax = 0.2, 
                                 start = start, test = test, stop = stop, 
                                 final = final)

# Simulasyona iliskin grafikleri çikti alma
par(mar=rep(2,4))
plot(catResults2, type = "all")

start <- list(nrItems=1, theta = 0, startSelect="MFI", randomesque = 10)
test <- list(method = "EAP", itemSelect = "MFI", priorDist = "norm", 
             priorPar = c(0, 1), randomesque = 10)
stop <- list(rule ="length", thr = 45) 
final <- list(method = "EAP", priorDist = "norm", priorPar = c(0, 1))

# Her döngü için farkli sonuçlar farkli dosyalara kaydetme
for (r in 1:10) {
  catResults3<-simulateRespondents(thetas=theta, itemBank=itPar, 
                                   start = start, test = test, stop=stop, 
                                   final = final, genSeed=1:length(theta),
                                   rmax=0.20, save.output = T,
                                   output=c("cat",-r,"catR","dat"))}

#par(mar=rep(2,4))
#plot(catResults3, type = "all")

