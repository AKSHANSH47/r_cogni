#Decision trees in R

#Features: vci i.e(1:n)
#levels :lc(j)(no of rows:j)
#Class labels:C(c1,c2....ck)[no of classes]
#
#
#train-test:70-30

#classification model
#complexitiy parameter plor
#report accuracy based on misclassification tree
smp_size <- floor(0.70 * nrow(adult))

train_ind <- sample(seq_len(nrow(adult)), size = smp_size)
train <- adult[train_ind, ]
test <- adult[-train_ind, ]

num_vars<-c(names(train[,]))

xyz<-setdiff(names(train),"")
b = paste("train",xyz,sep = "$",collapse = "+")
a = paste("train$",b,sep = "~")

my_formula <- paste(b,a,sep ="-")
my_formula <- as.formula(a)

