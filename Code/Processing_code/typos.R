require("palmerpenguins")
data(penguins)

pen <- as.data.frame(penguins_raw)
pensp <- penguins_raw$Species   # species vector

ssample <- sample(length(pensp), 6)  # randomly sample records
lsample <- sample(15, 6)   # randomly sample characters

substr(pensp[ssample], lsample, lsample) <- sample(c(LETTERS, letters), 6)  # replace with typos
pen$Species <- pensp   # save back to dataframe

nn <- length(pen$`Body Mass (g)`)      
ii <- sample(nn, 3)        # choose three values at random
pen$`Body Mass (g)`[ii] <-  pen$`Body Mass (g)`[ii]/100   # make dirty, replace

ii <- sample(nn, 3)
pen$`Culmen Length (mm)`[ii] <- pen$`Culmen Length (mm)`[ii]*10

ii <- which(is.na(pen$`Culmen Length (mm)`)) # where are the missing values? 
pen$`Culmen Length (mm)`[ii[1]] <- "missing"  

write.csv(pen, file="../../Data/Raw_data/penguins_raw_dirty.csv", row.names=FALSE)
