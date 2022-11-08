


# make a fake popkin dataset
popkin <- matrix(runif(144),nrow=12)
rownames(popkin) <- paste0("sample", 1:12)
colnames(popkin) <- paste0("sample", 1:12)

# take a look at it
popkin

popkin <- pk_f4



# make a fake metadata file to go with it
meta <- cbind( paste0("sample", 1:12), paste0("site", c(rep(1,6), rep(2,6))), paste0("microsite", c(rep(1,3), rep(2,3), rep(3,3), rep(3,3) ) ))

meta <- cbind(dmv4$sample_names, dmv4$meta$analyses$S, dmv4$meta$analyses$micro)
colnames(meta) <- c("sample", "site", "microsite")


# now, loop through and collect the popkin measures for two categories:

# in same site, and same microsite
insite_inms <- c()

# in sames site, and different microsite
insite_outms <- c()

outsite <- c()

# nested loop through pairs
for (i in 1:nrow(popkin)) {
   for (j in 1:ncol(popkin)) {
      # only work with matrix beneath the diagonal
      if (i > j) {

         # get popkin val, and other info about samples i,j
         pk_ij <- popkin[i,j]
         sampi <- rownames(popkin)[i] 
         sampj <- rownames(popkin)[i]
         mi    <- which(meta[,"sample"] == sampi) 
         mj    <- which(meta[,"sample"] == sampj)
         sitei <- meta[,"site"][i] 
         sitej <- meta[,"site"][j]
         msitei <- meta[,"microsite"][i] 
         msitej <- meta[,"microsite"][j]

         # if same site, same microsite, add to that list
         if (  sitei == sitej & msitei == msitej ) {
            insite_inms <- c(insite_inms, pk_ij)
         }

         # if same site, diff microsite, add to that list
         if (  sitei == sitej & msitei != msitej ) {
            insite_outms <- c(insite_outms, pk_ij)
         }
         
         # if diff site, diff microsite, add to that list
         if (  sitei != sitej) {
           outsite <- c(outsite, pk_ij)
         }
    
      }
   }
}

av_inms <- mean(insite_inms)
av_outms <- mean(insite_outms)
av_out <- mean(outsite)

b <- min(c(insite_inms,insite_outms,outsite)) # Set the minimum for the breakpoints
e <- max(c(insite_inms,insite_outms,outsite)) # Set the maximum for the breakpoints
ax <- pretty(b:e, n = 12) # Make a neat vector for the breakpoints
ax

c1 <- rgb(173,216,230, max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(120,255,190, max = 255, alpha = 80, names = "lt.yellow")

ot <- hist(outsite, breaks=ax, plot=FALSE)
io <- hist(insite_outms, plot=FALSE)
ii <- hist(insite_inms, plot=FALSE)

plot(ot, col=c3, xlim = c(0, 0.8))
plot(io, col=c2, xlim = c(0, 1))
plot(ii,col=c1, add=TRUE)


