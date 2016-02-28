#######################################
##Question1 Floating-point
#1
.Machine$integer.max
#The largest integer is 2147483647
#Unit test:
is.integer(2147483647L) == TRUE
is.integer(2147483648L) == FALSE
#L is added since R automatically save a number as double.

#2 Subnormal number example: 1e60

#3 Function for subnormal number
ifsubnormal = function (x){
   return ( (1 + x ==1) || (1-x == 1) )
}
#Test
ifsubnormal(10^(-60) ) ==TRUE # it is subnormal
ifsubnormal(2) == FALSE# it is not subnormal


##########################################
##Question2 Cache Efficiency
#########################################
##Q2-part1
#Since each line in the processor has 64-byte and each integer takes 4 bytes,
#we can save 16 numbers on each line. When we read one number, the following
#15 numbers will also be read into the same line, so it will be fast to use
#the 15 numbers.As a result, when npass is a multiple of 16, it will be 
#very slow for the program to run. Furthermore, we know that there are 12
#cache line in each block, and it is time consuming to cross the block. So
#the worst case will happen when npass = 12*16 = 192.



#######################
##Q2-part2
#L3 is 8192kb on my computer
#way of associativity = 16;
#coherency line size = 64;
#number of set = 8192;
#Based on the analysis above, the worst case takes place when 
#npass = 16*16 = 256.



#######################
##Q2-part3 plot
array1 = rep(c(1:10),60e4); #22.9mb
npassTester = function(array, size, npasses){
  result = 0;
  for (pass in 1:npasses){
    i = pass;
    while(i<size){
      i = i+ npasses;
      result = result + array[i] * pass;
    }
  }
  return (result);
}
#test 
ptm = proc.time()
tester = npassTester(array1,length(array1), 50)
ptm2 = proc.time() - ptm ;
ptm2
 
npasses = seq(16, 16*20, by = 16);
usertime = rep(0, length(npasses));

for (t in 1:length(npasses) ){
  ptm = proc.time();
  result = npassTester(array1,length(array1), npasses[t]);
  ptm2 = proc.time() - ptm ;
  usertime[t] = ptm2[1] ;
}

plot(npasses, usertime, xlab = "npasses", ylab = "Time used")
lines(npasses, usertime, lty = 3, col = "red", lwd = 3)








