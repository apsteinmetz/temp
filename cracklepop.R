# Write a program that prints out the numbers 1 to 100 (inclusive). If the
# number is divisible by 3, print Crackle instead of the number. If it's
# divisible by 5, print Pop. If it's divisible by both 3 and 5, print
# CracklePop. You can use any language.

for (i in 1:100){
  s1 <- ""
  s2 <- ""
  if (i %% 3 == 0) s1 <-"Crackle"
  if (i %% 5 == 0) s2 <-"Pop"
  s3 <- paste0(s1,s2)
  if (nchar(s3)==0){
    print(i)
  } else{
    print(s3)
  }
  
}
