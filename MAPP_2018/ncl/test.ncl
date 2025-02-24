begin

   ncldir="./test"
   command="cd " + ncldir + "; pwd"

   system("pwd")
   system(command)
   print(systemfunc(command))
   system("pwd")

end	