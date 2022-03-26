
fileOpen ::IO()
fileOpen= do
    
myFile<-openFile "C:/Users/arhuoje/haskell/zip1/app/hello.txt" ReadMode
firstLine<- hGetLine myFile

print firstLine
secondLine<-hGetLine myFile
goodByteFile<-openFile "C:/Users/arhuoje/haskell/zip1/app/chau.txt" WriteMode
hPutStrLn goodByteFile secondLine
hClose myFile
hClose goodByteFile
putStrLn "Well Done !!"