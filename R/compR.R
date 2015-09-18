compR <-
function(path="."){print(paste(list.dirs(path=path)))
                  for(folder in list.dirs(full.names = TRUE)){
                    print(folder);
                    for(file in list.files(path=folder,pattern="*.rnw")){
                      print("##############################################")
                      print(file)
                      Sweavelst(file=file,path=folder)}}}
