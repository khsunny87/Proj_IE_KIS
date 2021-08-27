
## Proj_IE_KIS

library(Microsoft365R)

od <- get_personal_onedrive(auth_type="device_code") #Ubunbu
#od <- get_personal_onedrive() #Mac

fpath<-'My journal/OA/IE_KIS/Data/'
fname<-'IE_KIS_data.xlsx'
#od$list_items(fpath)

input_path<-'Input/'
destfile <- "IE_KIS_data.xlsx"

od$download_file(paste0(fpath,fname),paste0(input_path,destfile),overwrite=T)

