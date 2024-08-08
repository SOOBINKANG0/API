########################################################################
## This code is written for using API, in a 'https://www.data.go.kr/' ##
############################ ###########################################
source("source.R")  # you need pkg c('Jsonlite', 'data.table' and so on)

## preprocessing
colnames(area_list) <- c("code", "name", "existence")
area_list[,1] = as.character(area_list[,1])
code_list = area_list[,1] %>% str_sub(start = 1, end = 5) %>% as.data.frame() %>% unique()

row_num  = grep(code_list$., pattern = "\\.") ## to find "."
code_list = code_list[-row_num,]
rm(row_num)

## you can change temp1 and temp2 to change period
temp1 = seq(2023, 2024) %>% as.character()
temp2 = seq(1,12) %>% as.character()
temp2 = ifelse(str_count(temp2) == "1", paste0("0",temp2), temp2)

date_list =numeric()
index = 1

## making data_list loop
for(p in 1:length(temp1)){
    for(q in 1:length(temp2)){
       date_list[index] <- paste0(temp1[p], temp2[q])
       index = index + 1
    }
}
rm(list= ls()[!ls() %in% c("code_list", "date_list")])

## key should be keep safe, and you should check url in the developer page, callback url'
key = "your_key"
url = "https://apis.data.go.kr/1613000/RTMSDataSvcSHRent/getRTMSDataSvcSHRent"

final_result = data.frame() #data bin

## loop for api data 
for(i in 1:length(code_list)){
    LAWD_CD = code_list[i]

    for(j in 1:length(date_list)){
    DEAL_YMD = date_list[j]
    
    ## 
    response = fromJSON(txt = paste0(url,"?serviceKey=",key,"&LAWD_CD=",LAWD_CD,"&DEAL_YMD=",DEAL_YMD))
    
    if (!is.null(response$response$body$items) && response$response$body$items != "") {
    
        temp <- as.data.frame(response$response$body$items$item)
    
        ## I think I can write better code, not just counting number but check colnames
    if (nrow(temp) != 14) { 
        
        temp$buildYear <- NA
        temp = temp %>% relocate(buildYear, .before = contractTerm)
    }
    
    final_result <- rbind(final_result, temp)
    
    }
    Sys.sleep(1)
    }
}

## IF a loop stop, check below
## GET(paste0(url,"?serviceKey=",key,"&LAWD_CD=",LAWD_CD,"&DEAL_YMD=",DEAL_YMD))
write_excel_csv(final_result, "전월세거래내역.csv")
