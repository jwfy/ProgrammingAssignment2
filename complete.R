complete <- function(directory, id = 1:332) {
    ## 'directory' 是长度为1的字符向量，指明
    ##  CSV 文件的位置
    
    ## 'id' 是正整数向量，指明监测点的ID号，
    ## 将要被使用的
    
    ## 返回以下格式的数据帧：
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## 其中'id' 是监测点ID编号，而'nobs'是
    ## 完整案例的数量
    ans <- data.frame()
    ## 新建一个df，保存得到的数据
    for(item in id){
        if (item < 10)
            name <- paste("00", as.character(item), ".csv", sep="") 
        else if(item < 100)
            name <- paste("0", as.character(item), ".csv", sep="")
        else
            name <- paste(as.character(item), ".csv", sep="")
        path <- paste(directory, name, sep="/")
        ## path 为读取文件的路径
        data <- read.csv(path)
        ## data 为读取的数据        
        need <- nrow(data[complete.cases(data),])
        ## need 为筛选数据后的行数
        temp <- data.frame(id=item, nobs=need)
        ans <- rbind(ans, temp)
    }
    ans
}