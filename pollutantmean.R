pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' 是长度为1的字符向量，指明
    ## CSV文件的位置
    
    ## 'pollutant' 是长度为1的字符向量，指明
    ## 污染物的名称，我们将会计算其
    ## 平均值; 要么是“硫酸盐(sulfate)”要么是“硝酸盐(nitrate)”
    
    ## 'id'是正整数向量，指明监测点的ID，
    ## 将要被要使用的
    
    ## 返回列表内的所有监测点的污染物平均值，
    ## “id”向量中的 (忽略 NA值)
    ans <- data.frame()
    ## 新建一个df，保存得到的数据
    for (item in id){
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
        need <- data[complete.cases(data[,pollutant]),]
        ans <- rbind(ans, need)
        ## ans 就是把以前的数据和现在的数据合并在一起
        
        ## 测试
        #print ("====================")
        #print (head(need))
        #print (nrow(need))
        #print (head(ans))
        #print ("====================")
    }
    round(mean(ans[,pollutant]),3)
}