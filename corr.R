corr <- function(directory, threshold = 0) {
    ## 'directory' 是长度为1的字符向量，指明
    ##  CSV 文件的位置
    
    ## 'threshold' 是长度为1的数值向量，指明
    ## 完整观测的案例的数量 (针对所有
    ## 变量) 是必须的，为了计算这两个的相关性：
    ## 硝酸盐(nitrate)和硫酸盐(sulfate); 默认值为 0
    
    ## 返回相关性的数值向量
    ans <- as.numeric()
    ## 创建一个向量
    for(item in c(1:332)){
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
        dataTmp <- data[complete.cases(data),]
        need <- nrow(dataTmp)
        if(need > threshold){
            ans <- c(ans, cor(dataTmp["nitrate"],dataTmp["sulfate"])) 
        } 
    }
    ans
}