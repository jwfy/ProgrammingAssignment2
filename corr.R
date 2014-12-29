corr <- function(directory, threshold = 0) {
    ## 'directory' �ǳ���Ϊ1���ַ�������ָ��
    ##  CSV �ļ���λ��
    
    ## 'threshold' �ǳ���Ϊ1����ֵ������ָ��
    ## �����۲�İ��������� (�������
    ## ����) �Ǳ���ģ�Ϊ�˼���������������ԣ�
    ## ������(nitrate)��������(sulfate); Ĭ��ֵΪ 0
    
    ## ��������Ե���ֵ����
    ans <- as.numeric()
    ## ����һ������
    for(item in c(1:332)){
        if (item < 10)
            name <- paste("00", as.character(item), ".csv", sep="") 
        else if(item < 100)
            name <- paste("0", as.character(item), ".csv", sep="")
        else
            name <- paste(as.character(item), ".csv", sep="")
        path <- paste(directory, name, sep="/")
        ## path Ϊ��ȡ�ļ���·��
        data <- read.csv(path)
        ## data Ϊ��ȡ������ 
        dataTmp <- data[complete.cases(data),]
        need <- nrow(dataTmp)
        if(need > threshold){
            ans <- c(ans, cor(dataTmp["nitrate"],dataTmp["sulfate"])) 
        } 
    }
    ans
}