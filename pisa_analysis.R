library(dplyr)
library(XML)
htmlTreeParse("http://beta.icm.edu.pl/PISAcontest/data/", useInternalNodes = T) %>% 
  xmlRoot %>% 
  xpathApply("//a", xmlGetAttr, "href") %>% 
  lapply(function(i) download.file(paste0("http://beta.icm.edu.pl/PISAcontest/data/", i), paste0(".",i)))

oowd = getwd()
setwd("/media/robert/")

student2012 = list.files()[1] %>%
  paste0("/data/Pisa/student2012.csv") %>%
  read.csv

student2012dict = list.files()[1] %>%
  paste0("/data/Pisa/student2012dict.csv") %>%
  read.csv

library(ggplot2)

NAstudent <- student2012[student2012$CNT %in% c("Florida (USA)","Connecticut (USA)", "Massachusetts (USA)", "Canada", "Mexico", "United States of America"),]
rm(student2012)
NAstudent[,1] <- factor(NAstudent[,1])
n <- names(NAstudent)
mother_rows <- grep("Mother", student2012dict)[1:7]
father_rows <- grep("Father", student2012dict)[1:7]

mother_vars <- names(student2012dict[mother_rows])
father_vars <- names(student2012dict[father_rows])
iter = 0


PVmean <- function(row_number, subj = c("MATH", "SCIE", "READ")){
  outData <- row_number
  for(i in subj){
    column2 <- sapply(row_number, function(j) mean(sapply(1:5, function(k) NAstudent[j, paste0("PV", k, i)])))
    outData <- cbind(outData, column2)
  }
  outData <- as.data.frame(outData)
  names(outData) <- c("row.number", subj)
  return(outData)
}


for(i in 1:length(mother_rows)){
  mother <- mother_rows[i] 
  father <- father_rows[i]

    
    for(j in c(mother, father)){
      print(student2012dict[j])
      value <- levels(NAstudent[,j])
      for(k in value){
        student_rows <- which(NAstudent[,n[j]] == k)
        student_countries <- NAstudent$CNT[student_rows]
        if(length(unique(student_countries)) < 5) next
        CNTcounts <- ddply(NAstudent, "CNT", .fun = nrow)
        questionData <- data.frame(CNT = student_countries, 
                                   MATH = PVmean(student_rows, "MATH")[,-1],
                                   QUESTION = student2012dict[j],
                                   ANSWER = k)
        questionData <- merge(questionData, CNTcounts, by.y = "CNT")
        CNTcounts$pct.samp <- ddply(questionData, "CNT", .fun = nrow)[,2]/CNTcounts[,2]
        questionData <- merge(questionData, CNTcounts, by = "CNT")
        allData <- rbind(allData, questionData)
        
        p <- ggplot(questionData,
                    aes(x = CNT, y = MATH, fill = pct.samp)
                    )
        iter = iter + 1
        assign(paste0("p", iter), p + geom_violin(adjust = .7) 
                                    + scale_alpha_continuous()
                                    + ggtitle(paste0(student2012dict[j], "\n", k))
                                    + theme(axis.text.x = element_text(angle = 20, hjust = 0))
               )
      }
  }
}

maths.lm <- "PV1MATH" %>% 
  paste(grep("Math Behaviour", student2012dict) %>%
    names(NAstudent)[.] %>% 
    paste(collapse = "+"), sep = "~") %>% 
  as.formula %>% 
  lm(NAstudent)

