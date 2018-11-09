library(stringr)

Mcs1Y<- matrix(,6,9)
print(Mcs1Y)
time<-c('9-10','10-11','11-12','12-1','1-2','2-3','3-4','4-5')
days<-c('monday','tuesday','wednesday','thrusday','friday')

random_select<-function(number)
{  i<-0
   
  index<-c(0,0,0,0,0,0,0,0,0,0,0)
while(i<number)
  { flag<-0
  temp<-sample(1:11,1,replace=T)
     for(j in 1:length(index))
     {if(index[j]==temp)
       {flag<-1
        }
     }
    if(flag==0)
    {index[i+1]<-temp
       i<-i+1
    }
  }
  
  return(index)
}






naming_days<-function()
{ 
  for(i in 2:6)
  {Mcs1Y[i,1]<<-days[i-1]
  
  }
}
time_setting<-function()
{
  for(i in 2:9)
  {
    Mcs1Y[1,i]<<-time[i-1]
  }
  }

#temp<- naming_days(Mcs1Y)
#Mcs1Y<-temp 

#print(Mcs1Y)

subjects<-c('Mcs-101(NG)','Mcs-101(NG)(Tut.)','Mcs-102(PB)','Mcs-102(PB)(Lab)','Mcs-103(RG)','Mcs-103(RG)(Lab)','Mcs-104(RK)','Mcs-104(RK)(Lab)','Mcs-105(NK)','Mcs-105(NK)(Lab)','--')

count_limit<-function(sub)
{ count<-0
  for(i in 2:6)
  { for(j in 2:9)
  { if(is.na(Mcs1Y[i,j])!=TRUE) 
    {if(Mcs1Y[i,j]==sub)
        {count<-count+1
    }
    }
    }
    }
  return(count)
}



constraints<-function(sub,row,column)
{  for(i in 2:9)
  { if(is.na(Mcs1Y[row,i])!=TRUE && (str_detect(sub, "Lab")!=TRUE))
  {if(Mcs1Y[row,i]==sub)
     return(FALSE)
  }}
  if(sub=='Mcs-101(NG)')
  {
    if(count_limit("Mcs-101(NG)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-101(NG)(Tut.)')
  {
    if(count_limit("Mcs-101(NG)(Tut.)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-102(PB)')
  {
    if(count_limit("Mcs-102(PB)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-102(PB)(Lab)')
  {
    if(count_limit("Mcs-102(PB)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-103(RG)')
  {
    if(count_limit("Mcs-103(RG)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-103(RG)(Lab)')
  {
    if(count_limit("Mcs-103(RG)(Lab)")>=2)
      return(FALSE)
  }
  if(sub=='Mcs-104(RK)')
  {
    if(count_limit("Mcs-104(RK)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-104(RK)(Lab)')
  {
    if(count_limit("Mcs-104(RK)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-105(NK)')
  {
    if(count_limit("Mcs-105(NK)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-105(NK)(Lab)')
  {
    if(count_limit("Mcs-105(NK)(Lab)")>=8)
      return(FALSE)
  }
  if(sub=='--')
  {
    if(count_limit("--")>=2)
      return(FALSE)
  }
  
  if(str_detect(sub, "Lab"))
  {
    if(column==9)
    { return(FALSE)
    }
    if(is.na(Mcs1Y[row,column+1])!=TRUE)
    {
      return(FALSE)
    }
  }
return(TRUE)
}
naming_days()
time_setting()
print(Mcs1Y)

time_tableMsc1Y<-function()
{
  
  
  for(i in 2:6)
  {for(j in 2:9)
  { 
    if(is.na(Mcs1Y[i,j])==TRUE)
    
    { val<-random_select(11)
    value<-subjects[val]
    
    for(k in 1:length(value))
    { if(constraints(value[k],i,j) && (str_detect(value[k], "Lab")))
     { 
         Mcs1Y[i,j]<<-value[k]
         Mcs1Y[i,j+1]<<-value[k]
         if(time_tableMsc1Y())
         {  return(TRUE)}
         else
         {
           Mcs1Y[i,j]<<-NA
           Mcs1Y[i,j+1]<<-NA
         }
        
    }
      else if(constraints(value[k],i,j) && ((str_detect(value[k], "Lab"))!=TRUE))
      {  
        Mcs1Y[i,j]<<-value[k]
        if(time_tableMsc1Y())
        {  return(TRUE)}
        else
        {
          Mcs1Y[i,j]<<-NA
        }
      }
        
     
    }
    return(FALSE)
  }
  }
  }
  return(TRUE)
}
time_tableMsc1Y()
print(Mcs1Y)

time_table<-as.data.frame(Mcs1Y)

#index<-sample(1:10,10,replace=T)
#value<-subjects[index]
#print(value)
#for(k in 1:length(subjects))
#{print(str_detect(value[k], "Lab"))
#}

#index<-sample(1:10,10,replace=T)
#value<-subjects[index]
#print(value)
