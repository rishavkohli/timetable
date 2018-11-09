library(stringr)

Mca1Y<- matrix(,6,9)
print(Mca1Y)
time<-c('9-10','10-11','11-12','12-1','1-2','2-3','3-4','4-5')
days<-c('monday','tuesday','wednesday','thrusday','friday')

random_select<-function(number)
{  i<-0

index<-c(0,0,0,0,0,0,0,0,0,0,0,0)
while(i<number)
{ flag<-0
temp<-sample(1:12,1,replace=T)
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
  {Mca1Y[i,1]<<-days[i-1]
  
  }
}
time_setting<-function()
{
  for(i in 2:9)
  {
    Mca1Y[1,i]<<-time[i-1]
  }
}

#temp<- naming_days(Mcs1Y)
#Mcs1Y<-temp 

#print(Mcs1Y)

subjects<-c('Mca-101(NK)','Mca-101(NK)(Lab)','Mca-102(SV)','Mca-102(SV)(Lab)','Mca-103(PR)','Mca-103(PR)(Lab)','Mca-104(NS)','Mca-104(NS)(Lab)','Mca-106(SP)','Mca-105(DK)(Tut.)','Mca-105(DK)','--')
#
count_limit<-function(sub)
{ count<-0
for(i in 2:6)
{ for(j in 2:9)
{ if(is.na(Mca1Y[i,j])!=TRUE) 
{if(Mca1Y[i,j]==sub)
{count<-count+1
}
}
}
}
return(count)
}



constraints3<-function(sub,row,column)
{  for(i in 2:9)
{ if(is.na(Mca1Y[row,i])!=TRUE && (str_detect(sub, "Lab")!=TRUE) && (sub!='--') )
{if(Mca1Y[row,i]==sub)
  return(FALSE)
}}
  if(sub=='Mca-101(NK)')
  {
    if(count_limit("Mca-101(NK)")>=2)
      return(FALSE)
  }
  if(sub=='Mca-101(NK)(Lab)')
  {
    if(count_limit("Mca-101(NK)(Lab)")>=6)
      return(FALSE)
  }
  if(sub=='Mca-102(SV)')
  {
    if(count_limit("Mca-102(SV)")>=3)
      return(FALSE)
  }
  if(sub=='Mca-102(SV)(Lab)')
  {
    if(count_limit("Mca-102(SV)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mca-103(PR)')
  {
    if(count_limit("Mca-103(PR)")>=3)
      return(FALSE)
  }
  if(sub=='Mca-103(PR)(Lab)')
  {
    if(count_limit("Mca-103(PR)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mca-104(NS)')
  {
    if(count_limit("Mca-104(NS)")>=3)
      return(FALSE)
  }
  if(sub=='Mca-104(NS)(Lab)')
  {
    if(count_limit("Mca-104(NS)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mca-106(SP)')
  {
    if(count_limit("Mca-106(SP)")>=4)
      return(FALSE)
  }
  if(sub=='Mca-105(DK)(Tut.)')
  {
    if(count_limit("Mcs-326(SK)(Lab)")>=1)
      return(FALSE)
 }
  
  if(sub=='Mca-105(DK)')
  {
    if(count_limit("Mca-105(DK)")>=4)
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
    if(is.na(Mca1Y[row,column+1])!=TRUE)
    {
      return(FALSE)
    }
  }
  return(TRUE)
}
naming_days()
time_setting()
print(Mca1Y)

time_tableMca1Y<-function()
{
  
  
  for(i in 2:6)
  {for(j in 2:9)
  { 
    if(is.na(Mca1Y[i,j])==TRUE)
      
    { val<-random_select(12)
    value<-subjects[val]
    
    for(k in 1:length(value))
    { if(constraints3(value[k],i,j) && (str_detect(value[k], "Lab")))
    { 
      Mca1Y[i,j]<<-value[k]
      Mca1Y[i,j+1]<<-value[k]
      if(time_tableMca1Y())
      {  return(TRUE)}
      else
      {
        Mca1Y[i,j]<<-NA
        Mca1Y[i,j+1]<<-NA
      }
      
    }
      else if(constraints3(value[k],i,j) && ((str_detect(value[k], "Lab"))!=TRUE))
      {  
        Mca1Y[i,j]<<-value[k]
        if(time_tableMca1Y())
        {  return(TRUE)}
        else
        {
          Mca1Y[i,j]<<-NA
        }
      }
      
      
    }
    return(FALSE)
    }
  }
  }
  return(TRUE)
}
time_tableMca1Y()
print(Mca1Y)

time_table2<-as.data.frame(Mca1Y)

#index<-sample(1:10,10,replace=T)
