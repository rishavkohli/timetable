library(stringr)

Mcs2Y<- matrix(,6,9)
print(Mcs2Y)
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
  {Mcs2Y[i,1]<<-days[i-1]
  
  }
}
time_setting<-function()
{
  for(i in 2:9)
  {
    Mcs2Y[1,i]<<-time[i-1]
  }
}

#temp<- naming_days(Mcs1Y)
#Mcs1Y<-temp 

#print(Mcs1Y)

subjects<-c('Mcs-304(MS)','Mcs-304(MS)(Lab)','Mcs-303(PB)','Mcs-303(PB)(Lab)','Mcs-302(RC)','Mcs-302(RC)(Lab)','Mcs-311(VB)','Mcs-311(VB)(Lab)','Mcs-312(NG)','Mcs-326(SK)(Lab)','Mcs-326(SK)','--')

count_limit<-function(sub)
{ count<-0
for(i in 2:6)
{ for(j in 2:9)
{ if(is.na(Mcs2Y[i,j])!=TRUE) 
{if(Mcs2Y[i,j]==sub)
{count<-count+1
}
}
}
}
return(count)
}



constraints2<-function(sub,row,column)
{  for(i in 2:9)
{ if(is.na(Mcs2Y[row,i])!=TRUE && (str_detect(sub, "Lab")!=TRUE) && (sub!='--') )
{if(Mcs2Y[row,i]==sub)
  return(FALSE)
}}
  if(sub=='Mcs-304(MS)')
  {
    if(count_limit("Mcs-304(MS)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-304(MS)(Lab)')
  {
    if(count_limit("Mcs-304(MS)(Lab)")>=2)
      return(FALSE)
  }
  if(sub=='Mcs-303(PB)')
  {
    if(count_limit("Mcs-303(PB)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-303(PB)(Lab)')
  {
    if(count_limit("Mcs-303(PB)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-302(RC)')
  {
    if(count_limit("Mcs-302(RC)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-302(RC)(Lab)')
  {
    if(count_limit("Mcs-302(RC)(Lab)")>=4)
      return(FALSE)
  }
  if(sub=='Mcs-311(VB)')
  {
    if(count_limit("Mcs-311(VB)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-311(VB)(Lab)')
  {
    if(count_limit("Mcs-311(VB)(Lab)")>=2)
      return(FALSE)
  }
  if(sub=='Mcs-312(NG)')
  {
    if(count_limit("Mcs-312(NG)")>=3)
      return(FALSE)
  }
  if(sub=='Mcs-326(SK)(Lab)')
  {
    if(count_limit("Mcs-326(SK)(Lab)")>=2)
      return(FALSE)
  }
  
  if(sub=='Mcs-326(SK)')
  {
    if(count_limit("Mcs-326(SK)")>=3)
      return(FALSE)
  }
  
  if(sub=='--')
  {
    if(count_limit("--")>=8)
      return(FALSE)
  }
  
  if(str_detect(sub, "Lab"))
  {
    if(column==9)
    { return(FALSE)
    }
    if(is.na(Mcs2Y[row,column+1])!=TRUE)
    {
      return(FALSE)
    }
  }
  return(TRUE)
}
naming_days()
time_setting()
print(Mcs2Y)

time_tableMsc2Y<-function()
{
  
  
  for(i in 2:6)
  {for(j in 2:9)
  { 
    if(is.na(Mcs2Y[i,j])==TRUE)
      
    { val<-random_select(12)
    value<-subjects[val]
    
    for(k in 1:length(value))
    { if(constraints2(value[k],i,j) && (str_detect(value[k], "Lab")))
    { 
      Mcs2Y[i,j]<<-value[k]
      Mcs2Y[i,j+1]<<-value[k]
      if(time_tableMsc2Y())
      {  return(TRUE)}
      else
      {
        Mcs2Y[i,j]<<-NA
        Mcs2Y[i,j+1]<<-NA
      }
      
    }
      else if(constraints2(value[k],i,j) && ((str_detect(value[k], "Lab"))!=TRUE))
      {  
        Mcs2Y[i,j]<<-value[k]
        if(time_tableMsc2Y())
        {  return(TRUE)}
        else
        {
          Mcs2Y[i,j]<<-NA
        }
      }
      
      
    }
    return(FALSE)
    }
  }
  }
  return(TRUE)
}
time_tableMsc2Y()
print(Mcs2Y)

time_table1<-as.data.frame(Mcs2Y)

#index<-sample(1:10,10,replace=T)
 