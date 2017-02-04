#Intuit Coding Challenge 
##@author Preeti Sah
#Date created - 01/31/2017

#installing all required R packages and library
install.packages(c("Rcpp", "readr"))
install.packages('reshape')
install.packages('scales')
install.packages('igraph')
install.packages('plyr')
install.packages('dplyr')
install.packages('cluster')
library(fpc)
library(igraph)
library(readr)
library(plyr)
library(dplyr)
library(data.table)
library(reshape)
library(scales)
library(cluster)
library(fpc)


#setting the working directory , this is the path of directory where all data files are present
setwd("C:/Users/vijetasah/Desktop/rit-challenge-master/rit-challenge-master/transaction-data/")

#retrieving all csv files
files<-list.files(pattern = "*.csv")

#combining all 100 csv files into one table named as tbl
tbl = lapply(files, read_csv) %>% bind_rows()

#Performing the data cleaning 
#setting invalid dates to last date of the month
tbl$Date<-gsub("/32/","/31/",tbl$Date)

#setting the date format as mm/dd/yy
tbl$Date1<-as.Date(tbl$Date,"%m/%d/%Y")

#finding all user transactions that are credit transactions like paycheck 
#where user is getting the amount and not spending
users_with_income<-tbl[tbl$Amount>0,]


#detach a package (done to avoid any error)
detach("package:plyr", unload=TRUE)

#creating a table called income which has different derived attributes related to income of each user
#such as mean income, minimum income, maximum income value , total number of times paycheck received,  
#first & last date of income paycheck received over a period of two years  
income<-users_with_income  %>%group_by(auth_id)  %>%summarise(mean_income=mean(Amount),min_income=min(Amount),max_income=max(Amount),count_n=n(),last_date=max(Date1),first_date=min(Date1))

#user_exp is table to store all expenditure transactios of user over 2year time
#ie transaction having Amount < 0
users_exp<-tbl[tbl$Amount<0,]

# grouping the expenses based by auth_id and storing the mean expenditure , maximum expenditure
#minimum expenditure , total times purchase done and total expenditure over 2 years 
expense<-users_exp  %>%group_by(auth_id)  %>%summarise(mean_exp=mean(Amount),max_exp=min(Amount),min_exp=max(Amount),count_n=n(),total_exp=sum(Amount))

#merging the income and expense tables based on auth_id
income_expense<-merge(income,expense,by.x = "auth_id",by.y = "auth_id",all.x = T,all.y = T)

###Finding a derived attribute "have_child " ,to determine if a given user has child or not###

#fetching all auth_id that have Baby products in their transaction
babies_account_id<-unique(tbl$auth_id[tbl$Vendor %like% "Bab"])

#adding the have_child column in income_expense table
#if any auth id is equal to any element in list of babies_count_id set this 
#attribute as 1 else 0
income_expense$have_child<-ifelse(income_expense$auth_id %in% babies_account_id,1,0)


#groupig the user expenditure based on auth_id and vendor and find mean, max , min
#total number of time purchase done  , total expenditure for each vendor , 
#first and last transaction for a vendor in 2 years
All_Activity<-users_exp  %>%group_by(auth_id,Vendor)  %>%summarise(mean_exp=mean(Amount),max_exp=min(Amount),min_exp=max(Amount),count_n=n(),total_exp=sum(Amount),min_date=min(Date1),max_date=max(Date1))

#finding how recently a purchase is done by user at particular vendor
#ie what is the interval count between two purchases at particular vendor
All_Activity$recency<-(All_Activity$max_date-All_Activity$min_date)/All_Activity$count_n
All_Activity$recency<-ifelse(All_Activity$recency==1,365,All_Activity$recency)

#removing Housing rent and Loan type of transactions from hobies  
hobies<-All_Activity[All_Activity$Vendor !="Housing Rent" & !All_Activity$Vendor %like% "Loan",]


##Creating a table named new_order which will store only top 10 transactions 
##(based on highest expenditure) for each user

new_order<-arrange(hobies,auth_id,-count_n,total_exp)
#addinng a column named cum_sum to find the cumulative sum of each user top 10 expenses
new_order$cum_sum<-0
#adding column named total_sum to find sum of all top 10 user expenses in 2 years
new_order$total_sum<-0
#enumerate to store 1 to 10 values for each user
new_order$enum<-0

#calculating cumulative sum and total sum for each user

j<-1
for(i in 1:nrow(new_order)){
  if(i==1){
    new_order$cum_sum[i]<-new_order$total_exp[i]
    new_order$enum[i]<-j
    j<-j+1
  }
  else{
    if(new_order$auth_id[i-1]==new_order$auth_id[i]){
      new_order$cum_sum[i]<-new_order$total_exp[i]+new_order$cum_sum[i-1]
      new_order$enum[i]<-j
      new_order$total_sum[new_order$auth_id==new_order$auth_id[i]]<-new_order$cum_sum[i]
      j<-j+1
      
    }
    else
    {
      
      new_order$cum_sum[i]<-new_order$total_exp[i]
      new_order$enum[i]<-1
      j<-2
    }
  }
}

#finding the percentile to observe the pattern that what is percentage
#change in expenditure as me move from top to bottom 10 transaction for each change
new_order$percentile<-new_order$cum_sum/new_order$total_sum


#creating a copy of new_order table 
new_order_1<-new_order
new_order<-new_order[new_order$enum<=10,]

#finding whether a user expenditure is on weekday or wwekend
users_exp$weekday<-weekdays(users_exp$Date1)
users_exp$is_week_end<-ifelse(users_exp$weekday %in% c("Saturday",'Sunday'),1,0)

#finding mean expenditure on different days of the week
#it is observed that maximum expenditure occurs on Tuesday
weekend_exp<-users_exp %>% group_by(weekday)%>% summarise(mean_exp=mean(Amount))


#creating a copy of all_activity table
All_Activity_copy<-cbind(All_Activity)

#pivoting based on mean value , so the table will have all mean expenditures of diff hobbies
#for each user
All_Activity_mean <- subset(All_Activity_copy, select = c(auth_id,Vendor,mean_exp))
All_Activity_mean_pivot<-cast(All_Activity_mean, auth_id ~ Vendor)
colnames(All_Activity_mean_pivot)[2:ncol(All_Activity_mean_pivot)] <- paste("mean_", colnames(All_Activity_mean_pivot[,c(2:ncol(All_Activity_mean_pivot))]), sep = "")
All_Activity_mean_pivot[is.na(All_Activity_mean_pivot)] <- 0

#pivoting based on max value , so the table will have all max expenditures of diff hobbies
#for each user
All_Activity_max <- subset(All_Activity_copy, select = c(auth_id,Vendor,max_exp))
All_Activity_max_pivot<-cast(All_Activity_max, auth_id ~ Vendor)
colnames(All_Activity_max_pivot)[2:ncol(All_Activity_max_pivot)] <- paste("max_", colnames(All_Activity_max_pivot[,c(2:ncol(All_Activity_max_pivot))]), sep = "")
All_Activity_max_pivot[is.na(All_Activity_max_pivot)] <- 0

#pivoting based on min value , so the table will have all min expenditures of diff hobbies
#for each user
All_Activity_min <- subset(All_Activity_copy, select = c(auth_id,Vendor,min_exp))
All_Activity_min_pivot<-cast(All_Activity_min, auth_id ~ Vendor)
colnames(All_Activity_min_pivot)[2:ncol(All_Activity_min_pivot)] <- paste("min_", colnames(All_Activity_min_pivot[,c(2:ncol(All_Activity_min_pivot))]), sep = "")
All_Activity_min_pivot[is.na(All_Activity_min_pivot)] <- 0

#pivoting based on count value , so the table will have all count of vedor expenditures of diff hobbies
#for each user
All_Activity_count<- subset(All_Activity_copy, select = c(auth_id,Vendor,count_n))
All_Activity_count_pivot<-cast(All_Activity_count, auth_id ~ Vendor)
colnames(All_Activity_count_pivot)[2:ncol(All_Activity_count_pivot)] <- paste("count_", colnames(All_Activity_count_pivot[,c(2:ncol(All_Activity_count_pivot))]), sep = "")
All_Activity_count_pivot[is.na(All_Activity_count_pivot)] <- 0

#pivoting based on recency value , so the table will have all count of expenditures of diff hobbies
#for each user
All_Activity_recency <- subset(All_Activity_copy, select = c(auth_id,Vendor,recency))
All_Activity_recency_pivot<-cast(All_Activity_recency, auth_id ~ Vendor)
colnames(All_Activity_recency_pivot)[2:ncol(All_Activity_recency_pivot)] <- paste("recency_", colnames(All_Activity_recency_pivot[,c(2:ncol(All_Activity_recency_pivot))]), sep = "")
All_Activity_recency_pivot[is.na(All_Activity_recency_pivot)] <- 0

#pivoting based on total expense value , so the table will have all total_expense of diff hobbies
#for each user
All_Activity_expense <- subset(All_Activity_copy, select = c(auth_id,Vendor,total_exp))
All_Activity_expense_pivot<-cast(All_Activity_expense, auth_id ~ Vendor)
colnames(All_Activity_expense_pivot)[2:ncol(All_Activity_expense_pivot)] <- paste("expense_", colnames(All_Activity_expense_pivot[,c(2:ncol(All_Activity_expense_pivot))]), sep = "")
All_Activity_expense_pivot[is.na(All_Activity_expense_pivot)] <- 0


###Merging all pivot dataframes into one dataframe

PIVOT_MERGE_1 <-merge.data.frame(All_Activity_mean_pivot,All_Activity_recency_pivot,by="auth_id")
PIVOT_MERGE_ALL<-merge.data.frame(PIVOT_MERGE_1,All_Activity_expense_pivot,by="auth_id")

#calculting subset of income_expense table
income_subset<-subset(income_expense,select=c(auth_id,mean_income,mean_exp,total_exp,have_child))

#merging pivot and income_subset
MERGE_ALL<-merge.data.frame(PIVOT_MERGE_ALL,income_subset,by="auth_id")


###TOP 10 ACTIVITIES MATRIX
#After finding top 10 hobies for each user , set flag as 1 if a user has that hobby
#and pivot the table based on hobies
#so each column is hobby name and if user has that column in top 10 hobby list then 
#that column will have value as 1 else 0
hobies_copy_top_10<-cbind(new_order)
hobies_copy_top_10$flag<-1
hobies_copy_top_10 <- subset(hobies_copy_top_10, select = c(auth_id,Vendor,flag))
hobies_copy_pivot_top_10<-cast(hobies_copy_top_10, auth_id ~ Vendor)
hobies_copy_pivot_top_10[is.na(hobies_copy_pivot_top_10)]<-0


##FINDIG THE TOTAL GROUPS IN DATA ####

#Making a table called meta_data to store all pivoted data
meta_data<-MERGE_ALL

#cleaning the table by replacing all na, nan and Inf values as 0
for(row in 1:nrow(meta_data)){
  for(col in 2:ncol(meta_data)){
    if(is.na(meta_data[row,col]) || is.nan(meta_data[row,col]) || meta_data[row,col]==Inf){
      meta_data[row,col]=0
    }
  }
}

#calculating sum of squared errors
SSE <- (nrow(meta_data)-1)*sum(apply(meta_data[c(-1)],2,var))
#for eack k(number of clusters from 2 to 15) storing the corresponding SSE values
for(index in 2:15) SSE[index] <- sum(kmeans(meta_data[c(-1)],centers = index)$withinss)
#plotting the knee graph to determine the most appropriate K value
plot(1:15, SSE, type="b", xlab="K value", ylab="SSE")

##By observing the knee graph we can say that we will have about 6 clusters of users
#based on different hobbies


#Plotting the different clusters based of user hobbies
dist=cor(t(data.matrix(hobies_copy_pivot_top_10[,-c(1)])))
dist[dist<0.5]=0
diag(dist)=0
g1<-graph.adjacency(dist,weighted="TRUE",mode="undirected")
lay<-layout.fruchterman.reingold(grap=g1,niter=6000)
par(mar=c(0.5,0.5,0.5,0.5))
plot.igraph(g1, 
            mark.shape=1 , 
            #mark.groups=list(c(1:3) , c(4:6) , c(7:9) ) , 
            layout=lay,vertex.label=rownames(dist) ,
            vertex.color="green" , 
            vertex.size=0.5 , 
            edge.arrow.size=13 , 
            main="")

##based on the cluster plot find the rows related to each cluster based index number 
##present in cluster circles

#Students
##this group is purchased science , maths books and student goods
Student_group<-hobies_copy_pivot_top_10[c(64,22,84,6,62,52,91,15,10,70,75,81,6,82,93,85),]

#Athetics & Sports Lover
#group of Amazon order atheltic equipment, Athletic apparel,bike rental, Dicks sporting goods, GNC, NBA Tickets , NFL Tickets, Sam's sporting Apparel, Total Gym Fees, Vitamin Shoppe
Sports_Athlethics_Group<-hobies_copy_pivot_top_10[c(30,53,72,33,78,19,94,44,90,24,45,59,55,96,51,18,37,41,57,83,76,55,8,39),]

#Food ,Home Entertaiment & Pet Lovers
#group having transaction realted to Food Delivery GrubHub, Food Delivery Uber Eats, On Demand Movie, On Demand TV, Pet Smart, Pet Supply Cat Food, Playstation Membership
#PodCast subscription Red Box DVD Rental, Wine Delivery
Foddie_Pet_Movies_Group<-hobies_copy_pivot_top_10[c(46,14,25,27,87,89,43,32,56,49,81,77,48,13,29,20,79,98,77,16),]


#Bowlig,Ice Skating & Bar - Club Lovers
#group having transaction related to bowling, concert ticket, Fisker Night Club , Green Flash Brewery, Ice Skating, John's Bar & Restauarnt
#Karaoke Bar, Movie, Owl Night Club, Rodrigue's Bar & Grill , Seaside Bar , Wine Bar
Movies_Club_Group <- hobies_copy_pivot_top_10[c(60,73,63,88,35,38,12,31,26,58,92,54,99,47,2,97,40,95),]

#Art & Music Lover
#group having transaction related to Amazon order Paint Brushes, Art, Guitar , Paino
Art_Music_Group<-hobies_copy_pivot_top_10[c(3,4,1,69,36,66,100,86,23,80,7,65,74,68,42,17,21,71,50),]


###getting the group in order_new_1

#creating a new column named Vendor group to assign vendor group names for each cluster of users formed above 
#find the vendor description that lie in Sports and athelitcs interested user
list_of_hobies_in_this_group<-unique(hobies_copy_top_10$Vendor[hobies_copy_top_10$auth_id %in% unique(Sports_Athlethics_Group$auth_id)])

#assigning other as intial vendor group 
new_order_1$Vendor_group<-"Others"


#all vendor description of users who buy Sprts and Athletics stuff is a named as Sports_and_Athlethics
new_order_1$Vendor_group[new_order_1$Vendor %in% list_of_hobies_in_this_group]<-"Sports_and_Athlethics"

#all vendor description of users who buy Music and Arts stuff is a named as Art and Music
list_of_hobies_in_this_group<-unique(hobies_copy_top_10$Vendor[hobies_copy_top_10$auth_id %in% unique(Art_Music_Group$auth_id)])
#removing vendors who are are not dominant in all users
new_order_1$Vendor_group[new_order_1$Vendor %in% list_of_hobies_in_this_group[-c(10,11,12,13,14)]]<-"Art and Music"

#all vendor description of users who spend on Movie, Concert  & Club is a named as Movie_concert_club 
list_of_hobies_in_this_group<-unique(hobies_copy_top_10$Vendor[hobies_copy_top_10$auth_id %in% unique(Movies_Club_Group$auth_id)])
new_order_1$Vendor_group[new_order_1$Vendor %in% list_of_hobies_in_this_group[-c(12,14,15,16)]]<-"Movie_concert_club"

#all vendor description of users who spend on Food , Pet and Movies is a named as Foodie, pet lover and Movies
list_of_hobies_in_this_group<-unique(hobies_copy_top_10$Vendor[hobies_copy_top_10$auth_id %in% unique(Foddie_Pet_Movies_Group$auth_id)])
new_order_1$Vendor_group[new_order_1$Vendor %in% list_of_hobies_in_this_group[-c(11,12,13,14)]]<-"Foodie, pet lover and Movies"

#all vendor description of users who spend on student stuff like books etc is a named as Student
list_of_hobies_in_this_group<-unique(hobies_copy_top_10$Vendor[hobies_copy_top_10$auth_id %in% unique(Student_group$auth_id)])
new_order_1$Vendor_group[new_order_1$Vendor %in% list_of_hobies_in_this_group[-c(6,10,11,16,17,18,19,20,21,22,23,24:28)]]<-"Student"


#Plotting a PIE Graph for amount spend by all users on different vendors
hobies_copy_top_10<-cbind(new_order)
all_expenses<-hobies_copy_top_10 %>% group_by(Vendor)%>% summarise(Count_n=n())
pie(all_expenses$Count_n,all_expenses$Vendor,main="Expenditure on Different Vendors by all users")


#overall pie chart based on 6 vendor groups formed for 6 cluster

overall<-new_order_1 %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(overall$cc),overall$Vendor_group,main="Expenses for 6 Vendor Group by All Users")


#plotting pie chart showing overall expenses of each group in its own group as well as 5 other groups

#Process is to find auth_id for each cluster
#Then find sum of expenses of user for vendors of that cluster and other cluster as well
#Plotting this data on Pie chart
auth_id_Sports_Health_group<-unique(Sports_Athlethics_Group$auth_id)
Sport_Health_User_Hobies<-new_order_1[new_order_1$auth_id %in% auth_id_Sports_Health_group,]
Sports_Health_Vendor_Grouped_Transc<-Sport_Health_User_Hobies %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(Sports_Health_Vendor_Grouped_Transc$cc),Sports_Health_Vendor_Grouped_Transc$Vendor_group,main="Expenses of sports and Health group Users in all clustered Vendor Groups")

auth_id_Student_group<-unique(Student_group$auth_id)
Student_group_User_Hobies<-new_order_1[new_order_1$auth_id %in% auth_id_Student_group,]
Student_group_Vendor_Grouped_Transc<-Student_group_User_Hobies %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(Student_group_Vendor_Grouped_Transc$cc),Student_group_Vendor_Grouped_Transc$Vendor_group,main="Expenses of Student group users in all clustered Vendor Groups")

auth_id_Foddie_Pet_Movies_Group<-unique(Foddie_Pet_Movies_Group$auth_id)
Foddie_Pet_Movies_Group_User_Hobies<-new_order_1[new_order_1$auth_id %in% auth_id_Foddie_Pet_Movies_Group,]
Foddie_Pet_Movies_Group_Vendor_Grouped_Transc<-Foddie_Pet_Movies_Group_User_Hobies %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(Foddie_Pet_Movies_Group_Vendor_Grouped_Transc$cc),Foddie_Pet_Movies_Group_Vendor_Grouped_Transc$Vendor_group,main="Expenses of Foddie_Pet_Movies_Group in all clustered Vendor Groups")

auth_id_Movies_Club_Group<-unique(Movies_Club_Group$auth_id)
Movies_Club_Group_User_Hobies<-new_order_1[new_order_1$auth_id %in% auth_id_Movies_Club_Group,]
Movies_Club_Group_Vendor_Grouped_Transc<-Movies_Club_Group_User_Hobies %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(Movies_Club_Group_Vendor_Grouped_Transc$cc),Movies_Club_Group_Vendor_Grouped_Transc$Vendor_group,main="Expenses of Movies_Club_Group in all clustered Vendor Groups")

auth_id_Art_Music_Group<-unique(Art_Music_Group$auth_id)
Art_Music_Group_User_Hobies<-new_order_1[new_order_1$auth_id %in% auth_id_Art_Music_Group,]
Art_Music_Group_Vendor_Grouped_Transc<-Art_Music_Group_User_Hobies %>% group_by(Vendor_group)%>% summarise(cc=sum(total_exp))
pie(abs(Art_Music_Group_Vendor_Grouped_Transc$cc),Art_Music_Group_Vendor_Grouped_Transc$Vendor_group,main="Expenses of Art_Music_Group in all clustered Vendor Groups")


#BONUS QUESTION ##
#Finding comptability of two users from 0 to 1 values , 1 being highest comptability 
#and comptability of user to itself is set as infinity

###calculatig distance of one user with every other user based on hobbies
total_rows<-nrow(hobies_copy_pivot_top_10)
total_col<-nrow(hobies_copy_pivot_top_10)
#distance matrix is measure of by how much amount are two users different
#more the hobbies different between two users greater will be distnace matrix value
distance_matrix <- matrix(,nrow = total_rows,ncol = total_col )
for(data_row in 1:total_rows){
  for(data_col in 1:total_col){
    #setting distance of a point to itself as infinity
    if(data_row==data_col){
      distance<-Inf
    }
    #calculating euclidean distance between points
    else{
      distance<-0
      for(index in 2:ncol(hobies_copy_pivot_top_10)){
        distance <- distance + ((as.numeric(hobies_copy_pivot_top_10[data_row,index])-as.numeric(hobies_copy_pivot_top_10[data_col,index]))^2)
      }                
    }
    distance_matrix[data_row,data_col] <- sqrt(distance)
  }
}

#rescaling distance matrix values from 0 to 1
distance_matrix<-rescale(distance_matrix,to=c(0,1))

#finding comptablity matrix , which stores score from 0 to 1 between two users
compatability_matrix<-distance_matrix
for(row in 1:nrow(compatability_matrix)){
  for(col in 1:ncol(compatability_matrix)){
    #compatabilty measure is opposite to distance measure , so subtracting 1 from
    #distance matrix gives compatbility value
    compatability_matrix[row,col]=1-distance_matrix[row,col]
  }
}

#adding auth_id as a column in compatability matrix
compatability_matrix<- cbind(auth_id=hobies_copy_pivot_top_10[c(1)],compatability_matrix)

#adding column header in compatability matrix (all auth_id should be used a column header)
colnames(compatability_matrix)<-c("auth_id",as.list(compatability_matrix$auth_id))

#writing the csv file in current working directory
write.csv(compatability_matrix,"Comptability_Matrix.csv")
