# intuit-recruiting-rit-challenge
Intuit RIT Recruiting Challenge - 2017

Hi Team

I have tried to solve the problem using R programming
R version 3.2.3 (2015-12-10)
Kindly install all the packages and library and set the working directory where the csv files are

Firstly I have cleaned the data , the date format and invalid dates cases

The variuos derived features calculated are:
income_expense table :
1) Features based on Income of each user : mean_income, min_income,max_income,count_n,last_date,first_date
2) Features based on Expenses of each user : mean_exp,max_exp,min_exp,count_n,total_exp
3) have_child : boolean value stating whether the user has kid or not (found if the user has any transaction related to baby products)

Determing clusters based on hobbies:
- Based on SSE find the idea of how many clusters will be formed(which turn out to be 5). The inflection point in knee graph is 5
- Now we plot each cluster 
- Each cluster is shown as distinct circles with row id as elements in that cluster
- We find auth_id for each cluster
- Then we find the vendor descriptions which is set to 1 for majority of auth_id in that cluster, this shows that user in that cluster purchase these common products
- The 5 clusters of users formed based on their hobbies are :
  - Students_group: This group purchased science , maths books and student goods
  - Sports_Athlethics_Group : This group purchased Amazon order atheltic equipment, Athletic apparel,bike rental, Dicks sporting goods,       GNC, NBA Tickets , NFL Tickets, Sam's sporting Apparel, Total Gym Fees, Vitamin Shoppe 
  - Foddie_Pet_Movies_Group : This group has transaction realted to Food Delivery GrubHub, Food Delivery Uber Eats, On Demand Movie, On       Demand TV, Pet Smart, Pet Supply Cat Food, Playstation Membership , PodCast subscription Red Box DVD Rental, Wine Delivery
  - Movies_Club_Group : group having transaction related to bowling, concert ticket, Fisker Night Club , Green Flash Brewery, Ice Skating,    John's Bar & Restauarnt, Karaoke Bar, Movie, Owl Night Club, Rodrigue's Bar & Grill , Seaside Bar , Wine Bar
  - Art_Music_Group : group having transaction related to Amazon order Paint Brushes, Art, Guitar , Paino
  
  -Plots : I have plotted the expenditure pie chart of each cluster groups wrt to other cluster groups
  
  Determining Compatability :
  - Table used : hobies_copy_pivot_top_10
  - This table has 100 rows , one row for each user and 300 columns related to different transactions which is amongst top 10 most widely used vendors description by different users
  - For each user only 10 columns values are set to 1 ie they most commonly purchase those 10 vendor products
  - I am finding euclidean distance of one user with every other user
  - Rescaling my 100*100 in range 0 to 1
  - Finding comptability of ine user with other ie 1- distance
  - 1 is the highest comptability 
  - It based on how many hobbies match for two users
  - A csb file named comptability will be saved in current working directory . Open this csv to check the comtability matrix
 
