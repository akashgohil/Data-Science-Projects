use class;

create table Big_mart(Item_Identifier varchar(255),Item_Weight float,Item_Fat_Content char(255),Item_Visibility float,Item_Type char(255),	Item_MRP float,	Outlet_Identifier varchar(255),	Outlet_Establishment_Year int(255),	Outlet_Size char(255),Outlet_Location_Type varchar(255),	Outlet_Type char(255),	Item_Outlet_Sales float);


select * from Big_mart;

#1 WRITE a query TO  show Item_Identifier
select Item_Identifier from Big_mart;

#2 WRITE a query TO  show  count OF total Item_Identifier
select count(Item_Identifier) from Big_mart;

#3 WRITE a query TO  show maximum Item Weight
select Item_Identifier,max(Item_Weight) from Big_mart;
#4 WRITE a query TO  show minimun  Item Weight
select Item_Identifier,min(Item_Weight) from Big_mart;
#5 WRITE a query TO  show average Item_Weight
select avg(Item_Weight) from Big_mart;
#6 WRITE a query TO  show count OF Item_Fat_Content WHERE Item_Fat_Content IS Low Fat
select count(Item_Fat_Content) from Big_mart where Item_Fat_Content = 'Low Fat';

#7 WRITE a query TO  show count OF Item_Fat_Content WHERE Item_Fat_Content IS Regular
select count(Item_Fat_Content) from Big_mart where Item_Fat_Content = 'Regular';

#8 WRITE a query TO  show maximum Item_MRP
select max(Item_MRP) from Big_mart; 

#9 WRITE a query TO  show minimum Item_MRP
select min(Item_MRP) from Big_mart; 

#10 WRITE a query TO  show Item_Identifier , Item_Fat_Content ,Item_Type,Item_MRP and  Item_MRP IS greater than 200
select Item_Identifier , Item_Fat_Content ,Item_Type,Item_MRP from Big_mart where Item_MRP>200;

#11 WRITE a query TO  show maximum Item_MRP WHERE  Item_Fat_Content IS Low Fat
select max(Item_MRP) from Big_mart where Item_Fat_Content = 'Low Fat';

#12 WRITE a query TO  show minimum Item_MRp AND Item_Fat_Content IS Low Fat
select min(Item_MRP) from Big_mart where Item_Fat_Content = 'Low Fat';

#13 WRITE a query TO  show ALL DATA WHERE item MRP IS BETWEEN 50 TO 100
select * from Big_mart where Item_MRP between 50 and 100;

#14 WRITE a query TO  show ALL UNIQUE value Item_Fat_Content
select distinct Item_Fat_Content from Big_mart;

#15 WRITE a query TO  show   ALL UNIQUE value Item_Type
select distinct Item_Type from Big_mart;

#16 WRITE a query TO  show ALL DATA IN descending ORDER BY Item MRP
select * from Big_mart order by Item_MRP desc;

#17 WRITE a query TO  show ALL DATA IN ascending ORDER BY Item_Outlet_Sales
select * from Big_mart order by Item_Outlet_Sales asc;

#18 WRITE a query TO  show ALL DATA IN ascending BY Item_Type
select * from Big_mart order by Item_Type asc;

#19 WRITE a query TO  show DATA OF item_type dairy & Meat
select * from Big_mart where Item_Type in ('Dairy','Meat');

#20 WRITE a query TO  show ALL UNIQUE value OF Outlet_Size
select distinct Outlet_Size from Big_mart;

#21 WRITE a query TO  show ALL UNIQUE value OF Outlet_Location_Type
select distinct Outlet_Location_Type from Big_mart;
 
#22 WRITE a query TO  show ALL UNIQUE value OF Outlet_Type
select distinct Outlet_Type from Big_mart;

#23 WRITE a query TO  show count NO. OF item BY Item_Type AND ordered it IN descening
SELECT Item_Type , count(Item_Identifier)No_Of_Item  
FROM Big_mart  
GROUP BY Item_Type  
ORDER BY No_Of_Item DESC;

#24 WRITE a query TO  show  count NO. OF item BY Outlet_Size AND ordered it IN ascening
SELECT Outlet_Size , count(Item_Identifier)No_Of_Item  
FROM Big_mart  
GROUP BY Outlet_Size  
ORDER BY No_Of_Item aSC; 

#25 WRITE a query TO  show  count NO. OF item BY Outlet_Establishment_Year AND ordered it IN ascening
SELECT Outlet_Establishment_Year , count(Item_Identifier)No_Of_Item  
FROM Big_mart  
GROUP BY Outlet_Establishment_Year  
ORDER BY No_Of_Item aSC; 

#26 WRITE a query TO  show  count NO. OF item BY Outlet_Type AND ordered it IN descening
SELECT Outlet_Type , count(Item_Identifier)No_Of_Item  
FROM Big_mart  
GROUP BY Outlet_Type  
ORDER BY No_Of_Item desc; 

#27 WRITE a query TO  show ount NO. OF item BY Outlet_Location_Type AND ordered it IN descening
SELECT Outlet_Location_Type , count(Item_Identifier)No_Of_Item  
FROM Big_mart  
GROUP BY Outlet_Location_Type  
ORDER BY No_Of_Item desc;

#28 WRITE a query TO  show maximum MRP BY Item_Type
SELECT  Item_Type, Max(Item_MRP)Max_MRP  
FROM Big_mart  
GROUP BY Item_Type; 

#29 WRITE a query TO  show minimum MRP BY Item_Type
SELECT  Item_Type, min(Item_MRP)Min_MRP  
FROM Big_mart  
GROUP BY Item_Type;

#30 WRITE a query TO  show minimum MRP BY Outlet_Establishment_Year AND ordered it IN descending
SELECT  Outlet_Establishment_Year, min(Item_MRP)Min_MRP  
FROM Big_mart  
GROUP BY Outlet_Establishment_Year order by Min_MRP desc;

#31 WRITE a query TO  show maximum MRP BY Outlet_Establishment_Year AND ordered IN descending
SELECT  Outlet_Establishment_Year, Max(Item_MRP)Max_MRP  
FROM Big_mart  
GROUP BY Outlet_Establishment_Year order by Max_MRP desc;


#32 WRITE a query TO  show average MRP BY Outlet_Size AND ordered IN descending
SELECT  Outlet_Size, avg(Item_MRP)Average_MRP  
FROM Big_mart  
GROUP BY Outlet_Size order by Average_MRP desc;

#33 WRITE a query TO  show average MRP BY Outlet_Size
SELECT  Outlet_Size, avg(Item_MRP)Average_MRP  
FROM Big_mart  
GROUP BY Outlet_Size order by Average_MRP desc;

#34 WRITE a query TO  show Average MRP BY  Outlet_Type AND ordered IN ascending
SELECT  Outlet_Type, avg(Item_MRP)Average_MRP  
FROM Big_mart  
GROUP BY Outlet_Type order by Average_MRP asc;

#35 WRITE a query TO  show maximum MRP BY Outlet_Type
SELECT  Outlet_Type, max(Item_MRP)Max_MRP  
FROM Big_mart  
GROUP BY Outlet_Type order by Max_MRP asc;

#36 WRITE a query TO  show maximum  Item_Weight BY Item_Type 
SELECT Item_Type , max(Item_Weight)max_weight 
FROM Big_mart  
GROUP BY Item_Type  
ORDER BY max_weight DESC;

#37 WRITE a query TO  show maximum  Item_Weight BY Outlet_Establishment_Year 
SELECT Outlet_Establishment_Year , max(Item_Weight)max_weight  
FROM Big_mart  
GROUP BY Outlet_Establishment_Year  
ORDER BY max_weight aSC; 

#38 WRITE a query TO  show minimum  Item_Weight BY Outlet_Type 
SELECT Outlet_Type , min(Item_Weight)min_weight  
FROM Big_mart  
GROUP BY Outlet_Type  
ORDER BY min_weight desc; 

#39 WRITE a query TO  show average Item_Weight BY  Outlet_Location_Type ORDER BY descending
SELECT Outlet_Location_Type , avg(Item_Weight)Average_weight  
FROM Big_mart  
GROUP BY Outlet_Location_Type  
ORDER BY Average_weight desc;


#40 WRITE a query TO  show maximum  Item_Outlet_Sales BY  Item_Type
SELECT  Item_Type, Max(Item_Outlet_Sales)Max_sales  
FROM Big_mart  
GROUP BY Item_Type; 

#41 WRITE a query TO  show minimum  Item_Outlet_Sales BY  Item_Type
SELECT  Item_Type, min(Item_Outlet_Sales)Min_sales  
FROM Big_mart  
GROUP BY Item_Type;

#42 WRITE a query TO  show minimum Item_Outlet_Sales BY Outlet_Establishment_Year
SELECT  Outlet_Establishment_Year, min(Item_Outlet_Sales)Min_sales  
FROM Big_mart  
GROUP BY Outlet_Establishment_Year order by Min_sales desc;

#43 WRITE a query TO  show maximum Item_Outlet_Sales BY Outlet_Establishment_Year ordered BY descending
SELECT  Outlet_Establishment_Year,  Max(Item_Outlet_Sales)Max_sales FROM Big_mart
GROUP BY Outlet_Establishment_Year order by Max_sales desc;

#44 WRITE a query TO  show average Item_Outlet_Sales BY  Outlet_Size AND ORDER it itn descending
SELECT  Outlet_Size, avg(Item_Outlet_Sales)Average_sales  
FROM Big_mart  
GROUP BY Outlet_Size order by Average_sales desc;

#45 WRITE a query TO  show average Item_Outlet_Sales BY Outlet_Size 
SELECT  Outlet_Size, avg(Item_Outlet_Sales)Average_sales  
FROM Big_mart  
GROUP BY Outlet_Size order by Average_sales desc;

#46 WRITE a query TO  show average Item_Outlet_Sales BY Outlet_Type
SELECT  Outlet_Type, avg(Item_Outlet_Sales)Average_sales  
FROM Big_mart  
GROUP BY Outlet_Type order by Average_sales asc;

#47 WRITE a query TO  show maximum Item_Outlet_Sales BY Outlet_Type
SELECT  Outlet_Type, max(Item_Outlet_Sales)Max_sales  
FROM Big_mart  
GROUP BY Outlet_Type order by Max_sales asc;

#48 WRITE a query TO  show total Item_Outlet_Sales BY Outlet_Establishment_Year
select Outlet_Establishment_Year, sum(Item_Outlet_Sales)total_sales 
from Big_mart 
group by Outlet_Establishment_Year 
order by total_sales desc;

#49 WRITE a query TO  show total Item_Outlet_Sales BY Item_Type

select Item_Type, sum(Item_Outlet_Sales)total_sales
from Big_mart 
group by Item_Type 
order by total_sales desc;

#50 WRITE a query TO  show  total Item_Outlet_Sales BY Outlet_Location_Type
select Outlet_Location_Type, sum(Item_Outlet_Sales)total_sales
from Big_mart 
group by Outlet_Location_Type 
order by total_sales desc;

#51 WRITE a query TO  show total Item_Outlet_Sales BY Item_Fat_Content
select Item_Fat_Content, sum(Item_Outlet_Sales)total_sales
from Big_mart 
group by Item_Fat_Content 
order by total_sales desc;

#52 WRITE a query TO  show maximum  Item_Visibility BY  Item_Type
select Item_Type, Max(Item_Visibility)max_visibility
from Big_mart 
group by Item_Type 
order by max_visibility desc; 

#53 WRITE a query TO  show Minimum Item_Visibility BY Item_Type 
select Item_Type, Min(Item_Visibility)min_visibility
from Big_mart 
group by Item_Type 
order by min_visibility desc; 

#54 WRITE a query TO  show total  Item_Outlet_Sales BY Item_Type but ONLY WHERE  Outlet_Location_Type IS Tier 1
select Item_Type, sum(Item_Outlet_Sales)Total_sales
from Big_mart where Outlet_Location_Type = 'Tier 1'
group by Item_Type 
order by Total_sales desc; 

#55 WRITE a query TO  show total Item_Outlet_Sales BY Item_Type WHERE Item_Fat_Content IS ONLY Low Fat & LF

select Item_Type, sum(Item_Outlet_Sales)Total_sales
from Big_mart where Item_Fat_Content in ('Low Fat', 'LF')
group by Item_Type 
order by Total_sales desc; 

