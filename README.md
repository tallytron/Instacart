# Instacart

With the rise in online transactions, Instacart is trying to leverage the humongous data generated by the transaction activities to transform it to meaningful insights. Market Basket Analysis (MBA) is a technique always used by retailers to understand customer behavior while purchasing. When we shop online, we have probably seen a section called “You may also like” or “Customers who bought this item also bought” in which Market Basket Analysis plays an important role.

In this project I am predicting future purchases based on patterns derived from historical data and further using these patterns along with the recommendation engine. It will help Instacart to enhance the user experience by suggesting the next likely product to purchase to the user during the order process and increase the basket size by recommending the users most frequently bought together products. The end goal is to increase the user basket size which would directly lead to an increase in revenue. 

## Data Description

The anonymized dataset is openly sourced from Kaggle. There are six datasets containing a sample of over34 million products and3 million grocery orders from more than 200,000 Instacart users.

![image](https://user-images.githubusercontent.com/80222038/154808577-dd34b912-9b4b-456a-a797-2ed0139cd900.png)

Each user’s basket contains between 4 and 100 of their orders which also has the sequence of products purchased in each order. Additionally, the week and day of the week in which the user made the purchase were given, as well as a relative period between orders. Instacart has taken many measures to safeguard the privacy of its customers and retail partners, including randomizing user IDs and include only items purchased by various individuals at different retail locations without providing retailer IDs. There is a unique id for each of the variables (aisles, products, orders, customers, etc.).

The following is an illustration of the variables blended together in the image below. It displays user id 1's two orders, together with their order id number, order date (weekday), order hour of day, and order added to card, all of which are linked with the product id and product name.

![image](https://user-images.githubusercontent.com/80222038/154808615-231adb42-2ff7-4b58-b86b-3d9c68988ee1.png)

## EDA 

I examined the purchasing behaviour in this area by examining many key factors. The following is a synopsis of our findings.

![image](https://user-images.githubusercontent.com/80222038/154808644-5a38f218-caa4-4adc-bccf-6943f3a26a9f.png)

Following the data exploration, the following sections describe some important results.
1)	Customer Traffic

![image](https://user-images.githubusercontent.com/80222038/154808657-10e7187e-9539-49d4-926f-2532f4fb1165.png)

As indicated by the time chart on the figure, most of the orders are placed between the hours of 10 AM and 4PM.

![image](https://user-images.githubusercontent.com/80222038/154808663-d1a7296b-afe5-48b2-8f4a-7ac8ba4fb64b.png)



