library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
library(forcats)
library(tidytext)
library(scales)

sales_w_nans <- read_csv("Sales_with_NaNs_v1.3.csv")%>%
  clean_names()

str(sales_w_nans)
glimpse(sales_w_nans)
summary(sales_w_nans)
skim(sales_w_nans)

sum(is.na(sales_w_nans))
colSums(is.na(sales_w_nans))

#Check the distribution of sales before
sales_w_nans%>%
  ggplot(aes(x = sales_before))+
  geom_histogram(fill = "pink", alpha = 0.5)+
  labs(title = "Distribution of sales before", x = "Sales before", y = "Frequency")+
  theme_minimal()

#Check the distribution of sales before for each group 
sales_w_nans%>%
  ggplot(aes(x = sales_before))+
  geom_histogram(fill = "pink")+
  facet_wrap(vars(group))+
  labs(title = "Distribution of sales before", x = "Sales before", y = "Frequency")+
  theme_minimal()

#Check the distribution of sales after 
sales_w_nans%>%
  ggplot(aes(x = sales_after))+
  geom_histogram(fill = "blue", alpha = 0.5)+
labs(title = "Distribution of sales after", x = "Sales after", y = "Frequency")+
  theme_minimal()

#Check the distribution of sales after for each group 
sales_w_nans%>%
  ggplot(aes(x = sales_after))+
  geom_histogram(fill = "blue")+
  facet_wrap(vars(group))+
  labs(title = "Distribution of sales after", x = "Sales after", y = "Frequency")+
  theme_minimal()

#check the density of sales before
ggplot(sales_w_nans, aes(x=sales_before))+
  geom_density(fill = "magenta", alpha = 0.5)+
  labs(title = "Distribution of sales before", x = "Sales before", y = "Frequency")+
  theme_minimal()

#check the density of sales before for each group 
ggplot(sales_w_nans, aes(x=sales_before))+
  geom_density(fill = "purple", alpha = 0.3)+
  facet_wrap(vars(group))+
  labs(title = "Distribution of sales before", x = "Sales before", y = "Frequency")+
  theme_minimal()


#check the density of sales after 
ggplot(sales_w_nans, aes(x= sales_after))+
  geom_density(fill = "coral", alpha = 0.5)+
  labs(title = "Distribution of sales after", x = "Sales after", y = "Frequency")+
  theme_minimal()

#Use a box plot to check sales_before
ggplot(sales_w_nans, aes(x = sales_before))+
  geom_boxplot(fill = "green", alpha = 0.5)+
  ggtitle("Distribution of Sales before")+
  theme_minimal() 

#Use a box plot to check sales_after
ggplot(sales_w_nans, aes(x = sales_after))+
  geom_boxplot(fill = "forestgreen", alpha = 0.5)+
  ggtitle("Distribution of sales after")+
  theme_minimal() 

#Which group has the highest and lowest sales after
group_sales_after <- sales_w_nans %>%
  group_by(group) %>%
  summarize(avg_sales_after = mean(sales_after, na.rm = TRUE)) %>%
  arrange(desc(avg_sales_after))

top_groups <- head(group_sales_after, 3)
bottom_groups <- tail(group_sales_after, 3)

top_groups
bottom_groups

#Which customer segment has the highest and lowest sales after
cust_seg_sales_after <- sales_w_nans %>%
  group_by(customer_segment) %>%
  summarize(avg_sales_after = mean(sales_after, na.rm = TRUE)) %>%
  arrange(desc(avg_sales_after))

top_segments <- head(cust_seg_sales_after, 4)
bottom_segments <- tail(cust_seg_sales_after, 4)

top_segments
bottom_segments

#Check the median sales before 
median_sales_before <- sales_w_nans %>%
  group_by(group) %>%
  summarize(median_sales_b = median(sales_before, na.rm = TRUE)) %>%
  arrange(desc(median_sales_b))

median_sales_before

#Visualise the median sales before against the groups 
ggplot(median_sales_before, aes(x = fct_reorder(group, median_sales_b, .desc = T), y = median_sales_b, fill = group)) +
  geom_histogram(stat = "identity")+
  labs(title = "Median sales before across groups", x = "group", y = "Median Price" )+
  theme_minimal() + theme(legend.position = "none")


#Check the median sales after 
median_sales_after <- sales_w_nans %>%
  group_by(group) %>%
  summarize(median_sales_a = median(sales_after, na.rm = TRUE)) %>%
  arrange(desc(median_sales_a))

median_sales_after

#Visualise the median sales after against the groups 
ggplot(median_sales_after, aes(x = fct_reorder(group, median_sales_a, .desc = T), y = median_sales_a, fill = group)) +
  geom_histogram(stat = "identity")+
  labs(title = "Median sales after across groups", x = "group", y = "Median Price" )+
  theme_minimal() + theme(legend.position = "none")


#check median purchases made after customer satisfaction
purchases_made <- sales_w_nans %>%
  group_by(purchase_made) %>%
  summarize(median_customer_sat = median(customer_satisfaction_after, na.rm = TRUE)) %>%
  arrange(desc(median_customer_sat))

purchases_made

#is there a correlation between sales before and customer satisfaction before?
ggplot(sales_w_nans, aes(x = customer_satisfaction_before, y = sales_before)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation between sales before and customer satisfaction before", x = "Customer Satisfaction Before", y = "Sales Before")

#is there a correlation between sales after and customer satisfaction after 
ggplot(sales_w_nans, aes(x = customer_satisfaction_after, y = sales_after)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Correlation between sales before and customer satisfaction after", x = "Customer Satisfaction After", y = "Sales After")

#How do the sales after vary across groups based on customer segment?
 # Calculate sales after by group and customer segment
median_sales_after_by_group <- sales_w_nans %>%
  group_by(group, customer_segment) %>%
  summarize(median_sales_af = median(sales_after, na.rm = TRUE))

# Reorder group within each customer segemt
sales_w_nans <- sales_w_nans %>%
  mutate(group = reorder_within(group, sales_after, customer_segment))

# Create the boxplot with groups ordered by their median price and facet by avocado type
ggplot(sales_w_nans, aes(x = group, y = sales_after, fill = customer_segment)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Sales After Across groups by customer segment",
       x = "Group", y = "Sales After") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  facet_wrap(vars(customer_segment), scales = "free_x") +  # Facet by avocado type and free x-axis scale for better display
  scale_x_reordered()  # Apply reordering


#check for total customer satisfaction before by purchases made, a bit confused if the purchase made is made before or after custome satisfaction
sales_w_nans%>%
  group_by(purchase_made)%>%
  summarise(tot_cust_sat_before = sum(customer_satisfaction_before, na.rm = TRUE))%>%
  ggplot(aes(x=purchase_made, y=tot_cust_sat_before)) +
  geom_bar(stat = "identity")

#check for total customer satisfaction after by purchases made 
sales_w_nans%>%
  group_by(purchase_made)%>%
  summarise(tot_cust_sat_after = sum(customer_satisfaction_after, na.rm = TRUE))%>%
  ggplot(aes(x=purchase_made, y=tot_cust_sat_after)) +
  geom_bar(stat = "identity")

#Tried checking for with count instead of with sum, but error
sales_w_nans%>%
  group_by(purchase_made)%>%
  ggplot(aes(x=purchase_made, y=count(customer_satisfaction_after, na.rm)) +
  geom_bar(stat = "identity"))

#Create a distribution of groups according to sales before 
ggplot(sales_w_nans, aes(x = fct_reorder(group, sales_before, na_rm = TRUE))) +
  geom_histogram(stat = count, fill = "#4a7337", alpha = 0.5) +
  ggtitle("Distribution groups") +
  theme_minimal()

#check for total of sales after by group. error sales_after not found?
sales_w_nans%>%
  group_by(group)%>%
  summarise(tot_sales_after = sum(sales_after))%>%
  ggplot(aes(x=fct_reorder(group, sales_after, .desc = T), y=sales_after, fill = group)) +
  geom_bar(stat = "identity")+
  labs(title = "Total Sales After", x = "Group", y = "Sales After")+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))+
  theme(legend.position = "none")

#check for total of sales after by customer segment, sales_after not found?
sales_w_nans%>%
  group_by(customer_segment)%>%
  summarise(tot_sales_after = sum(sales_after))%>%
  ggplot(aes(x=fct_reorder(customer_segment, sales_after, .desc = T), y=sales_after, fill = customer_segment)) +
  geom_bar(stat = "identity")+
  labs(title = "Total Sales After", x = "Customer Segment", y = "Sales After")+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))+
  theme(legend.position = "none")




