`%notin%` <- Negate(`%in%`)
## Analysts by segment teams
teams <- read_csv("analyst teams.csv") %>%
  unite(Name, First:Last, sep = " ") %>%
  filter(Team %notin% c('WFA','L2','Manager','Robert Half','TAM')) %>%
  select(Name, Team)

## SVC REPORT LOAD
svc_raw <- read_csv("report1614398294864.csv") %>%
  slice(1:(n()-5)) %>%
  rename(svc = 'Service Timecard: Service Timecard ID',
         analyst = 'Service Timecard: Owner Name',
         cs_segment = 'Account CS Segment',
         fi_segment = 'Account FI Team',
         resolution = 'Resolution',
         subresolution = 'Sub-Resolution',
         date = 'Service Timecard: Created Date',
         l2 = 'Assigned To') %>%
  left_join(teams, by= c("analyst" = "Name")) %>%
  filter(!is.na(Team)) %>%
  mutate(date = mdy(date),
         date = floor_date(date, "month"))

## CASE REPORT LOAD
case_raw <- read_csv('report1614398336025.csv') %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date),
         date = floor_date(date, "month")) %>%
  filter(date > '2020-07-31')%>%
  semi_join(teams, by=c("analyst" = "Name")) %>%
  left_join(teams, by=c("analyst" = "Name"))

#plot totalRatio
totalRatioPlot <- function(filter_var) {
  
  p <- ggplot(totalRatio(filter_var), aes(month, ratio)) + 
    geom_bar(stat="identity",aes(fill=ratio)) +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
    labs(x="month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3),legend.position = "none")
  ggplotly(p)
}

avgRatio <- function(filter_var) {
  svc_total_all <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    group_by(month) %>%
    mutate(svc_total = n()) %>%
    select(month, svc_total) %>%
    distinct()
  
  svccase_ratio_all <- case_report %>%
    left_join(svc_total_all) %>% 
    mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
    arrange(desc(ratio)) %>%
    rename("Total Cases" = "case_total",
           "Total SVCs" = "svc_total") %>%
    arrange(month) %>%
    ungroup() %>%
    summarise(across(everything(),mean)) %>%
    mutate(month = "") %>%
    rename(Average = month)
  
  svccase_ratio_all
}

 # count by Subresolution by month
resBreakdown <- function(filter_var) {
  svc_total_by_subresolution <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    mutate(month = month(date),
           subresolution = case_when(is.na(subresolution) ~ "Not Coded",
                                     is.na(resolution) ~ "Not Coded",
                                     TRUE ~ subresolution)) %>% 
    group_by(resolution, subresolution, month) %>%
    summarise(total = n()) %>%
    spread(month, total) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(Total = sum(c_across(where(is.numeric)))) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    arrange(desc(Total)) %>%
    filter(resolution != "NA", subresolution != "Not Coded")
    
}

topResLastMonth <- function(filter_var) {
  topRes <- svc_report %>% 
    mutate(month = month(date),
         year = year(date)) %>%
    filter((Team == filter_var | filter_var == dummy),
           !is.na(subresolution),
           year == max(year),
           month == max(month)) %>%
    group_by(subresolution) %>%
    mutate(total = n()) %>%
    select(subresolution, total) %>%
    distinct() %>%
    arrange(desc(total)) %>%
    ungroup() %>% 
    top_n(10)
  
  p <- ggplot(topRes, aes(x=reorder(subresolution, -total),y=total)) + 
    geom_col(aes(fill=total)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3)) +
    xlab("") +
    theme(legend.position = "none")
  ggplotly(p)
}