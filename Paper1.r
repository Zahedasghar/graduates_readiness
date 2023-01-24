## ----setup, include=FALSE--------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------
library(tidyverse)
library(readr)
library(gridExtra)
library(googlesheets4)
#Career_survey <- read_csv("career_survey.csv")
#Career_survey<-read_sheet('https://docs.google.com/spreadsheets/d/1Qyw7TUNExdlsLaV55PH9SKjrPw5b6hng0oRxsVgIL9Q/edit?resourcekey#gid=607567742')
#glimpse(Career_survey)

#career<-Career_survey %>% rename(time=Timestamp,name=`Name (Optional)`,email=`Email (Optional)`,program=`Program of study`, subject= `Name of the degree Program (e.g BBA/BS Sociology/...)`, career_entry=`How do you feel about career planning?`, tech_challenges=`Do you think that Artificial Intelligence, Machine Learning, Robotics and Other automation are serious threats to your job prospects in 5 to 10 years times.`, notfamily_career=`Have you learned about different careers other than those of your family careers?`, job_info=`Do you know how to find information about jobs and careers?`
#, future_job=`Have you thought about how jobs and careers may change in the future?`,job_options=`Do you understand the options you have for education and work after you leave university?`,cv_creation=`Do you feel you can create a CV?`,skill_strengths=`Can you list your strengths related to the skills you possess?`,future_plan= `Do you have ideas about what you might do in the future?`,skill_job_match=`Do you have ideas about which careers match your interests and skills?`
#,career_interest=`Do you know yet what career areas you are interested in?`, group_talk=`Are you comfortable speaking in front of a group of people?`,team_skills= `Over the last year, have you developed listening, teamwork and problem solving skills?`,comm_skills=`How would you rate your strength in the following career skills? [Communcation skill]`,writing_skills=`How would you rate your strength in the following career skills? [Communicating in writing]`,working_data_numbers=`How would you rate your strength in the following career skills? [Working with data and numbers]`,working_with_people=`How would you rate your strength in the following career skills? [Working with people]`, dressing_properly=`How would you rate your strength in the following career skills? [Dressing appropriately for a workplace]`,punctuality=`How would you rate your strength in the following career skills? [Being on time for work or meetings]`, attendance=`How would you rate your strength in the following career skills? [Having good attendance]`,workplace_safety=`How would you rate your strength in the following career skills? [Understanding workplace safety rules]`,motivation_and_initiative=`How would you rate your strength in the following career skills? [Being motivated and taking initiative]`,other_comments=`Anything you want to say about future career challenges and opportunities or related feedback`)

#saveRDS(career,file="career_data.Rds")
career<- readRDS("D:/RepTemplates/Module3/career_data.Rds")
career<-career %>% rename(challenging_career=`If you can, please list one or more career areas that you think will become very challenging in next 5 years.`, 
                          learning=`Which of the following have you done? Check as many as apply.`,
                          knowledge_AIML=`Are you familiar how Artificial Intelligence, Robots, Machine Learning and other new technologies are a challenge to your future job?`)

career<-career %>%
  mutate(program=dplyr::recode(program,'BS'='BS','MPhil'='MPhil','PhD'='PhD',
                        'Engineering'='Other','M. S. Ed'='Other','M.S.Ed'='Other',
                     'M.s.ed'='Other','M.S.Ed.'='Other','MA'='Other','MBA'='Other',
                     'MBBS'='Other','Mphil completed'='MPhil','Ms'='Other',
                     'MSc'='Other','MSC'='Other','MsEd'='Other','MS.Ed'='Other'))



glimpse(career)
library(gtsummary)
trial1<-career %>% select(Gender,program,subject, career_entry,career_interest) 
trial1 %>% select(program) %>% tbl_summary()  %>%
  bold_labels() %>% as_gt() %>% 
  gtsave("Table1.png")


trial1 %>% select(career_entry) %>%  tbl_summary()%>%
  modify_caption("**Table 2. How do you feel about career planning?**") %>%
  bold_labels()

trial1 %>%
  tbl_cross(
    row = Gender,
    col = program,
    percent = "cell"
  ) %>%
  add_p()

career  %>% sjPlot::view_df()

trial1 %>% select(career_entry, program) %>% filter(program=="BS") %>% tbl_summary() %>% 
  modify_caption("**Table 3. Career Planing feelings by BS students**") %>%
  bold_labels() 

trial1 %>% select(career_entry, program) %>% filter(program=="MPhil") %>% tbl_summary() %>% 
  modify_caption("**Table 4. Career Planing feelings by MPhil students**") %>%
  bold_labels() 



trial1 %>% select(career_entry, program) %>% filter(program=="PhD") %>% tbl_summary() %>% 
  modify_caption("**Table 5. Career Planing feelings by BS students**") %>%
  bold_labels() 

trial1 %>% select(career_entry, Gender) %>% filter(Gender=="Male") %>% tbl_summary() %>% 
  modify_caption("**Table 6. Career entry feelings by Male**") %>%
  bold_labels()

trial1 %>% select(career_entry, Gender) %>% filter(Gender=="Female") %>% tbl_summary() %>% 
  modify_caption("**Table 7. Career entry feelings by Female**") %>%
  bold_labels()

trial1 %>% select(career_entry, Gender,program) %>% filter(Gender=="Female"&program=="MPhil") %>% tbl_summary(Gender) 

trial1 %>% select(career_entry, Gender,program) %>% filter(Gender=="Male"&program=="MPhil") %>% tbl_summary(Gender) 




trial1 %>% select(program, career_entry) %>% tbl_summary(by = program) %>% add_p()

trial1 %>% select(program, career_entry) %>% tbl_cross(
  row = career_entry,
  col = program,
  percent = "cell"
) %>%
  add_p() %>%  
    modify_caption("**Table 1. Participants feeling about career entry planning**") %>%
  bold_labels() 




tbl<-trial1 %>% select(Gender, career_entry) %>% tbl_cross(
  row = career_entry,
  col = Gender,
  percent = "cell"
) %>%
  add_p()
tbl
show_header_names(tbl)


career %>% select( knowledge_AIML) %>% 
  dplyr::mutate(response = factor(knowledge_AIML) %>% forcats::fct_explicit_na()) %>% 
  tbl_summary()  %>%  
  modify_caption("**Table 2. Respondents awareness about challenges**") %>%
  bold_labels() 



AI_ML<-read_excel("paper_tables.xlsx",   sheet = "AIML")
AI_ML%>% gt::gt() %>% 
  gt_theme_538() %>%
  gt_highlight_rows(rows = 3, font_weight = "normal") %>% 
  tab_header(title="Table2: Awareness about AI, ML, Robotics and other disruption in future jobs") %>% 
  gtsave("AI_ML.png")



  
career_excitement<-read_excel("paper_tables.xlsx",   sheet = "Sheet9")
career_excitement%>% gt::gt() %>% 
  gt_theme_538() %>%
  gt_highlight_rows(rows = 3, font_weight = "normal") %>% 
  tab_header(title="Table3: How prepared one feels for job entry") %>% gtsave("Table3.png")
as_gt(tbl) %>%             # convert gtsummary table to gt
  gt::tab_style(           # use gt::tab_style() to shade column
    style = list(gt::cell_fill(color = "grey")),
    locations = gt::cells_body(columns = vars(stat_0)))

trial1 %>% select(Gender,career_entry) %>%  group_by(Gender) %>% summarise(count=n()) %>% gt::gt()

career %>% select( knowledge_AIML) %>% tbl_summary() %>% 
  modify_caption("**1/14 recently passed out or MPhil/PhD students  report that they have \n sufficient information how AI,ML, Robotics
                 and Other automation are serious threats to your job prospects in 5 to 10 years times.**") %>%
  bold_labels() 

tbl_AI<- career %>% select(knowledge_AIML, career_entry)
 %>%tbl_summary(by=) %>%  add_p() 
datasummary_skim(tbl_AI,type="categorical")

career %>% select(Gender, career_entry) %>%  tbl_summary(by = career_entry) %>% 
  modify_caption("Table 6: Gender and career entry feeling relationship") %>% 
  add_p() %>% as_gt() %>% 
  gtsave("Table6.png")


career %>% select(Gender,tech_challenges,notfamily_career,job_info,future_job,job_options, 
                  skill_strengths, cv_creation,group_talk,future_plan,skill_job_match,
                  team_skills, career_entry) %>%  tbl_summary(by = career_entry) %>% add_p()

career %>% select(tech_challenges,notfamily_career,job_info,future_job,job_options, 
                  skill_strengths, cv_creation,group_talk,future_plan,skill_job_match,
                  team_skills) %>%  tbl_summary() 



career %>% select(comm_skills,writing_skills,working_data_numbers,working_with_people,
                  dressing_properly,punctuality,attendance,workplace_safety,
                  motivation_and_initiative)  %>% 
  tbl_summary()
#View(career)
library(modelsummary)
df1<-career %>% select(comm_skills,writing_skills,working_data_numbers,working_with_people,
                  dressing_properly,punctuality,attendance,workplace_safety,
                  motivation_and_initiative)  
  datasummary_skim(df1,type = "categorical") 

  
skills_rating<- read_excel("paper_tables.xlsx",   sheet = "Sheet4 (2)")

 skills_rating%>% gt::gt()
library(gtExtras)
library(gt)
library(readxl)
 skills_rating %>% gt::gt() %>% 
  gt_theme_538() %>%
  gt_hulk_col_numeric(`Very Strong`) %>%  tab_header(title="Table 4:How you rate your skills") %>% 
   gtsave("Table4.png")
skill_set$`Yes to some extent`
skill_set<-read_excel("paper_tables.xlsx",sheet="Sheet4")
skill_set %>% gt::gt() %>% 
  gt_theme_538() %>%
  gt_hulk_col_numeric(4) %>%  tab_header(title="Rating of respondents to various skills (%)")

#install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra) 
Data<-read_excel("paper_tables.xlsx",sheet="Sheet8")
Data
m1 <- CA(X  =  Data[ ,-1], graph = TRUE)
Data
m1 <- 
  Data |> 
  select(-Skills) |> 
  as.matrix()

rownames(m1) <- Data$Skills
glimpse(career)
m1

fm2 <- CA(X =  m1, graph = TRUE)
fviz_ca_biplot(fm2, repel = TRUE)

chisq<-chisq.test(m1)
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
#The relative contribution of each cell to the total Chi-square score give some indication of the nature of the dependency |n 
#between rows and columns of the contingency table.
#This shows that the row technological challenges, there is more who have either no information or not sure . While `mostattributes<-`()
#of other variables seem independent of each other. CV creation seems highly correlated with great extent , not aware and to some extent.
Data1<-read_excel("paper_tables.xlsx",sheet="Sheet4 (2)")
m2 <- CA(X  =  Data1[ ,-1], graph = TRUE)
Data1

#View(Data1)
m2 <- 
  Data1 |> 
  select(-Skills) |> 
  as.matrix()

rownames(m2) <- Data1$Skills

m2

fm3 <- CA(X =  m2, graph = TRUE)
fviz_ca_biplot(fm3, repel = TRUE)


chisq1<-chisq.test(m2)
library(corrplot)
corrplot(chisq1$residuals, is.cor = FALSE)













## ----rename-variables, eval=FALSE, message=FALSE, warning=FALSE----
## career<-career %>% rename(
##   cv_creation=`Do you feel you can create a CV?`,skill_strengths=`Can you list your strengths related to the skills you possess?`,future_plan= `Do you have ideas about what you might do in the future?`,skill_job_match=`Do you have ideas about which careers match your interests and skills?`
## ,career_interest=`Do you know yet what career areas you are interested in?`, group_talk=`Are you comfortable speaking in front of a group of people?`,team_skills= `Over the last year, have you developed listening, teamwork and problem solving skills?`,comm_skills=`How would you rate your strength in the following career skills? [Communcation skill]`,writing_skills=`How would you rate your strength in the following career skills? [Communicating in writing]`,working_data_numbers=`How would you rate your strength in the following career skills? [Working with data and numbers]`,working_with_people=`How would you rate your strength in the following career skills? [Working with people]`, dressing_properly=`How would you rate your strength in the following career skills? [Dressing appropriately for a workplace]`,punctuality=`How would you rate your strength in the following career skills? [Being on time for work or meetings]`, attendance=`How would you rate your strength in the following career skills? [Having good attendance]`,workplace_safety=`How would you rate your strength in the following career skills? [Understanding workplace safety rules]`,motivation_and_initiative=`How would you rate your strength in the following career skills? [Being motivated and taking initiative]`,other_comments=`Anything you want to say about future career challenges and opportunities or related feedback`)
## 


## ----message=FALSE, warning=FALSE------------------------
#career1<-career %>% mutate_at(c("tech_challenges","notfamily_career","job_info","future_job","job_options","skill_strengths","cv_creation","group_talk","future_plan","skill_job_match","team_skills"), funs(recode(.,`No, not yet`=1, `No , not yet`=1, `Probably, I'm not sure`=2, `Yes to some extent`=3,`Yes to a great extent`=4,.default = NaN)))
#career %>% select(comm_skills) %>% group_by(comm_skills) %>% 
#  summarise(n=n()) %>% 
#  mutate(freq = n*100 / sum(n))
library(stringr)
library(tidyverse)
library(ggplot2) 
library(kableExtra)
library(gt)
career %>% select(comm_skills) %>% group_by(comm_skills) %>%    summarise(n()/256*100)
career %>% select(career_entry) %>% group_by(career_entry) %>%    summarise(n()/256*100)
career %>% select(group_talk,Gender) %>% group_by(group_talk) %>%    summarise(n()/256*100)
career %>% select(skill_strengths,Gender) %>% group_by(skill_strengths) %>%    summarise(n()/256*100) 


c1<-career %>% select(comm_skills) %>% 
  ggplot(aes(x=comm_skills))+geom_bar()+labs(title = "How would you rate your strength \n in the following career skills?")+theme_minimal()+coord_flip()



## ----echo=FALSE------------------------------------------
c2<-career %>% select(career_entry) %>% 
  ggplot(aes(x=career_entry))+geom_bar(position = 'dodge')+labs(title = "How do you feel about career  planning?")+theme_minimal()+coord_flip()
gA1 <- ggplotGrob(c1)
 gB1 <- ggplotGrob(c2)
 maxWidth = grid::unit.pmax(gA1$widths[2:5], gB1$widths[2:5])
 gA1$widths[2:5] <- as.list(maxWidth)
 
 gB1$widths[2:5] <- as.list(maxWidth)



## ----echo=FALSE, warning=FALSE---------------------------
c3<- career %>% select(group_talk) %>% ggplot(aes(x=group_talk))+geom_bar(position = 'dodge')+labs(title = "How do you feel about career planning?")+theme_minimal()+coord_flip()




## ----echo=FALSE, warning=FALSE---------------------------
c4<-career %>% select(skill_strengths) %>% ggplot(aes(x=skill_strengths))+geom_bar(position = 'dodge')+
   labs(title = "Can you list your strengths related \n to the skills you possess?")+theme_minimal()+coord_flip()

 

## ----echo=FALSE, warning=FALSE---------------------------

c5<-career %>% select(team_skills) %>% ggplot(aes(x=team_skills))+geom_bar(position = 'dodge')+labs(title = "Over the last year, have you developed \n listening, teamwork and problem solving skills?`")+theme_minimal()+coord_flip()
gA11 <- ggplotGrob(c3)
 gB11 <- ggplotGrob(c4)
 gC11<-ggplotGrob(c5)
 maxWidth = grid::unit.pmax(gA11$widths[2:5], gB11$widths[2:5],gC11$widths[2:5])
 gA11$widths[2:5] <- as.list(maxWidth)
 gB11$widths[2:5] <- as.list(maxWidth)
 gC11$widths[2:5] <- as.list(maxWidth)
 grid.arrange(gA1, gB1, ncol=1)

grid.arrange(gB11,gC11, ncol=1)
c5


## ----echo=FALSE, warning=FALSE---------------------------
library(gtExtras)
comm_sk<-career %>% select(comm_skills) %>% group_by(comm_skills) %>% 
  summarise(n=n()/256*100) 
comm_sk %>%  gt::gt() %>%
  gt_hulk_col_numeric(n)
# basic use
head(mtcars) %>%
  gt::gt() %>%
  gt_hulk_col_numeric(mpg)

writing_sk<-career %>% select(writing_skills) %>%group_by(writing_skills) %>%  summarise(n=n())
data_num_sk<-career %>% select(working_data_numbers) %>% group_by(working_data_numbers) %>% summarise(n=n())
punctuality_sk<-career %>% select(punctuality) %>%group_by(punctuality) %>%  summarise(n=n())
teamwork_sk<-career %>% select(working_with_people) %>% group_by(working_with_people) %>% summarise(n=n())
attendance<-career %>% select(attendance) %>% group_by(attendance) %>% summarise(n=n())
dressing_sk<-career %>% select(dressing_properly) %>% group_by(dressing_properly)%>% summarise(n=n())

motivation_sk<-career %>% select(motivation_and_initiative) %>%group_by(motivation_and_initiative) %>%  summarise(n=n())

dta<-bind_cols(comm_sk,writing_sk,data_num_sk,punctuality_sk,teamwork_sk,dressing_sk,motivation_sk)

dta <- qpcR:::cbind.na(comm_sk,writing_sk,data_num_sk,punctuality_sk,teamwork_sk,dressing_sk,motivation_sk)

dta 
library(readxl)
summ_dta<-read_excel("dta.xlsx")

summ_dta %>% gt::gt() 

dta1<-read_csv("dta.csv")
summ_dta<-as_tibble(summ_dta)
dta1<-dta1 %>% drop_na()

dta1 %>% gt::gt() %>% gt_theme_nytimes() %>% 
  tab_header(title = "Rating of skills by survey respondents") %>%  gt_hulk_col_numeric(2:8)

summ_dta %>% gt::gt()%>%
  fmt_number(
    columns = vars(msrp),
    decimals = 0,
    suffixing = TRUE
  )

cat_summary<-read.csv("dta_summary.csv")

cat_summary

library(modelsummary)
library(surveytoolbox)
library(survey)
library(expss)
library(gridExtra)



## ----echo=FALSE, message=FALSE, warning=FALSE------------

#cat_summary  %>% 
#ggplot(aes(x=Categories, y=Value))+geom_bar()+labs(title = "Over the last year, have you developed listening, teamwork and problem solving skills?`")

p1<-cat_summary %>% filter(Categories=="Very_strong" ) %>% 
  ggplot(aes(Variable, Value, fill = Categories)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+labs(title = " **Very_Strong** skills")+coord_flip()
p2<-cat_summary %>% filter(Categories=="In-between" ) %>% 
  ggplot(aes(Variable, Value, fill = Categories)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+labs(title=" **In-between** skills")+coord_flip()
library(gridExtra)

gA <- ggplotGrob(p1)
 gB <- ggplotGrob(p2)
 maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
 gA$widths[2:5] <- as.list(maxWidth)
 gB$widths[2:5] <- as.list(maxWidth)
 grid.arrange(gA, gB, ncol=1)


