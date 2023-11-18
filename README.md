# phiriyaphong.github.io
---
title: "Help System"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
---

```{=html}
<style>                     
.navbar {
  background-color:#E48586;
  border-color:#33BBC5;
}
.navbar-brand {
color:black!important;
}
</style>
```
```{r setup, include=FALSE}
library(flexdashboard)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)
library(treemapify)
library(DT)


dat_template = read_excel("/Users/pahphrame/Library/CloudStorage/OneDrive-SatitPrasarnmitDemonstrationSchool(Secondary)/master degree/year 2 semester 1/data vis_bayes_66/data_template.xlsx")
```

### **จำนวนนักเรียนที่เป็นกลุ่มเสี่ยงในชั้นเรียนจำแนกตามด้าน**

## row1 {data-width="650"}

### 

```{r}
poor =dat_template %>% 
  filter(`ฐานะทางบ้าน`=="ยากจน" )%>%
  count() %>%
  as.numeric()


gauge(value = poor,
      min = 0,
      max = length(dat_template),
      label = "ฐานะทางบ้าน",
      sectors = gaugeSectors(
                              success = c(0,0), 
                              warning = c(0,31), 
                              danger = c(32,32)
))
```

### 

```{r}
health =dat_template %>% 
  filter(`สุขภาพทางกาย`!= "แข็งแรง")%>%
  count() %>%
  as.numeric()

gauge(value = health,
      min = 0,
      max = length(dat_template),
      label = "ด้านสุขภาพทางกาย",
      sectors = gaugeSectors(
                              success = c(0,0), 
                              warning = c(0,31), 
                              danger = c(32,32)
))


```

### 

```{r}
study_risk_guage = dat_template %>% 
  mutate(gpax_type = ifelse(`เกรดเฉลี่ยสะสม (GPAX)` < 1.5, 1,0),
         atd_type = ifelse(`การเข้าเรียน` !="ขาดตั้งแต่ 3 ครั้งต่อวิชา", 0, 1),
         hw_type = ifelse(`การส่งงาน`!= "ขาดส่งตั้งแต่ 3 ครั้งต่อวิชา",0,1),
         study_risk = ifelse((gpax_type + atd_type + hw_type) > 0, 1,0))%>%
  #group_by(study_risk)%>%
  filter(study_risk == 1)%>%
  count() %>%
  as.numeric()

gauge(value = study_risk_guage,
      min = 0,
      max = length(dat_template),
      label = "ด้านการเรียน",
      sectors = gaugeSectors(
                              success = c(0,0), 
                              warning = c(0,31), 
                              danger = c(32,32)
))
```

## row2 {data-width="350"}

### 

**ผลการจัดกลุ่มนักเรียนที่เป็นกลุ่มเสี่ยงในชั้นเรียน**

-   แผนภาพด้านขวาเรียกว่า Treemap

-   ตัวอักษรภาษาอังกฤษ A, B, C, ... แทนชื่อกลุ่มลักษณะความเสี่ยงของนักเรียน

-   ขนาดกล่องสี่เหลี่ยมแทนจำนวนนักเรียนในแต่ละกลุ่ม

-   ภายในแต่ละกล่องสี่เหลี่ยมแสดง combination ของความเสี่ยงของนักเรียนภายในกลุ่ม

```{r}

```

### 

```{r}
temp = dat_template %>%
  mutate(gpax_type = ifelse(`เกรดเฉลี่ยสะสม (GPAX)` < 1.5, 1,0),
         atd_type = ifelse(`การเข้าเรียน` !="ขาดตั้งแต่ 3 ครั้งต่อวิชา", 0, 1),
         hw_type = ifelse(`การส่งงาน`!= "ขาดส่งตั้งแต่ 3 ครั้งต่อวิชา",0,1),
         study_risk = ifelse((gpax_type + atd_type + hw_type) > 0, 1,0)) %>%
  mutate(health  = ifelse(`สุขภาพทางกาย`!= "แข็งแรง",1,0),
         poor = ifelse(`ฐานะทางบ้าน`=="ยากจน",1,0),
         gpax = ifelse(study_risk>0,1,0),
         count = poor + health + gpax)%>%
  filter(count>0)%>%
  group_by(health, poor,gpax)%>%
  count() %>%
  ungroup()%>%
  arrange(-n)%>%
  mutate(group = LETTERS[1:nrow(.)])

temp %>% 
  mutate(area_percent = n/(sum(temp$n)),
         sum_problem = poor+gpax+health) %>%
  pivot_longer(cols = c("health","poor","gpax"),
               names_to = "risk_type",
               values_to = "risk_value")%>%
  mutate(area_size = risk_value*area_percent/sum_problem)%>%
  mutate(risk_type = str_replace(risk_type,"poor",
                                  "มีฐานะยากจน"))%>%
  mutate(risk_type = str_replace(risk_type,"health",
                                  "สุขภาพทางกาย"))%>%
  mutate(risk_type = str_replace(risk_type,"gpax",
                                  "การเรียน"
                                 ))%>%
  ggplot(aes(area = area_size,
             fill = risk_type,
             subgroup = group))+
  geom_treemap(col="black")+
  geom_treemap_text(aes(label=risk_type),
                    family = "THSarabunPSK",
                    padding.y = grid::unit(3, "mm"),
                    padding.x = grid::unit(2, "mm"))+
  geom_treemap_subgroup_border(col="white")+
  geom_treemap_subgroup_text(grow = T, col="white", alpha = 0.5)+
  theme(text = element_text(family = "THSarabunPSK"),
        legend.position="none")
```
