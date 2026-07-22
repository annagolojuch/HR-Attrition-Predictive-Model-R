<img width="680" height="531" alt="Rplot1" src="https://github.com/user-attachments/assets/6474b3e9-71d4-46d7-acba-a3942a4f9d1e" />
<img width="1920" height="1080" alt="wykres+korelacji" src="https://github.com/user-attachments/assets/4c741584-d376-4584-9b34-01fe461bea1d" />

=======================================================================================
# PROJEKT: Analiza i prognozowanie rotacji pracowników (People Analytics)
# AUTOR: Ania
# =============================================

# 1. ŁADOWANIE BIBLIOTEK
library(tidyverse)
library(corrplot)
library(recipes)
library(caret)

# 2. IMPORT I CZYSZCZENIE DANYCH
# Pamiętaj o poprawnym formacie ścieżki w Windows (zwykłe ukośniki "/")
hr_data <- read_csv("C:/Users/Ania/Desktop/Projekty moje/R - Projekt/WA_Fn-UseC_-HR-Employee-Attrition.csv")

hr_clean <- hr_data %>%
  select(-EmployeeCount, -Over18, -StandardHours, -EmployeeNumber) %>%
  mutate(Attrition_Numeric = ifelse(Attrition == "Yes", 1, 0))

# 3. WIZUALIZACJA: WYKRES PUDEŁKOWY (ZAROBKI A ROTACJA)
ggplot(hr_clean, aes(x = Attrition, y = MonthlyIncome, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Female" = "#E06666", "Male" = "#3D85C6")) +
  labs(title = "Rozkład miesięcznych zarobków a rotacja pracowników",
       subtitle = "Analiza z uwzględnieniem podziału na płeć (Gender)",
       x = "Czy pracownik odszedł z firmy? (Attrition)",
       y = "Miesięczne zarobki ($)",
       fill = "Płeć") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"))

# 4. STATYSTYKA: MACIERZ KORELACJI
dane_do_korelacji <- hr_clean %>%
  select(Attrition_Numeric, Age, DailyRate, DistanceFromHome, 
         EnvironmentSatisfaction, JobSatisfaction, MonthlyIncome, 
         NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, 
         YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion)

macierz_matematyczna <- cor(dane_do_korelacji, use = "complete.obs")

corrplot(macierz_matematyczna, 
         method = "color",       
         type = "upper",         
         order = "hclust",       
         tl.col = "black",       
         tl.srt = 45,            
         addCoef.col = "black",  
         number.cex = 0.7,       
         col = colorRampPalette(c("#E06666", "#FFFFFF", "#3D85C6"))(200))

# 5. MACHINE LEARNING: PODZIAŁ DANYCH (70/30)
set.seed(123)
indeksy <- createDataPartition(hr_clean$Attrition, p = 0.7, list = FALSE)

dane_treningowe <- hr_clean[indeksy, ]
dane_testowe    <- hr_clean[-indeksy, ]

# 6. TRENOWANIE MODELU REGRESJI LOGISTYCZNEJ
model_hr <- glm(Attrition_Numeric ~ Age + OverTime + MonthlyIncome + 
                JobSatisfaction + DistanceFromHome, 
                data = dane_treningowe, 
                family = "binomial")

summary(model_hr)

# 7. TESTOWANIE MODELU I MACIERZ BŁĘDU
prognozy_prawd <- predict(model_hr, newdata = dane_testowe, type = "response")
prognozy_binarne <- ifelse(prognozy_prawd > 0.35, 1, 0)

macierz_bledu <- table(Realne_Odejscia = dane_testowe$Attrition_Numeric, 
                       Prognoza_Modelu = prognozy_binarne)
print(macierz_bledu)
