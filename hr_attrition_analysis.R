# install.packages(c("tidyverse", "corrplot", "recipes", "caret"))
# library(tidyverse)
# library(corrplot)
# library(recipes)
# library(caret)

hr_data <- read_csv("C:/Users/Ania/Desktop/Projekty moje/R - Projekt/WA_Fn-UseC_-HR-Employee-Attrition.csv")                                                                                         

hr_clean <- hr_data %>%
  select(-EmployeeCount, -Over18, -StandardHours, -EmployeeNumber)

hr_clean <- hr_clean %>%
  mutate(Attrition_Numeric = ifelse(Attrition == "Yes", 1, 0))

hr_clean %>%
  group_by(OverTime) %>%
  summarise(
    Liczba_Pracownikow = n(),
    Wskaźnik_Rotacji_Procent = mean(Attrition_Numeric) * 100
  )

hr_clean %>%
  group_by(Attrition) %>%
  summarise(
    Srednie_Zarobki = mean(MonthlyIncome),
    Mediana_Zarobkow = median(MonthlyIncome)
  )

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

dane_do_korelacji <- hr_clean %>%
  select(Attrition_Numeric, Age, DailyRate, DistanceFromHome, 
         EnvironmentSatisfaction, JobSatisfaction, MonthlyIncome, 
         NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, 
         YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion)

macierz_matematyczna <- cor(dane_do_korelacji, use = "complete.obs")
View(macierz_matematyczna)

corrplot(macierz_matematyczna, 
         method = "color",       # Korelacja pokazana jako kolorowe kwadraty
         type = "upper",         # Pokazuje tylko górną połowę wykresu (żeby nie dublować danych)
         order = "hclust",       # Grupuje zmienne najbardziej podobne do siebie obok siebie
         tl.col = "black",       # Kolor podpisów zmiennych
         tl.srt = 45,            # Obrócenie napisów o 45 stopni dla lepszej czytelności
         addCoef.col = "black",  # Dodaje dokładne liczby korelacji wewnątrz kwadratów
         number.cex = 0.7,       # Rozmiar czcionki liczb w kwadratach
         col = colorRampPalette(c("#E06666", "#FFFFFF", "#3D85C6"))(200))

set.seed(123)

indeksy <- createDataPartition(hr_clean$Attrition, p = 0.7, list = FALSE)

dane_treningowe <- hr_clean[indeksy, ]

dane_testowe    <- hr_clean[-indeksy, ]

model_hr <- glm(Attrition_Numeric ~ Age + OverTime + MonthlyIncome + 
                  JobSatisfaction + DistanceFromHome, 
                data = dane_treningowe, 
                family = "binomial") 

summary(model_hr)

prognozy_prawdopodobienstwa <- predict(model_hr, newdata = dane_testowe, type = "response")

prognozy_koncowe <- ifelse(prognozy_prawdopodobienstwa > 0.35, 1, 0)

macierz_bledu <- table(Realne_Odejscia = dane_testowe$Attrition_Numeric, Prognoza_Modelu = prognozy_koncowe)

print(macierz_bledu)