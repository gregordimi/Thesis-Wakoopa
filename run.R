rmarkdown::render('Main3.Rmd')

rmarkdown::render('methodology2.Rmd')
rmarkdown::render('appendix.Rmd')


export_files$desktop_example

 stargazer(export_files$table_tests$final_restricted[41:46,1:14],
          digits = 2,
          type = "text",
          summary = FALSE,
          rownames = FALSE)

export_files[["API_Categ"]] <- c("Arts and Entertainment", "Autos", "Business Finance", "Celebrity", "College", "Cooking", "Dating and Romance", "Exercise", "Fashion and Beauty", "Games", "Health", "Home Improvement", "News", "Parents and Family", "Technology", "Travel")

``` {r echo=FALSE, results = 'asis', message = FALSE}

stargazer(export_files$ols$restricted_desktop_11,
          export_files$ols$restricted_desktop_12,
          export_files$ols$restricted_desktop_13,
          export_files$ols$restricted_desktop_14,
          export_files$ols$restricted_desktop_15, 
          type = "latex",
          digits = 2,
          header=FALSE,
          dep.var.labels = c("log(MM)","log(Domains)","log(PV)","log(Time)","log(Length)"),
          covariate.labels = c("Risk:seek","Uncertainty:seek","Days","D MM","D Domains",
                               "D PV","D Time","D Length","D Purchase","Risk x Uncertainty"))


```


``` {r echo=FALSE, results = 'asis', message = FALSE}
texreg(export_files$ols$restricted_mobile_11, 
       float = TRUE, 
       float.env = "table*", 
       longtable = TRUE, 
       dcolumn = TRUE, 
       use.packages = FALSE, 
       fontsize = "small", 
       label = "hurdleddc")
```


``` {r echo=FALSE, results = 'asis', message = FALSE}

stargazer(export_files$ols$restricted_mobile_11,
          export_files$ols$restricted_mobile_12,
          export_files$ols$restricted_mobile_13,
          export_files$ols$restricted_mobile_14,
          export_files$ols$restricted_mobile_15,
          type = "latex",
          digits = 2,
          header=FALSE,
          dep.var.labels = c("log(MM)","log(Domains)","log(PV)","log(Time)","log(Length)"),
          covariate.labels = c("Risk:seek","Uncertainty:seek","Days","D MM","D Domains",
                               "D PV","D Time","D Length","Risk x Uncertainty"))

```

export_files$RAUA_factor

stargazer(export_files$ols$restricted_desktop_1, type = "text", header = FALSE)


print(export_files$desktop_columns[1:6])


``` {r echo=FALSE, results = 'asis'}

print(export_files$RAUA_factor, digits = 2, cutoff = .4, sort = FALSE)

```

stargazer(export_files$rawsurvey[14:64], type="text")

summary(export_files$full_info_sdata[55:105], type="text")
colnames(export_files$full_info_sdata[55:105])



stargazer(rawsurvey[34:51], type="text")

t(apply(rawsurvey[12:17], 2, FUN = function(x) summary(factor(x))),1, print), 1, FUN = function(y) y/rowsum(y))

 apply(apply(rawsurvey[12:17], 2, FUN = function(x) summary(factor(x))), 1, function(y) round(y/426*100,1))
 
 
 
facsumtrans <- function(y){
  t( apply(y, 2, FUN = function(x) summary(factor(x))))
}

facsumtransp <- function(y){
  cbind(
    t( apply(y, 2, FUN = function(x) summary(factor(x)))),
    apply(apply(y, 2, FUN = function(x) summary(factor(x))), 1, function(y) round(y/426*100,1))
  )
}


survey_results <- list()
survey_results[["survey_source"]] <- facsumtrans(rawsurvey[12:17])
survey_results[["survey_tc"]] <- facsumtrans(rawsurvey[26:30])
survey_results[["surveyBIG5"]] <- facsumtrans(rawsurvey[34:44])
survey_results[["surveyRAUA"]] <- facsumtransp(rawsurvey[45:51])

q5 <- c(
  "Researched alternatives online",
  "Used advice of friends or relatives",
  "Used tourist information office",
  "Used travel magazines",
  "Used travel agent",
  "Other"
)
survey_results[["survey_source"]] <- as.data.frame(cbind(q5, survey_results[["survey_source"]]))
stargazer(survey_results[["survey_source"]], type="text", summary = FALSE)

q11 <- c(
  "On this trip, were there any children age 18 or less that you were responsible for?",
  "Have you visited friends or relatives during your trip?",
  "Have you stayed at home of friends or relatives during your trip?",
  "Have you stayed in hotel/motel/airbnb?",
  "Was it a group trip?"
)

survey_results[["survey_tc"]] <- as.data.frame(cbind(q11, survey_results[["survey_tc"]]))
stargazer(survey_results[["survey_tc"]], type="text", summary = FALSE)

big5 <- c(
  "Extraverted, enthusiastic.",
  "Critical, quarrelsome.",
  "Dependable, self-disciplined.",
  "Anxious, easily upset.",
  "Open to new experiences, complex.",
  "Reserved, quiet.",
  "Sympathetic, warm.",
  "Disorganized, careless.",
  "Calm, emotionally stable.",
  "Conventional, uncreative.",
  "Is considerable and kind to almost everyone"
)

survey_results[["surveyBIG5"]] <- as.data.frame(cbind(big5, survey_results[["surveyBIG5"]]))
stargazer(survey_results[["surveyBIG5"]], type="text", summary = FALSE)

raua <- c(
  "I would rather be safe than sorry.",
  "I want to be sure before I purchase anything.",
  "I avoid risky things.",
  "It is important to have instructions spelled out in detail so I always know what I am expected to do.",
  "It is important to closely follow instructions and procedures.",
  "Rules and regulations are important because they tell me what is expected of me.",
  "Standardised work procedures are helpful."
)

survey_results[["surveyRAUA"]] <- as.data.frame(cbind(raua, survey_results[["surveyRAUA"]]))
stargazer(survey_results[["surveyRAUA"]], type="text", summary = FALSE)

stargazer( t(apply(rawsurvey[,c(1:4,6:8)],2, FUN = function(x) summary(factor(x)))), type="text")

stargazer(survey_results[["survey_source"]], type="text", summary = FALSE)
stargazer(survey_results[["survey_tc"]], type="text", summary = FALSE)
stargazer(survey_results[["surveyBIG5"]], type="text", summary = FALSE)
stargazer(survey_results[["surveyRAUA"]], type="text", summary = FALSE)



stargazer(export_files$ols$restricted_desktop_1,
          export_files$ols$restricted_desktop_2,
          export_files$ols$restricted_desktop_3,
          export_files$ols$restricted_desktop_4,
          export_files$ols$restricted_desktop_5, type = "latext")
