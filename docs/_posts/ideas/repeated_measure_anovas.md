Don't do it
===========

**Ha! Got ya!** Trying to run some *old school* ANOVAs *mmh*? **I'll show you even better!**

There is now a tremendous amount of data showing the inadequacy of ANOVAs as a statistical procedure ([Camilli, 1987](http://journals.sagepub.com/doi/abs/10.3102/10769986012001087); [Levy, 1978](https://www.tandfonline.com/doi/abs/10.1080/00949657808810247); [Vasey, 1987](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1469-8986.1987.tb00324.x); [Chang, 2009](https://link.springer.com/article/10.1007/s00180-009-0162-z); ...). Instead, many papers suggest moving toward the mixed-modelling framework ([Kristensen, 2004](https://www.physiology.org/doi/abs/10.1152/advan.00042.2003); [Jaeger, 2008](https://www.sciencedirect.com/science/article/pii/S0749596X07001337)), which was shown to be more flexible, accurate, powerful and suited for psychological data.

Using this framework, we will see how can very simply obtained answers to our questions.

The Emotion Dataset
===================

Let's take the example dataset included in the `psycho` package.

``` r
library(psycho)
library(tidyverse)

df <- psycho::emotion %>% 
  select(Participant_ID, 
         Participant_Sex, 
         Emotion_Condition, 
         Subjective_Arousal,
         Recall)

summary(df)
```

     Participant_ID Participant_Sex Emotion_Condition Subjective_Arousal
     10S    : 48    Female:720      Negative:456      Min.   :  0.00    
     11S    : 48    Male  :192      Neutral :456      1st Qu.: 18.66    
     12S    : 48                                      Median : 51.04    
     13S    : 48                                      Mean   : 45.94    
     14S    : 48                                      3rd Qu.: 69.13    
     15S    : 48                                      Max.   :100.00    
     (Other):624                                                        
       Recall       
     Mode :logical  
     FALSE:600      
     TRUE :312      
                    
                    
                    
                    

Our dataframe (called `df`) contains data from several subjects, that were shown neutral and negative pictures (the `Emotion_Condition` column). Each row corresponds here to a single trial. As such, there are 48 rows by participant (48 trials). During each trial, the participant has also to rate the emotional arousal experienced during picture presentation. Moreover, 20min after this emotional rating task, the participant was asked to freely recall all the pictures he remembered.

Our dataframe contains, for each trial, 5 variables: the name of the participant (`Participant_ID`), its sex (`Participant_Sex`), the emotion condition (`Emotion_Condition`), the arousal rating (`Subjective_Arousal`) and whether the participant recalled the picture (`Recall`).

The Effect of Emotion
=====================

Does the emotion condition modulate the subjective arousal? How to answer?

**Whith a repeated measures ANOVA of course!**

Let's run it:

``` r
summary(aov(Subjective_Arousal ~ Emotion_Condition + Error(Participant_ID/Emotion_Condition), data=df))
```


    Error: Participant_ID
              Df Sum Sq Mean Sq F value Pr(>F)
    Residuals 18 150406    8356               

    Error: Participant_ID:Emotion_Condition
                      Df Sum Sq Mean Sq F value   Pr(>F)    
    Emotion_Condition  1 258572  258572   117.7 2.51e-09 ***
    Residuals         18  39540    2197                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Error: Within
               Df Sum Sq Mean Sq F value Pr(>F)
    Residuals 874 378749   433.4               

Wow, we found that there is a significant effect of the emotional condition on arousal ratings.

As you know, an ANOVA is pretty much a *summary* of a linear model where the predictors are factors. Therefore, we can run an ANOVA on a linear mixed model (which includes the "error" term, or *random effect*).

``` r
library(lmerTest)
fit <- lmer(Subjective_Arousal ~ Emotion_Condition + (1|Participant_ID), data=df)
anova(fit)
```

    Analysis of Variance Table of type III  with  Satterthwaite 
    approximation for degrees of freedom
                      Sum Sq Mean Sq NumDF  DenDF F.value    Pr(>F)    
    Emotion_Condition 258572  258572     1 891.96  551.41 < 2.2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The results are, for the important part (sum of squares, mean square and p value), almost identical to the *traditional* approach. However, it is much more powerful.

Post-hoc / Contrast Analysis
============================

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>
