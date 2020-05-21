# NFL HANIC 

Slides for my HANIC presentation

You can find the longer form writeup on my blog at [themockup.blog](https://themockup.blog/).

To get the 2.2 GB of raw data you'll need to download the data from 2000-2019 via `nflfastR`, otherwise my code as included should get you to the end result.

Full Disclosure:
- The Random Forest model takes about 20 min to run w/ 1000 trees.
- You lose about 3% accuracy w/ only 100 trees but cut your run time down to 2 min
- The logistic regression is almost as accurate as the Random Forest model and takes about 2 min as well


![Thank you `nflscrapR` and `nflfastR`](thank_you.png)

# Learn more about `tidymodels`

* [`tidymodels`.org](https://www.tidymodels.org/learn/) has step by step guides of various complexities

* I'll be adding additional NFL-focused examples at: [TheMockup.blog](https://themockup.blog/posts/2020-05-01-tidy-long-models/)

* Julia Silge's (a `tidymodels` maintainer) [blog](https://juliasilge.com/) or [video series](https://www.youtube.com/channel/UCTTBgWyJl2HrrhQOOc710kA)  
  * She has 10+ videos/blogposts covering various aspects of the full pipeline
  * Most recently covered [predicting Beach Volleyball winners](https://juliasilge.com/blog/xgboost-tune-volleyball/) w/ a tuned XGBoost model

* Alison Hill's [Workshop from rstudio::conf2020](https://conf20-intro-ml.netlify.app/)

* Gentle Intro to `tidymodels` on [RStudio RViews blog](https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/)

* Julia Silge's online [free interactive course](https://supervised-ml-course.netlify.app/)
