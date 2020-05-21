# NFL HANIC 

Slides for my HANIC presentation

You can find the longer form writeup on my blog at [themockup.blog](https://themockup.blog/).

To get the 2.2 GB of raw data you'll need to download the data from 2000-2019 via `nflfastR`, otherwise my code as included should get you to the end result.

Full Disclosure:
- The Random Forest model takes about 20 min to run w/ 1000 trees.
- You lose about 3% accuracy w/ only 100 trees but cut your run time down to 2 min
- The logistic regression is almost as accurate as the Random Forest model and takes about 2 min as well


![Thank you `nflscrapR` and `nflfastR`](thank_you.png)
