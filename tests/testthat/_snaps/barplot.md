# barplot module works as expected in the test app

    Code
      cat(res)
    Output
      'data.frame':	3 obs. of  16 variables:
       $ y          : num  1 3 1
       $ count      : num  1 3 1
       $ prop       : num  1 1 1
       $ x          : 'mapped_discrete' num  1 2 3
       $ flipped_aes: logi  FALSE FALSE FALSE
       $ PANEL      : Factor w/ 2 levels "1","2": 1 1 2
       $ group      : int  1 2 3
       $ ymin       : num  0 0 0
       $ ymax       : num  1 3 1
       $ xmin       : 'mapped_discrete' num  0.55 1.55 2.55
       $ xmax       : 'mapped_discrete' num  1.45 2.45 3.45
       $ colour     : logi  NA NA NA
       $ fill       : chr  "grey35" "grey35" "grey35"
       $ linewidth  : num  0.5 0.5 0.5
       $ linetype   : num  1 1 1
       $ alpha      : logi  NA NA NA

