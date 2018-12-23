# What to complete by Thursday
# 2 working apps

# Next steps
# 1 get reformatted version of 4 years data file saved
# 2 remove reading in and formatting from scripts
# 2b have bar chart only pull top 20 categories
# 3b add a related one for top 20 items
# 3c add the ranking info and growth in $s and %
# 3 move titles from bottom to top
# 4 use FileZilla to move files over to server
# 4b figure out where to store data for shiny


#Questions
# For me to answer
# do suppliers and categories fit into the departments
# how much money can be saved in inventory.  Not so much due to storage (though that's a consideration too)
     # but just because of the money tied up in the inventory
#what is the right test period for a product? how many innings to let the pitcher go?
# look at hist of gross profit margin by supplier & maybe also toys
# evaluate seasonality by top suppliers by month and top products for those months
# need to look at seasonality at least for day of week for t-testing

#seasonality: confirm it should be by supplier not dept or category / or should it just be exapndable eventually?

#General notes:
  #11235 different products (SKUs) sold in 2017
  #need to work on T-test with AB tests and new products
      #use daily sales to establish a non-overlapping daily sales mean
  #could look at daily sales of I LOST MY DOG LLC and compare 17 vs 16 for June-July (almost the same) vs. May-Aug (pretty different)
  

Chuck
# Do we have data for 2015 & 2014 also? or are circumstances so different that wouldn't be relevant?
# Trunk sale of jewelry?  What is it?  Is it non-item based?
# Either in data i have or other, do we have brand new suppliers to evaluate?  What were their start dates?  
  #How about Deborah Richardson Designs before Feb of '16 and 
  #Patricia Locke before Feb '16, 
  #Leight Works before March '16 
  #DOWN THE SHORE PUBL before Apr '16
  #EMILY MCDOWELL before March '16
  #NAKAMOL DESIGNS before March '16
  #PATRICIA RAINEY March '16
  #I LOST MY DOG LLC March '16

#12/28 Email & Thoughts

# Share some initial observations and mock ups
  # Concentration of Suppliers cleaned up R Markdown
  # AB Testing 
# Outline expected scope
  # Existing Suppliers
    # Observations of Concentration of suppliers concept
    # Analysis of which suppliers are less valuable to inform decision making
    # Seasonality of Suppliers compared with whole biz
    # Means of looking at trend on one supplier including 
          #y/y line graph compared to whole biz, 
          #prophet showing seasonality
          #ranking in $ size and % growth vs. same period a year ago

  #AB Testing
    # Functionality to test any Supplier, Deptartment, Category, Item or Cart Size vs. any other for any time period
    # Means of testing new Suppliers vs. some standard and know how long to test (how many innings to let it go)
  
  # Additional Analysis of Trends in Seasonality and Cart Size

# Get feedback
# Get other data and get questions answered
  # 2014 & 2015 data
  # Dates of when new suppliers or products launched in 2014, 2015, 2016 so I can chart progress?
  # Full year 2017 data after 12/31


# Can we do supplier concentration analysis by department, by category within department

# how can we predict the sales of a certain company, e.g. firefly
# cart size baselines and difference by time of year
# what time of year sell most X, sell most X
# (Sam question: need aggregate overall business numbers)
# need to filter supplier trends by date of every year for example; maybe "include months"