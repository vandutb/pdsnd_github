
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

#import library ggplot
library(ggplot2)

# Function merge data together
two_col_df = function(Column, Col.Name,  City.Name){
    new_df = data.frame(Column, City.Name, stringsAsFactors = FALSE)
    names(new_df) = c(Col.Name, 'City')
    return(new_df)
}

trips = rbind(two_col_df(chi$Trip.Duration, 'Trip.Duration', 'Chicago'),
              two_col_df(ny$Trip.Duration, 'Trip.Duration', 'New York'), 
              two_col_df(wash$Trip.Duration, 'Trip.Duration', 'Washington'))
users = rbind(two_col_df(chi$User.Type, 'User.Type', 'Chicago'),
              two_col_df(ny$User.Type, 'User.Type', 'New York'), 
              two_col_df(wash$User.Type, 'User.Type', 'Washington'))

trips = cbind(trips, 'User.Type'=users[,1])

# 3 Functions to extract month, day, and hour from the Start.Time of 3 table of cities
month = function(df){
    return(substring(df$Start.Time,6,7))
}

day = function(df){
    return(substring(df$Start.Time, 9,10))
}

hour = function(df){
    return(substring(df$Start.Time, 12,13))
}

# The Function to prepaire list day of week 
weekday = function(df){
    return(factor(weekdays(as.Date(df$Start.Time)), 
                 levels = c('Monday', 'Tuesday', 'Wednesday',
                            'Thursday', 'Friday', 'Saturday', 'Sunday')))
}

# Function creates a frequency table of a variable, assigns column names
count_table = function(var, var.Name, City.Name){
    var_count = data.frame(table(var))
    names(var_count) = c(var.Name, City.Name)
    var_count = subset(var_count, subset = var_count[1] != '') 
    return(var_count)
}
months_table = cbind(count_table(month(chi), 'Month', 'Chicago'), 
                      'New.York'=count_table(month(ny), 'Month', 'New.York')[,2], 
                      'Washington'=count_table(month(wash), 'Month', 'Washington')[,2])
months_table

weekday_table = cbind(count_table(weekday(chi), 'Weekday', 'Chicago'), 
                      'New.York'=count_table(weekday(ny), 'Weekday', 'New.York')[,2], 
                      'Washington'=count_table(weekday(wash), 'Weekday', 'Washington')[,2])
weekday_table

hour_table = cbind(count_table(hour(chi), 'Hour', 'Chicago'), 
                      'New.York'=count_table(hour(ny), 'Hour', 'New.York')[,2], 
                      'Washington'=count_table(hour(wash), 'Hour', 'Washington')[,2])

# Create a data frame combining months data from different cities
month_df = rbind(two_col_df(as.integer(month(chi)), 'Month', 'Chicago'),
              two_col_df(as.integer(month(ny)), 'Month', 'New York'), 
              two_col_df(as.integer(month(wash)), 'Month', 'Washington'))

weekday_df = rbind(two_col_df(weekday(chi), 'Weekday', 'Chicago'),
              two_col_df(weekday(ny), 'Weekday', 'New York'), 
              two_col_df(weekday(wash), 'Weekday', 'Washington'))

hour_df = rbind(two_col_df(hour(chi), 'Hour', 'Chicago'),
              two_col_df(hour(ny), 'Hour', 'New York'), 
              two_col_df(hour(wash), 'Hour', 'Washington'))

ggplot(data = month_df, aes(x = Month)) +
    geom_histogram(stat="count", color='black', fill='orange') +
    facet_wrap(~City, scales = "free") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Monthly Trips') +
    ylab('Monthly trip count')

ggplot(data = weekday_df, aes(x = Weekday)) +
    geom_histogram(stat="count", color='black', fill='orange') +
    facet_wrap(~City, scales = "free") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Weekly Trips') +
    ylab('Weekly trip count')

ggplot(data = hour_df, aes(x = Hour)) +
    geom_histogram(stat="count", color='black', fill='orange') +
    facet_wrap(~City, scales = "free") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Hour Trips') +
    ylab('Hour trip count')

# Define a function called two_col_df
# The function creates a new data frame with two columns: one for the given column data and 
# another for the city name. It then sets column names accordingly and returns the new data frame.
two_col_df = function(Column, Col.Name,  City.Name){
    new_df = data.frame(Column, City.Name, stringsAsFactors = FALSE)
    names(new_df) = c(Col.Name, 'City')
    return(new_df)
}

# Create data frames for trip durations and user types from different cities
trips = rbind(two_col_df(chi$Trip.Duration, 'Trip.Duration', 'Chicago'),
              two_col_df(ny$Trip.Duration, 'Trip.Duration', 'New York'), 
              two_col_df(wash$Trip.Duration, 'Trip.Duration', 'Washington'))
users = rbind(two_col_df(chi$User.Type, 'User.Type', 'Chicago'),
              two_col_df(ny$User.Type, 'User.Type', 'New York'), 
              two_col_df(wash$User.Type, 'User.Type', 'Washington'))

trips = cbind(trips, 'User.Type'=users[,1])

qplot(x=City, y=Trip.Duration/60,
    data=subset(trips, User.Type!=''),
    geom='boxplot',
    ylim=c(0,30),
    ylab='Trip Duration (minutes)',
    main='Trip Duration by City and User Type') +
    facet_wrap(~User.Type, scales = "free") 

# I'm using the loop to merge data from table
city = c('Chicago', 'New York', 'Washington')
c = 0
for (i in list(chi, ny, wash)){
    c = c+1
    print(city[c])
    valid_trips = subset(i$Trip.Duration/60, !is.na(i$Trip.Duration))
    print(summary(valid_trips), digits=2)
}

system('python -m nbconvert Explore_bikeshare_data.ipynb')
