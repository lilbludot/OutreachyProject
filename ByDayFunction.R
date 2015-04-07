library(dplyr); 
by_day<- function(df){
        df_limited <- select(df, -(date))
        by_day_df <- group_by(df_limited,moDayYear)
        avg_by_day_df<-summarise(by_day_df, count=n(), avgRTT = round(mean(RTT), digits = 2))
        dailyTS <- ts(avg_by_day_df$avgRTT)
        plot(dailyTS, type="l")
        return(avg_by_day_df)
}