### Using time series cutoffs to calculate change in cover over that period of time ###
#Nicole Krampitz
#9/27/2025

##The purpose of this script is to use the bounds provided by TBS to calculate the change in coral cover (from benthic filming) during that time to create a "disturbance value."
##right now, the coral cover sheet is being weird so i calculated my own cover, but i'm not 100% about it because of so many abbreviations
## so the function will be if it's ready or not
calc_dist_intervals <- function (ready) {
if(ready == "no"){
  ##read in the data
  ##Something funky is going on with the benthic_cover_summary its giving super high coral cover values
  #df <- read.csv("data/benthic_cover_summary.csv") %>% mutate(FilmDate = as.Date(FilmDate, format = "%m/%d/%y"))
  # so we'll go with the other datasheet which doesn't have anything summarised
  df <- read.csv("data/TCRMP_Master_BenthicCover_BenthicData.csv") %>%
    mutate(
    CoralCov = rowSums(select(., AA:SSPP, -MILA, -MILC, -MILS), na.rm = TRUE),
    FilmDate = as.Date(FilmDate, format = "%m/%d/%y")) %>%
    select(SampleYear:Transect, CoralCov)

  dates <- read.csv("data/Disturbance_Dates.csv") %>% select(Location:Notes) %>% mutate(FilmDate = as.Date(FilmDate))

  ##start by summarizing data to average coral cover / site / sample date
  df <- df %>%
    group_by(SampleYear, Period, Location, FilmDate) %>%
    summarise(cover_site = mean(CoralCov))

  ## next, create bounds for each site. this does not use the interval disturances, need to clarify with TBS
  dist_bounds <- dates %>%
    filter(Bound %in% c("start", "end")) %>%
    select(FilmDate, Bound, Location, Disturbance) %>%
    pivot_wider(names_from = Bound, values_from = FilmDate)

  # Join coral cover at start
  cover_change<- dist_bounds %>%
    left_join(df, by = c("Location", "start" = "FilmDate")) %>%
    rename(Cover_Start = cover_site) %>%
    left_join(df, by = c("Location", "end" = "FilmDate")) %>%
    rename(Cover_End = cover_site)

  ## calculate change in cover
  cover_change <- cover_change %>%
    mutate(relative.cover.percent = round(((Cover_End-Cover_Start)/Cover_Start )*100, 2),
           absolute.cover = round(Cover_End-Cover_Start,2 ))

  # pretty up table
  cover_change <- cover_change %>%
    mutate(Period.Year.Start = paste(SampleYear.x, Period.x, "start", sep = "."),
           Period.Year.End = paste(SampleYear.y, Period.y, "end", sep = ".")) %>%
    select(Location:end, Period.Year.Start, Cover_Start, Period.Year.End, Cover_End:absolute.cover) }

if(ready == "yes"){
  print("go back in and change the code for the proper dataset")
}
}
