#===================================================================================================================
# Author: Aloysius Ong
# Dataset: #tidytuesday - African-American Achievements
# Purpose: Visualization 
#===================================================================================================================

setwd("C:/Users/User/Desktop")

library(dplyr)
library(ggcharts)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(forcats)
library(gghighlight)
library(dplyr)
library(tibble)
library(ggThemeAssist)
library(data.table)
library(tidyverse)
library(rvest)
library(ggridges)
library(lubridate)
library(scales)
library(ggridges)
library(tidytext)
library(tidyverse)
library(dplyr)
library(debugr)
library(textdata)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tomtom)
library(colorspace)

#---- Cleaning Data ----
# URL 1
url_science <- "https://en.wikipedia.org/wiki/List_of_African-American_inventors_and_scientists"

raw_html_sci <- read_html(url_science)

get_source <- function(x){
  raw_html_sci %>% 
    html_nodes("tbody") %>% 
    .[[2]] %>% 
    html_nodes(glue::glue("tr:nth-child({x})")) %>% 
    html_nodes("td:nth-child(1) > a") %>% 
    html_attr("href")
}

raw_sci_tab <- raw_html_sci %>% 
  html_table() %>% 
  .[[2]] %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(links = map(row_number(), get_source))

clean_sci <- raw_sci_tab %>% 
  mutate(references = str_replace_all(references, "\\]", ","),
         references = str_remove_all(references, "\\[")) %>% 
  unnest(links) %>% 
  mutate(links = paste0("https://en.wikipedia.org", links)) %>% 
  separate(years, into = c("birth", "death"), sep = "-") %>% 
  mutate_at(c("birth", "death"), as.integer) %>% 
  mutate(occupation_s = str_replace_all(occupation_s, ",", ";"))

clean_sci %>% 
  filter(str_detect(tolower(occupation_s), "statistician"))


sci_citations <- raw_html_sci %>% 
  html_node("#mw-content-text > div > div.reflist > div") %>% 
  html_nodes("li") %>% 
  html_text() %>% 
  str_remove("\\^ ") %>% 
  enframe() %>% 
  rename(citation_num = name, citation = value) %>% 
  mutate(citation = str_replace_all(citation, "\"", "'"),
         citation = str_remove_all(citation, "\\n"))

sci_citations

clean_sci %>%
  add_row(name = "Amos, Harold", birth = 1918, death = 2003, occupation_s = "Microbiologist",
    inventions_accomplishments = "First African-American department chair at Harvard Medical School",
    references = "6,", links = "https://en.wikipedia.org/wiki/Harold_Amos") %>% 
  write_csv(path = "science.csv")

science <- read_csv("science.csv")


# Firsts ------------------------------------------------------------------

first_url <- "https://en.wikipedia.org/wiki/List_of_African-American_firsts"

raw_first <- read_html(first_url)

get_year <- function(id_num) {
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > h4:nth-child({id_num}) > span")) %>% 
    html_attr("id") %>% 
    .[!is.na(.)]
}

get_first <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > ul:nth-child({id_num})")) %>% 
    html_text() %>% 
    str_split("\n")
}

tidyr::crossing(id_num = 9:389, count = 1:5)

raw_first_df <- tibble(id_num = 9:390) %>% 
  mutate(year = map(id_num, get_year),
         text = map(id_num, get_first))

clean_first <- raw_first_df %>% 
  mutate(year = as.integer(year)) %>% 
  fill(year) %>% 
  unnest(text) %>% 
  unnest(text) %>% 
  separate(text, into = c("role", "person"), sep = ": ") %>% 
  mutate(person = str_remove_all(person, "\\\\"),
         person = str_trim(person),
         role = str_replace(role, "African American", "African-American")) %>% 
  select(year, role, person)

clean_first %>% 
  group_by(year) %>% 
  summarize(n =n())

first_role <- function(category){
  str_detect(tolower(role), category)
}

edu <- paste0(c(
  "practice", "graduate", "learning", "college", "university", "medicine",
  "earn", "ph.d.", "professor", "teacher", "school", "nobel", "invent", "patent",
  "medicine", "degree", "doctor", "medical", "nurse", "physician", "m.d.", "b.a.", "b.s.", "m.b.a",
  "principal", "space", "astronaut"
), collapse = "|")

religion <- c("bishop", "rabbi", "minister", "church", "priest", "pastor", "missionary",
              "denomination", "jesus", "jesuits", "diocese", "buddhis") %>%
  paste0(collapse = "|")

politics <- c(
  "diplomat", "elected", "nominee", "supreme court", "legislature", "mayor", "governor",
  "vice President", "president", "representatives", "political", "department", "peace prize",
  "ambassador", "government", "white house", "postal", "federal", "union", "trade",
  "delegate", "alder", "solicitor", "senator", "intelligience", "combat", "commissioner",
  "state", "first lady", "cabinet", "advisor", "guard", "coast", "secretary", "senate",
  "house", "agency", "staff", "national committee"
) %>%
  paste0(collapse = "|")

sports <- c(
  "baseball", "football", "basketball", "hockey", "golf", "tennis",
  "championship", "boxing", "games", "medal", "game", "sport", "olympic", "nascar",
  "coach", "trophy", "nba", "nhl", "nfl", "mlb", "stanley cup", "jockey", "pga",
  "race", "driver", "ufc", "champion"
) %>%
  paste0(collapse = "|")

military <- c(
  "serve", "military", "enlist", "officer", "army", "marine", "naval",
  "officer", "captain", "command", "admiral", "prison", "navy", "general",
  "force"
) %>%
  paste0(collapse = "|")

law <- c("american bar", "lawyer", "police", "judge", "attorney", "law", 
         "agent", "fbi") %>%
  paste0(collapse = "|")

arts <- c(
  "opera", "sing", "perform", "music", "billboard", "oscar", "television",
  "movie", "network", "tony award", "paint", "author", "book", "academy award", "curator",
  "director", "publish", "novel", "grammy", "emmy", "smithsonian",
  "conduct", "picture", "pulitzer", "channel", "villain", "cartoon", "tv", "golden globe",
  "comic", "magazine", "superhero", "pulitzer", "dancer", "opry", "rock and roll", "radio",
  "record") %>%
  paste0(collapse = "|")

social <- c("community", "freemasons", "vote", "voting", "rights", "signature", 
            "royal", "ceo", "community", "movement", "invited", "greek", "million",
            "billion", "attendant", "chess", "pilot", "playboy", "own", "daughter",
            "coin", "dollar", "stamp", "niagara",
            "stock", "north pole", "reporter", "sail around the world", "press", "miss ",
            "everest")  %>%
  paste0(collapse = "|")

first_df <- clean_first %>% 
  mutate(gender = if_else(str_detect(role, "woman|Woman|her|she|female"), 
                          "Female African American Firsts", "African-American Firsts"),
         role = str_remove_all(role, "\""),
         person = str_remove_all(person, "\""),
         category = case_when(
           str_detect(tolower(role), military) ~ "Military",
           str_detect(tolower(role), law) ~ "Law",
           str_detect(tolower(role), arts) ~ "Arts & Entertainment",
           str_detect(tolower(role), social) ~ "Social & Jobs",
           str_detect(tolower(role), religion) ~ "Religion",
           str_detect(tolower(role), edu) ~ "Education & Science",
           str_detect(tolower(role), politics) ~ "Politics",
           str_detect(tolower(role), sports) ~ "Sports",
           TRUE ~ NA_character_
         )) %>% 
  rename(accomplishment = role)

first_df %>% write_csv(path = "firsts.csv")

firsts <- read_csv("firsts.csv")

plot_ex <- first_df %>% 
  mutate(n = 1) %>% 
  group_by(category) %>% 
  mutate(roll_n = cumsum(n)) %>% 
  ggplot(aes(x = year, y = roll_n, color = category)) +
  geom_step(size = 1) +
  theme(legend.position = "top") +
  tomtom::theme_tomtom() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(1750, 2020, 25)) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  labs(x = "", y = "",
       title = "Cumulative African-Americans firsts over time",
       subtitle = "479 'Firsts' of African-Americans breaking the color barrier across a wide range of topics",
       caption = "Data: wikipedia.org/wiki/List_of_African-American_firsts")

ggsave("pic2.png", plot_ex, height = 8, width = 14, units = "in", dpi = "retina")

#---- Making Viz ----

glimpse(firsts)

firsts  %>% 
  group_by(year,gender) %>% 
  count(gender) %>% 
  mutate(roll_n = cumsum(n)) %>%
  ggplot(aes(x = year, y = n, fill=gender)) +
  geom_bar(stat = "identity", width = 5) +
  facet_wrap(~ gender, ncol=2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(1750, 2020, 25)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum_rc()

firsts  %>% 
  group_by(year,gender) %>% 
  count(person)

table(firsts$gender)

firsts  %>% 
  group_by(year,gender) %>% 
  count(gender) %>% 
  mutate(roll_n = cumsum(n)) %>% 
  ggplot(aes(x = year, y = n, fill=gender)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  facet_wrap(~ gender, ncol=2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(1750, 2020, 25)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum_rc()

glimpse(firsts)

firsts$gender[which(firsts$gender == "African-American Firsts")] <- "Male African-American Firsts"

firsts %>% 
  mutate(n = 1) %>% 
  group_by(gender) %>%
  mutate(roll_n = cumsum(n)) %>% 
  ggplot(aes(x = year, y = roll_n, color = gender)) +
  geom_line(size=1) +
  facet_wrap(~ gender, ncol=2) +
  scale_color_manual(values=c("#9999CC","#CC6666")) +
  theme_ipsum_rc() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(1750, 2020, 25)) +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "107 African-American Females Firsts and 372 African-American Males First Achievements\nacross various disciplines",
       subtitle = "The numbers have been increasing EXPONENTIALLY since the 1850s although Males have witnessed a more dramatic increase.",
       caption = "Data: Wikipedia (List_of_African-American_firsts), 2020")


#---- Looking at Science Dataset ----

table(science$inventions_accomplishments)

science_accomplishments <- Corpus(VectorSource(science$inventions_accomplishments))


science_accomplishmentsClean<-tm_map(science_accomplishments, PlainTextDocument)
science_accomplishmentsClean<-tm_map(science_accomplishments,tolower)
science_accomplishmentsClean<-tm_map(science_accomplishmentsClean,removeNumbers)
science_accomplishmentsClean<-tm_map(science_accomplishmentsClean,removeWords,stopwords("english"))
science_accomplishmentsClean<-tm_map(science_accomplishmentsClean,removePunctuation)
science_accomplishmentsClean<-tm_map(science_accomplishmentsClean,stripWhitespace)
science_accomplishmentsClean<-tm_map(science_accomplishmentsClean,stemDocument)

system.time(wordcloud(words = science_accomplishmentsClean, min.freq = 2,
                      max.words=200, random.order=FALSE, 
                      rot.per=0.35, 
                      colors=brewer.pal(8, "Set2")))

class(science_accomplishmentsClean)

science_accomplishmentsClean

dtm <- TermDocumentMatrix(science_accomplishmentsClean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

d

d %>%
  filter(freq > 5) %>%
bar_chart(word, freq, 
          highlight = c("first", "univers", "research"), 
          sort = TRUE, limit = 20) + 
  theme_ft_rc() +
  theme(legend.position="none") +
  theme(panel.grid.major.y = element_blank()) +
  labs(title="Scientific Achievements of African-Americans",
       subtitle = "They were some of the pioneers of inventions and theories, having worked mostly with universities and researchers.",
       caption="Source: Wikipedia, 2020", 
       x = "Word",
       y = "Frequency") 















