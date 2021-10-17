library(tidyverse)
library(glue)

# make fake data
n_obs <- 20

fake_dat <-
  tibble(
    #id = row_number(),
    #case = glue("case_{id}"),
    age = sample(30:60, size = n_obs, replace = TRUE),
    married = sample(c("no", "yes", NA), size = n_obs, replace = TRUE),
    gender = sample(c("M", "F", NA), size = n_obs, replace = TRUE),
    location = sample(letters[1:4], size = n_obs, replace = TRUE), 
    health = sample(c("LT", "LT/p", "LT/m", "LT/pm", NA), size = n_obs, 
                    replace = TRUE),
    ethnicity = sample(c("white", "other", "asian", "opt out", NA),
                       size = n_obs, 
                       replace = TRUE),
    channel = sample(c("email", "phone", "in person", "adviceline", NA),
                       size = n_obs, 
                       replace = TRUE),
    abuse = sample(c("Physical", "Emotional/psychological", "Sexual", "Financial", NA), 
                   size = n_obs,
                   replace = TRUE
                   ),
    context = sample(c("male current partner", "female current partner", 
                       "same sex relationship", NA), 
                     size = n_obs,
                     replace = TRUE
                     )
  ) 

# count nas


fake_dat %>% 
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x) 
            / nrow(fake_dat)
            
            )
    )
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "attribute",
    values_to = "proportion"
  ) %>%
  mutate(
    max = 1,
    attribute = fct_reorder(attribute, proportion)
  ) %>% 
  ggplot() +
  geom_col(aes(x = attribute, y = max),
           alpha = 0.2
  ) +
  geom_col(aes(x = attribute, y = proportion), 
           alpha = 0.8
  ) +
  coord_flip() +
  ggthemes::theme_tufte() +
  labs(
    title = glue("Proportion of missing attribute for total number ({nrow(fake_dat)}) of case files"),
    subtitle = "We can only ask if an attribute affected an outcome for the case files where we recorded that attribute" %>% str_wrap(),
    y = "Attribute",
    x = "Proportion of total number of case files"
  )

  
# take a look at the data
fake_dat %>% 
  View()
