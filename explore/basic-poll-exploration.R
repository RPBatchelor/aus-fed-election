

# Explore the poll results over time

p <- ozpolls %>%
  ggplot(aes(x = mid_date, y = intended_vote, colour = party)) +
  geom_point(size = 0.2) +
  geom_smooth(span = 0.1, se = T) +
  facet_wrap(~preference_type) +
  scale_colour_manual(values = oz_party_cols) +
  geom_vline(data = election_dates_and_results,
             aes(xintercept = as.numeric(election_date)),
             linetype = "dashed", colour = "snow3") #+
  # geom_text(data = election_dates_and_results, 
  #           aes(x = as.numeric(election_date), 
  #               y = Inf, label = election_winner), 
  #           angle = 90, vjust = -0.5, hjust = -0.1, size = 3)


p

ggplotly(p)


