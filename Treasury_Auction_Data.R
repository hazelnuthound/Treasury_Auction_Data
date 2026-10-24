library(ustfd)
library(tidyverse)

auc <- "v1/accounting/od/auctions_query"

auc <- ustfd_all_pages(
  endpoint = auc,
  filter = NULL,
  fields = NULL,
  sort = NULL,
  page_size = 10000L,
  slowly = FALSE,
  pause = 0.25,
  quiet = TRUE,
  user_agent = "http://github.com/groditi/ustfd"
)

auc_vars <- c(
  "auction_date",
  "security_type",
  "security_term",
  "security_term_week_year",
  "security_term_day_month",
  "cash_management_bill_cmb",
  "offering_amt",
  "bid_to_cover_ratio",
  "total_accepted",
  "total_tendered",
  "high_yield",
  "comp_tendered",
  "comp_tenders_accepted",
  "direct_bidder_tendered",
  "noncomp_accepted",
  "noncomp_tenders_accepted",
  "fima_noncomp_tendered",
  "indirect_bidder_tendered",
  "primary_dealer_tendered",
  "soma_tendered",
  "treas_retail_tenders_accepted"
)

maturity_map <- c(
  "4-Week" = "1-Month",
  "6-Week" = "1.5-Month",
  "8-Week" = "2-Month",
  "13-Week" = "3-Month",
  "17-Week" = "4-Month",
  "26-Week" = "6-Month",
  "52-Week" = "1-Year",

  "1-Year 10-Month" = "2-Year",
  "1-Year 11-Month" = "2-Year",
  "2-Year" = "2-Year",

  "3-Year" = "3-Year",
  "3-Year 3-Month" = "3-Year",

  "3-Year 6-Month" = "4-Year",
  "3-Year 11-Month" = "4-Year",
  "4-Year" = "4-Year",
  "4-Year 4-Month" = "4-Year",

  "4-Year 6-Month" = "5-Year",
  "4-Year 8-Month" = "5-Year",
  "4-Year 9-Month" = "5-Year",
  "4-Year 10-Month" = "5-Year",
  "4-Year 11-Month" = "5-Year",
  "5-Year" = "5-Year",
  "5-Year 2-Month" = "5-Year",
  "5-Year 5-Month" = "5-Year",
  "5-Year 6-Month" = "5-Year",

  "6-Year 4-Month" = "7-Year",
  "6-Year 7-Month" = "7-Year",
  "6-Year 10-Month" = "7-Year",
  "6-Year 11-Month" = "7-Year",
  "7-Year" = "7-Year",
  "7-Year 3-Month" = "7-Year",

  "9-Year 3-Month" = "10-Year",
  "9-Year 4-Month" = "10-Year",
  "9-Year 6-Month" = "10-Year",
  "9-Year 8-Month" = "10-Year",
  "9-Year 9-Month" = "10-Year",
  "9-Year 10-Month" = "10-Year",
  "9-Year 11-Month" = "10-Year",
  "10-Year" = "10-Year",

  "14-Year 10-Month" = "15-Year",
  "15-Year 1-Month" = "15-Year",

  "19-Year 6-Month" = "20-Year",
  "19-Year 9-Month" = "20-Year",
  "19-Year 10-Month" = "20-Year",
  "19-Year 11-Month" = "20-Year",
  "20-Year" = "20-Year",
  "20-Year 1-Month" = "20-Year",
  "20-Year 6-Month" = "20-Year",

  "28-Year 6-Month" = "30-Year",
  "29-Year 3-Month" = "30-Year",
  "29-Year 4-Month" = "30-Year",
  "29-Year 6-Month" = "30-Year",
  "29-Year 8-Month" = "30-Year",
  "29-Year 9-Month" = "30-Year",
  "29-Year 10-Month" = "30-Year",
  "29-Year 11-Month" = "30-Year",
  "30-Year" = "30-Year",
  "30-Year 3-Month" = "30-Year",
  "30-Year 6-Month" = "30-Year"
)

auctions <- auc$data %>%
  select(auc_vars) %>%
  filter(cash_management_bill_cmb != "Yes") %>%
  mutate(
    maturity = unname(maturity_map[security_term]),
    maturity = factor(
      maturity,
      levels = c(
        "1-Month",
        "1.5-Month",
        "2-Month",
        "3-Month",
        "4-Month",
        "6-Month",
        "1-Year",
        "2-Year",
        "3-Year",
        "4-Year",
        "5-Year",
        "7-Year",
        "10-Year",
        "15-Year",
        "20-Year",
        "30-Year"
      )
    )
  )

################ Graph
graph_title <- "Treasury Auction Bid-Cover Ratio by Maturity"
begin_date <- "2024-01-01"

bid_cover_facet_graph <- auctions %>%
  filter(auction_date >= begin_date) %>%
  ggplot(aes(x = auction_date, y = bid_to_cover_ratio)) +
  geom_line(color = "blue") +
  facet_wrap(~maturity, scales = "free") +
  geom_vline(
    xintercept = as.Date("2026-02-28"),
    #  linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = as.Date("2025-04-01"),
    #  linetype = "dashed",
    color = "purple"
  ) +
  theme_minimal() +
  labs(
    title = graph_title,
    subtitle = "Purple Line Represents Announcement of Liberation Day Tariffs\nRed Line Indicates Start of Iran Conflict",
    y = NULL,
    x = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

bid_cover_facet_graph

fname <- paste0("Output/", "bid_cover_facet_graph", ".jpg")
ggsave(
  bid_cover_facet_graph,
  filename = fname,
  width = 3200,
  height = 1800,
  dpi = 300,
  units = "px"
)
