library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)


install.packages('reshape2')
library(reshape2)
library(maps)
library(grid)




#READING THE DATASET
setwd('/Users/user/Desktop/proj_folder')
ds_chess <- read.csv('chess_wc_history_game_info.csv')
View(ds_chess)
num_rows <- nrow(ds_chess)
num_rows

extract_surname <- function(name) {
  strsplit(name, ",")[[1]][1]
}

# Apply the function to the white and black columns
ds_chess$white <- sapply(ds_chess$white, extract_surname)
ds_chess$black <- sapply(ds_chess$black, extract_surname)

# Print the modified dataframe
#View(ds_chess)

all_players <- unique(c(ds_chess$white, ds_chess$black ))
all_players

projected_df <- ds_chess[, c("white", "black")]
colnames(projected_df) <- c("Source", "Target")
# Print the new dataframe
View(projected_df)

#---------------------
sorted_df <- t(apply(projected_df, 1, sort))
sorted_df <- as.data.frame(sorted_df, stringsAsFactors = FALSE)
colnames(sorted_df) <- c("Source", "Target")

# Remove duplicates
unique_sorted_df <- unique(sorted_df)

# Print the resulting dataframe
View(unique_sorted_df)

write.csv(unique_sorted_df, "/Users/user/Desktop/proj_folder/Edges.csv", row.names = FALSE)

nodes <- data.frame(id = all_players)
View(nodes)
# Convert the all_players dataframe to a CSV file
write.csv(nodes, "/Users/user/Desktop/proj_folder/Nodes.csv", row.names = FALSE)

#-------------------------
View(nodes)

surname_counts <- list()

# Iterate over unique_sorted_df$Source and count appearances
surname_counts <- list()
for (surname in all_players) {
  count <- sum(unique_sorted_df$Source == surname) + sum(unique_sorted_df$Target == surname)
  surname_counts[[surname]] <- count
}
surname_counts_df <- data.frame(
  Surname = names(surname_counts),
  Appearances = as.numeric(unlist(surname_counts))
)
all_players_df <- surname_counts_df

nodes <- surname_counts_df
write.csv(nodes, "/Users/user/Desktop/proj_folder/Nodes.csv", row.names = FALSE)
#------------------------- 
gm_players_statistics <- read.csv('GM_players_statistics.csv')
gm_players_statistics$surname <- sapply(strsplit(as.character(gm_players_statistics$name), " "), function(x) x[length(x)])
gm_players_statistics$surname <- as.character(gm_players_statistics$surname)
additional_surname_country_lookup <- gm_players_statistics %>% 
  select(surname, country) %>%
  distinct(surname, .keep_all = TRUE)

all_players_df <- all_players_df %>%
  left_join(additional_surname_country_lookup, by = c("Surname" = "surname")) %>%
  mutate(nationality = ifelse(is.na(country), "No Data", country)) %>%
  select(Surname, Appearances, nationality)
colnames(all_players_df)[1] <- "Id"
colnames(all_players_df)[3] <- "Nationality"

write.csv(all_players_df, "/Users/user/Desktop/proj_folder/Nodes.csv", row.names = FALSE)

#------------------
View(all_players_df)
bar_df <- read.csv("/Users/user/Desktop/proj_folder/Nodes.csv")

filtered_df <- bar_df %>%
  filter(Appearances > 10) %>%
  arrange(desc(Appearances))

# Create the bar chart
ggplot(filtered_df, aes(x = reorder(Id, -Appearances), y = Appearances, fill = Nationality)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Appearances by Players",
       x = "Id",
       y = "Appearances") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#------------------------------------

ds_chess$eco <- substr(ds_chess$eco, 1, 1)

# Print the resulting dataframe
ds_chess$file_name <- substr(ds_chess$file_name, nchar(ds_chess$file_name) - 7, nchar(ds_chess$file_name) - 4)
ds_chess <- ds_chess %>%
  rename(year = file_name)
# Print the resulting dataframe
print(head(ds_chess))
View(ds_chess)

#------------------------------
#eco popularity
eco_year_counts <- ds_chess %>%
  group_by(year, eco) %>%
  summarise(count = n()) %>%
  ungroup()

# Filter to keep only the most popular ECO code per year
most_popular_eco <- eco_year_counts %>%
  group_by(year) %>%
  filter(count == max(count)) %>%
  ungroup()

# Plot the data using ggplot2
ggplot(most_popular_eco, aes(x = as.factor(year), y = count, fill = eco)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Popular ECO Code by Year", x = "Year", y = "Count", fill = "ECO Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

install.packages("rworldmap")
#----------------------------------
install.packages("maps")
library(maps)

# Read the data from the CSV file
df <- read.csv("worldchampions.csv")

# Summarize the data by country
country_summary <- df %>%
  group_by(Country) %>%
  summarise(Years = n())

# Adjust Soviet Union to post-Soviet countries
post_soviet_countries <- c("Russia", "Ukraine", "Belarus", "Uzbekistan", "Kazakhstan",
                           "Georgia", "Azerbaijan", "Lithuania", "Moldova", "Latvia",
                           "Kyrgyzstan", "Tajikistan", "Armenia", "Turkmenistan", "Estonia")

# Sum the years for Soviet Union and distribute to all post-Soviet countries
soviet_union_years <- country_summary %>%
  filter(Country == "Soviet Union") %>%
  summarise(Years = sum(Years)) %>%
  pull(Years)

# Assign Soviet Union years to all post-Soviet countries
expanded_country_summary <- country_summary %>%
  filter(Country != "Soviet Union") %>%
  mutate(region = case_when(
    Country == "Austria" ~ "Austria",
    Country == "Germany" ~ "Germany",
    Country == "Cuba" ~ "Cuba",
    Country == "France" ~ "France",
    Country == "Netherlands" ~ "Netherlands",
    Country == "Russia" ~ "Russia",
    Country == "USA" ~ "USA",
    Country == "India" ~ "India",
    Country == "Norway" ~ "Norway",
    TRUE ~ Country
  )) %>%
  bind_rows(data.frame(
    Country = post_soviet_countries,
    Years = soviet_union_years,
    region = post_soviet_countries
  ))

# Sum the years for Russia explicitly including the Soviet Union years
expanded_country_summary <- expanded_country_summary %>%
  group_by(region) %>%
  summarise(Years = sum(Years))

# Get world map data
world_map <- map_data("world")

# Merge world map data with country summary
world_map <- left_join(world_map, expanded_country_summary, by = "region")

# Plot the world map
ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = Years)) +
  geom_polygon(color = "black") +
  scale_fill_continuous(na.value = "grey90", low = "lightblue", high = "darkblue") +
  theme_void() +
  labs(title = "Distribution of World Chess Champions by Country",
       fill = "Years of Reign")

#-------------------------------------------------------------------------------
# #Heatmap of all major opennings 
# opennings_ds <- read.csv('eco_codes.csv')
# 
# # Split the dataframe by eco_type
# split_dfs <- split(opennings_ds, opennings_ds$eco_type)
# 
# # Print the names of the resulting list to see the split
# split_dfs$A$eco_example
# 
# split_dfs <- split(split_dfs$A, split_dfs$A$eco_type)
# 
# # Function to split a column by spaces and convert to list
# split_column_by_space <- function(dataframe, column_name) {
#   dataframe[[column_name]] <- strsplit(as.character(dataframe[[column_name]]), " ")
#   return(dataframe)
# }
# 
# # Apply the function to each dataframe in the list for the column 'eco_example'
# split_dfs <- lapply(split_dfs, split_column_by_space, column_name = "eco_example")
# 
# # Print the modified dataframe for a specific eco_type (e.g., "A")
# print(split_dfs$A$eco_example)
# 
# #-----
# split_dfs <- split(opennings_ds, opennings_ds$eco_type)
# 
# # Function to split a column by spaces and convert to list
# split_column_by_space <- function(dataframe, column_name) {
#   dataframe[[column_name]] <- strsplit(as.character(dataframe[[column_name]]), " ")
#   return(dataframe)
# }
# 
# # Apply the function to each dataframe in the list for the column 'eco_example'
# split_dfs <- lapply(split_dfs, split_column_by_space, column_name = "eco_example")
# 
# # Function to remove elements that consist of only numbers
# remove_number_elements <- function(dataframe, column_name) {
#   dataframe[[column_name]] <- lapply(dataframe[[column_name]], function(lst) {
#     lst <- lst[!grepl("^[0-9]+$", lst)]
#     return(lst)
#   })
#   return(dataframe)
# }
# 
# # Apply the function to each dataframe in the list for the column 'eco_example'
# split_dfs <- lapply(split_dfs, remove_number_elements, column_name = "eco_example")
# 
# # Print the modified dataframe for a specific eco_type (e.g., "A")
# print(split_dfs$A$eco_example)
# split_dfs$A$eco_example
# split_dfs$B$eco_example
# 
# 
# #--------------
# 
# split_dfs <- lapply(split_dfs, remove_number_elements, column_name = "eco_example")
# 
# #For class A
# # Flatten the list of elements into a single vector
# all_elements_a <- unlist(split_dfs$A$eco_example)
# 
# # Create a frequency table
# frequency_table_a <- table(all_elements_a)
# 
# # Print the frequency table
# View(frequency_table_a)
# 
# #For class B
# all_elements_b <- unlist(split_dfs$B$eco_example)
# 
# # Create a frequency table
# frequency_table_b <- table(all_elements_b)
# 
# # Print the frequency table
# View(frequency_table_b)
# 
# #For class c
# all_elements_c <- unlist(split_dfs$C$eco_example)
# 
# # Create a frequency table
# frequency_table_c <- table(all_elements_c)
# 
# # Print the frequency table
# View(frequency_table_)
# 
# #For class d
# all_elements_d <- unlist(split_dfs$D$eco_example)
# 
# # Create a frequency table
# frequency_table_d <- table(all_elements_d)
# 
# # Print the frequency table
# View(frequency_table_d)
# 
# #For class e
# all_elements_e <- unlist(split_dfs$E$eco_example)
# 
# # Create a frequency table
# frequency_table_e <- table(all_elements_e)
# 
# # Print the frequency table
# View(frequency_table_e)
# 
# View(opennings)

#-----------------------
openings_ds <- read.csv('eco_codes.csv', header = TRUE, stringsAsFactors = FALSE)
openings_ds <- openings_ds[-1, ]
split_dfs <- split(openings_ds, openings_ds$eco_type)
split_column_by_space <- function(dataframe, column_name) {
  dataframe[[column_name]] <- strsplit(as.character(dataframe[[column_name]]), " ")
  return(dataframe)
}
split_dfs <- lapply(split_dfs, split_column_by_space, column_name = "eco_example")
remove_number_elements <- function(dataframe, column_name) {
  dataframe[[column_name]] <- lapply(dataframe[[column_name]], function(lst) {
    lst <- lst[!grepl("^[0-9]+$", lst)]
    return(lst)
  })
  return(dataframe)
}
split_dfs <- lapply(split_dfs, remove_number_elements, column_name = "eco_example")
differentiate_castling_moves <- function(moves_list) {
  differentiated_moves <- unlist(lapply(seq_along(moves_list), function(i) {
    move <- moves_list[[i]]
    if (move %in% c("O-O", "O-O-O")) {
      if (i %% 2 == 1) {
        return(paste0(move, "(w)"))
      } else {
        return(paste0(move, "(b)"))
      }
    } else {
      return(move)
    }
  }))
  return(differentiated_moves)
}
create_frequency_table <- function(dataframe, column_name) {
  all_elements <- unlist(lapply(dataframe[[column_name]], differentiate_castling_moves))
  return(table(all_elements))
}
frequency_tables <- lapply(split_dfs, create_frequency_table, column_name = "eco_example")
frequency_table_a <- frequency_tables$A
frequency_table_b <- frequency_tables$B
frequency_table_c <- frequency_tables$C
frequency_table_d <- frequency_tables$D
frequency_table_e <- frequency_tables$E

process_opening_data <- function(df) {
  extract_square <- function(move) {
    move <- gsub("[+#=QRBNx]*", "", move)
    if (move == "O-O(w)") {
      return(c("g1", "f1"))
    } else if (move == "O-O(b)") {
      return(c("g8", "f8"))
    } else if (move == "O-O-O(w)") {
      return(c("c1", "d1"))
    } else if (move == "O-O-O(b)") {
      return(c("c8", "d8"))
    } else {
      final_square <- substr(move, nchar(move) - 1, nchar(move))
      if (nchar(final_square) == 2 && grepl("^[a-h][1-8]$", final_square)) {
        return(final_square)
      } else {
        return(NA)
      }
    }
  }
  move_list <- names(df)
  frequencies <- as.numeric(df)
  squares_list <- unlist(lapply(move_list, extract_square))
  frequencies_list <- rep(frequencies, sapply(move_list, function(x) length(extract_square(x))))
  valid_indices <- !is.na(squares_list)
  squares <- squares_list[valid_indices]
  frequencies <- frequencies_list[valid_indices]
  square_freq <- data.frame(square = squares, Freq = frequencies)
  agg_df <- aggregate(Freq ~ square, data = square_freq, sum)
  max_square <- agg_df[which.max(agg_df$Freq), ]
  list(agg_df = agg_df, max_square = max_square)
}

plot_heatmap <- function(agg_df, title_suffix) {
  chessboard <- matrix(0, nrow = 8, ncol = 8)
  rownames(chessboard) <- 1:8
  colnames(chessboard) <- letters[1:8]
  for (i in 1:nrow(agg_df)) {
    square <- agg_df$square[i]
    row <- which(rownames(chessboard) == substr(square, 2, 2))
    col <- which(colnames(chessboard) == substr(square, 1, 1))
    chessboard[row, col] <- agg_df$Freq[i]
  }
  melted_chessboard <- melt(chessboard)
  melted_chessboard$label <- ifelse(melted_chessboard$value > 0, melted_chessboard$value, "")
  melted_chessboard$text_color <- ifelse(melted_chessboard$value >= 60, "black", "white")
  ggplot(data = melted_chessboard, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = label, color = text_color), size = 3) +
    scale_color_identity() +
    scale_fill_gradientn(colors = c("black", "purple", "yellow")) +
    scale_x_discrete(breaks = letters[1:8], labels = letters[1:8]) +
    scale_y_continuous(breaks = 1:8, labels = 1:8) +
    coord_fixed() +
    labs(title = paste("Heatmap for", title_suffix),
         x = "Columns", y = "Rows", fill = "Frequency") +
    theme_minimal(base_size = 15) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          legend.background = element_rect(fill = "black"),
          legend.key = element_rect(fill = "black"),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"))
}

data_frames <- list(
  list(df = frequency_table_a, title = "Opening A"),
  list(df = frequency_table_b, title = "Opening B"),
  list(df = frequency_table_c, title = "Opening C"),
  list(df = frequency_table_d, title = "Opening D"),
  list(df = frequency_table_e, title = "Opening E")
)

max_squares <- list()

# Process data and generate individual heatmaps
for (data in data_frames) {
  processed <- process_opening_data(data$df)
  max_squares[[data$title]] <- processed$max_square
  heatmap_plot <- plot_heatmap(processed$agg_df, data$title)
  print(heatmap_plot)
}

# Create the combined max heatmap
combined_chessboard <- matrix(0, nrow = 8, ncol = 8)
rownames(combined_chessboard) <- 1:8
colnames(combined_chessboard) <- letters[1:8]
for (max_square in max_squares) {
  row <- which(rownames(combined_chessboard) == substr(max_square$square, 2, 2))
  col <- which(colnames(combined_chessboard) == substr(max_square$square, 1, 1))
  combined_chessboard[row, col] <- max_square$Freq
}

# Create a data frame for the combined heatmap
combined_data <- data.frame(
  square = rep(NA, sum(combined_chessboard != 0)),
  Freq = rep(NA, sum(combined_chessboard != 0))
)

index <- 1
for (i in 1:8) {
  for (j in 1:8) {
    if (combined_chessboard[i, j] != 0) {
      combined_data$square[index] <- paste0(colnames(combined_chessboard)[j], rownames(combined_chessboard)[i])
      combined_data$Freq[index] <- combined_chessboard[i, j]
      index <- index + 1
    }
  }
}

# Plot the combined max heatmap
combined_heatmap_plot <- plot_heatmap(combined_data, "Most Competitive squares")
print(combined_heatmap_plot)

#----------------------------------------------------------------------------------------
#Edges
edges <- read.csv('Edges.csv')
View(edges)
View(ds_chess)
write.csv(ds_chess, "/Users/user/Desktop/proj_folder/Prep_ds.csv", row.names = FALSE)
ds_chess <- ds_chess %>%
  mutate(result = case_when(
    result == "1-0" ~ "white",
    result == "0-1" ~ "black",
    result == "1/2-1/2" ~ "draw",
    TRUE ~ result
  ))

filtered_ds_chess <- ds_chess %>%
  filter(result != "draw")

View(filtered_ds_chess)
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create a new dataframe with year and the mode of result
mode_result_per_year <- filtered_ds_chess %>%
  group_by(year) %>%
  summarise(mode_result = calculate_mode(result))

# Print the new dataframe
View(ds_chess)

result_counts <- ds_chess %>%
  filter(result %in% c("white", "black", "draw")) %>%
  count(result)

# Create a pie chart
ggplot(result_counts, aes(x = "", y = n, fill = result)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Game Results", x = "", y = "") +
  theme_void()


