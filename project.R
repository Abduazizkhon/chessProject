# Installing all required packages
install.packages("igraph")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages('reshape2')
install.packages('maps')
install.packages('grid')
install.packages("here")

# Enable all libraries
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(maps)
library(grid)
library(here)

# Construct the path to the dataset 
ds_chess <- read.csv(file.path("datasets", "chess_wc_history_game_info.csv"))

#View(ds_chess)

# Obviously, raw data

# Get the number of rows
num_rows <- nrow(ds_chess)

# View the number of rows
# num_rows

# Extracting surnames
extract_surname <- function(name) {
  strsplit(name, ",")[[1]][1]
}

# Apply the function to the "white" and "black" columns (sides)
ds_chess$white <- sapply(ds_chess$white, extract_surname)
ds_chess$black <- sapply(ds_chess$black, extract_surname)

# View modified dataset
#View(ds_chess)

# Extracting unique players
all_players <- unique(c(ds_chess$white, ds_chess$black ))

# View unique players
#all_players

# Prepare "Edges.csv" 
projected_df <- ds_chess[, c("white", "black")]
colnames(projected_df) <- c("Source", "Target")

# View the new dataframe
#View(projected_df)

#---------------------

# Sort each row alphabetically
sorted_df <- t(apply(projected_df, 1, sort))

# Convert matrix to dataframe
sorted_df <- as.data.frame(sorted_df, stringsAsFactors = FALSE)
colnames(sorted_df) <- c("Source", "Target")

# Remove duplicates
unique_sorted_df <- unique(sorted_df)

# View(unique_sorted_df)

# Save Edges.csv in datasets folder
write.csv(unique_sorted_df, file.path("datasets", "Edges.csv"), row.names = FALSE)

# Prepare Nodes.csv
nodes <- data.frame(id = all_players)

#View(nodes)

#-------------------------

# Initialize an empty list for surname counts
surname_counts <- list()

# Count appearances of each player in Source and Target columns
for (surname in all_players) {
  count <- sum(unique_sorted_df$Source == surname) + sum(unique_sorted_df$Target == surname)
  surname_counts[[surname]] <- count
}

# Create a dataframe of surname counts
surname_counts_df <- data.frame(
  Surname = names(surname_counts),
  Appearances = as.numeric(unlist(surname_counts))
)

# Assign the counts dataframe to 'all_players_df'
all_players_df <- surname_counts_df

# Read Grandmaster statistics and extract surnames
gm_players_statistics <- read.csv(file.path("datasets", "GM_players_statistics.csv"))
gm_players_statistics$surname <- sapply(strsplit(as.character(gm_players_statistics$name), " "), function(x) x[length(x)])
gm_players_statistics$surname <- as.character(gm_players_statistics$surname)

# Create a lookup table for surnames and countries
additional_surname_country_lookup <- gm_players_statistics %>%
  select(surname, country) %>%
  distinct(surname, .keep_all = TRUE)

# Join surname counts with country data and clean columns
all_players_df <- all_players_df %>%
  left_join(additional_surname_country_lookup, by = c("Surname" = "surname")) %>%
  mutate(nationality = ifelse(is.na(country), "No Data", country)) %>%
  select(Surname, Appearances, nationality)

# Rename columns for clarity
colnames(all_players_df)[1] <- "Id"
colnames(all_players_df)[3] <- "Nationality"

# Save the updated nodes CSV 
write.csv(all_players_df, file.path("datasets", "Nodes.csv"), row.names = FALSE)

#------------------

# Use 'all_players_df' 
filtered_df <- all_players_df %>%
  filter(Appearances > 10) %>%
  arrange(desc(Appearances))

# Create a bar chart of player appearances by nationality
ggplot(filtered_df, aes(x = reorder(Id, -Appearances), y = Appearances, fill = Nationality)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Appearances by Players",
       x = "Id",
       y = "Appearances") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Saving barchart 
#ggsave(filename = file.path("images", "barchart.png"), width = 8, height = 6, dpi = 300, bg = "white")

#------------------------------------

# Extract the first character of the ECO code
ds_chess$eco <- substr(ds_chess$eco, 1, 1)

# Extract year from file name and rename column
ds_chess$file_name <- substr(ds_chess$file_name, nchar(ds_chess$file_name) - 7, nchar(ds_chess$file_name) - 4)
ds_chess <- ds_chess %>%
  rename(year = file_name)

# Print the updated chess dataset and view it
#print(head(ds_chess))
#View(ds_chess)

#------------------------------
#ECO popularity

# Count occurrences of each ECO code by year
eco_year_counts <- ds_chess %>%
  group_by(year, eco) %>%
  summarise(count = n()) %>%
  ungroup()

# Keep only the most popular ECO code per year
most_popular_eco <- eco_year_counts %>%
  group_by(year) %>%
  filter(count == max(count)) %>%
  ungroup()

# Plot the most popular ECO codes over the years
ggplot(most_popular_eco, aes(x = as.factor(year), y = count, fill = eco)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Popular ECO Code by Year", x = "Year", y = "Count", fill = "ECO Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

# Save ECO chart to images
#ggsave(filename = file.path("images", "ECO_Popularity.png"), width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

# Read world champions CSV from datasets folder
df <- read.csv(file.path("datasets", "worldchampions.csv"))

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

# Save world map plot to images
#ggsave(filename = file.path("images", "World_Champions_Map.png"), width = 10, height = 7, dpi = 300, bg = "white")

#-------------------------------------------------------------------------------

# Read the ECO codes dataset from the datasets folder
openings_ds <- read.csv(file.path("datasets", "eco_codes.csv"), header = TRUE, stringsAsFactors = FALSE)

# Remove the first row (header or irrelevant row)
openings_ds <- openings_ds[-1, ]

# Split dataset by ECO type
split_dfs <- split(openings_ds, openings_ds$eco_type)

# Function to split a column's values by space
split_column_by_space <- function(dataframe, column_name) {
  dataframe[[column_name]] <- strsplit(as.character(dataframe[[column_name]]), " ")
  return(dataframe)
}

# Apply splitting function to all split datasets
split_dfs <- lapply(split_dfs, split_column_by_space, column_name = "eco_example")

# Function to remove numeric elements from a list
remove_number_elements <- function(dataframe, column_name) {
  dataframe[[column_name]] <- lapply(dataframe[[column_name]], function(lst) {
    lst <- lst[!grepl("^[0-9]+$", lst)]
    return(lst)
  })
  return(dataframe)
}

# Apply number removal function to all split datasets
split_dfs <- lapply(split_dfs, remove_number_elements, column_name = "eco_example")

# Function to differentiate castling moves for white/black
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

# Function to create a frequency table of moves
create_frequency_table <- function(dataframe, column_name) {
  all_elements <- unlist(lapply(dataframe[[column_name]], differentiate_castling_moves))
  return(table(all_elements))
}

# Generate frequency tables for each ECO type
frequency_tables <- lapply(split_dfs, create_frequency_table, column_name = "eco_example")

# Extract frequency tables for each opening type
frequency_table_a <- frequency_tables$A
frequency_table_b <- frequency_tables$B
frequency_table_c <- frequency_tables$C
frequency_table_d <- frequency_tables$D
frequency_table_e <- frequency_tables$E

# Function to process opening data and find frequent squares
process_opening_data <- function(df) {
  extract_square <- function(move) {
    move <- gsub("[+#=QRBNx]*", "", move)
    if (move == "O-O(w)") return(c("g1", "f1"))
    else if (move == "O-O(b)") return(c("g8", "f8"))
    else if (move == "O-O-O(w)") return(c("c1", "d1"))
    else if (move == "O-O-O(b)") return(c("c8", "d8"))
    else {
      final_square <- substr(move, nchar(move) - 1, nchar(move))
      if (nchar(final_square) == 2 && grepl("^[a-h][1-8]$", final_square)) return(final_square)
      else return(NA)
    }
  }

  # Extract move list and calculate frequencies
  move_list <- names(df)
  frequencies <- as.numeric(df)
  squares_list <- unlist(lapply(move_list, extract_square))
  frequencies_list <- rep(frequencies, sapply(move_list, function(x) length(extract_square(x))))
  
  # Filter out invalid squares
  valid_indices <- !is.na(squares_list)
  squares <- squares_list[valid_indices]
  frequencies <- frequencies_list[valid_indices]

  # Aggregate frequencies by square
  square_freq <- data.frame(square = squares, Freq = frequencies)
  agg_df <- aggregate(Freq ~ square, data = square_freq, sum)
  max_square <- agg_df[which.max(agg_df$Freq), ]
  
  list(agg_df = agg_df, max_square = max_square)
}

# Function to plot a heatmap of move frequencies
plot_heatmap <- function(agg_df, title_suffix) {
  chessboard <- matrix(0, nrow = 8, ncol = 8)
  rownames(chessboard) <- 1:8
  colnames(chessboard) <- letters[1:8]

  # Populate the chessboard matrix with frequencies
  for (i in 1:nrow(agg_df)) {
    square <- agg_df$square[i]
    row <- which(rownames(chessboard) == substr(square, 2, 2))
    col <- which(colnames(chessboard) == substr(square, 1, 1))
    chessboard[row, col] <- agg_df$Freq[i]
  }

  # Create a melted dataframe for plotting
  melted_chessboard <- melt(chessboard)
  melted_chessboard$label <- ifelse(melted_chessboard$value > 0, melted_chessboard$value, "")
  melted_chessboard$text_color <- ifelse(melted_chessboard$value >= 60, "black", "white")

  # Generate the heatmap plot
  ggplot(data = melted_chessboard, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = label, color = text_color), size = 3) +
    scale_color_identity() +
    scale_fill_gradientn(colors = c("black", "purple", "yellow")) +
    coord_fixed() +
    labs(title = paste("Heatmap for", title_suffix), x = "Columns", y = "Rows", fill = "Frequency") +
    theme_minimal(base_size = 15)
}

# Process data for each ECO type and generate heatmaps
data_frames <- list(
  list(df = frequency_table_a, title = "Opening A"),
  list(df = frequency_table_b, title = "Opening B"),
  list(df = frequency_table_c, title = "Opening C"),
  list(df = frequency_table_d, title = "Opening D"),
  list(df = frequency_table_e, title = "Opening E")
)

# Store max squares for combined heatmap
max_squares <- list()

# Loop through each data frame and save heatmaps 
for (data in data_frames) {
  processed <- process_opening_data(data$df)
  heatmap_plot <- plot_heatmap(processed$agg_df, data$title)
  file_name <- paste0("Heatmap_", gsub(" ", "_", data$title), ".png")
  #ggsave(filename = file.path("images", file_name), plot = heatmap_plot, width = 8, height = 6, dpi = 300, bg = "white")
}

# Create combined heatmap data and plot
combined_chessboard <- matrix(0, nrow = 8, ncol = 8)
rownames(combined_chessboard) <- 1:8
colnames(combined_chessboard) <- letters[1:8]

for (max_square in max_squares) {
  row <- which(rownames(combined_chessboard) == substr(max_square$square, 2, 2))
  col <- which(colnames(combined_chessboard) == substr(max_square$square, 1, 1))
  combined_chessboard[row, col] <- max_square$Freq
}

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

# Display individual heatmaps for each ECO opening type (A to E with combined heatmap)
#heatmap_a <- plot_heatmap(process_opening_data(frequency_table_a)$agg_df, "Opening A")
#heatmap_b <- plot_heatmap(process_opening_data(frequency_table_b)$agg_df, "Opening B")
#heatmap_c <- plot_heatmap(process_opening_data(frequency_table_c)$agg_df, "Opening C")
#heatmap_d <- plot_heatmap(process_opening_data(frequency_table_d)$agg_df, "Opening D")
#heatmap_e <- plot_heatmap(process_opening_data(frequency_table_e)$agg_df, "Opening E")
#combined_heatmap_plot <- plot_heatmap(combined_data, "Most Competitive squares")
#print(heatmap_a)
#print(heatmap_b)
#print(heatmap_c)
#print(heatmap_d)
#print(heatmap_e)
#print(combined_heatmap_plot)

# Save the combined heatmap plot to the images folder
#ggsave(filename = file.path("images", "Combined_Heatmap.png"), plot = combined_heatmap_plot, width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------------------------------------------------------------
# Load the Edges CSV file from the datasets folder
edges <- read.csv(file.path("datasets", "Edges.csv"))
ds_chess <- read.csv(file.path("datasets", "Prep_ds.csv"))

# View the loaded dataframes
#View(edges)
#View(ds_chess)

# Update the 'result' column with more descriptive labels
ds_chess <- ds_chess %>%
  mutate(result = case_when(
    result == "1-0" ~ "white",     # White wins
    result == "0-1" ~ "black",     # Black wins
    result == "1/2-1/2" ~ "draw",  # Draw
    TRUE ~ result                  # Keep other values as they are
  ))

# Filter the dataset to exclude games that ended in a draw
filtered_ds_chess <- ds_chess %>%
  filter(result != "draw")

# View the filtered dataframe
#View(filtered_ds_chess)

# Define a function to calculate the mode (most frequent value)
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create a new dataframe with the mode of game results per year
mode_result_per_year <- filtered_ds_chess %>%
  group_by(year) %>%
  summarise(mode_result = calculate_mode(result))

# View the updated 'ds_chess' dataframe
#View(ds_chess)

# Count the number of occurrences of each result type (white, black, draw)
result_counts <- ds_chess %>%
  filter(result %in% c("white", "black", "draw")) %>%
  count(result)

# Create a pie chart showing the distribution of game results
ggplot(result_counts, aes(x = "", y = n, fill = result)) +
  geom_bar(width = 1, stat = "identity") +  # Create a bar chart
  coord_polar("y", start = 0) +             # Convert to a pie chart using polar coordinates
  labs(title = "Distribution of Game Results", x = "", y = "") +
  theme_void()                              # Remove background and axis elements

# Save the pie chart to the images folder
#ggsave(filename = file.path("images", "Game_Results_Distribution.png"), width = 8, height = 6, dpi = 300, bg = "white")
