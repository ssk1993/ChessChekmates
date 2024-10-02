# Libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpattern)  # For patterned tiles
library(dplyr)
library(patchwork)  # For combining plots


#getting the data 
chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv',show_col_types = FALSE)

#keeping the games where there was checkmate 
chess <- chess[chess$victory_status == "mate" & !is.na(chess$victory_status), ]


#getting the last movefrom the chess moves 
chess <- chess %>%
  mutate(last = sapply(strsplit(moves, " "), function(x) tail(x, 1)))

#keeping the last move with only position onthe chess board (removing other parts in move code)
chess <- chess %>%
  mutate(lastmove = gsub(".*([a-h][1-8])#", "\\1", last))


chess$lastmove <- str_extract(chess$lastmove, "^[a-zA-Z]+\\d")


# getting the piece name which gave the checkmate , and creating coliumn with names of pieces
chess$piece <- sapply(chess$last, function(move) {
  first_char <- substr(move, 1, 1)
  
  if (first_char == toupper(first_char)) {
    switch(first_char,
           "B" = "Bishop",
           "Q" = "Queen",
           "R" = "Rook",
           "N" = "Knight",
           "K" = "King",
           "Pawn")  # Default case if the capital letter doesn't match
  } else {
    "Pawn"
  }
})


# Function to create a single heatmap
create_heatmap <- function(data, title) {
  # Count occurrences of each chess position in the `lastmove` column
  counts <- data %>%
    group_by(lastmove) %>%
    summarise(count = n())
  
  # Create chessboard layout
  chessboard <- expand.grid(x = 1:8, y = 1:8) %>%
    mutate(
      label = paste0(letters[x], y),  # Assign positions like a1, b2, etc.
      is_striped = (x + y) %% 2 == 0  # Determine which squares should have the pattern
    )
  
  # Merge the counts with the chessboard layout
  chessboard_counts <- chessboard %>%
    left_join(counts, by = c("label" = "lastmove")) %>%
    mutate(count = ifelse(is.na(count), 0, count))  # Set missing counts to 0
  
  # Plot the chessboard heatmap
  ggplot(chessboard_counts, aes(x, y)) +
    # Add striped squares with diagonal pattern
    geom_tile_pattern(data = subset(chessboard_counts, is_striped), 
                      fill = "white",                # White background fill
                      pattern = "stripe",            # Stripe pattern
                      pattern_fill = "white",       # Grey stripes
                      pattern_angle = 35,            # Diagonal stripes
                      pattern_density = 0.7,         # Adjust stripe density
                      pattern_spacing = 0.02,        # Space between stripes
                      color = "grey50") +            # Box border color
    
    # Add white squares for non-striped areas
    geom_tile(data = subset(chessboard_counts, !is_striped), 
              fill = "white", color = "grey50") +
    
    # Add faint lines between rows and columns
    geom_hline(yintercept = seq(0.5, 8.5, by = 1), color = "grey75", size = 0.5) +
    geom_vline(xintercept = seq(0.5, 8.5, by = 1), color = "grey75", size = 0.5) +
    
    # Heatmap layer for counts with updated color scale
    geom_tile(aes(fill = count), alpha = 0.7) +
    scale_fill_gradientn(colors = c("white", "lightgreen", "green", "darkgreen"), na.value = "white") +
    
    # Set axis labels to a-h for x and 1-8 for y
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0, 0)) +
    scale_y_continuous(breaks = 1:8, labels = 1:8, expand = c(0, 0)) +
    
    # Adjust aesthetics
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(b = 10)),  # Increased bottom margin for title
      legend.position = "right",
      axis.text = element_text(size = 6),
      plot.margin = margin(10, 10, 10, 10)  # Increased overall plot margins
    ) +
    coord_fixed() +
    labs(title = title, fill = "Count")
}
# Create a list to store all plots
plots <- list()


# Create the bar plot with count of moves 
plots[["barchart"]] <- ggplot(chess, aes(x = piece)) +
  geom_bar(fill = "#ccffcc", width = 0.7, color = "black") +
  geom_text(stat = 'count', 
            aes(label = ..count..), 
            vjust = -0.5, 
            size = 4, 
            fontface = "bold") +
  labs(x = "",
       y = "Count") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


# Create a plot for all pieces
plots[["All Pieces"]] <- create_heatmap(chess, "All Pieces")


# Create plots for each piece
for (piece_type in unique(chess$piece)) {
  piece_data <- chess %>% filter(piece == piece_type)
  plots[[piece_type]] <- create_heatmap(piece_data, piece_type)
}

# Combine all plots using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) +
  plot_annotation(
    title = "The Final Move: Anatomy of Checkmate",
    subtitle = "Squares where pieces delivered the decisive blow",
    theme = theme( plot.background = element_rect(fill = "lightyellow"),
                   plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, size = 18, margin = margin(b = 20))
    )
  )

# Save the combined plot
ggsave("chess_heatmap_subplots.png", combined_plot, width = 16, height = 16)

# Display the combined plot
print(combined_plot)


