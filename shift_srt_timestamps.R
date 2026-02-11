shift_srt_timestamps <- function(input_srt, output_srt, shift_seconds) {

  library(stringr)
  
  # Input SRT file
  srt_lines <- readLines(input_srt, warn = FALSE)
  
  # Shift timestamps
  timestamp_lines <- grep("-->", srt_lines, value = TRUE)
  
  parse_timestamp <- function(ts) {
    parts <- unlist(strsplit(ts, ":|,| --> "))
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    milliseconds <- as.numeric(parts[4])
    return(hours * 3600 + minutes * 60 + seconds + milliseconds / 1000)
  }
  
  timestamps <- t(sapply(timestamp_lines, function(line) {
    ts_parts <- unlist(strsplit(line, " --> "))
    c(parse_timestamp(ts_parts[1]), parse_timestamp(ts_parts[2]))
  }))
  
  adjusted_timestamps <- timestamps + shift_seconds
  
  # Remove negative timestamps
  valid_indices <- which(adjusted_timestamps[,1] >= 0 & adjusted_timestamps[,2] >= 0)
  adjusted_timestamps <- adjusted_timestamps[valid_indices, , drop = FALSE]
  
  # Format new timestamps
  format_timestamp <- function(time) {
    hours <- sprintf("%02d", floor(time / 3600))
    minutes <- sprintf("%02d", floor((time %% 3600) / 60))
    seconds <- sprintf("%02d", floor(time %% 60))
    milliseconds <- sprintf("%03d", round((time %% 1) * 1000))
    return(paste0(hours, ":", minutes, ":", seconds, ",", milliseconds))
  }
  
  new_srt_lines <- c()
  subtitle_counter <- 1
  j <- 1
  
  for (i in seq_along(valid_indices)) {
    new_srt_lines <- c(new_srt_lines, as.character(subtitle_counter))
    new_srt_lines <- c(new_srt_lines, paste0(format_timestamp(adjusted_timestamps[i,1]), " --> ", format_timestamp(adjusted_timestamps[i,2])))
    subtitle_counter <- subtitle_counter + 1
    
    start_index <- which(srt_lines == timestamp_lines[valid_indices[i]])[1]
    end_index <- ifelse(i < length(valid_indices), which(srt_lines == timestamp_lines[valid_indices[i + 1]])[1] - 1, length(srt_lines))
    subtitle_text <- srt_lines[(start_index + 1):end_index]
    subtitle_text <- subtitle_text[subtitle_text != ""]
    
    subtitle_text <- subtitle_text[!grepl("^[0-9]+$", subtitle_text)]
    
    new_srt_lines <- c(new_srt_lines, subtitle_text, "")
  }
  
  # Output SRT file
  if (length(new_srt_lines) > 0) {
    writeLines(new_srt_lines, output_srt)
  } else {
    warning("No valid value after shift!")
  }
}

# Example usage:
# source("shift_srt_timestamps.R")
# shift_srt_timestamps("input.srt", "output.srt", 42)  # Shift forward by 42 seconds
# shift_srt_timestamps("input.srt", "output.srt", -10) # Shift backward by 10 seconds