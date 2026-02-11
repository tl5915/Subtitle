shearwater_srt_shift <- function(input_csv, output_srt, shift_seconds) {
  library(data.table)
  library(stringr)
  
  # Convert CSV into SRT format
  df <- fread(input_csv, skip = 2)
  df <- df[, .(`Time (sec)`, Depth, `Time To Surface (min)`, `Water Temp`)]
  df[, `Time (sec)` := as.numeric(`Time (sec)`)]
  df[, Depth := round(as.numeric(Depth), 1)]
  df[, `Time To Surface (min)` := as.character(floor(as.numeric(`Time To Surface (min)`)))]
  df[, `Water Temp` := as.integer(round(as.numeric(`Water Temp`)))]
  
  format_timestamp <- function(seconds) {
    hrs <- sprintf("%02d", floor(seconds / 3600))
    mins <- sprintf("%02d", floor((seconds %% 3600) / 60))
    secs <- sprintf("%02d", floor(seconds %% 60))
    millis <- sprintf("%03d", (seconds %% 1) * 1000)
    return(paste0(hrs, ":", mins, ":", secs, ",", millis))
  }
  
  srt_content <- ""
  for (i in 1:nrow(df)) {
    if (i == 1) {
      start_time <- format_timestamp(0)
      end_time <- format_timestamp(5)
    } else {
      start_time <- format_timestamp(df$`Time (sec)`[i] - 5)
      end_time   <- format_timestamp(df$`Time (sec)`[i] + 5)
    }
    
    srt_content <- paste0(srt_content, i, "\n")
    srt_content <- paste0(srt_content, start_time, " --> ", end_time, "\n")
    srt_content <- paste0(srt_content, "Depth: ", df$Depth[i], "m\n")
    srt_content <- paste0(srt_content, "TTS: ", df$`Time To Surface (min)`[i], "min\n")
    srt_content <- paste0(srt_content, "Temp: ", df$`Water Temp`[i], "°C\n\n")
  }
  
  temp_srt <- paste0(tempfile(), ".srt")
  writeLines(srt_content, temp_srt)
  
  # Shift Timestamps
  srt_lines <- readLines(temp_srt, warn = FALSE)
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
  valid_indices <- which(adjusted_timestamps[,1] >= 0 & adjusted_timestamps[,2] >= 0)
  adjusted_timestamps <- adjusted_timestamps[valid_indices, , drop = FALSE]
  
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
  
  if (length(new_srt_lines) > 0) {
    writeLines(new_srt_lines, output_srt)
  } else {
    warning("No valid subtitles remaining after shift")
  }
}

# Example usage:
# source("Shearwater_Subtitle.R")
# shearwater_srt_shift("input.csv", "output.srt", 42)  # Shift timestamp forward by 42s
# shearwater_srt_shift("input.csv", "output.srt", -10) # Shift timestamp backward by 10s