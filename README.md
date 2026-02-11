**Shearwater dive computer log convert to SRT sutitle file for video.**

Get Shearwater dive computer log file from Shearwater Cloud, export as CSV file.

Run R script, input CSV file name, SRT file name to be created, and timestamp shift time (positive number for seconds to shift forward, negative number for seconds to shift backwards). A SRT file will be created including depth, TTS, and temperature.

Use shift_srt_timestamps.R script to further shift timestamps if required. Input SRT file name and new SRT file name to be created. Input positive number for seconds to shift forward, negative number for seconds to shift backwards.
