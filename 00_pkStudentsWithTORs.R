
# Solution: ----
# use the MASTER SCHEDULE to ID all *teachers*... then flag for PK/KG split type... and then use the SPA-ROSTER to ID the unique kids per grade level teacher



# CHECK against HOMEROOM COUNTS FOR CAPPING report ----
# C:\Users\lprice5\Downloads\homeroom_counts_for_capping_2023_0913.csv



## From Cognos SPA : By-Kid Enrollment and Class Schedule -----
# Rename file slightly before moving it to the server
# https://a4epwr.houstonisd.org/cognos1117/bi/?pathRef=.public_folders%2FShared%2BFolder%2FLPRICE5%2FPanorama%2FSPA_By-Kid%2BEnrollment%2BAttendance%2Band%2BClass%2BSchedule&action=edit

file.copy("C:/Users/lprice5/Downloads/SPA_By-Kid Class Schedule_2023_0912.csv", "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/1 Source Files/SPA_By-Kid Enrollment Attendance and Class Schedule_2023_0912.csv", overwrite = FALSE, copy.date = TRUE)

# Remember to recode TEMP20901 into 777720901
