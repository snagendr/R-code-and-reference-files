options(warn=-1) # supress warnings in calling RMD doc

wd2 <- "C:/Users/shekhar.nagendra/Documents/Johns Hopkins/Data"

library(dplyr,warn.conflicts=FALSE)
library(tidyr,warn.conflicts=FALSE)

fn <- paste0(wd2, "/Capability Assessment Template 3 0 v1.csv")

df <- read.csv(fn)

# Now Read in only the "Category", "Capability", "As-is", and "Vision" columns
# And convert to dplyr tbl_df dataframe for efficiency

df1 <- tbl_df(df[,9:12])  

df2 <- mutate(df1,Gap=(Vision - As.Is))  # Add a new column called "Gap" 

names(df2) <- c("Category","Capability","Asis","Vision","Gap")  # Better looking column names

# Now we will create the top level overall summary graph (which is by "Category" only)

df_by_category <- group_by(df2,Category)
df_s1 <- summarize(df_by_category,mean(Asis),mean(Vision),mean(Gap))
names(df_s1) <- c("Category","Avg_asis","Avg_vision","Avg_gap")

# Round the numbers to only 1 digit after decimal point 
# This is for better display, & no need for greater precision

df_s1 <- mutate(df_s1, Avg_asis=round(Avg_asis,digits=1))
df_s1 <- mutate(df_s1, Avg_vision=round(Avg_vision,digits=1))
df_s1 <- mutate(df_s1, Avg_gap=round(Avg_gap,digits=1))

# Sort in descending order of Gap to show highest gap first (leftmost bar)

df_s1 <- arrange(df_s1,desc(Avg_gap))

# Now transpose the matrix and create vector to prepare the data for correct bat graph display
# Convert df_s1 to a matrix and transpose it 

m1 <- as.matrix(df_s1[,2:4])
m2 <- t(m1)

# Now convert m2 to a vector
# We will need m2 and v_m2 for bar plot

v_m2 <- as.vector(m2)

bp <- barplot(m2,
              ylim = c(0,5),
              beside = TRUE,
              main = "Capability Assessment: Overall",
              xlab = "Category", names.arg = df_s1$Category,
              ylab = "Rating",
              col = c("lightblue","lightgreen","red"),
              args.legend = list(title = "Assessment", x = "top",horiz=TRUE),
              legend.text = c("As-is","Vision","Gap")
             )
text(bp, 0,v_m2,cex=1,pos=3)

#bp


