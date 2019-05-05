# Descriptive Statistics




### df.cross
df.cross <- df_cross


# df.cross - numeric variables
des.cross <- df.cross[ ,2]

des.cross$Deal_Value <- df.cross$`Deal_Value_$_(Face)`
des.cross$Spread <- df.cross$`Spread_To_Benchmark/Discount_margin_%`

des.cross <- as.data.frame(des.cross[, c(2,3)])
stargazer(des.cross, type="html", out="table_cross_01.htm")


# df.cross - categorical variables
summary(df.cross)





# df.gov

# df.culture

# df.trust




# Sources -----------------------------------------------------------------

# Stargazer output types
# https://www.princeton.edu/~otorres/NiceOutputR.pdf



