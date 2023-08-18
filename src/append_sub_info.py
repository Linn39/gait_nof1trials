import json
import os
import pandas as pd

# load the json file
with open("path.json") as f:
    paths = json.load(f)

# load subject info from csv
sub_info = pd.read_csv(
    os.path.join(paths["data"], "processed", "features", "subject_info.csv")
)

# keep only the colums "age","sex","height(cm)","weight(kg)","leg_length(cm)" in sub_info
sub_info = sub_info[["sub", "age","sex","height(cm)","weight(kg)","leg_length(cm)"]]

# load strides info from csv
strides_df = pd.read_csv(
    os.path.join(paths["data"], "processed", "features", "df_all.csv")
)

# merge sub_info and strides_df
df = pd.merge(
    sub_info, strides_df, how="inner", left_on="sub", right_on="sub"
)

# save the data
df.to_csv(
    os.path.join(paths["data"], "processed", "features", "df_all_strides_test.csv"),
    index=False,
)