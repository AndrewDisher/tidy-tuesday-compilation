# ----------------
# --- Packages ---
# ----------------

import pandas as pd
import geopandas as gpd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import statistics as stat

from siuba import _, filter, mutate, select


# ------------------
# --- Fetch Data ---
# ------------------

# First download data to local directory
munros = pd.read_csv(
'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv',
encoding='unicode_escape'
)

# --------------------------
# --- Data Visualization ---
# --------------------------

# Question to Answer: What is the average distance between a Munro and its nearest Munro Top?

# Convert pandas df to geopandas df
munros_geo = gpd.GeoDataFrame(
    data=munros,
    crs=27700,
    geometry=gpd.points_from_xy(munros.xcoord, munros.ycoord)
)

# Filter the geo data frame to obtain only the munros
only_munros = (
    munros_geo >>
        filter(_['2021'] == "Munro") >>
        select(_.Name, _.Height_m, _.Height_ft, _['2021'], _.geometry)
)

# Filter the geo data frame to obtain only the munro tops
only_munro_tops = (
    munros_geo >>
        filter(_['2021'] == "Munro Top") >>
        select(_.Name, _.Height_m, _.Height_ft, _['2021'], _.geometry)
)

# Find nearest munro/munro top pairings
closest_points = only_munros.geometry.apply(
    lambda x:  only_munro_tops.loc[only_munro_tops.geometry.distance(x).idxmin()]
)

only_munros = (
    only_munros >>
        mutate(closest_name = closest_points['Name']) >>
        mutate(closest_geometry = closest_points['geometry']) >>
        mutate(closest_distance = _.apply(
            lambda row: row['geometry'].distance(row['closest_geometry']) / 1000,
            axis=1
        ))
)

# Average distance between a munro and its nearest munro top
dist_mean = sum(only_munros.closest_distance) / len(only_munros)
dist_median = stat.median(only_munros.closest_distance)

# Define base figure and axes
f, ax1 = plt.subplots(figsize=(7, 5))

# Set x axis limits
ax1.set_xlim(0, max(only_munros["closest_distance"]) + 5)

# Change axis labels
ax1.set_xlabel("Distance between Munro and Nearest Munro Top (km)")
ax1.set_ylabel("Count")

sns.histplot(
    data=only_munros,
    x="closest_distance",
    linewidth=.5,
    color="#34adfa",
    ax=ax1
)

# Create twin axis
ax2 = ax1.twinx()

sns.kdeplot(
    data=only_munros,
    x="closest_distance",
    color="#FA8134",
    ax=ax2
)

# Add vertical lines for mean and median
ax1.axvline(dist_mean, color="black", linestyle="--", label="Mean")
ax1.axvline(dist_median, color="black", linestyle=":", label="Median")
ax1.legend()

# Save the graph to static folder
plt.savefig("projects/2025/August/scottish_munros/static/munro_top_distribution.png")
