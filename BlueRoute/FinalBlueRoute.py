import os
import json
import pandas as pd
import requests
import time
import folium
from folium.plugins import MarkerCluster, HeatMap
from folium import Element
from branca.element import Template, MacroElement
import numpy as np

# --- STEP 1: Define Whale Species List ---
whale_species = {
    "Balaenoptera musculus": "Blue Whale",
    "Balaenoptera physalus": "Fin Whale",
    "Orcinus orca": "Orca",
    "Physeter macrocephalus": "Sperm Whale",
    "Megaptera novaeangliae": "Humpback Whale",
    "Eschrichtius robustus": "Gray Whale",
    "Monodon monoceros": "Narwhal",
    "Delphinapterus leucas": "Beluga Whale",
    # … add remaining species …
}

# --- STEP 2: Fetch & Clean Whale Sightings ---
def load_or_fetch_data(filename, fetch_fn, species_list, max_age=3153600000):
    if os.path.exists(filename):
        try:
            mtime = os.path.getmtime(filename)
            if time.time() - mtime < max_age:
                df = pd.read_csv(filename)
                if {'latitude', 'longitude'}.issubset(set(df.columns)):
                    return df
        except Exception:
            pass

    df = fetch_fn(list(species_list))
    try:
        df.to_csv(filename, index=False)
    except Exception:
        pass
    return df

def fetch_gbif_data(species_list):
    rows = []
    for latin in species_list:
        common = whale_species.get(latin, latin)
        offset = 0
        limit = 100
        max_pages = 5
        pages = 0
        while pages < max_pages:
            params = {
                'scientificName': latin,
                'hasCoordinate': 'true',
                'limit': limit,
                'offset': offset
            }
            try:
                resp = requests.get("https://api.gbif.org/v1/occurrence/search", params=params, timeout=60)
                if resp.status_code != 200:
                    break
                data = resp.json()
                results = data.get('results', [])
                if not results:
                    break
                for r in results:
                    lat = r.get('decimalLatitude')
                    lon = r.get('decimalLongitude')
                    date = r.get('eventDate') or r.get('year')
                    if lat is None or lon is None:
                        continue
                    rows.append({
                        'latitude': float(lat),
                        'longitude': float(lon),
                        'date': str(date),
                        'species': common,
                        'latin_name': latin,
                        'source': 'GBIF'
                    })
                if len(results) < limit:
                    break
                offset += limit
                pages += 1
            except Exception:
                break
    df = pd.DataFrame(rows)
    if not df.empty:
        df['latitude'] = df['latitude'].astype(float)
        df['longitude'] = df['longitude'].astype(float)
    return df

def fetch_obis_data(species_list):
    rows = []
    for latin in species_list:
        common = whale_species.get(latin, latin)
        offset = 0
        limit = 100
        max_pages = 5
        pages = 0
        while pages < max_pages:
            url = "https://api.obis.org/v3/occurrence"
            params = {
                'scientificName': latin,
                'limit': limit,
                'offset': offset
            }
            try:
                resp = requests.get(url, params=params, timeout=60)
                if resp.status_code != 200:
                    break
                data = resp.json()
                results = data.get('results', [])
                if not results:
                    break
                for r in results:
                    lat = r.get('decimalLatitude') or r.get('latitude')
                    lon = r.get('decimalLongitude') or r.get('longitude')
                    date = r.get('eventDate') or r.get('date')
                    if lat is None or lon is None:
                        continue
                    rows.append({
                        'latitude': float(lat),
                        'longitude': float(lon),
                        'date': str(date),
                        'species': common,
                        'latin_name': latin,
                        'source': 'OBIS'
                    })
                if len(results) < limit:
                    break
                offset += limit
                pages += 1
            except Exception:
                break
    df = pd.DataFrame(rows)
    if not df.empty:
        df['latitude'] = df['latitude'].astype(float)
        df['longitude'] = df['longitude'].astype(float)
    return df

# load sightings
df_gbif = load_or_fetch_data("gbif_whale_sightings.csv", fetch_gbif_data, whale_species.keys())
df_obis = load_or_fetch_data("obis_whale_sightings.csv", fetch_obis_data, whale_species.keys())

# combine + clean
if not df_gbif.empty or not df_obis.empty:
    df_combined = pd.concat([df_gbif, df_obis], ignore_index=True)
else:
    df_combined = pd.DataFrame(columns=['latitude','longitude','date','species','latin_name','source'])

if not df_combined.empty:
    df_combined = df_combined.dropna(subset=['latitude', 'longitude'])
    df_combined['latitude'] = df_combined['latitude'].astype(float)
    df_combined['longitude'] = df_combined['longitude'].astype(float)
    df_combined = df_combined.drop_duplicates(subset=['latitude', 'longitude', 'date', 'species'])

print(f"Whale sightings loaded: {len(df_combined)} rows")

# --- STEP 3: Prepare Species Colors (map legend) ---
species_colors = {
    "Blue Whale": "blue",
    "Fin Whale": "purple",
    "Orca": "black",
    "Sperm Whale": "darkslategray",
    "Humpback Whale": "green",
    "Gray Whale": "orange",
    "Narwhal": "pink",
    "Beluga Whale": "cyan",
    # … add remaining mapped species colors as needed …
}

# --- STEP 4: Initialize Map & Whale Layers ---
SAFE_PUBLIC_MODE = os.getenv("SAFE_PUBLIC_MODE", "1") != "0"

whale_map = folium.Map(location=[0, 0], zoom_start=2)

if SAFE_PUBLIC_MODE and not df_combined.empty:
    df_combined['latitude'] = df_combined['latitude'].round(1)
    df_combined['longitude'] = df_combined['longitude'].round(1)

# Plot sightings — one toggleable layer PER SPECIES, ON by default
if not df_combined.empty:
    df_markers = df_combined.sample(min(len(df_combined), 500), random_state=1)
    species_groups = {}
    for species_name in df_markers['species'].unique():
        species_groups[species_name] = folium.FeatureGroup(name=f"🐋 {species_name}", show=True)

    for _, row in df_markers.iterrows():
        col = species_colors.get(row['species'], 'gray')
        folium.CircleMarker(
            location=[row['latitude'], row['longitude']],
            radius=6, color=col, fill=True, fill_color=col, fill_opacity=0.8,
            popup=f"<b>Species:</b> {row['species']}"
        ).add_to(species_groups[row['species']])

    for group in species_groups.values():
        group.add_to(whale_map)

# Sightings heatmap — ON by default
if not df_combined.empty:
    sightings_heat = folium.FeatureGroup(name="🐋 Sightings Heatmap", show=True)
    HeatMap(
        df_combined[['latitude','longitude']].values.tolist(),
        radius=10, blur=5,
        gradient={"0.2": '#fee5d9', "0.4": '#fcae91', "0.6": '#fb6a4a', "0.8": '#cb181d'}
    ).add_to(sightings_heat)
    sightings_heat.add_to(whale_map)

# --- STEP 5: Legend + Caption ---
legend_html = """
<div style="
    position: fixed; bottom: 30px; left: 30px; z-index: 9999;
    background-color: white; padding: 10px 14px; border-radius: 6px;
    border: 2px solid grey; font-size: 13px; line-height: 1.5; max-width: 220px;">
    <b>Whale Species</b><br>
"""
for species, color in species_colors.items():
    legend_html += f'<i style="background:{color};width:10px;height:10px;display:inline-block;margin-right:6px;border-radius:50%;"></i>{species}<br>\n'
legend_html += """
</div>
"""
whale_map.get_root().html.add_child(Element(legend_html))

caption_html = """
<div style="
    position: fixed; top: 12px; left: 50%; transform: translateX(-50%); z-index: 9999;
    background-color: white; padding: 8px 16px; border-radius: 6px;
    border: 2px solid grey; font-size: 14px; text-align: center; max-width: 520px;">
    <b>Global Whale Sightings</b><br>
    <span style="font-size:12px;">Colored dots = whale species sightings &nbsp;|&nbsp;
    Heatmap layer shows overall sighting density</span>
</div>
"""
whale_map.get_root().html.add_child(Element(caption_html))

# --- STEP 6: Layer Control & Save ---
folium.LayerControl(collapsed=False).add_to(whale_map)
whale_map.save("whale_sightings_map.html")
print("Map saved to whale_sightings_map.html")

