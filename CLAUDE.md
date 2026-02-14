# CLAUDE.md — Project Handover

## Project summary

This is a Quarto-based research and policy project investigating the decline of Ayr town centre (Scotland) and potential revitalisation strategies. The project aims to:

1. Compare Ayr's demographics with similar Scottish towns to assess whether Ayr is ageing unusually fast or passing a critical tipping point
2. Develop and test ideas for revitalising the town centre

The core hypothesis is a vicious cycle: poor access/infrastructure → reduced footfall → business closures → less attractive town → depopulation → weaker economic base → further decline. Proposed interventions target improved access/amenities and anchoring essential services in the town centre.

## Folder structure

```
├── CLAUDE.md              # This file — project context for Claude sessions
├── .gitignore
├── _quarto.yml            # Quarto project config (output dir, bibliography, format)
├── docs/                  # Narrative documents (background, briefs, reports)
│   └── background.qmd    # Project brief, theory of change, aims, data sources
├── analysis/              # Data analysis Quarto notebooks (.qmd)
│   ├── demographic-comparison.qmd  # Council area demographic comparison
│   └── simd-ayr-town-centre.qmd   # SIMD deprivation analysis at datazone level
├── data/
│   ├── raw/               # Raw downloaded data (Excel, CSV) — gitignored by default
│   └── processed/         # Cleaned/derived datasets
├── refs/
│   └── references.bib     # BibTeX bibliography
├── _outputs/              # Rendered outputs (html, docx, pptx) — gitignored
│   ├── docx/
│   └── pptx/
└── R/                     # Reusable R functions and scripts
    ├── fetch_population_data.R   # Download & cache NRS population estimates
    ├── fetch_simd_data.R         # Download & cache SIMD 2020v2 + datazone boundaries
    ├── calculate_indicators.R    # Demographic indicators, rankings, similarity
    ├── calculate_simd.R          # SIMD spatial joins, focus areas, Moran's I
    ├── plot_demographics.R       # Population pyramids, comparison charts
    └── plot_simd.R               # SIMD choropleths, LISA maps, Leaflet reference map
```

## Technology stack

- **Quarto** (.qmd) — primary authoring format for documents and analysis
- **Mermaid** — diagrams and logic models (use ```` ```{mermaid} ```` syntax in .qmd, NOT ```` ```mermaid ````)
- **R** — data analysis and visualisation (when needed)
- **BibTeX** — reference management via `refs/references.bib`
- **Git** — version control

## Key conventions

- Use `.qmd` (not `.md`) for any document that needs rendering (diagrams, code, citations)
- Narrative/background docs go in `docs/`, analysis notebooks go in `analysis/`
- Raw data in `data/raw/`, processed data in `data/processed/`
- Large data files are gitignored by default — track only small reference datasets
- Rendered outputs go to `_outputs/` and are gitignored (regenerable via `quarto render`)
- Quarto project config is in `_quarto.yml` at root

## Data pipelines

### Demographics (NRS population estimates)
- NRS mid-year population estimates (single year of age, by sex, all 32 council areas + Scotland)
- Downloaded from nrscotland.gov.uk as Excel, cached as CSV in `data/processed/`
- `get_population_data()` handles download + caching automatically
- South Ayrshire code: `S12000028`, Scotland code: `S92000003`

### SIMD 2020v2 (deprivation at datazone level)
- SIMD 2020v2 from gov.scot: 3 Excel files (ranks, indicators, lookup) merged into 6,976 datazones × 63 columns
- 2011 datazone boundary shapefile from gov.scot (~20 MB)
- `get_simd_data()` downloads Excel files, merges, and caches as CSV
- `get_dz_boundaries()` downloads shapefile ZIP, extracts, reads via `sf::st_read()`
- 7 domains: Income, Employment, Health, Education, Access, Crime, Housing
- South Ayrshire has 153 datazones (2011 basis)

### Scottish statistical geographies (reference)
| Level | 2011 basis | 2022 basis |
|-------|-----------|-----------|
| Council areas | 32 | 32 |
| Health boards | 14 | 14 |
| HSCPs | 31 | 31 |
| Intermediate zones | 1,279 | 1,334 |
| Data zones | 6,976 | 7,392 |
| Output areas | 46,351 | 46,363 |

## Current status

- **Done**: Project brief with theory of change (`docs/background.qmd`); demographic comparison analysis (`analysis/demographic-comparison.qmd`) with population pyramids, indicator rankings, and council area similarity analysis
- **Key finding**: South Ayrshire median age 49.2 (Scotland: 41.8), ranks 5th oldest. Most similar areas: Scottish Borders, Dumfries & Galloway, Na h-Eileanan Siar
- **In progress**: SIMD 2020v2 deprivation analysis at datazone level (`analysis/simd-ayr-town-centre.qmd`) — R pipeline built, awaiting user-defined datazone tiers for Ayr town centre and wider Ayr area

## Planned analyses

1. **Sociodemographic profile** (in progress) — SIMD 2020v2 deprivation at datazone level, choropleth maps, domain profiles, Moran's I spatial clustering
2. **Urban accessibility** — transport routes, road network, parking, public transport access to Ayr town centre
3. **Local economy** — business counts, vacancy rates, sectoral employment, footfall data

## Key references

- Pride in Place Programme: £20m over 10 years for "Northern Ayr and Town Centre Regeneration Corridor" (`@ukgov2025prideinplace`)
- Burns Statue Square Redevelopment: £16m Levelling Up Fund + £2m ward fund for pedestrianisation and A70 realignment (`@southayrshire2025burnsstatue`)
