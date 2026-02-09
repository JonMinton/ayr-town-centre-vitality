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
│   └── demographic-comparison.qmd  # Council area demographic comparison
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
    ├── calculate_indicators.R    # Demographic indicators, rankings, similarity
    └── plot_demographics.R       # Population pyramids, comparison charts
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

## Data pipeline

- NRS mid-year population estimates (single year of age, by sex, all 32 council areas + Scotland)
- Downloaded from nrscotland.gov.uk as Excel, cached as CSV in `data/processed/`
- `get_population_data()` handles download + caching automatically
- South Ayrshire code: `S12000028`, Scotland code: `S92000003`

## Current status

- **Done**: Project brief with theory of change (`docs/background.qmd`); demographic comparison analysis (`analysis/demographic-comparison.qmd`) with population pyramids, indicator rankings, and council area similarity analysis
- **Key finding**: South Ayrshire median age 49.2 (Scotland: 41.8), ranks 5th oldest. Most similar areas: Scottish Borders, Dumfries & Galloway, Na h-Eileanan Siar
- **Next steps**: Population trends over time, migration flows, economic indicators (vacancy rates, footfall), Census 2022 deep dive
