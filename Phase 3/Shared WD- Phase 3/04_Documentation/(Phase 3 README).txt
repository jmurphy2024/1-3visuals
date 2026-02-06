THE 1/3 COUNTRY PROJECT: PHASE 3 NARRATIVE DEVELOPMENT README
Last Updated: October 27, 2025

PROJECT OVERVIEW
This repository continues to contain the R scripts, data, and documentation for the "1/3 Country Project." This initiative analyzes socioeconomic disparities in the U.S. by reimagining the population as three distinct income-based "countries": the Bottom Third, Middle Third, and Top Third.

The core goal of Phase 3 is to synthesize the previously established analytical framework with a new thematic storytelling layer. We will define the lived experience of citizens in each "country" across five critical themes, moving beyond simple data visualization to build a comprehensive narrative.

PHASE 3 OBJECTIVES
Phase 3 shifts the focus from building the analytical framework to developing the comprehensive, multi-dimensional narrative for the three income-based "countries." This phase focuses on defining the lived experiences of citizens across five core thematic areas: Income/Wealth, Health, Education, Crime, and Quality of Life.



PART 1: THEMATIC FRAMEWORK DEVELOPMENT

Theme Definition: For each country, establish a Guiding Philosophy—a core belief system that shapes its culture, values, and the inner lives of its citizens.

Comprehensive Narrative: Build the narrative in a sequence that logically explains the national experience:

Income & Wealth: The economic foundation.

Health: The physical and mental consequences.

Education: The mechanism for generational transmission of values and status.

Crime & Justice: The societal response to breakdown and inequality.

The National Myth vs. Reality: A concluding analysis of the country's story about itself.

Quality of Life: The daily experience shaped by that economy.

PART 2: STORYTELLING FRAMEWORK

Develop Guiding Questions for each theme to anchor the narrative and ensure comparative consistency across the three countries. Examples include:

Income and Wealth: What is the main financial goal? What financial fear keeps people up at night?

Health: How do people manage staying sick and getting well? What is their relationship with the healthcare system?

Education: What is school for? What is the number one thing parents want from their kids' education?

Crime: Is crime a real, everyday concern, or an abstract problem? Who do they trust to handle it?

Quality of Life: What does a neighborhood feel like? What’s the most common daily frustration?

National Myth vs. Reality: What is their version of the "good life"? What is the biggest disconnect between that belief and the reality of most people's lives (The Blind Spot)?

PART 3: METRIC IDENTIFICATION

Identify and select 5-6 key data metrics (data points) for each theme that best describe and quantify the qualitative narrative we are constructing.

PART 4: NARRATIVE & VISUAL STORYTELLING SYNTHESIS

Create the final deliverables, which include both the data-rich appendices and the thematic narrative chapters:

Thematic Narrative Chapters: Synthesize the data and themes into cohesive, compelling narrative sections (approximately 5 pages per theme, including visuals) that integrate the selected data points (metrics) and address the Guiding Questions.

Narrative Tables (Appendix A): Build the concise thematic narrative tables (1-2 sentence summaries per metric/country) for use as a project appendix.

Visual Integration: Ensure that all accompanying data visualizations are seamlessly woven into the narrative to maximize storytelling impact and move beyond a purely "reporty" structure.





GETTING STARTED & SETUP
The foundational setup from Phase 2 remains critical. To contribute to this phase, ensure your local development environment and data access are confirmed.

This detailed protocol ensures you are ready to contribute efficiently to the project.

3.1. Software Installation 💻

You'll need R, RStudio, and Git installed on your machine.

Install R: Download and install the latest version of R from the CRAN website.

Install RStudio Desktop: Download and install the latest free version of RStudio Desktop.

Install Git: Download and install Git from git-scm.com. Ensure Git Bash (Windows) or Terminal (macOS/Linux) is available.



3.2. Project Access & RStudio Environment 📂

The project management team handles the initial repository setup and cloning. Your steps focus on accessing the local environment.

Dropbox Access: Confirm the "1-3 Indicator Data Analysis" folder is synced to your local machine.

Open RStudio Project: Navigate into the Shared WD folder (e.g., ~\1-3 Indicator Data Analysis\Phase 3\Shared WD) and open the .Rproj file. This automatically configures RStudio's environment and enables the Git pane.



3.3. IPUMS API Key Setup 🔑

To download data using the ipumsr package, you need a personal IPUMS API key.

Get API Key: Register for a free IPUMS account and retrieve your unique API key from your account settings.

Set Key: In the RStudio Console, run usethis::edit_r_environ() (install usethis first if needed). Add the line: IPUMS_API_KEY="YOUR_API_KEY_HERE" (replace placeholder with your key).

Verify: Save the file, Restart RStudio, and verify with Sys.getenv("IPUMS_API_KEY").



PROJECT STRUCTURE
This Working Directory (Shared WD) is organized into the following subfolders:

01_data/: Contains raw, untransformed data (raw/), cleaned/processed data (processed/), and downloaded TIGER/Line shapefiles (gis_shapefiles/).


02_scripts/: Houses all R scripts for the project.

I-Geo Areas Master File/: Scripts related to Stage I for building the master geographic database.

II-Shared Functions/: Reusable R functions.

III-Data Prep Templates/: Scripts for data cleaning and transformation, including templates.

_archive/: For older, manually archived script versions.


03_output/: Location for generated figures, tables, and reports.

04_documentation/: Project-level documentation.

Codebooks/: Contains human-readable text codebooks automatically generated for each downloaded IPUMS dataset.


05_misc/: Miscellaneous project files.



KEY METHODOLOGIES & TOOLS
Programming Language: R
Integrated Development Environment (IDE): RStudio
Version Control: Git (local repository synchronized via Dropbox)
Data Sources: IPUMS NHGIS, IPUMS ACS, Census TIGER/Line Shapefiles
R Packages: ipumsr, tigris, sf, dplyr, purrr, here, ggplot2.
Key Concepts: Income Terciles/Ventiles, Spatial Joins, Geographic Crosswalks, Modular Scripting.



CONTRIBUTION GUIDELINES & BEST PRACTICES

6.1. Why Use Git for Local Version Control?

We use Git strictly for local version control (tracking changes and mistake recovery) within the RStudio environment. It serves as a comprehensive lab notebook for your code. Key benefits include:

Complete Version History: Git saves "snapshots" (commits) of your project, allowing you to easily look back at any saved state.

Fearless Experimentation (Branching): You can create separate "branches" to test different models or cleaning methods without risking damage to the main, working code.

Mistake Recovery: It is the ultimate "undo" button. If you delete a chunk of code or break a script, you can easily compare your current file to any previous version and restore the needed parts.

Meaningful Change Tracking: Every commit requires a descriptive message (e.g., "Fixed bug in visualization function"), which is crucial for team members to understand the project's development history.



6.2. Git Workflow: Daily Cycle

Adhere to this simple, repeating cycle using the Git pane in RStudio:

Pull: Before you start work, click the Pull button (downward arrow icon) in the Git pane to fetch the latest changes made by other team members from the shared directory.

Work: Make changes to your R scripts.

Stage: When you reach a good stopping point, go to the Git pane, and check the boxes next to the files you’ve changed.

Commit: Click Commit, write a clear message describing your changes (e.g., "Updated ACS analysis to include cost burden metric"), and click the commit button.

Sync/Save: Your committed changes are now locally saved and integrated with the shared project structure via Dropbox, allowing other team members to pull them.



6.3. General Best Practices

Commit frequently with clear, descriptive messages.

Always git pull before starting new work to avoid conflicts.

Commenting & Formatting: Adhere to the R Script Formatting Guidelines below. Provide extensive comments to explain logic, assumptions, and significant steps.

File Paths: Always use the here package for defining file paths.



R SCRIPT FORMATTING GUIDELINES

To maintain consistency and readability across all R scripts in this project, adhere to the following formatting standards:

Script Header:
Every R script must begin with a comprehensive header block providing essential metadata. This header will be formatted with ## for each line.

WD location: Where in the WD the script is located

Script: your-script-name.R

Purpose: A concise description of what the script accomplishes.

Author: Your Name/Team Name

Date Created: YYYY-MM-DD

Last Modified: YYYY-MM-DD (Brief note on changes, if applicable)

Dependencies: List of R packages required (e.g., readr, dplyr, ggplot2)

Input: Path(s) to primary input data files.

Output: Path(s) to primary output files generated by the script.




Hierarchical Sectioning:
Organize the script content into clearly defined, hierarchical sections using decimal notation and increasing equal signs, similar to the Phase 1 scripts. Each heading should concisely describe the subsequent code's purpose.

Level 0: ABOUT (Special Case)
The very first section, providing the script header and initial setup details, should be 0. ABOUT. It will not have preceding empty rows.

==== 0. ABOUT ====

Load necessary libraries

library(readr)
library(dplyr)

... other setup code ...

Level 1 Heading:
These are major sections, all caps, preceded by two empty lines (after Level 0), and marked by four equal signs (====). These act as main sections in RStudio's document outline.

==== 1. DATA LOADING AND INITIAL CLEANING ====

Level 2 Heading:
Subsections, marked by five equal signs (=====).

===== 1.1. Load Raw Data =====

Level 3 Heading:
Further subdivisions, marked by six equal signs (======).

====== 1.1.1. Read CSV Files ======

Level 4 Heading (and so on):
Continue adding an additional equal sign for each deeper level of heading as needed.

======= 1.1.1.1. Filter for Relevant Columns =======

KEY PROJECT DOCUMENTS & CHAT INITIATION
All core project documents, including the comprehensive overview for initiating a new Gemini chat, are stored in a dedicated folder within the Working Directory. This ensures all essential context is readily available.

Folder Location: 04_documentation/New Gemini Chat/

Contents of the "New Gemini Chat" Folder:

overall_info_script_for_gemini.txt: The primary script to use when initiating a new Gemini chat.

1-3 Country Reader v2 5.19.25.pdf: Provides the foundational concepts, narratives, and specific findings of Phase 1.

Indicators Accross Incomes Presentation.pptx: Offers a high-level visual summary of Phase 1 outcomes and outlines the strategic future directions.

⅓ Country Phase 2 Viz Project Plan.docx: The core planning document for Phase 2, detailing strategic objectives, stages, and technical methodologies. Essential for understanding prior project tasks.

Phase 1 script.txt: The original, monolithic R script from Phase 1.

Phase 2 New User Setup Protocol.docx: Your latest version of the step-by-step guide for onboarding new team members.

To initiate a new, fully contextualized Gemini chat, open the overall_info_script_for_gemini.txt file, copy its entire content, paste it into Gemini's chat interface, and ensure you upload all five (5) associated project documents listed above.