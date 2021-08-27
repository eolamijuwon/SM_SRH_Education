## Understanding how young African adults interact with peer-generated sexual health information on Facebook and uncovering strategies for successful organic engagement

### Short Summary:

The use of social media for sexual health communication is gaining intense discussion both globally and in Africa. Despite this reality, it remains unclear what features of sexual health information *[particularly those communicated by peers]* appeal to young African adults. This study aims to fill the gaps in scholarship by identifying post features and content associated with greater user engagement. We analyzed a corpus of 3,533 sexual and reproductive health messages shared on a public Facebook group by and for young African adults between June 1, 2018, and May 31, 2019. Facebook posts were independently classified into thematic categories such as topic, strategy, and tone of communication. The findings highlight that young people on the network as expected, generally engaged with posts superficially by liking (x̃ = 54; x̄ = 109.28; σ = 159.24) rather than engaging through comments (x̃ = 10; x̄ = 32.03; σ = 62.65) or sharing (x̃ = 3; x̄ = 11.34; σ = 55.12). Messages with fear [IRR:0.75, 95%CI: 0.66-0.86] or guilt [IRR:0.82, 95%CI: 0.72-0.92] appeals received a significantly lower number of reactions compared to neutral messages. Messages requesting an opinion [IRR:4.25, 95%CI: 3.57-5.10] had a significantly higher number of comments compared to status updates. The use of multimedia and storytelling formats were also significantly associated with a higher level of engagement and propagation of sexual health messages on the group. Altogether, our findings provide valuable insight and pave the way for the design of effective and context-specific sexual health information use of features that attract young African adults.

#### **How to replicate the analysis**

-   Download the analysis pack *[see structure below]*

    .

    **├── Data**
    
    │ ├── codebook.csv

    │ └── FB_SRH.csv

    **├── Output**

    │ └── ...

    **├── R**

    │ ├── 00_master.R

    │ ├── 01_packages_install.R

    │ ├── 02_theme.R

    │ ├── 03_visualizations.R

    │ └── 04_regression_models.R

    ├── README.md

    └── SM_SRH_Education.Rproj

-   Using RStudio open "SM_SRH_Education.Rproj" file in the main project directory.

-   Run the master file in "R/00_master.R".

    The analysis is split into four parts, which is reflected in the structure of R scripts.

    -   Step 1: Installation of Packages -------------------

        `source ("R/01_packages_install.R")`

    -   Step 2: Importing Fonts and Setting of Themes ------

        `source ("R/02_theme.R")`

    -   Step 3: Data Visualization -------------------------

        `source ("R/03_visualizations.R")`

    -   Step 4: Regression Models --------------------------

        `source ("R/04_regression_models.R")`


-   Outputs from the exploratory analysis, graphs and models are saved in the [output folder](Output/).


#### How to cite:\*\*\* **Olamijuwon, E.O.**, Odimegwu, C.O. and Adjiwanou, V. Advancing sexual health education for young African adults in the digital age: Uncovering strategies for successful organic engagement. *[Forthcoming]*
