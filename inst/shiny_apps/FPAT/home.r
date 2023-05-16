Home_UI <- function(id, label="settings") {
  ns <- NS(id)
  tagList(
    column(11,
           br(),
           h2('The Fisheries Performance Assessment Toolkit'),
           h3('The Coastal Fisheries Initiative (CFI)'),
           p('The Coastal Fisheries Initiative',
             a(href='http://www.fao.org/in-action/coastal-fisheries-initiative/en/', '(CFI)', target="_blank"),
             'is a global effort to preserve marine resources and ensure that coastal fisheries can continue to play their crucial role
             in society, contributing to food security, as well as economic and social development.'),
           p('Funded by the Global Environment Facility',
             a(href='https://www.thegef.org/', '(GEF)', target="_blank"), 'the initiative rallies UN agencies and international conservation
               organizations behind the common goal of promoting the sustainable use and management of coastal fisheries, championing
               innovative approaches to, improve governance and strengthening the seafood value chain.'),
           p("The CFI provides financial and hands-on technical support to coastal fisheries in six countries across three geographic regions:
             Indonesia, Latin America (Ecuador and Peru) and West Africa (Cape Verde, Cote d'Ivoire and Senegal)."),

           h3('The Fishery Performance Assessment Toolkit (FPAT)'),
           p('To measure the impact of the Coastal Fisheries Initiative, future performance must be compared to a baseline
           starting point. The measurement tool chosen by the CFI is the Fisheries Performance
           Assessment Toolkit (FPAT), a browser-based
           application designed to evaluate the ecological, economic, and social performance of a wide range of fisheries.'),
           p('FPAT relies on data from a variety of sources ranging from the technical to the informal, and was
           developed specifically for monitoring and evaluation of the CFI and built on the notion
           that an effective management system is one that is ecologically sustainable, socially acceptable, and generates
           sustainable resource rents or profits', a(href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0122809',
                                                     '(Anderson et al., 2015).', target="_blank")),


           h3('The FPAT Process'),
           p('FPAT includes two tools: the', a(href='https://www.fpilab.org/', 'Fisheries Performance Indicators:'),
             'a tool designed to determine how fisheries management systems are performing in order
               to achieve community, economic, and ecological sustainability,',
             'and', a(href='https://openmse.com/', 'openMSE:', target="_blank"), 'an open-source framework for
             evaluating the performance of alternative modes of management for a fishery.'),
           p('The FPAT process uses the two tools for several purposes:',
             tags$ol(
               tags$li('Characterize the current state of the fishery using the Fishery Performance Indicators;'),
               tags$li('Compare the FPI scores to baselines from other fisheries and regions;'),
               tags$li('Evaluate the expected performance of alternative management approaches with respect to fishery management objectives;'),
               tags$li('Implement changes to management and other interventions and use the FPIs to quantitatively evaluate the fishery improvements.'),
             )
           ),

           h3('Using FPAT'),
           p('There are four steps to using FPAT that correspond to the four tabs to the left:',
             tags$ol(
               tags$li('Load: Select an existing FPAT case study or load a new FPAT .xlsx data file;'),
               tags$li('FPI Scores: Evaluate the FPI input and output scores of the FPI;'),
               tags$li('Fishery Dynamics: Inspect the fishery dynamics specified in the case study or loaded FPAT xlsx data file;'),
               tags$li('Projections: Select management options and conduct fishery projections to comparatively evaluate expected performance.')
             )
           ),
           p('Detailed information on collecting the required information and using FPAT is available in the',
             a(href='https://blue-matter.github.io/FPAT/FPAT.html', 'FPAT User Guide.', target="_blank")),


    )
  )

}

Home_Server <- function(id, Info, Toggles) {
  moduleServer(id,
               function(input, output, session) {
               })
}
