


Home_UI <- function(id, label="Home") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
           HTML('<br>'),
           htmlOutput(ns('Intro')),

    )
  )

}

Home_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "<b> The Coastal Fisheries Initiative (CFI) </b> <br> <br>

        The Coastal Fisheries Initiative  <a href='http://www.fao.org/in-action/coastal-fisheries-initiative/en/'>(CFI)</a>
        is a global effort to preserve marine resources and ensure that coastal fisheries can continue to play their crucial role
        in society, contributing to food security, as well as economic and social development. <br> <br<

        Funded by the Global Environment Facility (GEF), the initiative rallies UN agencies and international conservation
        organizations behind the common goal of promoting the sustainable use and management of coastal fisheries, championing
        innovative approaches to improve governance and strengthening the seafood value chain. <br> <br>

        CFI provides financial and hands-on technical support to coastal fisheries in six countries across three geographies:
        Indonesia, Latin America (Ecuador and Peru) and West Africa (Cape Verde, Cote d'Ivoire and Senegal). <br> <br>

        <b> The Fishery Performance Assessment Toolkit (FPAT) </b> <br> <br>

        To measure the impact of the Coastal Fisheries Initiative, future performance must be compared to a baseline
        starting point. The measurement tool chosen by the CFI is the Fisheries Performance
        Assessment Toolkit (FPAT), developed specifically for monitoring and evaluation of the CFI and built on the notion
        that an effective management system is one that is ecologically sustainable, socially acceptable, and generates
        sustainable resource rents or profits. (Anderson et al., 2016). The FPAT relies on data from a variety of sources
        ranging from the technical to the informal. <br> <br>

        The Fisheries Performance Assessment Toolkit is a browser-based
        application designed to evaluate the ecological, economic, and social performance of a wide range of fisheries. <br> <br>

        <b> Using FPAT </b> <br> <br>

        Detailed information about using FPAT is available in the

        <a href='https://blue-matter.github.io/FPAT/FPAT.html'>FPAT User Guide </a>
        <br> <br>

        There are four steps to using FPAT that correspond to the four tabs to the left: <br> <br>

        1. Load: Select an existing FPAT case study or load a new FPAT .xlsx data file <br> <br>
        2. FPI Scores: Evaluate the FPI input and output scores of the FPI <br> <br>
        3. Fishery Dynamics: Inspect the fishery dynamics specified in the case study or loaded FPAT xlsx data file <br> <br>
        4. Projections: Select management options and conduct fishery projections to comparatively evaluate expected performance <br> <br>

        "
      })

    }
  )
}

