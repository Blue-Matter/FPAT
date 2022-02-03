# Create demo Info object

Info <- list(
  file=NULL,          # input file
  sheets=NULL,        # Sheet names
  Summary=NULL,       # FPI summary tab
  Output_table=NULL,  # FPI output-table
  Input_table=NULL,   # FPI input-table
  Data = NULL,        # Data
  MERA.Qs = NULL,     # M
  FPI.Inputs = NULL,  # FPI inputs table
  FPI.Cover = NULL,   # FPI cover sheet
  OM = NULL,          # Operating model
  MSEhist = NULL,     # Historical reconstruction
  MSEproj = NULL,     # MSE projection
  MPsel = c("CurC","curEref")        # Selected MPs for running the MSE
)

Info$file$datapath <- 'inst/shiny_apps/FPAT/Data/Demo_1.xlsx'


Info$sheets <- readxl::excel_sheets(Info$file$datapath)
Info$Summary <- readxl::read_excel(Info$file$datapath, sheet='4. Summary', .name_repair = 'minimal')
Info$Output_table <- readxl::read_excel(Info$file$datapath, sheet='5. Output-table', .name_repair = 'minimal')
Info$Data <- XL2Data(name=Info$file$datapath, sheet='12. Fishery Data')
Info$MERA.Qs <- readxl::read_excel(Info$file$datapath, sheet='13. openMSE Questions', .name_repair = 'minimal')
Info$FPI.Inputs <- readxl::read_excel(Info$file$datapath, sheet='6. Input-table', .name_repair = 'minimal')
Info$FPI.Cover <- readxl::read_excel(Info$file$datapath, sheet='3. Cover Page', .name_repair = 'minimal')


# Load BaseLines
baseline_in <- 'inst/shiny_apps/FPAT/Data/Baseline.csv'

BaseLine <- read.csv(baseline_in)


input <- list()
input$baseline <- 'High Income'

FPI.Summary <- Info$Summary
