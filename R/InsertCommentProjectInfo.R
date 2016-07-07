# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
InsertcommentProjectInfo <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Insert Comment"),
    miniContentPanel(
      textInput('ProjectName', "Project Name:", ''),
      textInput('Researcher', "Researcher:", ''),
      textInput('Statistician', "Statistician:", ''),
      textInput('StartDate', "Start Date:", format(Sys.Date())),
      textInput('UpdateDate', "Update Date:", '')
    )
  )

  server <- function(input, output, session) {


commentProjectInfo <- function(ProjectName='',
                        Researcher = '',
                        Statistician = '',
                        StartDate = '',
                        UpdateDate = '') {
  m <- new.env()
  m$l <- c()
  getStrings <- function(string, max){
    e <- new.env()
    e$P <- list()
    get <- function(string, i=1) {
      e$P[i] <- gsub("^\\s+|\\s+$", "", substr(string, 1, max))
      leftOver <- substr(string, max+1, 10000)
      if(nchar(leftOver)>0) {
        i = i + 1
        get(string=leftOver, i = i)
      }
    }
  get(string=string)
  return(e$P)
  }
  ProjectName <- getStrings(ProjectName, max=64)
  Researcher <- getStrings(Researcher, max=61)
  Statistician <- getStrings(Statistician, max=64)
  StartDate <- getStrings(StartDate, max=66)
  UpdateDate <- getStrings(UpdateDate, max=59)
  m$l[1] <- paste0(c(rep("#", 80), "\n"), collapse = '')
  m$count <- 2
  lapply(
  ProjectName,
  invisible(function(x) {
  m$l[m$count] <-
    paste0(c("#Project Name: ",
             x,
             paste0(c(rep(" ", 80-(16+nchar(x)))), collapse = ''),
             "#\n"), collapse = '')
  m$count <- m$count + 1
  }))
  lapply(
  Researcher,
  invisible(function(x) {
  m$l[m$count] <-
  paste0(c("#Researcher Name: ",
             x,
             paste0(c(rep(" ", 80-(19+nchar(x)))), collapse = ''),
             "#\n"), collapse = '')
  m$count <- m$count + 1
  }))
  lapply(
  Statistician,
  invisible(function(x) {
  m$l[m$count] <-
  paste0(c("#Statistician: ",
             x,
             paste0(c(rep(" ", 80-(16+nchar(x)))), collapse = ''),
             "#\n"), collapse = '')
  m$count <- m$count + 1
  }))
  lapply(
  StartDate,
  invisible(function(x) {
  m$l[m$count] <-
  paste0(c("#Start Date: ",
             x,
             paste0(c(rep(" ", 80-(14+nchar(x)))), collapse = ''),
             "#\n"), collapse = '')
  m$count <- m$count + 1
  }))
  lapply(
  UpdateDate,
  invisible(function(x) {
  m$l[m$count] <-
  paste0(c("#Updated Code Date: ",
             x,
             paste0(c(rep(" ", 80-(21+nchar(x)))), collapse = ''),
             "#\n"), collapse = '')
  m$count <- m$count + 1
  }))
   m$l[m$count] <- paste0(c(rep("#", 80), "\n"), collapse = '')
   m$l <- paste0(m$l, collapse = '')
   return(m$l)
}

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      rstudioapi::insertText(commentProjectInfo(ProjectName=input$ProjectName,
                                                Researcher=input$Researcher,
                                                Statistician=input$Statistician,
                                                StartDate=input$StartDate,
                                                UpdateDate=input$UpdateDate))
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}

# Try running the clock!
# InsertcommentProjectInfo()

