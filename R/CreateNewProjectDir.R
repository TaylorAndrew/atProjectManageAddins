# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
CreateNewProjectDir <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("New Project Directory"),
    miniContentPanel(
      textInput('Dir', "Directory:", getwd()),
      textInput('FolderName', "Folder Name:", ''),
      checkboxInput('addReportSkeleton', "Add Report .Rmd skeleton to /Reports/ Directory?", T),
      checkboxInput('addPDAReportSkeleton', "Add PDA .Rmd skeleton to /Documents/ Directory?", T),
      checkboxInput('recursive', "If full directory does not exist, create it?", T)
    )
  )

  server <- function(input, output, session) {


newProject <- function(Dir,
                       FolderName,
                       addReportSkeleton=FALSE,
                       addPDAReportSkeleton = FALSE,
                       recursive = FALSE) {
  if (substr(Dir, nchar(Dir), nchar(Dir)) != "/")
    Dir <- paste0(Dir, "/")
  if (dir.exists(paste0(Dir, FolderName)))
    return(paste0(Dir, FolderName, " already exists. Project folder was not made."))
  dir.create(paste0(Dir, FolderName), recursive = recursive)
  dir.create(paste0(Dir, FolderName, "/Data"))
  dir.create(paste0(Dir, FolderName, "/Syntax"))
  dir.create(paste0(Dir, FolderName, "/Output"))
  dir.create(paste0(Dir, FolderName, "/Reports"))
  dir.create(paste0(Dir, FolderName, "/Documents"))
  if(addReportSkeleton==T) {
    file.copy("/Docs/RMarkdownSkeleton.Rmd", paste0(Dir,
                                                        FolderName,
                                                        "/Reports/",
                                                        FolderName,
                                                        "_report.Rmd"))
  }
  if(addPDAReportSkeleton==T) {
    file.copy("/Docs/RMarkdown_PDA_Skeleton.Rmd", paste0(Dir,
                                                        FolderName,
                                                        "/Documents/",
                                                        FolderName,
                                                        "_PDA.Rmd"))
  }
}


    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      newProject(Dir=input$Dir,
                 FolderName=input$FolderName,
                 addReportSkeleton=input$addReportSkeleton,
                 addPDAReportSkeleton=input$addPDAReportSkeleton,
                 recursive=input$recursive)
      stopApp()
    })

  }

  viewer <- dialogViewer("New Project", 400, 400)
  runGadget(ui, server, viewer = viewer)
}

