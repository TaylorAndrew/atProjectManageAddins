# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
InsertComment80 <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Insert Comment"),
    miniContentPanel(
      textInput('comment', "Comment:", '')
    )
  )

  server <- function(input, output, session) {

    # Set some CSS styles for our clock.
   comment80 <- function(textString) {
  e <- new.env()
  e$l <- c()
  lentS <- nchar(textString)
  e$l[1] <- paste0(c(rep("#", 80), "\n"), collapse = '')
  if (lentS <= 78) {
    eachSide <- (80 - lentS) / 2
    if (paste0(c(rep('#', eachSide), textString, rep('#', eachSide), '\n'), collapse =
               '') != 81) {
     e$l[2] <- paste0(c(rep('#', floor(eachSide)), textString, rep('#', ceiling(eachSide)), "\n"), collapse = '')
    } else {
    e$l[2] <- paste0(c(rep('#', eachSide), textString, rep('#', eachSide), "\n"), collapse = '')
    }
  }
  if (lentS > 78) {
    getStrings <- function(textString) {
      i = 1
      getCut <- function(i) {
        cut <- nchar(textString) / i <= 78
        if (cut)
          return(substring(
            textString,
            seq(1,nchar(textString),nchar(textString) /
                  i),
            seq(
              nchar(textString) / i,nchar(textString), nchar(textString) / i
            )
          ))
        if (!cut) {
          i <- i + 1
          getCut(i)
        }
      }
      cutStrings <- getCut(i)
      library(stringi)
      cutWords <- stri_split_fixed(cutStrings, ' ')
      for (i in 1:(length(cutStrings) - 1)) {
        if (cutWords[[i]][length(cutWords[[i]])] != " " &
            cutWords[[i + 1]][1] != " ") {
          if (nchar(paste0(
            paste0(cutWords[[i]], collapse = " "),
            cutWords[[i + 1]][1], collapse = ''
          )) <= 78) {
            cutStrings[i] <- paste0(paste0(cutWords[[i]], collapse = " "),
                                    cutWords[[i + 1]][1], collapse = '')
            cutStrings[i + 1] <-
              paste0(cutWords[[i + 1]][-1], collapse = " ")
            cutWords[[i + 1]] <- cutWords[[i + 1]][-1]
          } else if (nchar(paste0(
            cutWords[[i]][length(cutWords[[i]])],
            paste0(cutWords[[i + 1]], collapse = " "),
            collapse = ''
          )) <= 78) {
            cutStrings[i] <-
              paste0(cutWords[[i]][-length(cutWords[[i]])], collapse = " ")
            cutStrings[i + 1] <-
              paste0(cutWords[[i]][length(cutWords[[i]])],
                     paste0(cutWords[[i + 1]], collapse = " "),
                     collapse = '')
            cutWords[[i]] <- cutWords[[i]][-length(cutWords[[i]])]
          } else {
            cutStrings[i] <-
              paste0(substring(
                paste0(cutWords[[i]], collapse = " "), 1,
                nchar(paste0(cutWords[[i]], collapse = " ")) -
                  1
              ), "-")
            cutStrings[i + 1] <-
              paste0(substring(
                paste0(cutWords[[i]], collapse = " "),
                nchar(paste0(cutWords[[i]], collapse = " ")),
                nchar(paste0(cutWords[[i]], collapse = " "))
              ),
              paste0(cutWords[[i + 1]], collapse = " "))
          }
        }
      }
      for (j in 1:length(cutStrings)) {

        eachSide <- (80 - nchar(cutStrings[j])) / 2
        if (nchar(paste0(c(
          rep('#', floor(eachSide)),
          cutStrings[j],
          rep('#', ceiling(eachSide)),
          '\n'
        ), collapse = '')) == 81) {
          e$l[j+1] <- paste0(c(
            rep('#',floor(eachSide)), cutStrings[j], rep('#', ceiling(eachSide)), '\n'
          ), collapse = '')
        } else {
          e$l[j+1] <- paste0(c(
            rep('#', eachSide), cutStrings[j], rep('#', eachSide), '\n'
          ), collapse = '')
        }
      }
    }
    getStrings(textString)
  }
  lenel <- length(e$l)
  e$l[lenel+1] <- paste0(rep("#", 80), collapse = '')
  e$l <- paste0(e$l, collapse = '')
 return(e$l)
}

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      rstudioapi::insertText(comment80(textString=input$comment))
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}

# Try running the clock!
# InsertComment80()
