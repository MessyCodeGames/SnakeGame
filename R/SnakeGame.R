SnakeGame <- function(sizeBoard = "normal", cheatCode = "", prank = FALSE) {

  #### Libraries required ####

  packages = c("ggplot2", "tcltk")

  # Load or install & load needed libraries #
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  #### Complete game ####
  message("WELCOME TO THE SNAKE GAME!")
  message("")
  message("Use the arrows to control the snake")
  message("Or a, w, s, d")
  message("")
  message("You can leave anytime by pressing 'l'")
  message("")
  message("Use the emergency pause by pressing 'n'")
  message("")
  message("Start the game by pressing 'p'")
  message("")
  message("The quicker you get the fruits, the more points you score!")
  message("")
  message("Will you reach the legendary snake level and witness the alternative ending?")
  message("")
  message("If the commands do not work automatically, make sure that you have selected the newly opened window with the feather symbol")
  message("")

  flush.console()

  # General information #
  difficulty <- "normal" # Means speed
  # sizeBoard <- "small"
  sizeBoard <- sizeBoard

  # Make the system wait for a specified amount of time, in seconds #
  if (sizeBoard == "small") {

    baseSpeed <- 0.40
    time <- baseSpeed
    speedMultiplier <- 0.002 # speed linear multiplier

  } else if (sizeBoard == "normal") {

    baseSpeed <- 0.3

    time <- baseSpeed
    speedMultiplier <- 0.0025 # speed linear multiplier

  }

  if (prank == TRUE) {

    Infos <- Sys.info()

    if(Infos["user"] == "robin") {

      baseSpeed <- 0.01
      time <- baseSpeed
      speedMultiplier <- 0.0025 # speed linear multiplier

    }

  }

  # Colors ground, tail snake, head snake, fruit #
  cols <- c("#028900","#74d600","#adff00", "#089000","#0a5d00",  "#CC1100")

  # Initiate first level alphas #
  alphaEvol <- c(0.3, 0.3, 0.3, 1, 1, 1)

  # CLean graph history sometimes #
  iterations <-  0

  # Level tresholds #
  if (sizeBoard == "small") {

    seuil1 <- 15
    seuil2 <- 30
    seuil3 <- 40
    seuil4 <- 50
    seuil5 <- 60
    seuil6 <- 70

  } else if (sizeBoard == "normal") {

    seuil1 <- 15
    seuil2 <- 30
    seuil3 <- 45
    seuil4 <- 60
    seuil5 <- 75
    seuil6 <- 90
  }

  if (cheatCode == "thereisnocowlevel") {
    # PlayTest tresholds #
    seuil1 <- 2
    seuil2 <- 4
    seuil3 <- 6
    seuil4 <- 8
    seuil5 <- 10
    seuil6 <- 12
  }

  # Board size depending on options #
  if (sizeBoard == "normal") {

    yTableLength <- 15
    xTableLength <- 15

  } else if (sizeBoard == "small") {

    yTableLength <- 10
    xTableLength <- 10

  }

  # Fill the table with 0 #
  plateauMatrix <- matrix(nrow = yTableLength, ncol = xTableLength)
  plateauMatrix[] <- 0 # state table

  # Tail
  if (sizeBoard == "small") {

    taily <- c(5,5)
    tailx <- c(2,3)

    # Head
    heady <- c(5)
    headx <- c(4)

    # Coordonnees pour dataframe plot #
    coX <- c(rep(1, yTableLength),
             rep(2, yTableLength),
             rep(3, yTableLength),
             rep(4, yTableLength),
             rep(5, yTableLength),
             rep(6, yTableLength),
             rep(7, yTableLength),
             rep(8, yTableLength),
             rep(9, yTableLength),
             rep(10, yTableLength))

    coY <- rep(seq(10, 1, -1), 10)

    # Create the environement and states of assets #
    theMatrix <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
                          0, 1, 1, 1, 1, 0, 1, 1, 0, 0,
                          1, 1, 2, 1, 1, 1, 1, 0, 0, 1,
                          1, 2, 2, 2, 1, 1, 0, 0, 0, 1,
                          1, 1, 2, 2, 1, 0, 0, 0, 1, 2,
                          0, 1, 2, 1, 1, 0, 0, 0, 1, 1,
                          0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
                          0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
                          1, 1, 0, 0, 1, 0, 1, 1, 1, 2),
                        ncol = 10, nrow = 10)

  } else if (sizeBoard == "normal") {

    taily <- c(7,7)
    tailx <- c(2,3)

    # Head
    heady <- c(7)
    headx <- c(4)

    # Coordonnees pour dataframe plot #
    coX <- c(rep(1, yTableLength),
             rep(2, yTableLength),
             rep(3, yTableLength),
             rep(4, yTableLength),
             rep(5, yTableLength),
             rep(6, yTableLength),
             rep(7, yTableLength),
             rep(8, yTableLength),
             rep(9, yTableLength),
             rep(10, yTableLength),
             rep(11, yTableLength),
             rep(12, yTableLength),
             rep(13, yTableLength),
             rep(14, yTableLength),
             rep(15, yTableLength))

    coY <- rep(seq(xTableLength, 1, -1), yTableLength)

    # Create the environement and states of assets #
    theMatrix <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 2,
                          0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1,
                          1, 1, 2, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0,
                          1, 2, 2, 2, 1, 1, 0, 0, 1, 1, 2, 1, 0, 0, 0,
                          1, 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 1, 0, 0,
                          0, 1, 2, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,
                          0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                          0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,
                          0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1,
                          0, 0, 0, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                          0, 1, 1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1,
                          1, 1, 1, 1, 2, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                          1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0,
                          0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0,
                          0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0),
                        ncol = 15, nrow = 15)

  }

  taily2 <- rep(1, length(taily))
  tailx2 <- rep(1, length(tailx))

  heady2 <- c()
  headx2 <- c()

  taily2 <- c(rep(1, length(taily)))
  tailx2 <- c(rep(1, length(taily)))

  # Generate random first fruit coordinates #
  firstFruitGo <- FALSE

  while (firstFruitGo == FALSE) {

    fruity <- sample(1:yTableLength , 1, replace = TRUE)
    fruitx <- sample(1:xTableLength , 1, replace = TRUE)

    if( (fruity[1] == heady[1] & fruitx[1] == headx[1]) &
        (fruity[1] == taily[1] & fruitx[1] == tailx[1]) &
        (fruity[1] == taily[2] & fruitx[1] == tailx[2]) ) {

      firstFruitGo <- FALSE

    } else {

      firstFruitGo <- TRUE

    }
  }

  # State fruit #
  plateauMatrix[fruity, fruitx] <- 3

  # State head
  plateauMatrix[heady, headx] <- 2

  # State tail
  for (i in 1:length(taily)) {

    plateauMatrix[taily[i], tailx[i]] <- 1

  }

  CreateEnvironment <- function(theMatrix, heady, headx, taily, tailx, fruity, fruitx) {

    # Create the table #
    plateauMatrix <- theMatrix

    # State head
    plateauMatrix[heady, headx] <- 4

    # State tail
    for (j in 1:length(taily)) {

      plateauMatrix[taily[j], tailx[j]] <- 3

    }

    # New fruit #
    plateauMatrix[fruity, fruitx] <- 5

    return(plateauMatrix)

  }

  # Create first environment
  plateauMatrix <- CreateEnvironment(theMatrix, heady, headx, taily, tailx, fruity, fruitx)

  # Data for the table #
  coordinatesData <- data.frame(coordx= coX,
                                coordy = coY,
                                state = as.vector(plateauMatrix))

  # Comptage des points #
  nFruitEaten <- 0
  userScore <- 0

  # set Initial level #
  level <- "Green Baby Snake"
  levelNumber <- 1

  # Plot the data #
  displayPlot <- function(coordx, coordy, state, cols) {

    return(ggplot(coordinatesData, aes(x = coordx, y = coordy, fill = as.factor(state))) +
             geom_tile() +
             scale_fill_manual(values = alpha(c(cols), alphaEvol))  +
             theme_void() +
             theme(legend.position = "none",
                   axis.title.x= element_blank(),
                   axis.text.x= element_blank(),
                   axis.ticks.x= element_blank(),
                   axis.title.y= element_blank(),
                   axis.text.y= element_blank(),
                   axis.ticks.y= element_blank()))

  }

  # Label stats for plot #
  statDisplay <- paste0("Fruits eaten: ", nFruitEaten,"     Score: ", round(userScore,0), "     Level ", levelNumber, ": ", level)

  # Plot the data #
  displayPlotStats <- function(coordx, coordy, state, cols, statDisplay) {

    return(ggplot(coordinatesData, aes(x = coordx, y = coordy, fill = as.factor(state))) +
             geom_tile() +
             scale_fill_manual(values = alpha(c(cols), alphaEvol))  +
             theme_void() +
             xlab(statDisplay) +
             theme(legend.position = "none",
                   axis.title.x= element_blank(),
                   axis.text.x= element_blank(),
                   axis.ticks.x= element_blank(),
                   axis.title.y= element_blank(),
                   axis.text.y= element_blank(),
                   axis.ticks.y= element_blank()) +
             annotate(geom="label", x= sum(seq(1,xTableLength,1))/xTableLength, y= -0.3,
                      label= statDisplay, color="#006633", fill = "#99FF00", size = 5))

  }

  # Boolean for eating a fruit #
  hasEaten <- FALSE

  # Game over #
  gameOver <- FALSE
  coordTailCheck2 <- c()

  # Fruit coordinate check #
  fruitCoordGo <- FALSE
  coordFruitCheck2 <- c()

  # Window for direct userInput #
  # Code shamelessly stolen from the Snake package
  SetFocus <- function(Window) {

    info_sys <- Sys.info()

    if (info_sys["sysname"] == "Windows") {

      Dir <- getwd()
      on.exit(setwd(Dir))
      setwd("C:/")
      shell("powershell -command [void] [System.Reflection.Assembly]::LoadWithPartialName('Microsoft.VisualBasic') ; [Microsoft.VisualBasic.Interaction]::AppActivate('Snake control')")

    } # else {

    # tcltk::tkwm.deiconify(Window)
    # }
  }

  # Open graphic window #
  x11()

  # Open command window #
  SnakeEnvir <- new.env()
  SnakeEnvir$KeyCode <- NA
  tt <- tcltk::tktoplevel()
  invisible(tcltk::tkwm.title(tt, "Snake control"))
  invisible(tcltk::tkwm.geometry(tt, "300x100+9000+500"))
  invisible(tcltk::tkbind(tt, "<Key>", function(K) {SnakeEnvir$KeyCode <- K}))

  SetFocus(tt)

  # Initialisation du temps passe #
  timeElapsed <- 0
  timeBetweenFruits <- 0.01

  # Direction generale du snake par default #
  direction <- 4

  # Main Menu #
  # Out of the loop #

  # Indications on how to play #
  label <- data.frame(
    x = c(3, 9.5, 9.5, 9.5, 16, 9.5),
    y = c(4.5, 5.5, 4.5,3.5, 4.5, 1.5),
    label = c(paste0("Make sure the\n window with the\n feather symbol\n is selected\n to enter keys"),
              paste0("Press 'p' to start playing"),
              paste0("Press 'n' to pause the game"),
              paste0("Press 'l' to leave the game"),
              paste0("'z' = up\n's' = down\n'q' = left\n'd' = right\n Or use arrow keys"),
              paste0("The longer it is, the harder it gets!\n Will you reach the legendary snake level? I doubt it but good luck!")))

  # Coordonnees X pour dataframe plot #
  coXMenu <- c(rep(1, 18), rep(2,18), rep(3,18), rep(4,18),
               rep(5,18), rep(6,18), rep(7,18), rep(8,18),
               rep(9,18), rep(10,18),  rep(11, 18), rep(12,18),
               rep(13,18), rep(14,18), rep(15,18), rep(16,18),
               rep(17,18), rep(18,18))

  coYMenu <- rep(seq(18, 1, -1), 18)

  # Main Menu colors c(top, letters, middle, headSnake, bodySnake, bottom) #
  colsMenu <- c("#6fcb9f","black" ,"#996600","#005522", "#009922", "#fb2e01" )

  PlotMainMenu <- function(coordinatesDataMenu, coordxMenu, coordyMenu,stateMenu, colsMenu, label) {

    return(ggplot() +
             geom_tile(data = coordinatesDataMenu, aes(x = coordxMenu, y = coordyMenu, fill = as.factor(stateMenu))) +
             scale_fill_manual(values = alpha(c(colsMenu), c(1, 1, 0.3, 1, 1, 0.7)))  +
             theme_void() +
             theme(legend.position = "none",
                   axis.title.x= element_blank(),
                   axis.text.x= element_blank(),
                   axis.ticks.x= element_blank(),
                   axis.title.y= element_blank(),
                   axis.text.y= element_blank(),
                   axis.ticks.y= element_blank()) +
             geom_label(data = label, aes(label = label, x = x, y = y), color = "#006633",
                        size = 4, label.size = 1.5, fill = "#99FF00"))

  }

  # Head Snake Menu start coordinates #
  menuHead <- list(9, 4)

  # Menu frames #
  menuIteration <- 0

  # Initialisation head ghost menu #
  menuHead_Ghost <- list()

  # Initialisation tail menu #
  tailyMenu <- c(11, 11, 11, 10, 9)
  tailxMenu <- c(1, 2, 3, 3, 3)

  # Initialisation ghost tail menu #
  tailyMenuGhost <- c(1, 1, 1, 1, 1)
  tailxMenuGhost <- c(1, 1, 1, 1, 1)

  # Boolean main menu #
  mainMenuBool <- TRUE

  # Pause initialization #
  Pause <- FALSE

  #### Starting Game ####
  while(TRUE) {

    while (mainMenuBool == TRUE) {

      # Add one to iteration for moving snake menu #
      menuIteration <- menuIteration + 1

      # Reinitialisation iterations pour loop
      if (menuIteration > 10) {

        menuIteration <- 1

      }

      if (!is.na(SnakeEnvir$KeyCode)) {

        if (SnakeEnvir$KeyCode == "l") {

          if(!is.null(dev.list())) dev.off()
          cat(rep("\n", 50))
          cat("\014")
          tcltk::tkdestroy(tt)

          break()

          SnakeEnvir$KeyCode <- NA
        }

        if (SnakeEnvir$KeyCode == "p"){

          message("Go go go little snake!")
          message("")

          flush.console()

          mainMenuBool <- FALSE
          statBoard <- FALSE
          SnakeEnvir$KeyCode <- NA

        }

      } else {
        # Background main menu and logo #
        mainMenuMatrix <- matrix(c(0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 1, 0, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 0, 1, 0, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 0, 1, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 1, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 0, 1, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 0, 1, 0, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 0, 0, 0, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 1, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 1, 1, 0, 1, 1, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5,
                                   0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5),
                                 ncol = 18, nrow = 18)

        # Ghost head snake menu before moving it #
        menuHead_Ghost[[1]] <- menuHead[[1]]
        menuHead_Ghost[[2]] <- menuHead[[2]]

        # Sequence of moving snake

        if (menuIteration == 1) {
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuIteration == 2) {
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuIteration == 3){
          menuHead[[1]] <- menuHead[[1]] + 1

        }
        if (menuIteration == 4){
          menuHead[[1]] <- menuHead[[1]] + 1

        }
        if (menuIteration == 5){
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuIteration == 6){
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuIteration == 7){
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuIteration == 8){
          menuHead[[1]] <- menuHead[[1]] - 1

        }
        if (menuIteration == 9){
          menuHead[[1]] <- menuHead[[1]] - 1

        }
        if (menuIteration == 10){
          menuHead[[2]] <- menuHead[[2]] + 1

        }
        if (menuHead[[2]] == 19) {
          menuHead[[2]] <- 1
        }

        # Move the rest of the booty #
        # Avancee premiere case du snake fantome
        tailyMenuGhost[length(tailyMenuGhost)] <- menuHead_Ghost[[1]]
        tailxMenuGhost[length(tailxMenuGhost)] <- menuHead_Ghost[[2]]

        # Avance du snake fantome #
        for (k in 1:(length(tailyMenuGhost) -1)) {
          tailyMenuGhost[k] <- tailyMenu[k+1]
          tailxMenuGhost[k] <- tailxMenu[k+1]
        }

        # Enregistrement du snake fantome dans le vrai snake #
        for (l in 1:length(tailyMenu)) {

          tailyMenu[l] <- tailyMenuGhost[l]
          tailxMenu[l] <- tailxMenuGhost[l]

        }

        # States of things #
        mainMenuMatrix[menuHead[[1]], menuHead[[2]]] <- 3

        # State tail
        for (i in 1:length(tailyMenu)) {

          mainMenuMatrix[tailyMenu[i], tailxMenu[i]] <- 4

        }

        coordinatesDataMenu <- data.frame(coordxMenu= coXMenu,
                                          coordyMenu = coYMenu,
                                          stateMenu = as.vector(mainMenuMatrix))

        print(PlotMainMenu(coordinatesDataMenu, coordxMenu, coordyMenu,stateMenu, colsMenu, label))

        Sys.sleep(0.25)

      }

    }

    # Code Game #
    # enregistrement position precedente tete, tete fantome
    heady2[1] <- heady[1]
    headx2[1] <- headx[1]

    # Pause feature #
    if (!is.na(SnakeEnvir$KeyCode)) {

      # Pause loop #
      if (SnakeEnvir$KeyCode == "n") {

        Pause <- TRUE

        SnakeEnvir$KeyCode <- NA

        # Emergency pause #
        cat(rep("\n", 50))
        cat("\014")
        if(!is.null(dev.list())) dev.off()

        while (Pause == TRUE) {

          if (!is.na(SnakeEnvir$KeyCode)) {

            if (SnakeEnvir$KeyCode == "n") {

              Pause <- FALSE
              message("Restarting")

              flush.console()

              x11()
              SetFocus(tt)

            }

            if (SnakeEnvir$KeyCode == "l") {
              cat("\014")
              if(!is.null(dev.list())) dev.off()

              tcltk::tkdestroy(tt)

              break()
            }

          }

        }

        SnakeEnvir$KeyCode <- NA

      }
    }

    # Direction tete snake en fonction userKey #
    if (!is.na(SnakeEnvir$KeyCode)) {

      if (SnakeEnvir$KeyCode == "z" | SnakeEnvir$KeyCode == "Up" | SnakeEnvir$KeyCode == "a") {

        if (direction != 2) {

          heady <- heady[1] - 1
          headx <- headx[1]

          direction <- 1

        }
      }

      if (SnakeEnvir$KeyCode == "s" | SnakeEnvir$KeyCode == "Down") {

        if (direction != 1) {

          heady <- heady[1] + 1
          headx <- headx[1]

          direction <- 2

        }
      }

      if (SnakeEnvir$KeyCode == "q" | SnakeEnvir$KeyCode == "Left" | SnakeEnvir$KeyCode == "w") {

        if (direction != 4) {

          heady <- heady[1]
          headx <- headx[1] - 1

          direction <- 3

        }
      }

      if (SnakeEnvir$KeyCode == "d" | SnakeEnvir$KeyCode == "Right"){

        if (direction != 3) {

          heady <- heady[1]
          headx <- headx[1] + 1

          direction <- 4

        }
      }

      if (SnakeEnvir$KeyCode == "l") {

        cat(rep("\n", 50))
        cat("\014")

        if(!is.null(dev.list())) dev.off()

        tcltk::tkdestroy(tt)

        break()
      }

      SnakeEnvir$KeyCode <- NA

    } else {

      if (direction == 1) {

        heady <- heady[1] - 1
        headx <- headx[1]

      }
      if (direction == 2) {

        heady <- heady[1] + 1
        headx <- headx[1]

      }
      if (direction == 3) {

        heady <- heady[1]
        headx <- headx[1] - 1

      }
      if (direction == 4){

        heady <- heady[1]
        headx <- headx[1] + 1

      }
    }

    # Si serpent se cogne contre un mur il passe de l'autre cote #
    if (heady[1] == (yTableLength + 1)) {

      heady <- 1

    } else if (heady[1] == 0) {

      heady <- yTableLength

    } else if (headx[1] == (xTableLength + 1)) {

      headx <- 1

    } else if (headx[1] == 0) {

      headx <- xTableLength

    }

    # Si serpent se mange la queue -> game over #
    for (x in 1:(length(taily)+1)) {

      coordTailCheck <- abs(heady[1] - taily[x+1]) +
        abs(headx[1] - tailx[x+1])

      coordTailCheck2 <- c(coordTailCheck2, coordTailCheck)

    }

    if (is.element(0, coordTailCheck2)*1 != 0) {

      gameOver <- TRUE

    }

    # Mouvement du snake #
    if (gameOver == FALSE) {

      # Si serpent mange un fruit #
      if (heady[1] == fruity[1] & headx[1] == fruitx[1]) {

        hasEaten <- TRUE

        nFruitEaten <- nFruitEaten + 1

        # Speed increase #
        time <- (baseSpeed - (nFruitEaten * speedMultiplier))

        if (sizeBoard == "small") {

          if (time < 0.2 ) {
            time <- 0.2
          }

        } else if (sizeBoard == "normal") {

          if (time < 0.1) {
            time <- 0.1
          }
        } else if (prank == TRUE) {

        if (time <0) {
          time <- 0.001
        }

      }

      # Change color of the snake and levels #
      if (nFruitEaten >= seuil1 & nFruitEaten < seuil2) {

        cols[4] <- "#ff48a5"
        cols[5] <- "#ff0081"

        alphaEvol[1] <- 0.40
        alphaEvol[2] <- 0.40
        alphaEvol[3] <- 0.40

        level <- "Limp Pink Snake"
        levelNumber <- 2

      } else if (nFruitEaten >= seuil2 & nFruitEaten < seuil3) {

        cols[4] <- "#090088"
        cols[5] <- "#010048"

        alphaEvol[1] <- 0.5
        alphaEvol[2] <- 0.5
        alphaEvol[3] <- 0.5

        level <- "Wet Blue Snake"
        levelNumber <- 3

      } else if (nFruitEaten >= seuil3 & nFruitEaten < seuil4) {

        cols[4] <- "#ac1e1e"
        cols[5] <- "#820909"
        cols[6] <- "#352340"

        alphaEvol[1] <- 0.6
        alphaEvol[2] <- 0.6
        alphaEvol[3] <- 0.6

        level <- "Red Hot Snake"
        levelNumber <- 4

      } else if (nFruitEaten >= seuil4 & nFruitEaten < seuil5) {

        cols[4] <- "#352340"
        cols[5] <- "#252339"
        cols[6] <- "#CC1100"

        alphaEvol[1] <- 0.65
        alphaEvol[2] <- 0.65
        alphaEvol[3] <- 0.65

        level <- "Deep Purple Snake"
        levelNumber <- 5

      } else if (nFruitEaten >= seuil5 & nFruitEaten < seuil6) {

        cols[4] <- "#ffcf40"
        cols[5] <- "#560d0d"

        alphaEvol[1] <- 0.7
        alphaEvol[2] <- 0.7
        alphaEvol[3] <- 0.7

        level <- "Huge Golden Snake"
        levelNumber <- 6

      } else if (nFruitEaten >= seuil6) {
        cols[3] <- "#ffdc73"
        cols[2] <- "#ffcf40"
        cols[1] <- "#ffbf00"
        cols[4] <- "#000000"
        cols[5] <- "white"

        alphaEvol[1] <- 0.65
        alphaEvol[2] <- 0.65
        alphaEvol[3] <- 0.65

        level <- "Legendary Black Mamba"
        levelNumber <- 7

      }

      userScore <- userScore + (10 + ((0.35 * nFruitEaten)) - log(timeBetweenFruits) )
      message("Wow you ate something! + ", round(10 + (0.35 * nFruitEaten) - log(timeBetweenFruits),2)," points!")

      flush.console()

      timeBetweenFruits <- 0.01

      while(fruitCoordGo == FALSE) {

        fruity <- sample(1:yTableLength, 1, replace= FALSE)
        fruitx <- sample(1:xTableLength, 1, replace= FALSE)

        for (a in 1:length(taily)) {

          coordFruitCheck <- abs(fruity[1] - taily[a]) +
            abs(fruitx[1] - tailx[a]) +
            (abs(fruity[1] - taily2[a]) +
               abs(fruitx[1] - tailx2[a]))
          coordFruitCheck2 <- c(coordFruitCheck2, coordFruitCheck)

        }

        if ((is.element(0, coordFruitCheck2)*1 == 0) &
            (abs(fruity[1] - heady[1]) + abs(fruitx[1] - headx[1]) != 0) &
            (abs(fruity[1] - heady2[1]) + abs(fruitx[1] - headx2[1]) != 0)) {

          fruitCoordGo <- TRUE
          coordFruitCheck2 <- c()

        } else {

          fruitCoordGo <- FALSE
          coordFruitCheck2 <- c()

        }
      }

      plateauMatrix[fruity, fruitx] <- 3

    }

    fruitCoordGo <- FALSE

    # Si ne mange rien, comportement classique #
    if (hasEaten == FALSE) {

      # Avancee premiere case du snake fantome
      taily2[length(taily)] <- heady2[1]
      tailx2[length(tailx)] <- headx2[1]

      # Avance du snake fantome #
      for (k in 1:(length(taily2 )-1)) {
        taily2[k] <- taily[k+1]
        tailx2[k] <- tailx[k+1]
      }

      # Enregistrement du snake fantome dans le vrai snake #
      for (l in 1:length(taily)) {

        taily[l] <- taily2[l]
        tailx[l] <- tailx2[l]

      }

      plateauMatrix <- CreateEnvironment(theMatrix, heady, headx, taily, tailx, fruity, fruitx)

      # If ate a fruit #
    } else if (hasEaten == TRUE) {

      # Nouvelle case a la place d'avancer
      taily <- c(taily2, heady2[1])
      tailx <- c(tailx2, headx2[1])

      plateauMatrix <- CreateEnvironment(theMatrix, heady, headx, taily, tailx, fruity, fruitx)

    }

    hasEaten <- FALSE

    # Data for the table #
    coordinatesData <- data.frame(coordx= coX,
                                  coordy = coY,
                                  state = as.vector(plateauMatrix))

    # Plot the data #
    # Label stats for plot #
    statDisplay <- paste0("Fruits eaten: ", nFruitEaten,"     Score: ", round(userScore,0), "     Level ", levelNumber, ": ", level)

    # Plot the data #
    print(displayPlotStats(coordx, coordy, state, cols, statDisplay))

    timeElapsed <- timeElapsed + time
    timeBetweenFruits <- timeBetweenFruits + time

    Sys.sleep(time)

    # Si a perdu la partie #
  } else if (gameOver == TRUE) {

    message("")
    message("You lost bro!")

    flush.console()

    # Death animation #
    # Colors ground, tail snake, head snake, fruit #
    # Different ending if reach level max #
    if (levelNumber == 7) {

      coloris <- c("#000000","#111111","#222222", "#FFFFFF", "#FFFFFF", "#000000")

      alphaEvol <- c(1, 1, 1, 1, 1, 1)

    } else {

      coloris <- c("#666666","#999999","#CCCCCC", "black", "black", "#000000")
      alphaEvol <- c(0.3, 0.3, 0.3, 1, 1, 1)

    }

    print(displayPlot(coordx, coordy, state, coloris))

    z <- 1

    # Death animation #
    while (z < ((length(taily)) + 1 + (yTableLength - heady[1])) ) {


      if (z <= length(taily)) {
        # Start falling #
        taily[z] <- taily[z] + 1
        tailx[z] <- tailx[z]

        # Continue falling #
        for (r in 1:z) {

          if ((z - 1) != 0) {

            taily[r-1] <- taily[r-1] + 1
            tailx[r-1] <- tailx[r-1]

          }

        }

        if (taily[r] > yTableLength) {

          taily[r] <- yTableLength
        }


        for (o in 1: length(taily)) {
          if (taily[o] > yTableLength) {
            taily[o] <- yTableLength
          }

        }

      } else {

        e <- 0

        while (e <= yTableLength) {

          e <- e + 1

          heady[1] <- heady[1] + 1

          for (r in 1:length(taily)) {
            taily[r] <- taily[r] + 1
            tailx[r] <- tailx[r]

          }

          if (heady[1] > yTableLength) {
            heady[1] <- yTableLength
          }

          for (o in 1: length(taily)) {
            if (taily[o] > yTableLength) {
              taily[o] <- yTableLength
            }
          }

          # Create the table #
          plateauMatrix <- theMatrix

          # State head
          plateauMatrix[heady, headx] <- 4

          # State tail
          for (h in 1:length(taily)) {

            plateauMatrix[taily[h], tailx[h]] <- 3

          }

          # Data for the table #
          coordinatesData <- data.frame(coordx= coX,
                                        coordy = coY,
                                        state = as.vector(plateauMatrix))

          print(displayPlot(coordx, coordy, state, coloris))

          Sys.sleep(0.05)

        }
      }

      # Create the table #
      plateauMatrix <- theMatrix

      # State head
      plateauMatrix[heady, headx] <- 4

      # State tail
      for (h in 1:length(taily)) {

        plateauMatrix[taily[h], tailx[h]] <- 3

      }

      # Data for the table #
      coordinatesData <- data.frame(coordx= coX,
                                    coordy = coY,
                                    state = as.vector(plateauMatrix))

      print(displayPlot(coordx, coordy, state, coloris))

      z <- z + 1

      Sys.sleep(0.05)
    }

    # Final score and stat plot #
    if (sizeBoard == "small") {

      xLabel = c(5.7, 5.7)
      yLabel = c(6.5, 2.2)

    }

    else if (sizeBoard == "normal") {

      xLabel = c(8, 8)
      yLabel = c(9.5, 2.7)

    }

    label <- data.frame(
      x = xLabel,
      y = yLabel,
      label = c(paste0("GAME OVER LAD\n\n Your stats:\n\n You ate ", nFruitEaten,
                       " bloody fruits\n\n Your score is: ",
                       round(userScore, 0), "\n\n You reached level ", levelNumber,
                       ", with da ", level, "!\n\n Your max speed was ",
                       round((1/time),2)," squares per sec\n\n You wasted ", round(timeElapsed/60, 2), " minute(s) on this session"),
                paste0("Press 'p' to start a new game\n Press 'l' to exit you noob")))

    print(ggplot() +
            geom_tile(data = coordinatesData, aes(x = coordx, y = coordy, fill = as.factor(state))) +
            scale_fill_manual(values = alpha(c(coloris), alphaEvol))  +
            theme_void() +
            theme(legend.position = "none",
                  axis.title.x= element_blank(),
                  axis.text.x= element_blank(),
                  axis.ticks.x= element_blank(),
                  axis.title.y= element_blank(),
                  axis.text.y= element_blank(),
                  axis.ticks.y= element_blank()) +
            geom_label(data = label, aes(label = label, x = x, y = y), color = "#006633",
                       size = 6, label.size = 2, fill = "#99FF00"))

    Sys.sleep(0.05)

    statBoard <- TRUE

    while(statBoard == TRUE) {

      if (!is.na(SnakeEnvir$KeyCode)) {

        if (SnakeEnvir$KeyCode == "p") {

          mainMenuBool <- TRUE

          gameOver <- FALSE

          statBoard <- FALSE

          #### Repeat this untill finding a better solution ####

          # Colors ground, tail snake, head snake, fruit #
          cols <- c("#028900","#74d600","#adff00", "#089000","#0a5d00",  "#CC1100")

          # Initiate first level alphas #
          alphaEvol <- c(0.3, 0.3, 0.3, 1, 1, 1)

          # CLean graph history sometimes #
          iterations <-  0

          # Level tresholds #
          if (sizeBoard == "small") {

            seuil1 <- 15
            seuil2 <- 30
            seuil3 <- 40
            seuil4 <- 50
            seuil5 <- 60
            seuil6 <- 70

          } else if (sizeBoard == "normal") {

            seuil1 <- 15
            seuil2 <- 30
            seuil3 <- 45
            seuil4 <- 60
            seuil5 <- 75
            seuil6 <- 90
          }

          if (cheatCode == "thereisnocowlevel") {
            # PlayTest tresholds #
            seuil1 <- 2
            seuil2 <- 4
            seuil3 <- 6
            seuil4 <- 8
            seuil5 <- 10
            seuil6 <- 12
          }

          # Board size depending on options #
          if (sizeBoard == "normal") {

            yTableLength <- 15
            xTableLength <- 15

          } else if (sizeBoard == "small") {

            yTableLength <- 10
            xTableLength <- 10

          }

          # Fill the table with 0 #
          plateauMatrix <- matrix(nrow = yTableLength, ncol = xTableLength)
          plateauMatrix[] <- 0 # state table

          # Tail
          if (sizeBoard == "small") {

            taily <- c(5,5)
            tailx <- c(2,3)

            # Head
            heady <- c(5)
            headx <- c(4)

            # Coordonnees pour dataframe plot #
            coX <- c(rep(1, yTableLength),
                     rep(2, yTableLength),
                     rep(3, yTableLength),
                     rep(4, yTableLength),
                     rep(5, yTableLength),
                     rep(6, yTableLength),
                     rep(7, yTableLength),
                     rep(8, yTableLength),
                     rep(9, yTableLength),
                     rep(10, yTableLength))

            coY <- rep(seq(10, 1, -1), 10)

            # Create the environement and states of assets #
            theMatrix <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
                                  0, 1, 1, 1, 1, 0, 1, 1, 0, 0,
                                  1, 1, 2, 1, 1, 1, 1, 0, 0, 1,
                                  1, 2, 2, 2, 1, 1, 0, 0, 0, 1,
                                  1, 1, 2, 2, 1, 0, 0, 0, 1, 2,
                                  0, 1, 2, 1, 1, 0, 0, 0, 1, 1,
                                  0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
                                  0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
                                  0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
                                  1, 1, 0, 0, 1, 0, 1, 1, 1, 2),
                                ncol = 10, nrow = 10)

          } else if (sizeBoard == "normal") {

            taily <- c(7,7)
            tailx <- c(2,3)

            # Head
            heady <- c(7)
            headx <- c(4)

            # Coordonnees pour dataframe plot #
            coX <- c(rep(1, yTableLength),
                     rep(2, yTableLength),
                     rep(3, yTableLength),
                     rep(4, yTableLength),
                     rep(5, yTableLength),
                     rep(6, yTableLength),
                     rep(7, yTableLength),
                     rep(8, yTableLength),
                     rep(9, yTableLength),
                     rep(10, yTableLength),
                     rep(11, yTableLength),
                     rep(12, yTableLength),
                     rep(13, yTableLength),
                     rep(14, yTableLength),
                     rep(15, yTableLength))

            coY <- rep(seq(xTableLength, 1, -1), yTableLength)

            # Create the environement and states of assets #
            theMatrix <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 2,
                                  0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1,
                                  1, 1, 2, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0,
                                  1, 2, 2, 2, 1, 1, 0, 0, 1, 1, 2, 1, 0, 0, 0,
                                  1, 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 1, 0, 0,
                                  0, 1, 2, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,
                                  0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                  0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1,
                                  0, 0, 0, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                                  0, 1, 1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1,
                                  1, 1, 1, 1, 2, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                                  1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0,
                                  0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0),
                                ncol = 15, nrow = 15)

          }

          taily2 <- rep(1, length(taily))
          tailx2 <- rep(1, length(tailx))

          heady2 <- c()
          headx2 <- c()

          taily2 <- c(rep(1, length(taily)))
          tailx2 <- c(rep(1, length(taily)))

          # Generate random first fruit coordinates #
          firstFruitGo <- FALSE

          while (firstFruitGo == FALSE) {

            fruity <- sample(1:yTableLength , 1, replace = TRUE)
            fruitx <- sample(1:xTableLength , 1, replace = TRUE)

            if( (fruity[1] == heady[1] & fruitx[1] == headx[1]) &
                (fruity[1] == taily[1] & fruitx[1] == tailx[1]) &
                (fruity[1] == taily[2] & fruitx[1] == tailx[2]) ) {

              firstFruitGo <- FALSE

            } else {

              firstFruitGo <- TRUE

            }
          }

          # State fruit #
          plateauMatrix[fruity, fruitx] <- 3

          # State head
          plateauMatrix[heady, headx] <- 2

          # State tail
          for (i in 1:length(taily)) {

            plateauMatrix[taily[i], tailx[i]] <- 1

          }

          # Create first environment
          plateauMatrix <- CreateEnvironment(theMatrix, heady, headx, taily, tailx, fruity, fruitx)

          # Data for the table #
          coordinatesData <- data.frame(coordx= coX,
                                        coordy = coY,
                                        state = as.vector(plateauMatrix))

          # Comptage des points #
          nFruitEaten <- 0
          userScore <- 0

          # set Initial level #
          level <- "Green Baby Snake"
          levelNumber <- 1

          # Label stats for plot #
          statDisplay <- paste0("Fruits eaten: ", nFruitEaten,"     Score: ", round(userScore,0), "     Level ", levelNumber, ": ", level)

          # Plot the data #
          displayPlotStats <- function(coordx, coordy, state, cols, statDisplay) {

            return(ggplot(coordinatesData, aes(x = coordx, y = coordy, fill = as.factor(state))) +
                     geom_tile() +
                     scale_fill_manual(values = alpha(c(cols), alphaEvol))  +
                     theme_void() +
                     xlab(statDisplay) +
                     theme(legend.position = "none",
                           axis.title.x= element_blank(),
                           axis.text.x= element_blank(),
                           axis.ticks.x= element_blank(),
                           axis.title.y= element_blank(),
                           axis.text.y= element_blank(),
                           axis.ticks.y= element_blank()) +
                     annotate(geom="label", x= sum(seq(1,xTableLength,1))/xTableLength, y= -0.3,
                              label= statDisplay, color="#006633", fill = "#99FF00", size = 5))

          }

          # Boolean for eating a fruit #
          hasEaten <- FALSE

          # Game over #
          gameOver <- FALSE
          coordTailCheck2 <- c()

          # Fruit coordinate check #
          fruitCoordGo <- FALSE
          coordFruitCheck2 <- c()

          # Initialisation du temps passe #
          timeElapsed <- 0
          timeBetweenFruits <- 0.01

          # Direction generale du snake par default #
          direction <- 4

          # Main Menu #
          # Out of the loop #

          # Indications on how to play #
          label <- data.frame(
            x = c(3, 9.5, 9.5, 9.5, 16, 9.5),
            y = c(4.5, 5.5, 4.5,3.5, 4.5, 1.5),
            label = c(paste0("Make sure the\n window with the\n feather symbol\n is selected\n to enter keys"),
                      paste0("Press 'p' to start playing"),
                      paste0("Press 'n' to pause the game"),
                      paste0("Press 'l' to leave the game"),
                      paste0("'z' = up\n's' = down\n'q' = left\n'd' = right\n Or use arrow keys"),
                      paste0("The longer it is, the harder it gets!\n Will you reach the legendary snake level? I doubt it but good luck!")))

          PlotMainMenu <- function(coordinatesDataMenu, coordxMenu, coordyMenu,stateMenu, colsMenu, label) {

            return(ggplot() +
                     geom_tile(data = coordinatesDataMenu, aes(x = coordxMenu, y = coordyMenu, fill = as.factor(stateMenu))) +
                     scale_fill_manual(values = alpha(c(colsMenu), c(1, 1, 0.3, 1, 1, 0.7)))  +
                     theme_void() +
                     theme(legend.position = "none",
                           axis.title.x= element_blank(),
                           axis.text.x= element_blank(),
                           axis.ticks.x= element_blank(),
                           axis.title.y= element_blank(),
                           axis.text.y= element_blank(),
                           axis.ticks.y= element_blank()) +
                     geom_label(data = label, aes(label = label, x = x, y = y), color = "#006633",
                                size = 4, label.size = 1.5, fill = "#99FF00"))

          }

          # Head Snake Menu start coordinates #
          menuHead <- list(9, 4)

          # Menu frames #
          menuIteration <- 0

          # Initialisation head ghost menu #
          menuHead_Ghost <- list()

          # Initialisation tail menu #
          tailyMenu <- c(11, 11, 11, 10, 9)
          tailxMenu <- c(1, 2, 3, 3, 3)

          # Initialisation ghost tail menu #
          tailyMenuGhost <- c(1, 1, 1, 1, 1)
          tailxMenuGhost <- c(1, 1, 1, 1, 1)

          # Boolean main menu #
          mainMenuBool <- TRUE

          # Pause initialization #
          Pause <- FALSE

          SnakeEnvir$KeyCode <- NA

        } else if (SnakeEnvir$KeyCode == "l") {

          statBoard <- FALSE

          if(!is.null(dev.list())) dev.off()
          cat(rep("\n", 50))
          cat("\014")

          tcltk::tkdestroy(tt)

          break()

        }
      }
    }
  }
  }
}
