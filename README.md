# SnakeGame
Launch a colourful fully-fledged game of Snake in R, with levels, an animated main menu, a defeat animation, an alternative ending, a "safe for work" pause button, a reasonable amount of bugs, among other features! Perfect for a little break, a workplace leaderboard or boring meetings!

## Features
- An animated main menu from which you can start or leave the game
- A fully-fledge colourful game of the classic snake game, displayed with the ggplot2 r package
- Seven levels to discover, with the snake and background changing colors
- An alternative ending if you manage to reach the last "legendary" level (don't expect too much though)
- A defeat animation
- A session statistic resume at the end of the session
- "Safe for work" pause and exit buttons, which clear all graphs and console
- Relatively fluid user input for the commands, using the tcltk package. All credits to the original author of this method from the r CRAN - Package Snake, from whom I shamelessly stole this part of the code: https://cran.r-project.org/web/packages/Snake/index.html
- The ability to cross field boundaries
- The speed increases linearly with the number of fruits eaten
- The points scored from eating fruits depend on the the total amount of fruits already eaten, as well as the speed at which you get the fruit, to reward risk-taking
- 2 different size of fields, one small and one normal (add sizeBoard = "normal" or "small") in the SnakeGame() function
- A couple of not-investigated-yet known display bugs, that do not compromise the gameplay
