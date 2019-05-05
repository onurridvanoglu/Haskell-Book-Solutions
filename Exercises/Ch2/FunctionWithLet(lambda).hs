--Different representation of "let" expression
printInc2' n = 
    (\plusTwo -> print plusTwo) (n + 2)