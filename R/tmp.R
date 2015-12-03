
permIntVec.One <- function(alpha, omega, cnum){

    # Check if the parameters are integers
    if(is.integer(alpha) & is.integer(omega) & is.integer(cnum)){
        # If alpha and omega are the equal, do nothing.
        if(alpha==omega) {
            break()
        } else {
            # alpha and omega are not equal then check for direction
            l2r <- logical()

            if(alpha < omega){
                l2r <- TRUE

            } else l2r <- FALSE

            xnum <- vector(mode = "integer") # container
            browser()

            # Move item from left to rigth.
            if(l2r==TRUE){

            } else {
                # Move item from right to left
                part2 <- seq(omega, alpha-1)
                if(alpha<cnum) part3 <- seq(alpha+1, cnum) else part3 <- NULL
                if(omega>1) part1 <- seq(1,omega-1) else part1 <- NULL
                xnum <- c(part1, alpha, part2, part3)
            }

        }


    } else stop("Function's parameters must be all integers.")

    return(xnum)
}
