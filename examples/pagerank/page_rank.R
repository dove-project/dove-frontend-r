# the number of rows & columns of input matrix
nodes <- 10

page_rank <- function(M) {
    ## Arguments
    ## =========
    ## M: n by n adjacency matrix
    ## 
    ## Output
    ## =========
    ## v: output of page-rank execution engine after 40 iterations
    ##
    
    d <- 0.8
    N <- nrow(M)
    C_d <- dove.wrap(d)
    # create a random nodes by 1 matrix from unif[0,1)
    v <- dove.matrix(nrow = nodes, ncol = 1, rand = TRUE)
    norm_one <- sum(abs(v))
    v <- v / norm_one
   
    M_hat <- (M * C_d) + dove.wrap((1-d) / N)
    iters <- 40
    dove.for(1, iters, 1, function(i) {
        v[,] <- M_hat %*% v
    })

    v
}

source('dove.R')
M <- dove.matrix("sample", nrow=nodes, ncol=nodes)
r <- page_rank(M)
