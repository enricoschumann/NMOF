## http://dlmf.nist.gov/3.5

tab <- list(nodes = 0, weights = 2)
res <- xwGauss(n =  1L, "legendre")
expect_equal(tab, res)

tab <- structure(list(nodes =
                          c(-0.97390652851717, -0.865063366688983,
                            -0.679409568299024, -0.433395394129246,
                            -0.148874338981631, 0.148874338981632,
                            0.433395394129248, 0.679409568299025,
                            0.865063366688984, 0.973906528517172),
                      weights =
                          c(0.0666713443086876, 0.149451349150583,
                            0.219086362515984, 0.269266719309995,
                            0.29552422471475, 0.295524224714753,
                            0.269266719309995, 0.219086362515983,
                            0.149451349150581, 0.066671344308688)),
                 .Names = c("nodes", "weights"))
res <- xwGauss(n =  10L, "legendre")
expect_equal(tab, res)

##xwGauss(n = 200, "legendre")

##xwGauss(n =   1, "laguerre")
##xwGauss(n =  10, "laguerre")
##xwGauss(n = 200, "laguerre")

##xwGauss(n =   1, "hermite")
##xwGauss(n =  10, "hermite")
##xwGauss(n = 200, "hermite")

expect_error(xwGauss(n = 1, "yo"))
expect_error(xwGauss(n = 0, "legendre"))
