ModelObject = setRefClass(Class = 'ModelObject',
                          fields = list(
                            name = 'character',
                            expr = 'expression'
                          ),
                          methods = list(
                            value = function(p, data){
                              "Evaluates a Gompertz-like Function"
                              eval(.self$expr, c(as.list(p), as.list(data)))
                            },
                            jacobian = function(p, data){
                              "Calculates the Jacobian"
                              J = t(sapply(all.vars(.self$expr), function(v, p, data){
                                eval(D(.self$expr, v), c(as.list(p), as.list(data)))
                              }, p=p, data=data))

                              return(J[names(p),,drop=F])
                            },
                            gradient = function(p, data){
                              "Calculates the Gradient"
                              r = data$y - value(p, data)
                              return(-jacobian(p, data) %*% r)
                            },
                            hessian = function(p, data){
                              "Calculates the Hesian"
                              J = jacobian(p, data)
                              return(J %*% t(J))
                            }
                          )
)

