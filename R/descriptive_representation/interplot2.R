interplot2 <- function (m, var1, var2, plot = TRUE, steps = NULL, ci = 0.95, 
          adjCI = FALSE, hist = FALSE, var2_dt = NA, predPro = FALSE, 
          var2_vals = NULL, point = FALSE, sims = 5000, xmin = NA, 
          xmax = NA, ercolor = NA, esize = 0.5, ralpha = 0.5, rfill = "grey70", 
          stats_cp = "none", txt_caption = NULL, facet_labs = NULL, 
          ...) 
{
    m.class <- class(m)
    if (m.class == "list") {
        m.sims <- m %>% 
            map_df(~ arm::sim(.x, sims/100) %>% 
                       pluck("fixef") %>% 
                       as_tibble())
        m <- m[[1]]
    } else m.sims <- arm::sim(m, sims) %>% 
            pluck("fixef") %>% 
            as_tibble()
    factor_v1 <- factor_v2 <- FALSE
    if (is.factor(eval(parse(text = paste0("m@frame$", var1)))) & 
        is.factor(eval(parse(text = paste0("m@frame$", var2))))) 
        stop("The function does not support interactions between two factors.")
    if (is.factor(eval(parse(text = paste0("m@frame$", var1))))) {
        var1_bk <- var1
        var1 <- paste0(var1, levels(eval(parse(text = paste0("m@frame$", 
                                                             var1)))))
        factor_v1 <- TRUE
        ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), 
               var12 <- paste0(var2, ":", var1)[-1])
        for (i in seq(var12)) {
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                var12[i] <- paste0(var1, ":", var2)[-1][i]
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                stop(paste("Model does not include the interaction of", 
                           var1, "and", var2, "."))
        }
    } else if (is.factor(eval(parse(text = paste0("m@frame$", 
                                                var2))))) {
        var2_bk <- var2
        var2 <- paste0(var2, levels(eval(parse(text = paste0("m@frame$", 
                                                             var2)))))
        factor_v2 <- TRUE
        ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), 
               var12 <- paste0(var2, ":", var1)[-1])
        for (i in seq(var12)) {
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                var12[i] <- paste0(var1, ":", var2)[-1][i]
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                stop(paste("Model does not include the interaction of", 
                           var1, "and", var2, "."))
        }
    } else {
        ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), 
               var12 <- paste0(var2, ":", var1))
        for (i in seq(var12)) {
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                var12[i] <- paste0(var1, ":", var2)[i]
            if (!var12[i] %in% unlist(dimnames(m@pp$X)[2])) 
                stop(paste("Model does not include the interaction of", 
                           var1, "and", var2, "."))
        }
    }
    if (factor_v2) {
        xmin <- 0
        xmax <- 1
        steps <- 2
    } else {
        if (is.na(xmin)) 
            xmin <- min(m@frame[var2], na.rm = T)
        if (is.na(xmax)) 
            xmax <- max(m@frame[var2], na.rm = T)
        if (is.null(steps)) {
            steps <- eval(parse(text = paste0("length(unique(na.omit(m@frame$", 
                                              var2, ")))")))
        }
        if (steps > 100) 
            steps <- 100
    }
    coef <- data.frame(fake = seq(xmin, xmax, length.out = steps), 
                       coef1 = NA, ub = NA, lb = NA)
    coef_df <- data.frame(fake = numeric(0), coef1 = numeric(0), 
                          ub = numeric(0), lb = numeric(0), model = character(0))
    if (factor_v1) {
        if (predPro == TRUE) 
            stop("The current version does not support estimating predicted probabilities for factor base terms.")
        for (j in 1:(length(levels(eval(parse(text = paste0("m@frame$", 
                                                            var1_bk))))) - 1)) {
            for (i in 1:steps) {
                coef$coef1[i] <- mean(first(m.sims[, match(var1[j + 
                                                                    1], unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                          m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]))
                coef$ub[i] <- quantile(first(m.sims[, match(var1[j + 
                                                                     1], unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                           m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]), 
                                       (1 - ci)/2)
                coef$lb[i] <- quantile(first(m.sims[, match(var1[j + 
                                                                     1], unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                           m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]), 
                                       1 - (1 - ci)/2)
            }
            if (plot == TRUE) {
                coef$value <- var1[j + 1]
                coef_df <- rbind(coef_df, coef)
                if (hist == TRUE) {
                    if (is.na(var2_dt)) {
                        var2_dt <- eval(parse(text = paste0("m@frame$", 
                                                            var2)))
                    }
                    else {
                        var2_dt <- var2_dt
                    }
                }
            }
            else {
                names(coef) <- c(var2, "coef", "ub", "lb")
                return(coef)
            }
        }
        if (is.null(facet_labs)) 
            facet_labs <- unique(coef_df$value)
        coef_df$value <- factor(coef_df$value, labels = facet_labs)
        interplot:::interplot.plot(m = coef_df, hist = hist, steps = steps, 
                       var2_dt = var2_dt, point = point, ercolor = ercolor, 
                       esize = esize, ralpha = ralpha, rfill = rfill, stats_cp = "none", 
                       txt_caption = NULL, ...) + facet_grid(. ~ value)
    } else if (factor_v2) {
        if (predPro == TRUE) 
            stop("The current version does not support estimating predicted probabilities for factor base terms.")
        for (j in 1:(length(levels(eval(parse(text = paste0("m@frame$", 
                                                            var2_bk))))) - 1)) {
            for (i in 1:steps) {
                coef$coef1[i] <- mean(first(m.sims[, match(var1, 
                                                           unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                          m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]))
                coef$ub[i] <- quantile(first(m.sims[, match(var1, 
                                                            unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                           m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]), 
                                       (1 - ci)/2)
                coef$lb[i] <- quantile(first(m.sims[, match(var1, 
                                                            unlist(dimnames(m@pp$X)[2]))] + coef$fake[i] * 
                                           m.sims[, match(var12[j], unlist(dimnames(m@pp$X)[2]))]), 
                                       1 - (1 - ci)/2)
            }
            if (plot == TRUE) {
                coef$value <- var2[j + 1]
                coef_df <- rbind(coef_df, coef)
                if (hist == TRUE) {
                    if (is.na(var2_dt)) {
                        var2_dt <- eval(parse(text = paste0("m@frame$", 
                                                            var2)))
                    }
                    else {
                        var2_dt <- var2_dt
                    }
                }
            }
            else {
                names(coef) <- c(var2, "coef", "ub", "lb")
                return(coef)
            }
        }
        if (is.null(facet_labs)) 
            facet_labs <- unique(coef_df$value)
        coef_df$value <- factor(coef_df$value, labels = facet_labs)
        interplot:::interplot.plot(m = coef_df, steps = steps, hist = hist, 
                       var2_dt = var2_dt, point = point, ercolor = ercolor, 
                       esize = esize, ralpha = ralpha, rfill = rfill, stats_cp = "none", 
                       txt_caption = NULL, ...) + facet_grid(. ~ value)
    } else {
        if (predPro == TRUE) {
            if (is.null(var2_vals)) 
                stop("The predicted probabilities cannot be estimated without defining 'var2_vals'.")
            df <- data.frame(m$model)
            df[[names(m@flist)]] <- NULL
            if (sum(grep("X.weights.", names(df))) != 0) 
                df <- select(df, -X.weights.)
            df_temp <- select(df, 1)
            df <- df[-1] %>% map(function(var) {
                if (is.factor(var)) {
                    model.matrix(~var - 1)[, -1] %>% as.data.frame()
                }
                else {
                    as.numeric(var)
                }
            })
            for (i in seq(df)) {
                if (!is.data.frame(df[[i]])) {
                    namesUpdate <- c(names(df_temp), names(df)[[i]])
                    df_temp <- cbind(df_temp, df[[i]])
                    names(df_temp) <- namesUpdate
                }
                else {
                    df_temp <- cbind(df_temp, df[[i]])
                }
            }
            df <- df_temp
            names(df)[1] <- "(Intercept)"
            df$`(Intercept)` <- 1
            if (var1 == var2) {
                names(df) <- sub("I\\.(.*)\\.2\\.", "I\\(\\1\\^2\\)", 
                                 names(df))
            }
            iv_medians <- summarize_all(df, funs(median(., na.rm = TRUE)))
            fake_data <- iv_medians[rep(1:nrow(iv_medians), 
                                        each = steps * length(var2_vals)), ]
            fake_data[[var1]] <- with(df, rep(seq(min(get(var1)), 
                                                  max(get(var1)), length.out = steps), steps = length(var2_vals)))
            fake_data[[var2]] <- rep(var2_vals, each = steps)
            fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]
            pp <- rowMeans(plogis(data.matrix(fake_data) %*% 
                                      t(data.matrix(m.sims))))
            row_quantiles <- function(x, probs) {
                naValue <- NA
                storage.mode(naValue) <- storage.mode(x)
                nrow <- nrow(x)
                q <- matrix(naValue, nrow = nrow, ncol = length(probs))
                if (nrow > 0L) {
                    t <- quantile(x[1L, ], probs = probs)
                    colnames(q) <- names(t)
                    q[1L, ] <- t
                    if (nrow >= 2L) {
                        for (rr in 2:nrow) {
                            q[rr, ] <- quantile(x[rr, ], probs = probs)
                        }
                    }
                }
                else {
                    t <- quantile(0, probs = probs)
                    colnames(q) <- names(t)
                }
                q <- drop(q)
                q
            }
            pp_bounds <- row_quantiles(plogis(data.matrix(fake_data) %*% 
                                                  t(data.matrix(m.sims))), prob = c((1 - 
                                                                                               ci)/2, 1 - (1 - ci)/2))
            pp <- cbind(pp, pp_bounds)
            pp <- pp * 100
            colnames(pp) <- c("coef1", "lb", "ub")
            pp <- cbind(fake_data[, c(var1, var2)], pp)
            pp[, var2] <- as.factor(pp[, var2])
            names(pp)[1] <- "fake"
            names(pp)[2] <- "value"
            coef <- pp
        } else {
            multiplier <- ifelse(var1 == var2, 2, 1) 
            
            for (i in 1:steps) {
                coef$coef1[i] <- mean(first(m.sims[, match(var1, 
                                                     unlist(dimnames(m@pp$X)[2]))] + {multiplier * 
                                          coef$fake[i] * m.sims[, match(var12, 
                                                                        unlist(dimnames(m@pp$X)[2]))]}))
                coef$ub[i] <- quantile(first(m.sims[, match(var1, 
                                                      unlist(dimnames(m@pp$X)[2]))] + multiplier * 
                                           coef$fake[i] * m.sims[, match(var12, 
                                                                         unlist(dimnames(m@pp$X)[2]))]), (1 - ci)/2)
                coef$lb[i] <- quantile(first(m.sims[, match(var1, 
                                                      unlist(dimnames(m@pp$X)[2]))] + multiplier * 
                                           coef$fake[i] * m.sims[, match(var12, 
                                                                         unlist(dimnames(m@pp$X)[2]))]), 1 - (1 - ci)/2)
            }
        }
        multiplier <- ifelse(var1 == var2, 2, 1) 
        min_sim <- m.sims[, match(var1, unlist(dimnames(m@pp$X)[2]))] + 
            multiplier * xmin * m.sims[, match(var12, 
                                                     unlist(dimnames(m@pp$X)[2]))]
        max_sim <- m.sims[, match(var1, unlist(dimnames(m@pp$X)[2]))] + 
            multiplier * xmax * m.sims[, match(var12, 
                                                     unlist(dimnames(m@pp$X)[2]))]
        diff <- first(max_sim - min_sim)
        ci_diff <- c(quantile(diff, (1 - ci)/2), quantile(diff, 
                                                          1 - (1 - ci)/2))
        if (plot == TRUE) {
            if (hist == TRUE) {
                if (is.na(var2_dt)) {
                    var2_dt <- eval(parse(text = paste0("m@frame$", 
                                                        var2)))
                }
                else {
                    var2_dt <- var2_dt
                }
            }
            interplot:::interplot.plot(m = coef, steps = steps, hist = hist, 
                           predPro = predPro, var2_vals = var2_vals, var2_dt = var2_dt, 
                           point = point, ercolor = ercolor, esize = esize, 
                           ralpha = ralpha, rfill = rfill, stats_cp = "none", 
                           txt_caption = NULL, ...)
        } else {
            if (predPro == TRUE) {
                names(coef) <- c(var2, paste0("values_in_", 
                                              var1), "coef", "ub", "lb")
            }
            else {
                names(coef) <- c(var2, "coef", "ub", "lb")
            }
            return(coef)
        }
    }
}
