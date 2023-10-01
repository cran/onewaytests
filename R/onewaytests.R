onewaytests <- function(formula, data, method=c("aov", "af", "ag", "agp", "ap", "aw", "b2", "bf", "box", "cochran",
                                                "gtb","gtf", "james", "johansen", "kw", "mbf", "pf", 
                                                "ss", "wa", "welch", "welch_tw", "wgf"), 
                                                N = 10^5, rate = 0.1, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  if(method=="af") return(out<- af.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="ag") return(out <- ag.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="agp") return(out<- agp.test(formula, data, alpha = alpha, N = N, na.rm = na.rm, verbose = verbose))
  if(method=="aov") return(out <- aov.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="ap") return(out<- ap.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="aw") return(out <- aw.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="b2") return(out<- b2.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="bf") return(out <- bf.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="box") return(out<- box.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="cochran") return(out <- cochran.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="gtb") return(out<- gp.test(formula, data, method = "gtb", alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="gtf") return(out<- gp.test(formula, data, method = "gtf", alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="james") return(out <- james.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="johansen") return(out<- johansen.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="kw") return(out <- kw.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="mbf") return(out<- mbf.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="pf") return(out <- pf.test(formula, data, N = N, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="ss") return(out<- ss.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="wa") return(out <- wa.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="welch") return(out<- welch.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = verbose))
  if(method=="welch_tw") return(out<- welch.test(formula, data, alpha = alpha, rate = rate, na.rm = na.rm, verbose = verbose))
  if(method=="wgf") return(out <- wgf.test(formula, data, alpha = alpha, N = N, na.rm = na.rm, verbose = verbose))
  }