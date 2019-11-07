calculateMisCoverage <- function(ei2doutput, data1, data2, nbins = 30) {
  ## allei <- ei2doutput$allei ## Changes made by Thiyanga
  allei <- ei2doutput
  data1.ei = allei %>% dplyr::filter(col == data1)
  data2.ei = allei %>% dplyr::filter(col == data2)
  xfrom = min(data1.ei$x, data2.ei$x) ## Instead of Comp1 notation x is used- Thiyanga
  xto = max(data1.ei$x, data2.ei$x) ## Instead of Comp2 notation y is used- Thiyanga
  yfrom = min(data1.ei$y, data2.ei$y)
  yto = max(data1.ei$y, data2.ei$y)
  data1.bincounts = hist2d(data1.ei$x, y = data1.ei$y, nbins = nbins,
                           xfrom = xfrom , xto = xto, yfrom = yfrom, yto = yto,
                           same.scale = FALSE, show = FALSE)$counts
  data2.bincounts = hist2d(data2.ei$x, y = data2.ei$y, nbins = nbins,
                           xfrom = xfrom , xto = xto, yfrom = yfrom, yto = yto,
                           same.scale = FALSE, show = FALSE)$counts
  data1.bincounts[data1.bincounts > 0] = 1
  data2.bincounts[data2.bincounts > 0] = 1
  return(
  sum(data1.bincounts == 0 & data2.bincounts == 1) / nbins ^ 2)
}

##