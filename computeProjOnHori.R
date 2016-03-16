computeProjOnHori = function(lv, gv) {
  absProj = ((
    lv[1] * gv[1] + lv[2] * gv[2] + lv[3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  pg = gv/absVector(gv[1],gv[2],gv[3])*absProj
  return(cbind(lv[1] - pg[1], lv[2] - pg[2], lv[3] - pg[3]))
}