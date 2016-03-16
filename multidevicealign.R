
deviceComputation = function(deviceRawData) {
  computeInitMatrix(deviceRawData)
  computeUpdateMatrixs(deviceRawData)
  # computeForce(deviceRawData)
}

iterInitRotationMatrix = function(x1, y1, z1, x2, y2, z2){
  rtmInit = getRotationMatrix(x1, y1, z1, x2, y2, z2)
  globalAccs = computeGlobalAccByGyro(rtmInit, gyros)
}

computeObject = function(resultantDataMultiDevices, dataXmultiDevices, dataYmultiDevices, dataZmultiDevices) {
  pcaMag = pcaMagnitude(resultantDataMultiDevices)
  pcaX = pcaX(dataXmultiDevices)
  pcaY = pcaY(dataYmultiDevices)
  pcaZ = pcaZ(dataZmultiDevices)
  deltaPca = pcaMag - sqrt(pcaX * pcaX - pcaY * pcaY - pcaZ * pcaZ)
}