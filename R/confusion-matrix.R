
.area_segment = function(theta) {
  return((theta - sin(theta))/2)
}

confusion_circle = function(TP,TN,FP,FN) {
  condition_pos = TP+FN
  condition_neg = TN+FP
  test_pos = TP+FP
  test_neg = TN+FN


}
