toleranceFactor = function( n, mode){
  if (strcmp(mode, 'TwoSided')) {
    initVector = c(32.019, 32.019, 8.380, 5.369, 4.275, 3.712, 3.369, 3.136, 2.967, 2.839,
                  2.737, 2.655, 2.587, 2.529, 2.48, 2.437, 2.4, 2.366, 2.337, 2.31,
                  2.31, 2.31, 2.31, 2.31, 2.208)
    if (n < 30){
      factor = initVector[min(n, 25)]
    }else if (n >= 30 && n < 35){
      factor = 2.14
    }else if (n >= 35 && n < 40){
      factor = 2.09
    }else if (n >= 40 && n < 45){
      factor = 2.052
    }else if (n >= 45 && n < 50){
      factor = 2.021
    }else if (n >= 50 && n < 60){
      factor = 1.996
    }else if (n >= 60 && n < 70){
      factor = 1.958
    }else if (n >= 70 && n < 80){
      factor = 1.929
    }else if (n >= 80 && n < 90){
      factor = 1.907
    }else if (n >= 90 && n < 100){
      factor = 1.889
    }else if (n >= 100 && n < 150){
      factor = 1.874
    }else if (n >= 150){
      factor = 1.825
    }
  }else if (strcmp(mode, 'OneSided')){
    initVector = c(20.581, 20.581, 6.156, 4.162, 3.407, 3.006, 2.756, 2.582, 2.454, 2.355, 
                2.275, 2.21, 2.155, 2.109, 2.068, 2.033, 2.002, 1.974, 1.949, 1.926, 
                1.926, 1.926, 1.926, 1.926, 1.838)
    if (n < 30){
      factor = initVector[min(n, 25)]
    }else if (n >= 30 && n < 35){
      factor = 1.777
    }else if (n >= 35 && n < 40){
      factor = 1.732
    }else if (n >= 40 && n < 45){
      factor = 1.697
    }else if (n >= 45 && n < 50){
      factor = 1.669
    }else if (n >= 50 && n < 60){
      factor = 1.646
    }else if (n >= 60 && n < 70){
      factor = 1.609
    }else if (n >= 70 && n < 80){
      factor = 1.581
    }else if (n >= 80 && n < 90){
      factor = 1.559
    }else if (n >= 90 && n < 100){
      factor = 1.542
    }else if (n >= 100 && n < 150){
      factor = 1.527
    }else if (n >= 150){
      factor = 1.478
    }
  }
  return(factor)
  
}