choose n k = product [n - k + 1 .. n] / product [1 .. k]

fs = 5

os = 6

xs = 5

foxCombos = choose 16 fs * choose (16 - fs) os * choose (16 - fs - os) xs

ofxCombos = choose 16 os * choose (16 - os) fs * choose (16 - os - fs) xs

main = do
  print foxCombos
  print ofxCombos
