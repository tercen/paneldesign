m1 = 1
m2 = 0.2
m3 = 0.5

m1m2 = 1
m1m3 = 0
m2m1 = 1

m2m3 = 0
m3m1 = 0
m3m2 = 0

f1 = 1
f2 = 0.8
f3 = 0.2

f1f2 = 0.9
f1f3 = 0.7
f2f1 = 0.2

f2f3 = 0.5
f3f1 = 0.2
f3f2 = 0.1

rscore = function(mA, mB,
                  fA, fB,
                  SAB, SBA,
                  mAB) {
  delta = abs(mA * fA - mB * fB)
  spread = (mA * fA * SAB + mB * fB * SBA) * mAB
  total = spread + delta
}

s1 = rscore(m1, m2, f1, f2, f1f2, f2f1, m1m2)

rscore(m[])
