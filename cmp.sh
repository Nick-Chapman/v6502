#n=5000
#rm -f t1 t2
#head -$n $1 > t1
#head -$n $2 > t2
#git diff --no-index t1 t2
#rm t1 t2

git diff --no-index $1 $2
