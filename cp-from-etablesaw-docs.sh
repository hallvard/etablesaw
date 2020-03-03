echo $1
cp ../etablesaw/etablesaw/etablesaw.docs/* .
git add .
git commit -m "$1"
git push
