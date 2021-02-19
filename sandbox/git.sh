for i in `git branch -r|grep -v HEAD|grep -v master|sed 's:origin/::g'`
do
        git checkout --track -b $i origin/$i
done
git checkout master
