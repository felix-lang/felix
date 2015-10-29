./newversion.sh $1
git commit -a -m "New version `python3 showversion.py`"
git tag `python3 showversion.py` -n "release `python3 showversion.py`"
git push --follow-tags
