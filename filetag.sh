find . \( -name .svn -o -wholename ./classes \) -prune -o -not -iregex ".*\.\(jar\|gif\|jpg\|class\|exe\|dll\|pdd\|sw[op]\|xls\|doc\|pdf\|zip\|tar\|ico\|ear\|war\|dat\|svn\|git/\).*" -type f -printf "%f\t%p\t1\n" > ./filenametags 

#find . \( -name .svn -o -wholename ./classes \) -prune -o -not -iregex "'.*\.\(jar\|gif\|jpg\|class\|exe\|dll\|pdd\|sw[op]\|xls\|doc\|pdf\|zip\|tar\|ico\|ear\|war\|dat\|svn\|git\).*'" -type f -printf "%f\t%p\t1\n" > ./filenametags 
#find . -not ( -iname "*.class" -o -iname "*.jar" -o -iname "*.svn-base" ) -type f -printf "%%f\t%%p\t1\n" | sort -f > ./filenametags
#find . -not -iregex ".*\.\(jar\|gif\|jpg\|class\|exe\|dll\|pdd\|sw[op]\|xls\|doc\|pdf\|zip\|tar\|ico\|ear\|war\|dat\|svn\|git\).*" -type f -printf "%%f\t%%p\t1\n" | sort -f > ./filenametags
