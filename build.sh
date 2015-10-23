#!/bin/bash

# ctags -R src --exclude=target --exclude=vendor
# mvn clean scala:compile scala:testCompile
# mvn clean resources:resources resources:testResources scala:compile scala:testCompile surefire:test

#!/bin/bash

echo "Tips:"
echo "-c compile"
echo "-t test"
echo "-a all"
echo "-r run main func"
echo "-e REPL"

while getopts "b:ctrae" arg #选项后面的冒号表示该选项需要参数
do
	case $arg in
		c)
			ctags -R src --exclude=target --exclude=vendor
			mvn compile test-compile
			;;
		t)
			mvn resources:resources resources:testResources surefire:test -Dtest=CommonTest
			;;
		a)
			ctags -R src --exclude=target --exclude=vendor
			# mvn compile test-compile resources:resources resources:testResources surefire:test
			# mvn compile test-compile resources:resources resources:testResources surefire:test -Dtest=CommonTest
				mvn clean compile test-compile resources:resources resources:testResources surefire:test -Dtest=SqliteDaoTest
			#  mvn clean compile test-compile resources:resources resources:testResources surefire:test -Dtest=SessionTest
			;;
		r)
			# mvn resources:resources scala:run -Dlauncher=foo
			  mvn resources:resources scala:run -DmainClass=example.ScalaAppExample
			;;
		e)
			# ctags -R src --exclude=target --exclude=vendor
			mvn compile resources:resources resources:testResources scala:console
			;;
		b)
			echo "b's arg:$OPTARG" #参数存在$OPTARG中
			;;
		?)  #当有不认识的选项的时候arg为?
			exit 1
			;;
	esac
done
