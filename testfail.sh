#!/bin/sh

# TODO:  

# Save the current value of OCAMLRUNPARAM
old_ocamlrunparam=$OCAMLRUNPARAM

# Unset OCAMLRUNPARAM
unset OCAMLRUNPARAM

# Path to the Rooc compiler. 
# Try "_build/install/default/bin/Rooc" if ocamlbuild was unable to create a symbolic link.
Rooc="Rooc"
#Rooc="_build/install/default/bin/Rooc"

LLC="llc"
CC="cc"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.rooc files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}


SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.rooc//'`
    reffile=`echo $1 | sed 's/.rooc$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."
    # echo $reffile
    # echo $basedir

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    # keepfiles="${basename}.ll" #keep all the .ll files automatically
    # generatedfiles="$generatedfiles ${basename}.s ${basename}.exe ${basename}.out" &&

    keepfiles="" 
    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "dune exec $Rooc" "$1" ">" "${basename}.ll" && # generate result
    Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" &&
    Run "./${basename}.exe" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.std ${basename}.diff

    # Report the status and clean up the generated files
    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    # %TODO: Now no check failed test. 
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.rooc//'`
    reffile=`echo $1 | sed 's/.rooc$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "dune exec $Rooc" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.std ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/negative/test-fail-*.rooc"
fi

# echo $files




for file in $files
do
    # echo $file
    case $file in
	*test-success*)
	    Check $file 2>> $globallog
	    ;;
	*test-fail*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror

# Reset OCAMLRUNPARAM to its original value
export OCAMLRUNPARAM=$old_ocamlrunparam
