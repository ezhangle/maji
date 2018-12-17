./bin/majic sample/s1.maji -o s1.bcr
echo ""
../bytecode/bin/bcr -p s1.bcr
RET=$?
echo "----"
echo "return value is ${RET}"
