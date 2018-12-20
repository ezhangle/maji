./bin/majic sample/s1.maji -o bin/s1.bcr
echo ""
../bytecode/bin/bcr -p bin/s1.bcr
RET=$?
echo "----"
echo "return value is ${RET}"
