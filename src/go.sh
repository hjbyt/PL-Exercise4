
./build.sh

#echo '****************************************'
#echo 'Enter the following:'
#echo '#load_rec "parser.cmo";;'
#echo 'open Parser;;'
#echo '****************************************'

utop -I . -init initfile utils.cmo lexer.cmo parser.cmo reducer.cmo

