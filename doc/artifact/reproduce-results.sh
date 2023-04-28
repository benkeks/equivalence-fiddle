#!

echo "Starting the equivalence-fiddle container if it is not yet running..."

docker start equivalence-fiddle || docker run -p 127.0.0.1:8080:8080 --name equivalence-fiddle -d equivalence-fiddle

echo "-----"
echo "Reproducing the easier examples of Table 1 (ordered by state count)..."

echo "The following will reproduce the lines peterson, vasy_0_1, vasy_1_4, vasy_5_9, cwi_3_14, vasy_8_24, vasy_8_38, vasy_25_25."

echo "-----"
echo "Clever energy game spectroscopy:"
echo "  The rows of the output and the table correspond like this: 1->system; 2->P; 5->P/B; 7->9,t/s; 9->8,↣▲; 10->P/E; 11->P/T; 12->P/1S"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark --reduced-sizes"

echo "-----"
echo "Formula game spectroscopy of [BJN2022]:"
echo "  peterson will timeout (which is the motivating example of the paper!); vasy_8_24 will timeout as well"
echo "  The interesting rows of the output and the table correspond like this: 7->5,t/s; 9->4,[4]-↣"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark --timeout=10000 --formula-spectroscopy"
# --reduced-sizes
# If you want, you can remove the 10-second timeout or set it to a much higher number. I've tried up to 10 minutes. (Then, vasy_8_24 might work.)

echo "-----"
echo "Unclever energy game spectroscopy:"
echo "  The interesting rows of the output and the table correspond like this: 7->7,t/s; 9->6,↣△"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark --unclever-spectroscopy"

echo "-----"
echo "All done!"
echo "If you've got time, you can also run this script including the harder examples, with more timeouts in ./reproduce-results-hard.sh"
echo "Bye!"
