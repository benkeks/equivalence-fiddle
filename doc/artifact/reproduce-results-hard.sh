#!

echo "Starting the equivalence-fiddle container if it is not yet running..."

docker start equivalence-fiddle || docker run -p 127.0.0.1:8080:8080 --name equivalence-fiddle -d equivalence-fiddle

echo "-----"
echo "Reproducing results for all examples of Table 1 (ordered by state count)..."

echo "The following will reproduce the lines peterson, vasy_0_1, vasy_1_4, vasy_5_9, cwi_3_14, vasy_8_24, vasy_8_38, vasy_25_25, cwi_1_2, vasy_10_56, vasy_18_73."

echo "-----"
echo "Clever energy game spectroscopy:"
echo "  The rows of the output and the table correspond like this: 1->system; 2->P; 5->P/B; 7->9,t/s; 9->8,↣▲"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark  --include-hard --timeout=600000"

echo "-----"
echo "Formula game spectroscopy of [BJN2022]:"
echo "  Many will timeout."
echo "  The interesting rows of the output and the table correspond like this: 7->5,t/s; 9->4,[4]-↣"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark  --formula-spectroscopy --include-hard --timeout=600000"

echo "-----"
echo "Unclever energy game spectroscopy:"
echo "  The interesting rows of the output and the table correspond like this: 7->7,t/s; 9->6,↣△"
echo "-----"

docker exec -it equivalence-fiddle sbt -warn "shared/run benchmark --unclever-spectroscopy --include-hard  --timeout=600000"

echo "-----"
echo "All done!"
echo "Bye!"
