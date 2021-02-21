mlkit: make_parser.sml
	mlkit -o lambda-nq lambda-nq.mlb

mlton: make_parser.sml
	mlton -output lambda-nq -default-ann 'warnUnused true' lambda-nq.mlb

make_parser.sml: make_parser.cmyacc
	cmyacc -o make_parser.sml make_parser.cmyacc

.PHONY: mlkit mlton
