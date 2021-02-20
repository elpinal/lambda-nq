mlkit: make_parser.sml
	mlkit -o lambda-nq lambda-nq.mlb

make_parser.sml: make_parser.cmyacc
	cmyacc -o make_parser.sml make_parser.cmyacc

.PHONY: mlkit
