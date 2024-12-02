VERILOG:=iverilog

haskell: module.hs
	@echo "Compiling..."
	@ghc module.hs -o haskell
	@echo "Running Haskell sample..."
	@echo -n "Result 0: "
	@./haskell 1 < in0.txt
	@echo -n "Result 1: "
	@./haskell 1 < in1.txt
	@echo -n "Result 2: "
	@./haskell 2 < in1.txt


verilog: module.v testbench.v
	@$(VERILOG) -o testbench.vvp testbench.v module.v
	@vvp testbench.vvp

clean:
	@rm -f testbench.vvp module
