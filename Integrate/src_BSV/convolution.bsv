package convolution;
import TubeHeader::*;
import FIFOF::*;
import datatypes::*;
import Vector::*;
import pulse::*;
import FixedPoint::*;
import mul::*;
import FIFOF::*;
import distribute::*;
import tree::*;

//#define DSP 256

(*synthesize*)
module mkConvBlock(ConvBlock);
	
	//################################## DataStructures  #########################	
	Distribute px <- mkDistribute;
	Distribute fx <- mkDistribute;
	TreeSum    tx <- mkTreeSum;
	Reg#(int) clk <- mkReg(0);
	Mult _PE[256];


	rule _CLK;
		clk <= clk + 1;
	endrule
	
	for(int i=0; i<fromInteger(256); i = i + 1)
		_PE[i] <- mkMult;

	rule _pushMAC;
		 let window <- px.result;
		 let coeffs <- fx.result;
		 for(int i=0; i<fromInteger(256); i = i + 1) begin			
			_PE[i].a(window[i]);
			_PE[i].b(coeffs[i]);
		 end
	endrule

	rule _SUM;
		Vector#(DspMax,DataType) res = replicate(0);
		for(int i=0; i<fromInteger(256); i = i + 1)
			res[i] <- _PE[i].out;
		//$display(" RECEIVING FROM MULTIPLIER @clk %d value %d ", clk, fxptGetInt(res[0]));
		tx.put(res);
	endrule
				
	method Action sendP(Vector#(VectorLength, DataType) datas);
			px.put(datas);
	endmethod

		
	method Action sendF(Vector#(DspMax, CoeffType) filter);
			fx.put2(filter);
	endmethod
	
	method ActionValue#(Vector#(DspMax, DataType)) result;
			let d <- tx.result;
			return d;
	endmethod
	
	method Action reset(Int#(8) id);
		$display(" CLEARING THE CONVOLUTION BLOCK @clk %d ", clk);
		px.clean(id);
		fx.clean(id);
		tx.clean(id);
		for(int i=0; i<fromInteger(256); i = i + 1)
			_PE[i].clean;
	endmethod

endmodule
endpackage
