package tree;
import FixedPoint::*;
import pulse::*;
import FIFO::*;
import datatypes::*;
import Real::*;
import Vector::*;


#define VOUT 16

interface TreeSum;
	method Action put(Vector#(VOUT, DataType) datas);
        method ActionValue#(Vector#(VOUT,DataType)) result(UInt#(8) level);
        method Action clean;
endinterface

module mkTreeSum(TreeSum);

	Integer paddedSize = VOUT;
	Integer steps = log2(VOUT);
	Reg#(Vector#(VOUT,DataType)) _reductionTree[steps+1];
   	Pulse _redPulse[steps+1];

   	for(int i=0;i< fromInteger(steps)+1; i = i+1)
        	_redPulse[i] <- mkPulse;

	UInt#(10) powers[10];
   	powers[0] = 2;
   	powers[1] = 4;
   	powers[2] = 8;
   	powers[3] = 16;
   	powers[4] = 32;
   	powers[5] = 64;
  	powers[6] = 128;
   	powers[7] = 256;
   	powers[8] = 512;
   	powers[9] = 1024;


	for(UInt#(10) i=0; i< fromInteger(steps)+1; i = i + 1)
                        _reductionTree[i] <- mkRegU;


        for(int l=0; l<fromInteger(steps); l = l + 1)
        rule level;
                _redPulse[l].ishigh;
		Vector#(VOUT,DataType) temp = _reductionTree[l];
		Vector#(VOUT,DataType) out =  replicate(0);
                	for(UInt#(10) i=0;i < fromInteger(paddedSize)/powers[l]; i = i +1) begin
                        	DataType d = fxptTruncate(fxptAdd(temp[i],  temp[fromInteger(paddedSize)/powers[l] + i]));
				out[i] = d;
                	end
		_reductionTree[l+1] <= out; 
                _redPulse[l+1].send;
        endrule
	
	
	method Action put(Vector#(VOUT, DataType) datas);
                _reductionTree[0] <= datas;
		_redPulse[0].send;
	endmethod

	
        method ActionValue#(Vector#(VOUT,DataType)) result(UInt#(8) level);
			_redPulse[level].ishigh;
			return _reductionTree[level];	
	endmethod
	
endmodule
endpackage
