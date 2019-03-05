package tree;
import TubeHeader::*;
import FixedPoint::*;
import pulse::*;
import FIFO::*;
import FIFOF::*;
import datatypes::*;
import Real::*;
import Vector::*;

//#define DSP 256

(*synthesize*) 
module mkTreeSum(TreeSum);

	Integer paddedSize = 256;
	Integer steps = log2(256);
	Reg#(Int#(8)) _KERNEL <- mkReg(3);
	FIFOF#(Vector#(DspMax,DataType)) _reductionTree[steps+1];
	FIFOF#(Vector#(DspMax,DataType)) _outQ <- mkFIFOF;
	Wire#(Bool) _l <- mkWire;
	Wire#(Vector#(DspMax,DataType)) x <- mkWire;
	Reg#(Bool)    _rebut <- mkReg(False);
	Reg#(Int#(4)) _FR <- mkReg(0);

	Reg#(int) clk <- mkReg(0);
	rule _CLK;
		clk <= clk + 1;
	endrule
		
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


	for(UInt#(10) i=0; i< fromInteger(steps)+1; i = i + 1) begin
                      _reductionTree[i] <- mkFIFOF;	
		
	end


        for(Int#(8) l=0; l<fromInteger(steps); l = l + 1)
        rule level (l <= _KERNEL && _rebut == False);
		//$display(" 						level %d active at @clk %d ", l, clk); 
		Vector#(DspMax,DataType) out = _reductionTree[l].first; _reductionTree[l].deq;
		Vector#(DspMax,DataType) temp =  replicate(0);
                	for(UInt#(10) i=0;i < fromInteger(paddedSize)/powers[l]; i = i + 1) begin
                        	DataType d = fxptTruncate(fxptAdd(out[2*i],  out[2*i+1]));
				temp[i] = d;
                	end
		//if(l == _KERNEL) begin
		//	_outQ.enq(temp);
			//_l <= True;
			//x <= temp;
		//end
		//	_outQ.enq(temp);
		//else
			//if(l < fromInteger(steps-1))
			_reductionTree[l+1].enq(temp);
        endrule

	/*rule _reset(_rebut == True && _FR > 0);
                _outQ.clear;
                for(UInt#(10) i=0; i< fromInteger(steps)+1; i = i + 1)
                        _reductionTree[i].clear;
                _FR <= _FR - 1;
        endrule*/
	
	method Action put(Vector#(DspMax, DataType) datas) if(_reductionTree[2].notFull);
                _reductionTree[0].enq(datas);
	endmethod

	
        method ActionValue#(Vector#(DspMax,DataType)) result;
			let d = _reductionTree[4].first; _reductionTree[4].deq;
			//_reductionTree[steps].deq;
			//let d = _outQ.first; _outQ.deq;
			//_reductionTree[steps].deq;
			//$display(" ON THE TREE @clk %d ", clk);
			return d;
	endmethod
	method Action clean(Int#(8) _KER);
		if(_KER == 3)
                        _KERNEL <= 3;
                if(_KER == 5)
                        _KERNEL <= 4;
                if(_KER == 7)
                        _KERNEL <= 5;
                if(_KER == 9)
                        _KERNEL <= 6;
                if(_KER == 11)
                        _KERNEL <= 6;
		 _outQ.clear;
                for(UInt#(10) i=0; i< fromInteger(steps)+1; i = i + 1)
                        _reductionTree[i].clear;

                                 //_rebut <= True;
                                 //_FR <= 2;
        endmethod

        /*method Action  cleaned if(_FR == 0);
                       _rebut <= False;
        endmethod*/

endmodule
endpackage
