package conv3;
import FIFOF::*;
import datatypes::*;
import Vector::*;
import pulse::*;
import FixedPoint::*;
import TubeHeader::*;
import mul::*;
import FIFOF::*;

(*synthesize*)
module mkConv3(Conv);
	
	//################################## DataStructures  #########################	
	FIFOF#(Bit#(144)) _inputQ <- mkFIFOF; 
	Wire#(DataType) window[9];
	Wire#(Bool) wc[9];
	Wire#(Bool) cl <- mkWire;
	Reg#(Bool) w <- mkReg(False);
	FIFOF#(DataType) accumulator1[9];
	FIFOF#(DataType) accumulator2[3];
	Reg#(DataType) accumulator3 <- mkReg(0);
	Reg#(DataType) acc4 <- mkReg(0);
	Reg#(DataType) acc5 <- mkReg(0);
	Reg#(DataType) acc6 <- mkReg(0);
	Reg#(CoeffType) coeffs[9];
	Pulse a0 <- mkPulse;
	Pulse a1 <- mkPulse;
	Reg#(int) clk <- mkReg(0);
	Reg#(Bit#(1)) p0 <- mkReg(0);
	Reg#(Bit#(1)) c0 <- mkReg(0);
	Reg#(Bit#(1)) p3 <- mkReg(0);
        Reg#(Bit#(1)) c3 <- mkReg(0);
	Mult _PE[9];
	FIFOF#(DataType) _outQ <- mkFIFOF;
	FIFOF#(DataType) _Q <- mkSizedFIFOF(8);
	
		for(int j= 0; j<9; j = j + 1) begin
			window[j] <- mkWire;
			if(j == 4)
			coeffs[j] <- mkReg(1);
			else
			coeffs[j] <- mkReg(0);
			_PE[j] <- mkMult;
			wc[j] <- mkWire;
			accumulator1[j] <- mkFIFOF;
		end
		for(int k=0; k<3; k = k +1)
			accumulator2[k] <- mkFIFOF;
	//##################################################################################

	rule _clk;
		clk <= clk + 1;
	endrule

	rule _input_decompose;
		let  packet = _inputQ.first; _inputQ.deq;
		Vector#(9, DataType) wb = unpack(packet);
		
		for(int _Elem = 0; _Elem < 9 ; _Elem  = _Elem + 1) begin
				window[_Elem] <= wb[_Elem];
				wc[_Elem] <= True;
		end	
	endrule

	
	
		for(int _Elem=0 ;_Elem< 9; _Elem = _Elem + 1) begin
				rule _pushMAC(wc[_Elem] == True);
					_PE[_Elem].a(window[_Elem]);
					
					_PE[_Elem].b(coeffs[_Elem]);
				endrule
				
				rule _ac1;
					let d <- _PE[_Elem].out;
					accumulator1[_Elem].enq(d);
				endrule
		end 

				rule _ac2;
					accumulator1[0].deq;
					accumulator1[1].deq;
					accumulator1[2].deq;
					accumulator1[3].deq;
					accumulator1[4].deq;
					accumulator1[5].deq;
					accumulator1[6].deq;
					accumulator1[7].deq;
					accumulator1[8].deq;
					DataType a = fxptTruncate(fxptAdd(accumulator1[0].first,fxptAdd(accumulator1[3].first, accumulator1[4].first)));
					DataType b = fxptTruncate(fxptAdd(accumulator1[1].first,fxptAdd(accumulator1[5].first, accumulator1[6].first)));
					DataType c = fxptTruncate(fxptAdd(accumulator1[2].first,fxptAdd(accumulator1[7].first, accumulator1[8].first)));
					accumulator2[0].enq(a);
					accumulator2[1].enq(b);
					accumulator2[2].enq(c);
                                endrule

				rule _ac3;
					accumulator2[0].deq;
					accumulator2[1].deq;
					accumulator2[2].deq;
					DataType d = fxptTruncate(fxptAdd(accumulator2[0].first, fxptAdd(accumulator2[1].first, accumulator2[2].first)));
					_outQ.enq(d);
				endrule

				rule _clean (cl == True); 
                                        a0.clean;
                                        a1.clean;
                                        for(int j= 0; j<9; j = j + 1)
                                                        _PE[j].clean;
                                        _Q.clear;
                                        _outQ.clear;
                                        _inputQ.clear;
                                        p0 <= 0;
                                        c0 <= 0;
                                endrule

 
	
	method Action sendP(Vector#(VectorLength, DataType) datas) if(_outQ.notFull);
		 Vector#(9, DataType) m = newVector;
		 for(int j=0; j<9; j= j + 1) begin
				m[j] = datas[j];
		 end
		_inputQ.enq(pack(m));	
	endmethod

		
	method Action sendF(Vector#(VectorLength, CoeffType) filter);
			for(int j=0; j<9; j= j + 1)
					coeffs[j] <= filter[j];
	endmethod
	
	method ActionValue#(DataType) result;
			let d = _outQ.first; _outQ.deq;
			return d;
	endmethod

	method Action clean;
		cl <= True;
        endmethod
	
endmodule
endpackage
