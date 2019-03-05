package Compose;
import TubeHeader::*;
import Vector::*;
import datatypes::*;
import FIFO::*;
import FIFOF::*;
import pulse::*;
import Stage::*;
import pool2::*;
import pool::*;
import BRAMFIFO::*;
import FixedPoint::*;
import bramfifo::*;
import BRam::*;
import mul::*;
import bram::*;
import lineBuffer::*;
import lineBuffer2::*;
import store::*;
import conv3::*;
import conv5::*;
import convolution::*;
import coalescedMemory::*;

// ############################################## SOURCE COMPONENT PRESSURIZED ###############################################
module mkSource#(Integer numOutputs, Integer buffer[], Integer _rate)(Component);
FIFOF#(DataType) instream[_rate];
FIFOF#(DataType) outstream[_rate][numOutputs];
Reg#(DataType) _in[_rate];
FIFOF#(Bit#(1))           _p[_rate];

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1) begin

	instream[k] <- mkSizedFIFOF(4);
        _in[k]	    <- mkReg(0);
        _p[k]	    <- mkFIFOF;
	for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
		outstream[k][i] <- mkSizedFIFOF(buffer[i]);
	end

end

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1) begin

rule _pop;
		let d = instream[k].first;
		//$display(" receiving in source %d ", fxptGetInt(d));
		instream[k].deq;
		for(int i=0; i<fromInteger(numOutputs); i = i+1)
			outstream[k][i].enq(d);
endrule

end

method Action send(DataType dat, BramLength i);
        instream[i].enq(dat);
endmethod

method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
                                res[k] = outstream[k][0].first;
                                outstream[k][0].deq;
                        end
                        return res;
endmethod

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = outstream[_rate][i].first; outstream[_rate][i].deq;
        return d;
endmethod

method Action clean;
		for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1)  begin
				instream[k].clear;
				_p[k].clear;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        outstream[k][i].clear;
                end

endmethod

endmodule

// ########################################################################################################################################################


// ########################################################## CONVOLVER PRESURRIZED #######################################################################
module mkConvolver#(Component inp, Integer inputID, Integer stencil, Integer numOutputs, Integer buffer[], Integer roof)(Component);
Conv _PE[roof];
FIFOF#(DataType) out[roof][numOutputs];
Reg#(int) clk <- mkReg(0);

rule _CLK;
        clk <= clk + 1;
endrule

		for(UInt#(10) k = 0;k < fromInteger(roof); k = k+1)
		        for(int i=0;i<fromInteger(numOutputs); i = i+1)
                		out[k][i] <- mkSizedFIFOF(buffer[i]);

	        if(stencil == 3) begin
                        for(int k = 0; k<fromInteger(roof); k = k+1)
                                _PE[k] <- mkConv3;
                end
                else if(stencil == 5) begin
                        for(int k = 0; k<fromInteger(roof); k = k+1)
                                _PE[k] <- mkConv5;
                end

		rule inputx;
                                let  d <- inp.receiveVector(fromInteger(inputID));
				for(UInt#(10) i=0; i < fromInteger(roof); i = i + 1) begin
					Vector#(VectorLength, DataType) vx = replicate(0);
						for(UInt#(10) k=0; k < fromInteger(stencil*stencil); k = k + 1) begin
							vx[k] = d[i*fromInteger(stencil) + k];
						end

					_PE[i].sendP(vx);
				end

		endrule

		for(int k=0; k < fromInteger(roof); k = k + 1)
		rule _output;
				//_p[k].deq;
                		let d <- _PE[k].result;
                		for(int i=0;i<fromInteger(numOutputs); i = i+1)
                        		out[k][i].enq(d);
		endrule



		method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        		let d = out[_rate][i].first; out[_rate][i].deq;
        		return d;
		endmethod

        	method Action sendVector(Vector#(VectorLength, DataType) inx);
				for(int i=0 ;i<fromInteger(roof); i = i + 1)
					_PE[i].sendF(unpack(pack(inx)));
		endmethod

		method Action clean;
        		for(UInt#(10) k = 0; k< fromInteger(roof); k = k + 1) begin
				_PE[k].clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;
			end
                endmethod
endmodule
//#####################################################################################################################################################################

// ######################################### RELAY BUFFER PRESURRIZED ############################################################################################
module mkRelayBuffer#(Component inp, Integer inputID, Integer row, Integer col, Integer numOutputs, Integer buffer[], Integer numRelays, Relay re[], Integer img, Integer rate, Integer rstride, Integer cstride)(Component);

LineBuffer line <- mkLineBuffer(row,col,img,rate, rstride, cstride);
Reg#(DataType) _in[rate];
FIFOF#(DataType) _out[row*col][numOutputs];
Pulse           _p <- mkPulse;
Pulse           _q <- mkPulse;
BFIFO                            forwardingQ[rate][numRelays];
BramWidth                       rindex = fromInteger(col/2);
Integer                         rx = col/2;
Reg#(BramWidth)                  r <- mkReg(0);
Reg#(BramWidth)                  index[numRelays];
Reg#(Bool)                      actRelays[numRelays];
Reg#(BramWidth)                  c <- mkReg(rindex);
Reg#(BramWidth)                  c1 <- mkReg(rindex);
Reg#(Vector#(VectorLength,DataType))			fQ <- mkRegU;
Reg#(int) clk <- mkReg(0);

	rule _CLK;
			clk <= clk + 1;
	endrule 

	for(UInt#(10) k = 0; k< fromInteger(rate); k = k+1) begin
        	 _in[k] <- mkReg(0);

	end

        for(int i=0;i<fromInteger(numRelays); i = i+1) begin
			index[i] <- mkReg(0);
			actRelays[i] <- mkReg(False);
	end
	 
	for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
        	for(UInt#(10) i = 0; i < fromInteger(numOutputs); i = i+1)
                	_out[k][i] <- mkSizedFIFOF(buffer[i]);
	end


	for(UInt#(10) k = 0;k < fromInteger(rate); k = k+1)
        for(int i=0;i<fromInteger(numRelays); i = i+1)begin 
                forwardingQ[k][i] <- mkBramFifo(re[i].size);
		end

	rule _relay;
                if(c == fromInteger(img)-rindex-1) begin
                        r <= r + fromInteger(rate);
                        c <= rindex;
                end
                else
                        c <= c + 1;
                let d <- line.relay;
		//$display(" %d %d %d  C = %d @clk %d ", fxptGetInt(d[0]), fxptGetInt(d[1]), fxptGetInt(d[2]), c, clk);
		fQ <= d;
                for(int i=0 ; i< fromInteger(numRelays); i = i+1) begin
			Integer rx2 = re[i].row1;
			if(r >= fromInteger(re[i].row1-rx) && r <= fromInteger(re[i].row2-rx)) begin
				if(actRelays[i] == False) begin
					actRelays[i] <= True;
					index[i] <= fromInteger(rx2)-r;
				end
				if(i == 0)
					c1 <= c;
			end
			
			if( actRelays[i] == True)
				if(c1 >=  fromInteger(re[i].col1) && c1 < fromInteger(re[i].col2))
					for(BramWidth j = 0; j< fromInteger(rate); j = j + 1) begin
						//$display(" enqueuing %d   c = %d r = %d @clk %d relay %d ", fxptGetInt(fQ[index[i]]) , c1, index[i],clk,i);
                                		forwardingQ[j][i].enq(fQ[index[i] + j]);
						//$display(" enqueuing %d ", fxptGetInt(fQ[index[i]]));
					end
		end
	endrule
		
	rule _input;
			Vector#(VectorLength, DataType) v = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(rate); k = k+1) begin
                                let  d <- inp.receive(fromInteger(inputID), k);
                                v[k] = d;
                        end
                        line.put(v);
	endrule


	rule get;
                        let d <- line.get;
                        for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
                                for(int i=0 ;i<fromInteger(numOutputs); i = i + 1)
                                        _out[k][i].enq(d[k]);
                        end

	endrule

	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
                                res[k] = _out[k][_ID].first;
                                _out[k][_ID].deq;
                        end
                        return res;
	endmethod

	method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
                let d = _out[_rate][i].first; _out[_rate][i].deq;
                return d;
	endmethod

	method Action clean;
                _p.clean;
                _q.clean;
                line.clean;

                for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1)
                                for(int i=0 ;i<fromInteger(numOutputs); i = i + 1)
                                       _out[k][i].clear;

		for(UInt#(10) k = 0;k < fromInteger(rate); k = k+1)
        		for(int i=0;i<fromInteger(numRelays); i = i+1)
                			forwardingQ[k][i].clean;
           


		for(int i=0;i<fromInteger(numRelays); i = i+1)
				actRelays[i] <= False;

		r <= 0;
		c <= rindex-1;

	endmethod

	method Action cleaned;
                line.cleaned;
	endmethod

	method ActionValue#(DataType) forwarded(BramLength i, UInt#(10) _rate);
            let d <- forwardingQ[_rate][i].deq;
            return d;
	endmethod

endmodule
// #############################################################################################################################################################

module mkTile#(Component inp, Integer inputID, Integer row, Integer col, Integer img, Integer numOutputs, Integer buffer[], Integer _rate, Integer rstride, Integer cstride)(Component);
LineBuffer line <- mkLineBuffer(row,col,img,_rate, rstride, cstride);
Reg#(DataType) _in[_rate];
FIFOF#(DataType) _out[row*col][numOutputs];
Pulse           _p <- mkPulse;
Pulse           _q <- mkPulse;

Reg#(int) clk <- mkReg(0);

rule _CLK;
        clk <= clk + 1;
endrule

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
         _in[k] <- mkReg(0);

end

for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
	for(UInt#(10) i = 0; i < fromInteger(numOutputs); i = i+1)
		_out[k][i] <- mkSizedFIFOF(buffer[i]);
end

rule _input;
			
                        Vector#(VectorLength,DataType) v = replicate(0);
			for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
                        	let  d <- inp.receive(fromInteger(inputID), k);
                        	v[k] = d;
			end

                        line.put(v);
endrule

rule get;
			let d <- line.get;
			
			/*if(print == True)
			for(int i=0; i<30; i = i + 1)
				$display(" %d ", fxptGetInt(d[i]));*/
			
			//$display(" ################################# ");
			
			for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
				for(int i=0 ;i<fromInteger(numOutputs); i = i + 1)
					_out[k][i].enq(d[k]);
			end

endrule

method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
			Vector#(VectorLength, DataType) res = replicate(0);
			for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1) begin
				res[k] = _out[k][_ID].first;
				_out[k][_ID].deq;
			end
			return res;
endmethod

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        	let d = _out[_rate][i].first; _out[_rate][i].deq;
        	return d;
endmethod

method Action clean;
		_p.clean;
		_q.clean;
		line.clean;
		for(UInt#(10) k = 0; k< fromInteger(row*col); k = k+1)
                                for(int i=0 ;i<fromInteger(numOutputs); i = i + 1)
                                        _out[k][i].clear;
endmethod

method Action cleaned;
        	line.cleaned;
endmethod

method Action reset(Int#(16) param);
	line.reboot(param);	
endmethod

endmodule

//###########################################################################################################################
module mkStorageComponent#(Component inp, Integer width, Integer outWidth, Integer numOutputs, Integer _DC, Bool flushOut)(Component);
Reg#(DataType) _in[width];
Int#(8) _DRAM = fromInteger(outWidth);
Int#(8) _DW = fromInteger(width);
Reg#(Bit#(1))   c[width];
Reg#(UInt#(16)) dataC <- mkReg(0);
Reg#(UInt#(16)) flushed <- mkReg(0);
Reg#(UInt#(16)) _DataCount <- mkReg(fromInteger(_DC));
Reg#(Bit#(1)) p                 <- mkReg(0);
BFIFO2 flushQ[width];
Reg#(Int#(8)) dr <- mkReg(0);
Reg#(int) clk <- mkReg(0);
Reg#(Bool) ready <- mkReg(False);
Pulse flushDone <- mkPulse;
FIFOF#(DataType) out[outWidth][numOutputs];

        for(int i = 0; i<fromInteger(width); i = i + 1) begin
                        _in[i] <- mkReg(0);
                        c[i] <- mkReg(0);
			flushQ[i] <- mkStore;
        end



	rule _CLK;
		clk <= clk + 1;
	endrule
	
	for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k+1)
        	for(UInt#(10) i = 0; i < fromInteger(numOutputs); i = i+1)
                	out[k][i] <- mkFIFOF;


        rule _input;
                        let d <- inp.receiveVector(0);
                        for(int i = 0; i < fromInteger(width); i = i + 1)
                                _in[i] <= d[i];
                        p <= ~p;
                        dataC <= dataC + 1;
        endrule

        for(UInt#(10) k = 0; k < fromInteger(width); k = k + 1)
                        rule _write(( p ^ c[k]) == 1);
                                c[k] <= p;

                                if(dataC <= _DataCount+2) begin
                                        flushQ[k].enq(_in[k]);
				end
                        endrule

        for(Int#(8) _dram = 0; _dram < fromInteger(width) ; _dram = _dram + _DRAM)
                rule _DRAMflush (dr == _dram/_DRAM && flushOut == True);

                                for (Int#(8) k=0; k<_DRAM; k = k+1) begin
                                        let d <-  flushQ[k + _dram].deq;
                                	for (Int#(8) i=0; i<fromInteger(numOutputs); i = i+1)
                                        	out[k][i].enq(d);
                                end

                                if(flushed == _DataCount-1) begin
                                                if(dr == (_DW -_DRAM)/_DRAM) begin
                                                        dr <= 0;
							flushDone.send;
                                                end
                                                else
                                                        dr <= dr + 1;
                                        flushed <= 0;
                                end
                                else
                                        flushed <= flushed + 1;



                endrule

	
	rule _clean (flushOut == True);
			flushDone.ishigh;
			ready <= True;
			dataC <= 0;
                	p <= 0;
                	for(UInt#(10) k = 0; k< fromInteger(width); k = k + 1) begin
                        	c[k] <= 0;
                	end

			for(UInt#(10) k = 0; k< fromInteger(width); k = k + 1)
                        	flushQ[k].clean;

	endrule
	
	
        method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
		let d = out[_rate][i].first; out[_rate][i].deq;
                return d;
	endmethod

	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID) if(flushOut == False);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(width); k = k+1)
                                res[k] <- flushQ[k].deq;
                        return res;
	endmethod

	method Action clean if(flushOut == False);
		dataC <= 0;
		p <= 0;
                for(UInt#(10) k = 0; k< fromInteger(width); k = k + 1) begin
			c[k] <= 0;
		end

                for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k + 1)
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;

	endmethod

	method Action  extra if(flushOut == False);
		for(UInt#(10) k = 0; k< fromInteger(width); k = k + 1) 
			flushQ[k].clean;
	endmethod

	method Action cleaned if(!flushOut || ready);
		ready <= False;
	endmethod

	method Action reset(Int#(16) _dc);
		_DataCount <= unpack(pack(_dc));
	endmethod

	method Action reset2(Int#(16) _dc);
			for(int i = 0; i<fromInteger(width); i = i + 1) 
				flushQ[i].reset(_dc);
	endmethod
	
endmodule

// ################################################################# UP SAMPLING PRESURRIZED #################################################################
module mkUpComponent#(Component inp, Bool relayed, Integer inputID, Integer numOutputs, Integer buffer[], Integer _rate)(Component);
Reg#(DataType) _in[_rate];
FIFO#(DataType)  out[_rate][numOutputs];
Pulse           _p[_rate];

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
        _p[k]     <- mkPulse;
        _in[k] <- mkReg(0);

end

for(int i=0;i< fromInteger(numOutputs) ; i = i+1)
        for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1)
                        out[k][i] <- mkSizedFIFO(buffer[i]);

rule _input;
		DataType d = 0;
                if(relayed == False) begin
                        d <- inp.receive(fromInteger(inputID), 0);
                end
                else begin
                        d <- inp.forwarded(fromInteger(inputID),0);
                end
		for(int i=0; i< fromInteger(numOutputs); i = i+1)
                        for(UInt#(10) j = 0 ; j<fromInteger(_rate); j = j + 1)
                                out[j][i].enq(d);
endrule

method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
                                res[k] = out[k][0].first; out[k][0].deq;
                        end

                        return res;
endmethod

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate][i].first; out[_rate][i].deq;
        return d;
endmethod

endmodule
// #############################################################################################################################################################

//######################################################################## SIMD PRESURRIZED ####################################################################
module mkSIMD#(Compute operation, Component _inp[], Integer numInputs, Integer width)(Component);
Reg#(Vector#(VectorLength,DataType))    _in[numInputs];
FIFOF#(DataType)  	       out[width];
Reg#(Bit#(1))                   p0 <- mkReg(0);
Pulse                           _r[width];
Pulse                           _o[width];
Reg#(Bit#(1))                   c0[width];
Reg#(DataType)			result[width];
FIFOF#(Vector#(VectorLength,DataType))	store[width];



	for(int i=0;i< fromInteger(numInputs); i = i +1) begin
        	_in[i] <- mkRegU;
	end


	for(int i=0; i<fromInteger(width); i = i +1) begin
	out[i] <- mkSizedFIFOF(32);
        _r[i] <- mkPulse;
        _o[i] <- mkPulse;
        c0[i] <- mkReg(0);
	store[i] <- mkSizedFIFOF(8);
	result[i] <- mkReg(0);
	end


        rule _input;
			Vector#(VectorLength,Vector#(VectorLength,DataType)) x = newVector;
			for(int i=0;i< fromInteger(numInputs); i = i +1) begin
                          	x[i] <- _inp[i].receiveVector(0);
			end

			for(int i = 0; i <fromInteger(width); i = i + 1) begin
				Vector#(VectorLength, DataType) m = newVector;
				for(int j = 0; j<fromInteger(numInputs); j = j + 1) begin
						m[j] = x[j][i];
						//$display(" ----- %d ", fxptGetInt(m[j]));
				end
				store[i].enq(m);
			end
        endrule

	for(int k = 0; k < fromInteger(width); k = k + 1)
	rule operate;
			let d = store[k].first; store[k].deq;
			let outx  = operation(d);
			out[k].enq(outx);
	endrule

	

        method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(width); k = k+1) begin
                                res[k] = out[k].first; out[k].deq;
                        end

                        return res;
        endmethod
	
	method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        	let d = out[_rate].first; out[_rate].deq;
        	return d;
	endmethod
        method Action clean;
			p0 <= 0;
                       for(int i=0; i<fromInteger(width); i = i+1) begin
                                out[i].clear;
                                c0[i] <= 0;
			       _r[i].clean;
        		       _o[i].clean;
                       end
        endmethod

endmodule
//####################################################################################################################################################

//############################################################################# VECTOR PRESURRIZED #####################################
module mkVector#(Component _inp[], Integer numInputs, Integer inputID[], Integer numOutputs, Integer buffer[], Integer rate)(Component);
FIFOF#(DataType)  out[rate * numInputs][numOutputs];


for(int k=0; k< fromInteger(numOutputs); k = k + 1)
for(int i=0; i< fromInteger(rate*numInputs); i = i +1) begin
	out[i][k] <- mkSizedFIFOF(buffer[k]);
end


	for(UInt#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1)
	rule _input;
        		 for(UInt#(10) k = 0 ;k < fromInteger(rate); k = k + 1) begin
                         	let  d <- _inp[i].receive(fromInteger(inputID[i]), k);
				for(int l = 0; l<fromInteger(numOutputs); l = l + 1)
					out[i*fromInteger(rate) + k][l].enq(d);
			end
	endrule

	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(rate*numInputs); k = k+1) begin
                                res[k] = out[k][_ID].first; out[k][_ID].deq;
			end

                        return res;
	endmethod

	method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
                let d = out[_rate][i].first; out[_rate][i].deq;
                return d;
	endmethod

	method Action clean;
                       for(UInt#(10) i=0; i<fromInteger(numInputs); i = i+1) begin
        			for(UInt#(10) k = 0 ;k < fromInteger(rate); k = k + 1)
					for(int l = 0; l<fromInteger(numOutputs); l = l + 1)
                               			out[i*fromInteger(rate) + k][l].clear;
                       end
	endmethod

endmodule
//###########################################################################################################################################################

// ###########################################################################  REDUCER PRESURRIZED   #######################################################
module mkReducer#(Compute operation, Component _inp, Integer numInputs, Integer inputID, Integer numOutputs, Integer buffer[])(Component);
Reg#(DataType) _in[numInputs];
FIFOF#(DataType)  out[numOutputs];
Reg#(DataType) 	 result <- mkReg(0);
Reg#(Bit#(1)) 	_res0 <- mkReg(0);
Reg#(Bit#(1)) 	_res1[numOutputs];
Pulse 		_p <- mkPulse;
FIFOF#(Vector#(VectorLength,DataType))			holder <- mkSizedFIFOF(8); 

for(int i=0;i< fromInteger(numInputs); i = i +1)
	_in[i] <- mkReg(0);

for(int i=0;i< fromInteger(numOutputs) ; i = i+1) begin
	out[i] <- mkSizedFIFOF(buffer[i]);
	_res1[i] <- mkReg(0);
end


rule _input;
	Vector#(VectorLength, DataType) x = newVector;
	for(UInt#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1) begin
			let  d <- _inp.receive(fromInteger(inputID), i);
			x[i] = d;
	end
	holder.enq(x);

endrule

rule _compute;
		let d = holder.first; holder.deq;

			//for(int i=0 ;i<16; i = i + 1)
			//	$display("					->%d ", fxptGetInt(d[i]));
			//$display("						-----------------------------");
		let outx  = operation(d);
		for(int i=0; i < fromInteger(numOutputs); i = i+1)
			out[i].enq(outx);
endrule


method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
	let d = out[i].first; out[i].deq;
	return d;
endmethod

method Action clean;
				holder.clear;
                               _res0 <= 0;
                                _p.clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[i].clear;
					_res1[i] <= 0;
				end

endmethod
endmodule
//################################################################################################################################################################

//############################################################################## POINT PRESSURIZED ###############################################################
module mkPointComponent#(Compute operation, Component inputs[], Integer numInputs, Bool relayedInputs[], Integer inputID[], Integer numOutputs, Integer buffer[], Integer _rate)(Component);
Reg#(DataType) _in[_rate][numInputs];
FIFOF#(DataType)  out[_rate][numOutputs];
Reg#(DataType) 	 result[_rate];
Reg#(Bit#(1)) 	_res0[_rate];
Reg#(int) clk <- mkReg(0);
Reg#(Bit#(1)) 	_res1[_rate][numOutputs];
FIFOF#(Bit#(1))		_p[_rate];
FIFOF#(Vector#(VectorLength,DataType))			holder[_rate]; 

rule _CLK;
	clk <= clk + 1;
endrule
for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
	result[k] <- mkReg(0);
	_p[k] 	  <- mkFIFOF;
	_res0[k]   <- mkReg(0);
	holder[k] <- mkSizedFIFOF(8);

for(int i=0;i< fromInteger(numInputs); i = i +1)
	_in[k][i] <- mkReg(0);

for(int i=0;i< fromInteger(numOutputs) ; i = i+1) begin
	out[k][i] <- mkSizedFIFOF(buffer[i]);
	_res1[k][i] <- mkReg(0);
end

end

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
rule _input;
	Vector#(VectorLength, DataType) x = newVector;
	for(Int#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1) begin
		if(relayedInputs[i] == False) begin
			let  d <- inputs[i].receive(fromInteger(inputID[i]), k);
			//$display(" comparing %d at clk %d ", fxptGetInt(d),clk);
		 	 x[i] = d;
		end
		else begin
			let  d <- inputs[i].forwarded(fromInteger(inputID[i]),k);

		 	 x[i] = d;
		end
	end
	holder[k].enq(x);

endrule

rule _compute;	
	let d       = holder[k].first; holder[k].deq;
	//if(pr == True)
	//$display(" comparing %d with %d ", fxptGetInt(d[0]), fxptGetInt(d[1]));
	let outx     = operation(d);
	for(int i=0; i < fromInteger(numOutputs); i = i+1)
                        out[k][i].enq(outx);
endrule

end

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
	let d = out[_rate][i].first; out[_rate][i].deq;
	//$display("					%d ", fxptGetInt(d));
	return d;
endmethod

method Action clean;

                for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1)  begin
                                _res0[k] <= 0;
				holder[k].clear;
                                _p[k].clear;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[k][i].clear;
					_res1[k][i] <= 0;
				end
                end

endmethod
endmodule 
//#################################################################################################################################################################

module mkPoolComponent#(Component inp, Bool relayed, Integer inputID, Integer numOutputs, Integer buffer[],Integer img)(Component);
Reg#(DataType)  _in <- mkReg(0);
FIFO#(DataType)  out[numOutputs];
Pool             pool <- mkPool(img);
Pulse           _p <- mkPulse;


for(int i=0;i< fromInteger(numOutputs) ; i = i+1)
        out[i] <- mkSizedFIFO(buffer[i]);

rule _inputP;
		DataType d = 0;
                if(relayed == False) begin
                        d <- inp.receive(fromInteger(inputID), 0);
                end
                else begin
                        d <- inp.forwarded(fromInteger(inputID),0);
                end
		pool.send(d);
endrule

rule _output;
                let d <- pool.reduced;
                for(int i=0; i< fromInteger(numOutputs); i = i+1) begin
                        out[i].enq(d);
                end
endrule


method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[i].first; out[i].deq;
        return d;
endmethod

method Action clean;
		_p.clean;
		for(int i=0 ;i< fromInteger(numOutputs); i = i + 1)
			out[i].clear;
		pool.clean;
endmethod


endmodule

module mkDownComponent#(Component inp, Bool relayed, Integer inputID, Integer numOutputs, Integer buffer[], Integer _rate)(Component);
Reg#(DataType) _in[_rate][2];
FIFO#(DataType)  out[_rate][numOutputs];
Pool2           pools[_rate];
Pulse           _p[_rate];

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
        _p[k]     <- mkPulse;
        pools[k]   <- mkPool2;
	for(int i=0 ;i<2; i = i+1)
        	_in[k][i] <- mkReg(0);

for(int i=0;i< fromInteger(numOutputs) ; i = i+1)
        out[k][i] <- mkSizedFIFO(buffer[i]);

end

for(UInt#(10) k = 0 ;k < fromInteger(2*_rate); k = k+1)
rule _input;
                if(relayed == False) begin
                        let  d <- inp.receive(fromInteger(inputID), k);
                        _in[k/2][k%2] <= d;
                end
                else begin
                        let  d <- inp.forwarded(fromInteger(inputID),k);
                        _in[k/2][k%2] <= d;
                end
	if( k < fromInteger(_rate))
        _p[k].send;

endrule

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k +1) begin
rule _pool;
                _p[k].ishigh;
		Vector#(2, DataType) datas = newVector;
		for(int i=0;i<2; i = i + 1)
		datas[i] = _in[k][i];
		pools[k].send(datas);
endrule

rule _output;
                let d <- pools[k].reduced;
                for(int i=0; i< fromInteger(numOutputs); i = i+1) begin
                        out[k][i].enq(d);
                end
endrule

end

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate][i].first; out[_rate][i].deq;
        return d;
endmethod

endmodule


module mkPadComponent#(Component inp, Integer inputID, Integer numOutputs, Integer buffer[], Integer _rate, Integer width)(Component);
Reg#(DataType) _in[_rate];
Reg#(DataType) _in1[_rate];
FIFO#(DataType)  out[_rate][numOutputs];
Pulse           _m <- mkPulse;
Reg#(UInt#(4)) 	_FR <- mkReg(0);
Reg#(BramWidth) c <- mkReg(1);
Reg#(BramWidth) r <- mkReg(0);
FIFORand	memory <- mkBuffer;
FIFO#(DataType) inf[_rate];
Reg#(Bool) _rebut <- mkReg(False);

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
        _in[k]    <- mkReg(0);
        _in1[k]    <- mkReg(0);

	inf[k] <- mkSizedFIFO(buffer[0]);
for(int i=0;i< fromInteger(numOutputs) ; i = i+1)
        out[k][i] <- mkSizedFIFO(buffer[i]);

end

for(UInt#(10) k = 0; k<fromInteger(_rate); k = k+1)
rule _read (_rebut == False);
	let d <- inp.receive(fromInteger(inputID), k);
	inf[k].enq(d);
endrule

rule _input (_rebut == False);

		if(r >= 1 && r <= fromInteger(width)) begin
		for(UInt#(10) k = 0; k<fromInteger(_rate); k = k+1) begin
				let d = inf[k].first; inf[k].deq;
				_in1[k] <= d;
				if(k == fromInteger(_rate-1))
					memory.enq(d,c);
			end
                	if(c == fromInteger(width))
                        	c <= 1;
                	else
                        	c <= c + 1;
		end

		else
			for(UInt#(10) k = 0; k<fromInteger(_rate); k = k+1)
				_in1[k] <= 0;

		 memory.deq(r);

                 if(r == fromInteger(width+1))
                                r <= 0;
                  else
                                r <= r + 1;


endrule

rule _latch (_rebut == False);
	 memory.latchData;
	for(int i=0 ;i< fromInteger(_rate); i = i+1)
		_in[i] <= _in1[i];
	_m.send;
endrule

rule _output (_rebut == False);
		_m.ishigh;
		let d = memory.get;
		Vector#(VectorLength, DataType) datas = newVector;

		if(d < 0)
			datas[0] = 0;
		else
			datas[0] = d;

		for(int i=1;i<fromInteger(_rate); i = i+1)
			datas[i] = _in[i-1];

		for(int k = 0; k<fromInteger(_rate); k = k+1) begin
                        for(int i=0; i< fromInteger(numOutputs); i = i+1)
                                out[k][i].enq(datas[k]);
		end

endrule

rule cleanData  (_rebut == True && _FR > 0);
                c <= 1;
		_m.clean;
                r <= 0;
                for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1)  begin
                                inf[k].clear;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;
                end
		_FR <= _FR - 1;

endrule


rule cleanMem (_rebut == True && _FR == 2);
		memory.clean;		
endrule

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate][i].first; out[_rate][i].deq;
        return d;
endmethod

method Action clean;
		_rebut <= True;
		_FR <= 2;

endmethod

method Action cleaned if(_FR == 0);
               _rebut <= False;
endmethod
endmodule

module mkMultiplyComponent#(Component inputs[], Integer numInputs, Bool relayedInputs[], Integer inputID[], Integer numOutputs, Integer buffer[], DataType constant, Integer _rate)(Component);
FIFOF#(DataType) _in[_rate][2];
FIFOF#(DataType)  out[_rate][numOutputs];
Reg#(DataType)   result[_rate];
Pulse           _res[_rate];
Pulse           _p[_rate];
Mult		m[_rate];
for(UInt#(10) k = 0 ; k < fromInteger(_rate); k = k+1)
	for(int i=0;i< 2; i = i +1)
		_in[k][i] <- mkSizedFIFOF(8);

for(UInt#(10) k = 0 ; k < fromInteger(_rate); k = k+1) begin
	result[k] <- mkReg(0);
        _p[k]     <- mkPulse;
        _res[k]   <- mkPulse;
	m[k] 	  <- mkMult;

	for(int i=0;i< fromInteger(numOutputs) ; i = i+1)
        	out[k][i] <- mkSizedFIFOF(buffer[i]);

end

for(UInt#(10) k = 0 ; k < fromInteger(_rate); k = k+1) begin

rule _input;
        for(Int#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1) begin
                if(relayedInputs[i] == False) begin
                        let  d <- inputs[i].receive(fromInteger(inputID[i]),k);
                        _in[k][i].enq(d);
                end
                else begin
                        let  d <- inputs[i].forwarded(fromInteger(inputID[i]),k);
                        _in[k][i].enq(d);
                end
        end
	if(numInputs == 1)
		_in[k][1].enq(1);

endrule

rule _compute;
		let a = _in[k][0].first; _in[k][0].deq;
		let b = _in[k][1].first; _in[k][1].deq;
		if(numInputs == 1 && constant == 0 ) begin
                 	m[k].a(a);
			m[k].b(unpack(pack(a)));
		end
		else if(numInputs == 1 && constant > 0) begin
			m[k].a(a);
			m[k].b(unpack(pack(constant)));
		end
		else begin
			m[k].a(a);
			m[k].b(unpack(pack(b)));
		end
endrule

rule _result;
		let d <- m[k].out;
		//result[k] <= d;
		//_res[k].send;
//endrule

//rule _output;
  //              _res[k].ishigh;
                for(int i=0; i< fromInteger(numOutputs); i = i+1)
                        out[k][i].enq(d);
endrule

end

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate][i].first; out[_rate][i].deq;
        return d;
endmethod

method Action clean;

                for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1)  begin
                                _res[k].clean;
                                _p[k].clean;
				m[k].clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;
                end

endmethod

endmodule

module  mkStencilComponent#(Stencil s, Component inP, Integer inputId, Bool relay, Integer numRelays,  Relay re[][], Integer numOutputs, Integer buffer[])(Component);
Integer rate = s.k;
Reg#(DataType) 			_in[rate];
Reg#(Bool) 			_rebut <- mkReg(False);
FIFOF#(DataType)  		out[rate][numOutputs];
Pulse 				_p[rate];
Component 			node 	<- mkStage(s.k, s.stencil,s.img,s.banks,s.weights);
BFIFO		                 forwardingQ[rate][numRelays];
BramWidth 			rindex = fromInteger(s.stencil/2);
Reg#(BramWidth)			r[rate];
Reg#(BramWidth)			c[rate];

Reg#(int) clk <- mkReg(0);

rule _CLK;
	clk <= clk + 1;
endrule


for(BramWidth k = 0; k < fromInteger(rate); k = k+1)
	r[k]   <- mkReg(rindex);

for(UInt#(10) k = 0;k < fromInteger(rate); k = k+1) begin

	_in[k] <- mkReg(0);
	_p[k]  <- mkPulse;
	c[k]   <- mkReg(rindex);

	for(int i=0;i<fromInteger(numOutputs); i = i+1) begin
		out[k][i] <- mkSizedFIFOF(buffer[i]);
	end

	for(int i=0;i<fromInteger(numRelays); i = i+1)
        	forwardingQ[k][i] <- mkBramFifo(re[k][i].size);


end

for(UInt#(10) k = 0;k < fromInteger(rate); k = k+1) begin
rule _input;
                let  d <- inP.receive(fromInteger(inputId), k);
                _in[k] <= d;
		_p[k].send;


endrule

rule _relay (relay == True);
		if(c[k] == fromInteger(s.img)-rindex-1)	begin
			r[k] <= r[k] + fromInteger(rate);
			c[k] <= rindex;
		end
		else
			c[k] <= c[k] + 1;
		let d <- node.forwarded(truncate(k),0);
		for(int i=0 ; i< fromInteger(numRelays); i = i+1)
if(r[k] + extend(k)  >= fromInteger(re[k][i].row1) && r[k] + extend(k) <= fromInteger(re[k][i].row2-1) && c[k]  >= fromInteger(re[k][i].col1) && c[k] <= fromInteger(re[k][i].col2-1)) begin
				forwardingQ[k][i].enq(d);
		end
endrule

rule _compute;
		_p[k].ishigh;
                node.send(_in[k], truncate(k));
endrule

rule _output;
                let d <- node.receive(truncate(k),0);
		//$display(" convolver out %d @clk %d ", fxptGetInt(d), clk);
		for(int i=0;i<fromInteger(numOutputs); i = i+1) begin
                	out[k][i].enq(d);
		end
endrule

end

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate][i].first; out[_rate][i].deq;
	//$display(" put data %d at @clk %d ", fxptGetInt(d), clk);
        return d;
endmethod

method ActionValue#(DataType) forwarded(BramLength i, UInt#(10) _rate);
            let d <- forwardingQ[_rate][i].deq;
            return d;
endmethod

method Action clean;
	node.clean;
	//$display(" STENCIL CLEARED @clk %d ", clk);
	for(UInt#(10) k = 0; k< fromInteger(rate); k = k + 1)  begin
				_p[k].clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[k][i].clear;
				end
                end
endmethod

method Action cleaned;
	node.cleaned;
	/*for(UInt#(10) k = 0; k< fromInteger(rate); k = k + 1)  begin
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[k][i].clear;
                                end
        end*/

endmethod


endmodule

module mkMemoryStream#(Component inp, Integer width, Integer numOutputs, Integer size, Integer outWidth )(Component);
FIFOF#(DataType) _in[width];
Reg#(Bit#(1))   c[width];
Reg#(Bit#(1)) p                 <- mkReg(0);
BFIFO flushQ[width];
FIFOF#(DataType) out[outWidth][numOutputs];
Int#(8) _DRAM = fromInteger(outWidth);
Int#(8) _DW = fromInteger(width);
Reg#(Int#(8)) dr <- mkReg(0);
Reg#(UInt#(20)) flushed <- mkReg(0);
Reg#(UInt#(20)) dataC <- mkReg(0);

Bool flushOut = !(width == outWidth);
	
        for(int i = 0; i<fromInteger(width); i = i + 1) begin
                        _in[i] <- mkSizedFIFOF(size);
                        c[i] <- mkReg(0);
			flushQ[i] <- mkBramFifo(size);
        end
	
	for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k+1)
        	for(UInt#(10) i = 0; i < fromInteger(numOutputs); i = i+1)
                	out[k][i] <- mkFIFOF;


        rule _input;
                        let d <- inp.receiveVector(0);
                        for(int i = 0; i < fromInteger(width); i = i + 1)
                                _in[i].enq(d[i]);
                        p <= ~p;
			dataC <= dataC + 1;
        endrule

        for(UInt#(10) k = 0; k < fromInteger(width); k = k + 1)
                        rule _write;
				let d = _in[k].first; _in[k].deq;
				if(flushOut == True) begin
					 if(dataC <= fromInteger(size+2))
						flushQ[k].enq(d);
				end
				else 
						flushQ[k].enq(d);

                        endrule

	
	

                rule _DRAMflush;
                                for (Int#(8) k=0; k<_DRAM; k = k+1) begin
                                        let d <-  flushQ[k + dr].deq;
                                	for (Int#(8) i=0; i<fromInteger(numOutputs); i = i+1)
                                        	out[k][i].enq(d);
                         
			      	end

				if(flushOut == True) begin
				if( dr + _DRAM == fromInteger(width)) begin
					if(flushed == fromInteger(size)) begin
						dataC  <= 0;
						flushed <= 0;
					end
					else
						flushed <= flushed + 1;
					dr <= 0;
				end
				else
					dr <= dr + _DRAM;
				end

                endrule

	method Action send(DataType dat, BramLength i);
        	_in[i].enq(dat);
	endmethod

        method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
		let d = out[_rate][i].first; out[_rate][i].deq;
                return d;
	endmethod

	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID); //if ( flushOut == False );
                        Vector#(VectorLength, DataType) res = replicate(0);
                        for(UInt#(10) k = 0; k< fromInteger(width); k = k+1) begin
                                //res[k] <- flushQ[k].deq;
                                res[k] = out[k][0].first; out[k][0].deq;
			end
                        return res;
	endmethod

	method Action clean;
		p <= 0;
                for(UInt#(10) k = 0; k< fromInteger(width); k = k + 1) begin
			c[k] <= 0;
		end

                for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k + 1)
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;

	endmethod

endmodule

module mkAccumulate#(Compute operation, Component inputs[], Integer row, Integer col, Integer numInputs, Bool relayedInputs[], Integer inputID[], Integer numOutputs, Integer buffer[], Bool once, Integer _rate)(Component);
Integer count   = row*col;
Reg#(DataType) _in[_rate][numInputs+1];
FIFOF#(DataType)  out[_rate][numOutputs];
Reg#(DataType) 	 result[_rate];
Reg#(Bit#(1)) 	_res0[_rate];
Reg#(Bit#(1)) 	_res1[_rate][numOutputs];
Pulse 		_p[_rate];
Reg#(UInt#(16)) counter[_rate];

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
	result[k] <- mkReg(0);
	_p[k] 	  <- mkPulse;
	_res0[k]   <- mkReg(0);
	counter[k] <- mkReg(fromInteger(count));

for(int i=0;i< fromInteger(numInputs+1); i = i +1)
	_in[k][i] <- mkReg(0);

for(int i=0;i< fromInteger(numOutputs) ; i = i+1) begin
	out[k][i] <- mkSizedFIFOF(buffer[i]);
	_res1[k][i] <- mkReg(0);
end

end

for(UInt#(10) k = 0; k< fromInteger(_rate); k = k+1) begin
rule _input;
	for(Int#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1) begin
		if(relayedInputs[i] == False) begin
			let  d <- inputs[i].receive(fromInteger(inputID[i]), k);
			_in[k][i] <= d;
		end
		else begin
			let  d <- inputs[i].forwarded(fromInteger(inputID[i]),k);
                        _in[k][i] <= d;
		end
	end
	_p[k].send;

endrule

rule _compute;
		_p[k].ishigh;
		Vector#(VectorLength, DataType) x = newVector;
		for(int i=0;i<=fromInteger(numInputs); i = i + 1)
			x[i] = _in[k][i];

				
		//$display(" 					%d ", fxptGetInt(x[0]));
		let d  = operation(x);
			if(counter[k] == 1) begin
				if(once == True) begin
                                	counter[k] <= fromInteger(count);
                                	_in[k][numInputs] <= 0;
				end
                                result[k] <= d;
                            	_res0[k] <= ~_res0[k];
                        end
                        else begin
                                _in[k][numInputs] <= d;
                                counter[k] <= counter[k] - 1;
                        end
		
endrule

for(int i=0; i < fromInteger(numOutputs); i = i+1)
rule _output ((_res0[k] ^ _res1[k][i])==1);
		_res1[k][i] <= _res0[k];
		out[k][i].enq(result[k]);
endrule

end

method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
	let d = out[_rate][i].first; out[_rate][i].deq;
	return d;
endmethod

method Action clean;

                for(UInt#(10) k = 0; k< fromInteger(_rate); k = k + 1)  begin
                                _res0[k] <= 0;
                                _p[k].clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[k][i].clear;
					_res1[k][i] <= 0;
				end
                end

endmethod
endmodule

module mkPermute#(Permute operation, Component _inp, Integer numInputs, Integer inputID, Integer numOutputs, Integer buffer)(Component);
Reg#(DataType) _in[numInputs];
FIFOF#(DataType)  out[numOutputs];
Reg#(Vector#(VectorLength,DataType))   result <- mkRegU;
Reg#(Bit#(1))   _res0 <- mkReg(0);
Reg#(Bit#(1))   _res1[numOutputs];
Pulse           _p <- mkPulse;
FIFOF#(Vector#(VectorLength,DataType))			holder <- mkSizedFIFOF(8); 

for(int i=0;i< fromInteger(numInputs); i = i +1)
        _in[i] <- mkReg(0);

for(int i=0;i< fromInteger(numOutputs) ; i = i+1) begin
        out[i] <- mkSizedFIFOF(buffer);
        _res1[i] <- mkReg(0);
end


rule _input;
	Vector#(VectorLength, DataType) x = newVector;
        for(UInt#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1) begin
                        let  d <- _inp.receive(fromInteger(inputID), i);
                        x[i] = d;
        end
        holder.enq(x);

endrule

rule _compute;
                let d = holder.first; holder.deq;

		//for(int i=0 ;i<16; i= i+1)
		//	$display("				$$$$ %d ", fxptGetInt(d[i]));

		//$display("####################### ");
                let outx  = operation(d);
		for(int i=0; i < fromInteger(numOutputs); i = i+1)
               		 out[i].enq(outx[i]);
endrule


method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        let d = out[_rate].first; out[_rate].deq;
        return d;
endmethod

method Action clean;
                               _res0 <= 0;
                                _p.clean;
                                for(int i=0; i<fromInteger(numOutputs); i = i+1) begin
                                        out[i].clear;
                                        _res1[i] <= 0;
                                end

endmethod
endmodule
//################################################################################################################################

//################################################################################################################################

module mkDeVectorize#(Component _inp[], Integer numInputs, Integer inputID[], Integer rate, Integer outRate)(Component);
Reg#(DataType)    _in[rate * numInputs];
FIFOF#(DataType)  out[outRate];
Pulse             _p[numInputs];
Int#(10) length = fromInteger(rate*numInputs);
Reg#(Int#(10)) count <- mkReg(0);
Reg#(Bool) enable <- mkReg(True);
Reg#(int) clk <- mkReg(0);

rule _CLK;
                clk <= clk + 1;
endrule

for(int i=0;i< fromInteger(rate*numInputs); i = i +1) begin
        _in[i] <- mkReg(0);
end

for(int i=0;i< fromInteger(outRate); i = i +1) begin
        out[i] <- mkSizedFIFOF(32);
end

        rule _input (enable == True);
                for(UInt#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1)
                         for(UInt#(10) k = 0 ;k < fromInteger(rate); k = k + 1) begin
                                let  d <- _inp[i].receive(fromInteger(inputID[i]), k);
				//$display(" %d ", fxptGetInt(d));
                                _in[i*fromInteger(rate) + k] <= d;
                         end

		//$display(" ################################################################ ");
                enable <= False;
        endrule

        rule _output (enable == False);

                        for(Int#(10) k = 0 ;k < fromInteger(outRate); k = k + 1) begin
                                out[k].enq(_in[count+ k]);
                        end

                        if(count + fromInteger(outRate) == length) begin
                                count <= 0;
                                ///$display("                                                      activating the receiver in DEvec @clk %d ",clk);
                                enable <= True;
                        end
                        else
                                count <= count + fromInteger(outRate);
        endrule

        method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
                let d = out[_rate].first; out[_rate].deq;
                return d;
        endmethod

        method Action clean;
                       for(UInt#(10) i=0; i<fromInteger(numInputs); i = i+1) begin
                                for(UInt#(10) k = 0 ;k < fromInteger(outRate); k = k + 1)
                                        out[i*fromInteger(outRate) + k].clear;
                       end
                      count <= 0;
                      enable <= True;
        endmethod
endmodule


//################################################################################################################################
module mkRepeater#(Component _inp[], Integer numInputs, Integer inputID[], Integer rate, Integer rep)(Component);
Integer outRate = rate;
Reg#(DataType)    _in[rate * numInputs];
FIFOF#(DataType)  out[outRate];
Pulse             _p[numInputs];
Int#(16) length = fromInteger(rate*numInputs);
Reg#(Int#(16)) count <- mkReg(0);
Reg#(Bool) enable <- mkReg(True);
Reg#(int) clk <- mkReg(0);
FIFOF#(Vector#(VectorLength,DataType))			holder <- mkSizedFIFOF(8 + rep); 

rule _CLK;
                clk <= clk + 1;
endrule

for(int i=0;i< fromInteger(rate*numInputs); i = i +1) begin
        _in[i] <- mkReg(0);
end

for(int i=0;i< fromInteger(outRate); i = i +1) begin
        out[i] <- mkSizedFIFOF(32+rep);
end

        rule _input (enable == True);

		Vector#(VectorLength, DataType) res = newVector;
                for(UInt#(10) i = 0 ;i < fromInteger(numInputs); i = i + 1)
                         for(UInt#(10) k = 0 ;k < fromInteger(rate); k = k + 1) begin
                                let  d <- _inp[i].receive(fromInteger(inputID[i]), k);
                                //$display(" enqueing in DeVector %d @clk %d ", fxptGetInt(d), clk);
                                //_in[i*fromInteger(rate) + k] <= d;
                                res[i*fromInteger(rate) + k] = d;
                         end
                    enable <= False;
		    holder.enq(res);
                //$display("                                            ########################################## ");
        endrule

        rule _output (enable == False);

			let d = holder.first;
                        for(Int#(10) k = 0 ;k < fromInteger(outRate); k = k + 1) begin
                                out[k].enq(d[k]);
                        end

                        if(count == fromInteger(rep)-1) begin
                                count <= 0;
				holder.deq;
                                enable <= True;
                        end
                        else
                                count <= count + 1;
        endrule

        method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
                let d = out[_rate].first; out[_rate].deq;
                return d;
        endmethod

        method Action clean;
                       for(UInt#(10) i=0; i<fromInteger(numInputs); i = i+1) begin
                                for(UInt#(10) k = 0 ;k < fromInteger(outRate); k = k + 1)
                                        out[i*fromInteger(outRate) + k].clear;
                       end
                      count <= 0;
                      enable <= True;
        endmethod
endmodule


// ########################################################## CONVOLVER PRESURRIZED #######################################################################
module mkConvolverBlock#(Component inp, Integer inputID, Integer numOutputs, Integer buffer[],Integer _V)(Component);
ConvBlock cx <- mkConvBlock;
Reg#(int) clk <- mkReg(0);
FIFOF#(Vector#(DspMax,DataType)) out[numOutputs];

		rule _CLK ;
			clk <= clk + 1;
		endrule
		
                for(int i=0;i<fromInteger(numOutputs); i = i+1)
                              out[i] <- mkSizedFIFOF(buffer[i]);

		rule inputx;
                                let  d <- inp.receiveVector(fromInteger(inputID));
				//$display(" receving from INPUT TILE %d @clk %d ", fxptGetInt(d[0]), clk);
				cx.sendP(d);
				Vector#(256,CoeffType) filter = replicate(0);
                        		filter[4]  = 1;
                        		filter[13] = 1;
                        		filter[22] = 1;
                        		filter[31] = 1;
                        		filter[40] = 1;
                        		filter[49] = 1;
                        		filter[58] = 1;
                        		filter[67] = 1;
                		cx.sendF(filter);
		endrule

		rule _output;
                		let d <- cx.result;
                		for(int i=0;i<fromInteger(numOutputs); i = i+1)
                        			out[i].enq(d);
		endrule

		method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
			Vector#(VectorLength,DataType) res = newVector;
			let d = out[_ID].first; out[_ID].deq;
			for(int i=0; i< fromInteger(_V); i = i + 1)
					res[i] = d[i];
			return res;
		endmethod
		
		method Action clean;
				$display(" CLEARING THE QUEUE @clk %d ", clk);
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[i].clear;
                endmethod

		method Action reset(Int#(16) param);
			Int#(8) _KERNEL = truncate(param);
			cx.reset(_KERNEL);
		endmethod
endmodule
//#####################################################################################################################################################################


//###########################################################################################################################
module mkDrain#(Integer outWidth, Integer numOutputs, Integer _DC, Bool flushOut)(Component);

UInt#(10) _DRAM = fromInteger(outWidth);
Reg#(UInt#(16)) dataC <- mkReg(0);
Reg#(UInt#(16)) flushed <- mkReg(0);
Reg#(UInt#(16)) _DataCount <- mkReg(fromInteger(_DC));
CoalescedMemory flushQ <- mkCoalescedMemory(_DC,flushOut);
Reg#(UInt#(10)) _DEGREE <- mkReg(0);
Reg#(UInt#(10)) dr <- mkReg(0);


FIFOF#(DataType) out[outWidth][numOutputs];
FIFOF#(Vector#(VectorLength,DataType)) out2[numOutputs];

	for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k+1)
        	for(UInt#(10) i = 0; i < fromInteger(numOutputs); i = i+1)
                	out[k][i] <- mkFIFOF;
		
        rule _DRAMflush (dr + _DRAM < _DEGREE && flushOut == True);

                                let d <-  flushQ.deq;			
				//$display(" %d %d %d %d %d %d %d %d DR = %d ", fxptGetInt(d[0]), fxptGetInt(d[1]), fxptGetInt(d[2]), fxptGetInt(d[3]), fxptGetInt(d[4]), fxptGetInt(d[5]), fxptGetInt(d[6]), fxptGetInt(d[7]), dr); 		                               
				for (UInt#(10) k=0; k<_DRAM; k = k+1) begin
                                	for (Int#(8) i=0; i<fromInteger(numOutputs); i = i+1)
                                        	out[k][i].enq(d[dr+k]);
                                end
                                if(flushed == _DataCount-1) begin
                                        dr <= dr + _DRAM;
                                        flushed <= 0;
                                end
                                else
                                flushed <= flushed + 1;

        endrule

        
	method Action sendVector(Vector#(VectorLength, DataType) inx);
			flushQ.enq(inx);	
	endmethod
	
        method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
		let d = out[_rate][i].first; out[_rate][i].deq;
                return d;
	endmethod

	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID) if(flushOut == False);
                                let d <- flushQ.deq;
                        	return d;
	endmethod

	method Action clean if(flushOut == False || dr + _DRAM == _DEGREE);
		dataC <= 0;
		dr <= 0;
                for(UInt#(10) k = 0; k< fromInteger(outWidth); k = k + 1)
                                for(int i=0; i<fromInteger(numOutputs); i = i+1)
                                        out[k][i].clear;
		
		
               for(int i=0; i<fromInteger(numOutputs); i = i+1)
			out2[i].clear;

	endmethod

	method Action drain (UInt#(10) _D, UInt#(16) sx);
		_DataCount <= sx;
		_DEGREE <= _D;
		flushQ.clean(_D,sx);
	endmethod

	
endmodule

endpackage
