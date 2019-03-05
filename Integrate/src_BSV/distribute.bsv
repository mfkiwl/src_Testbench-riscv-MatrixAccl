package distribute;
import TubeHeader::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import datatypes::*;
import FixedPoint::*;


//#define DSP 256

(*synthesize*)
module mkDistribute(Distribute);

	FIFOF#(Vector#(DspMax,DataType)) out <- mkFIFOF;
	Reg#(Int#(8)) _KERNEL <- mkReg(0);

        method Action put(Vector#(VectorLength, DataType) datas) if(out.notFull);
		
		Vector#(DspMax, DataType) res = replicate(0);
	
			
		if(_KERNEL == 3) begin
			for(int i=0; i<fromInteger(256); i = i + 16)
				for(int j=0; j<9; j = j + 1) begin
					res[i+j] = datas[j];	
				end

		end

		if(_KERNEL == 5) begin
			 for(int i=0; i<fromInteger(256); i = i + 32)
                                for(int j=0; j<25; j = j + 1)
                                        res[i] = datas[j];

		end
		
		if(_KERNEL == 7) begin
			 for(int i=0; i<fromInteger(256); i = i + 64)
                                for(int j=0; j<49; j = j + 1)
                                        res[i] = datas[j];

		end

		if(_KERNEL == 9) begin
			 for(int i=0; i<fromInteger(256); i = i + 128)
                                for(int j=0; j<81; j = j + 1)
                                        res[i] = datas[j];

		end

		if(_KERNEL == 11) begin
			 for(int i=0; i<fromInteger(256); i = i + 128)
                                for(int j=0; j<121; j = j + 1)
                                        res[i] = datas[j];
		end

		out.enq(res);

			
	endmethod

	method Action put2(Vector#(256, DataType) datas) if(out.notFull);

                Vector#(DspMax, DataType) res = replicate(0);


                if(_KERNEL == 3 && 256 > 9) begin
                        for(int i=0; i<fromInteger(256); i = i + 16)
                                for(int j=0; j<9; j = j + 1) begin
                                        res[i+j] = datas[(i/16)*9 + j];
                                end

                end

                if(_KERNEL == 5 && 256 > 20) begin
                         for(int i=0; i<fromInteger(256); i = i + 32)
                                for(int j=0; j<25; j = j + 1)
                                        res[i] = datas[(i/32)*25 + j];

                end

                if(_KERNEL == 7 && 256 > 48) begin
                         for(int i=0; i<fromInteger(256); i = i + 64)
                                for(int j=0; j<49; j = j + 1)
                                        res[i] = datas[(i/64)*49 + j];

                end

                if(_KERNEL == 9 && 256 > 80) begin
                         for(int i=0; i<fromInteger(256); i = i + 128)
                                for(int j=0; j<81; j = j + 1)
                                        res[i] = datas[(i/128)*81 + j];

                end

                if(_KERNEL == 11 && 256 > 120) begin
                         for(int i=0; i<fromInteger(256); i = i + 128)
                                for(int j=0; j<121; j = j + 1)
                                        res[i] = datas[(i/128)*121 + j];
                end

                out.enq(res);


        endmethod


	
	method ActionValue#(Vector#(DspMax,DataType)) result;
			let d = out.first; out.deq;
			return d;
	endmethod

	method Action clean(Int#(8) x);
		_KERNEL <= x;
		out.clear;
	endmethod
	
endmodule
endpackage
