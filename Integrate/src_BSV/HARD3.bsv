package Hardware; 
import TubeHeader:: *; 
import FixedPoint:: *; 
import datatypes:: *; 
import Compose:: *; 
import Vector:: *; 
import FIFO:: *; 
import FIFOF:: *;
import distribute::*;
import tree::*;
import convolution::*;

#define RECV  256
#define SEND  197
interface Stdin;

	method ActionValue#(Bit#(10)) get;
	method Action put(Bit#(10) datas);

endinterface

(*synthesize *) 
module mkHardware(Stdin);
FIFOF#(Bit#(10)) outQ[8][8]; 

ConvBlock cx <- mkConvBlock;
Reg#(int) clk <- mkReg(0); 
Reg#(UInt#(22)) recv <- mkReg(0); 
Reg#(UInt#(22)) send <- mkReg(0);
Reg#(Bool) _c <- mkReg(False);
Reg#(Bool) _clear <- mkReg(False);
for (int i = 0; i < 8; i = i + 1)
	for (int j = 0; j < 8; j = j + 1)
		outQ[i][j] <- mkFIFOF;
Reg#(Bool) init <- mkReg(True);

Integer buffer_source[1]={32};
Component _source <- mkSource(1 , buffer_source , 1);

Integer buffer_tile[1]={32};
Component _tile <- mkTile(_source , 0 , 3 , 3 , 16 , 1 , buffer_tile , 1 , 1, 1);


rule distribute;
	let d <- _tile.receiveVector(0);
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

rule initialize(init == True);
		 init <= False;
		 cx.reset(3);
endrule

rule _pad(recv >= RECV && send < SEND);
	Vector  # (8,DataType) pad = replicate(0);
	for (BramLength i=0; i <1; i = i + 1)begin
		_source.send(pad[i],i);
	end

endrule

rule flushOut (send < SEND);
	let d0  <- cx.result;
	$display("				%d %d ", fxptGetInt(d0[0]), fxptGetInt(d0[1]));
	for(UInt#(10) i=0; i<1; i = i + 1) begin
		outQ[i][0].enq(truncate(pack(fxptGetInt(d0[0]))));

	end
endrule
rule _ClearPipe(send == SEND && _c == False);
	for (BramLength i=0; i <1; i = i + 1)begin
		for (BramLength j=0; j <8 ; j = j + 1)
			outQ[i][j].clear;
	end
		_c <= True;
		_tile.clean;
		_source.clean;

endrule
rule _ResetDone (_c == True && _clear == False);
		 _clear <= True;
		_tile.cleaned;

endrule
rule _ResetDone2 (_c == True && _clear == True);
	_c <= False;
	 _clear <= False;
	send <= 0; recv <= 0;

endrule
method ActionValue#(Bit#(10)) get if(send < SEND);
		Vector#(1,Bit#(10)) data = newVector;
		for (BramLength i=0; i <1; i = i + 1)
		for (BramLength j=0; j <1; j = j + 1)begin
			 data[i*1+j] = outQ[i][j].first; outQ[i][j].deq;
 		end
			 send <= send + 1; 
	 return pack(data);

endmethod


method Action put(Bit#(10) datas) if(outQ[0][0].notFull && recv < RECV);
		Vector#(1,Bit#(10)) data = unpack(datas);
		for (BramLength i=0; i <1; i = i + 1) begin
				Int#(10) x_0 = unpack(data[0+i*1]);
				_source.send(fromInt(x_0),i);
		end
			 recv <= recv + 1; 

endmethod


endmodule
endpackage
