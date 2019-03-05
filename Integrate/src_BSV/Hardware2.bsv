package Hardware; 
import TubeHeader:: *; 
import FixedPoint:: *; 
import datatypes:: *; 
import Compose:: *; 
import Vector:: *; 
import FIFO:: *; 
import FIFOF:: *;

#define RECV  256
#define SEND  241
interface Stdin;

	method ActionValue#(Bit#(20)) get;
	method Action put(Bit#(10) datas);

endinterface

(*synthesize *) 
module mkHardware(Stdin);
FIFOF#(Bit#(10)) outQ[8][8]; 
Reg#(int) clk <- mkReg(0); 
Reg#(UInt#(22)) recv <- mkReg(0); 
Reg#(UInt#(22)) send <- mkReg(0);
Reg#(Bool) _c <- mkReg(False);
Reg#(Bool) _clear <- mkReg(False);
for (int i = 0; i < 8; i = i + 1)
	for (int j = 0; j < 8; j = j + 1)
		outQ[i][j] <- mkFIFOF;
Reg#(Bool) init <- mkReg(True);

Integer buffer_src[2]={32,32};
Component _src <- mkSource(2 , buffer_src , 1);

Integer buffer_tiley[1]={64};
Component _tiley <- mkTile(_src , 0 , 2 , 1 , 16 , 1 , buffer_tiley , 1 , 1, 1);

Integer buffer_tilex[1]={64};
Component _tilex <- mkTile(_src , 1 , 1 , 2 , 16 , 1 , buffer_tilex , 1 , 1, 1);

Integer buffer_redx[1]={64};
function DataType func_redx(Vector#(VectorLength,DataType) in);
 		return fxptTruncate(fxptAdd((in[0] + in[1]))) >> 1;
 	
endfunction
Component _redx <- mkReducer(func_redx,_tilex , 2 , 0, 1 , buffer_redx);

Integer buffer_reddy[1]={64};
function DataType func_reddy(Vector#(VectorLength,DataType) in);
                 return fxptTruncate(fxptAdd((in[0] + in[1]))) >> 1;
         
endfunction
Component _reddy <- mkReducer(func_reddy,_tiley , 2 , 0, 1 , buffer_reddy);



rule initialize(init == True);
		 init <= False;
endrule

rule _pad(recv >= RECV && send < SEND);
	Vector  # (8,DataType) pad = replicate(0);
	for (BramLength i=0; i <1; i = i + 1)begin
		_src.send(pad[i],i);
	end

endrule
rule flushOut (send < SEND);
	for(UInt#(10) i=0; i<1; i = i + 1) begin
		let d0  <-_redx.receive(0,i);
		outQ[i][0].enq(truncate(pack(fxptGetInt(d0))));
		let d1  <-_reddy.receive(0,i);
		outQ[i][1].enq(truncate(pack(fxptGetInt(d1))));

	end
endrule
rule _ClearPipe(send == SEND && _c == False);
	for (BramLength i=0; i <1; i = i + 1)begin
		for (BramLength j=0; j <8 ; j = j + 1)
			outQ[i][j].clear;
	end
		_c <= True;
		_tilex.clean;
		_reddy.clean;
		_src.clean;
		_redx.clean;
		_tiley.clean;

endrule
rule _ResetDone (_c == True && _clear == False);
		 _clear <= True;
		_tilex.cleaned;
		_tiley.cleaned;

endrule
rule _ResetDone2 (_c == True && _clear == True);
	_c <= False;
	 _clear <= False;
	send <= 0; recv <= 0;

endrule
method ActionValue#(Bit#(20)) get if(send < SEND);
		Vector#(2,Bit#(10)) data = newVector;
		for (BramLength i=0; i <1; i = i + 1)
		for (BramLength j=0; j <2; j = j + 1)begin
			 data[i*2+j] = outQ[i][j].first; outQ[i][j].deq;
 		end
			 send <= send + 1; 
	 return pack(data);

endmethod


method Action put(Bit#(10) datas) if(outQ[0][0].notFull && recv < RECV);
		Vector#(1,Bit#(10)) data = unpack(datas);
		for (BramLength i=0; i <1; i = i + 1) begin
				Int#(10) x_0 = unpack(data[0+i*1]);
				_src.send(fromInt(x_0),i);
		end
			 recv <= recv + 1; 

endmethod


endmodule
endpackage
