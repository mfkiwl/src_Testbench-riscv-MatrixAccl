package Stereo; 
import TubeHeader:: *; 
import FixedPoint:: *; 
import datatypes:: *; 
import Compose:: *; 
import Vector:: *; 
import FIFO:: *; 
import FIFOF:: *;

#define RECV  256
#define SEND  100
interface Stdin;

	method ActionValue#(Bit#(10)) get;
	method Action put(Bit#(10) datas);

endinterface

(*synthesize *) 
module mkStereo(Stdin);
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

Integer buffer_src1[2]={32,32};
Component _src1 <- mkSource(2 , buffer_src1 , 1);

Integer buffer_tileShift[1]={64};
Component _tileShift <- mkTile(_src1 , 0 , 4 , 10 , 16 , 1 , buffer_tileShift , 1 , 1, 1);

Integer buffer_tileOrigin[1]={64};
Component _tileOrigin <- mkTile(_src1 , 1 , 4 , 4 , 16 , 1 , buffer_tileOrigin , 1 , 1, 1);

Component _inputs_devec[1]= {_tileShift };
Integer _inputId_devec[1]= {0 };
Component _devec <- mkDeVectorize(_inputs_devec , 1 , _inputId_devec , 40 , 1);

Component _inputs_rep1[1]= {_tileOrigin };
Integer _inputId_rep1[1]= {0 };
Component _rep1 <- mkRepeater(_inputs_rep1 , 1 , _inputId_rep1 , 16 , 7);

Integer buffer_red2[1]={173};
function DataType func_red2(Vector#(VectorLength,DataType) in);
                         return in[15];
               
endfunction
Component _red2 <- mkReducer(func_red2,_rep1 , 16 , 0, 1 , buffer_red2);

Integer buffer_tileShift2[1]={96};
Component _tileShift2 <- mkTile(_devec , 0 , 4 , 4 , 10 , 1 , buffer_tileShift2 , 1 , 4, 1);

Integer buffer_red1[1]={96};
function DataType func_red1(Vector#(VectorLength,DataType) in);
 			return in[15];
               
endfunction
Component _red1 <- mkReducer(func_red1,_tileShift2 , 16 , 0, 1 , buffer_red1);

Integer buffer_point[1]={96};
Component _inputs_point[2]= {_red1 , _red2 };
Integer _inputId_point[2]= {0 , 0 };
Bool _relay_point[2]= {False,False};
function DataType func_point(Vector#(VectorLength,DataType) in);
 			return in[1];
               
endfunction
Component _point <- mkPointComponent(func_point,_inputs_point , 2 ,_relay_point , _inputId_point , 1 , buffer_point , 1);

Integer buffer_acc[1]={96};
Component _inputs_acc[1]= {_point };
Integer _inputId_acc[1]= {0 };
Bool _relay_acc[1]= {False};
function DataType func_acc(Vector#(VectorLength,DataType) in);
                  return in[0];
               
endfunction
Component _acc <- mkAccumulate(func_acc,_inputs_acc , 1 , 7 , 1 ,_relay_acc , _inputId_acc , 1 , buffer_acc , True  ,  1);



rule initialize(init == True);
		 init <= False;
endrule

rule _pad(recv >= RECV && send < SEND);
	Vector  # (8,DataType) pad = replicate(0);
	for (BramLength i=0; i <1; i = i + 1)begin
		_src1.send(pad[i],i);
	end

endrule
rule flushOut (send < SEND);
	for(UInt#(10) i=0; i<1; i = i + 1) begin
		let d0  <-_acc.receive(0,i);
		outQ[i][0].enq(truncate(pack(fxptGetInt(d0))));

	end
endrule
rule _ClearPipe(send == SEND && _c == False);
	for (BramLength i=0; i <1; i = i + 1)begin
		for (BramLength j=0; j <8 ; j = j + 1)
			outQ[i][j].clear;
	end
		_c <= True;
		_point.clean;
		_acc.clean;
		_tileShift.clean;
		_tileOrigin.clean;
		_src1.clean;
		_devec.clean;
		_red1.clean;
		_rep1.clean;
		_red2.clean;
		_tileShift2.clean;

endrule
rule _ResetDone (_c == True && _clear == False);
		 _clear <= True;
		_tileShift.cleaned;
		_tileOrigin.cleaned;
		_tileShift2.cleaned;

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
				_src1.send(fromInt(x_0),i);
		end
			 recv <= recv + 1; 

endmethod


endmodule
endpackage
