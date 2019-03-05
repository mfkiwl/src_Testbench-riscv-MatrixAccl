package Hardware; 
import TubeHeader:: *; 
import FixedPoint:: *; 
import datatypes:: *; 
import Compose:: *; 
import Vector:: *; 
import FIFO:: *; 
import FIFOF:: *;

#define RECV  571536
#define SEND  568517
interface Stdin;

	method ActionValue#(Bit#(30)) get;
	method Action put(Bit#(30) datas);

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

Integer buffer_srcB[1]={32};
Component _srcB <- mkSource(1 , buffer_srcB , 1);

Integer buffer_srcG[1]={32};
Component _srcG <- mkSource(1 , buffer_srcG , 1);

Integer buffer_srcR[1]={32};
Component _srcR <- mkSource(1 , buffer_srcR , 1);

Integer buffer_tileG[1]={64};
Component _tileG <- mkTile(_srcG , 0 , 3 , 3 , 756 , 1 , buffer_tileG , 1 , 1, 1);

Integer buffer_tileB[1]={64};
Component _tileB <- mkTile(_srcB , 0 , 3 , 3 , 756 , 1 , buffer_tileB , 1 , 1, 1);

Integer buffer_tileR[1]={64};
Component _tileR <- mkTile(_srcR , 0 , 3 , 3 , 756 , 1 , buffer_tileR , 1 , 1, 1);

Integer buffer_IB[1]={64};
Component _IB <- mkConvolver(_tileB , 0 , 3, 1 , buffer_IB , 1);

Integer buffer_IG[1]={64};
Component _IG <- mkConvolver(_tileG , 0 , 3, 1 , buffer_IG , 1);

Integer buffer_IR[1]={64};
Component _IR <- mkConvolver(_tileR , 0 , 3, 1 , buffer_IR , 1);



rule initialize(init == True);
		 init <= False;
		 Vector#(VectorLength,DataType) weights_0 = newVector;
		 weights_0[0]=0.25;
		 weights_0[1]=0.5;
		 weights_0[2]=0.25;
		 weights_0[3]=0.5;
		 weights_0[4]=1;
		 weights_0[5]=0.5;
		 weights_0[6]=0.25;
		 weights_0[7]=0.5;
		 weights_0[8]=0.25;
		_IR.sendVector(weights_0);
		 Vector#(VectorLength,DataType) weights_1 = newVector;
		 weights_1[0]=0.25;
		 weights_1[1]=0.5;
		 weights_1[2]=0.25;
		 weights_1[3]=0.5;
		 weights_1[4]=1;
		 weights_1[5]=0.5;
		 weights_1[6]=0.25;
		 weights_1[7]=0.5;
		 weights_1[8]=0.25;
		_IB.sendVector(weights_1);
		 Vector#(VectorLength,DataType) weights_2 = newVector;
		 weights_2[0]=0;
		 weights_2[1]=0.25;
		 weights_2[2]=0;
		 weights_2[3]=0.25;
		 weights_2[4]=1;
		 weights_2[5]=0.25;
		 weights_2[6]=0;
		 weights_2[7]=0.25;
		 weights_2[8]=0;
		_IG.sendVector(weights_2);
endrule

rule _pad(recv >= RECV && send < SEND);
	Vector  # (8,DataType) pad = replicate(0);
	for (BramLength i=0; i <1; i = i + 1)begin
		_srcB.send(pad[i],i);
	_srcG.send(pad[i],i);
	_srcR.send(pad[i],i);
	end

endrule
rule flushOut (send < SEND);
	for(UInt#(10) i=0; i<1; i = i + 1) begin
		let d0  <-_IB.receive(0,i);
		outQ[i][0].enq(truncate(pack(fxptGetInt(d0))));
		let d1  <-_IG.receive(0,i);
		outQ[i][1].enq(truncate(pack(fxptGetInt(d1))));
		let d2  <-_IR.receive(0,i);
		outQ[i][2].enq(truncate(pack(fxptGetInt(d2))));

	end
endrule
rule _ClearPipe(send == SEND && _c == False);
	for (BramLength i=0; i <1; i = i + 1)begin
		for (BramLength j=0; j <8 ; j = j + 1)
			outQ[i][j].clear;
	end
		_c <= True;
		_IB.clean;
		_srcG.clean;
		_srcR.clean;
		_IG.clean;
		_tileB.clean;
		_tileR.clean;
		_srcB.clean;
		_tileG.clean;
		_IR.clean;

endrule
rule _ResetDone (_c == True && _clear == False);
		 _clear <= True;
		_tileB.cleaned;
		_tileR.cleaned;
		_tileG.cleaned;

endrule
rule _ResetDone2 (_c == True && _clear == True);
	_c <= False;
	 _clear <= False;
	send <= 0; recv <= 0;

endrule
method ActionValue#(Bit#(30)) get if(send < SEND);
		Vector#(3,Bit#(10)) data = newVector;
		for (BramLength i=0; i <1; i = i + 1)
		for (BramLength j=0; j <3; j = j + 1)begin
			 data[i*3+j] = outQ[i][j].first; outQ[i][j].deq;
 		end
			 send <= send + 1; 
	 return pack(data);

endmethod


method Action put(Bit#(30) datas) if(outQ[0][0].notFull && recv < RECV);
		Vector#(3,Bit#(10)) data = unpack(datas);
		for (BramLength i=0; i <1; i = i + 1) begin
				Int#(10) x_0 = unpack(data[0+i*3]);
				_srcR.send(fromInt(x_0),i);
				Int#(10) x_1 = unpack(data[1+i*3]);
				_srcG.send(fromInt(x_1),i);
				Int#(10) x_2 = unpack(data[2+i*3]);
				_srcB.send(fromInt(x_2),i);
		end
			 recv <= recv + 1; 

endmethod


endmodule
endpackage
