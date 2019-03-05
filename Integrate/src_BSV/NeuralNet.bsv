package NeuralNet;
import BRam::*;
import TubeHeader::*;
import FixedPoint::*;
import datatypes::*;
import bram::*;
import Compose::*;
import pulse::*;
import Vector::*;
import Stage::*;
import FIFO::*;
import FIFOF::*;
import pool2::*;


#define Filters 8 
#define IMG 224
#define K 2
#define DRAM 2

#define Rx 25088  
#define SEND 24643 
#define WEIGHTS 72

interface Std;
        method Action put(Vector#(K,Bit#(16)) datas);
        method ActionValue#(Vector#(DRAM,Bit#(16))) get;
endinterface

(*synthesize*)
module mkNeuralNet(Std);

		FIFOF#(Bit#(16)) outQ[DRAM];
		Reg#(int) clk <- mkReg(0);
		Reg#(Int#(10)) depth <- mkReg(0);
		Reg#(DataType) store[Filters];
		Pulse p <- mkPulse;
		Reg#(Bool) _clear <- mkReg(False);
		Reg#(Bool) weightsIn <- mkReg(False);
		Reg#(Bool) init <- mkReg(False);
		Reg#(UInt#(16)) recv <- mkReg(0);
		Reg#(UInt#(16)) send <- mkReg(0);
		Reg#(UInt#(8)) weights <- mkReg(0);
		Reg#(UInt#(4)) params <- mkReg(0);
		Reg#(Int#(10)) filters <- mkReg(0);
		Reg#(DataType) weightArray[WEIGHTS];
		Reg#(Int#(16)) _LayerParameters[8];
		Reg#(Bool) _c <- mkReg(False);
		Reg#(Bool) _d <- mkReg(False);
		Wire#(Bool) l <- mkWire;
		
		//#################### LAYER PARAMETERS #################### 
			Reg#(UInt#(16)) _RECV 		<- mkReg(Rx);
			Reg#(Int#(2)) 	_POOL 		<- mkReg(0);	
			Reg#(UInt#(16)) _SEND 		<- mkReg(SEND);
			Reg#(Int#(8))  _IMG  		<- mkReg(0);
			Reg#(Int#(10)) _NUMFILTERS 	<- mkReg(0);
			Reg#(Int#(10)) _DEPTH 		<- mkReg(2);
			Reg#(Int#(10)) _MEMSIZE 	<- mkReg(0);
		//##########################################################	

		for(int i = 0; i<DRAM; i = i + 1)
			outQ[i] <- mkFIFOF;
		
		for(int i=0; i<8; i = i + 1)
			_LayerParameters[i] <- mkReg(0);

		for(int i=0 ;i<Filters; i = i + 1)
			store[i] <- mkReg(0);
	
		for(int i=0;i<WEIGHTS; i = i + 1)
			weightArray[i] <- mkReg(0);
	
		Pool2 pooler <- mkPool2;
	
		//############# IMGRed ##########################################
		Integer buffer_img[1] = {32};
		Component _imgR <- mkSource(1,buffer_img, K);
		//############################################################

		//############# IMGRed ##########################################
                Integer buffer[1] = {32};
                Component _vecSrc <- mkSource(1, buffer, K*Filters);
                //############################################################

		//############# IMGRed ##########################################
                Component _vecSrc1 <- mkSource(1, buffer, K*Filters);
                //############################################################
			
		//##################### TILING #################################
                Integer bufferx[Filters] = {32,32,32,32,32,32,32,32};
                Component _tile <- mkTile(_imgR, 0, 4, 3, IMG, Filters, bufferx, K, 1,1);
                //###########################################################//
				
		//############### Filter 1 ##################################
                Integer buffer1[1] = {32};
                Component _F1 <- mkConvolver(_tile, 0, 3, 1, buffer1, K);
                //###########################################################

		//############### Filter 2 ##################################
                Component _F2 <- mkConvolver(_tile, 1, 3, 1, buffer1, K);
                //###########################################################
	
		//############### Filter 3 ##################################
                Component _F3 <- mkConvolver(_tile, 2, 3, 1, buffer1, K);
                //###########################################################

		 //############### Filter 4 ##################################
                Component _F4 <- mkConvolver(_tile, 3, 3, 1, buffer1, K);
                //###########################################################
	
		 //############### Filter 5 ##################################
		Component _F5 <- mkConvolver(_tile, 4, 3, 1, buffer1, K);
                //###########################################################

                //############### Filter 6 ##################################
                Component _F6 <- mkConvolver(_tile, 5, 3, 1, buffer1, K);
                //###########################################################

                //############### Filter 7 ##################################
                Component _F7 <- mkConvolver(_tile, 6, 3, 1, buffer1, K);
                //###########################################################

                 //############### Filter 8 ##################################
                Component _F8 <- mkConvolver(_tile, 7, 3, 1, buffer1, K);
                //###########################################################


		//###################### VECTOR ###########################
                Component inputs[Filters] = {_F1,_F2,_F3,_F4,_F5,_F6,_F7,_F8};
                Integer inputID[Filters] = {0,0,0,0,0,0,0,0};
                Component vectorize <- mkVector(inputs, Filters, inputID,K);
                //#########################################################

		//############################ STORAGE ##############################
                Component memory <- mkStorageComponent(_vecSrc, K*Filters, K*Filters, 1, SEND-1, False);
                //###################################################################
		
		//############################ STORAGE ##############################
		Component memory2 <- mkStorageComponent(_vecSrc1, K*Filters, DRAM, 1, SEND-1, True);
                //###################################################################

	
		//########################### ACC PARTIAL PRODUCTS #################################
		
		(*descending_urgency = " startInit, accumulate, accumulate2, accumulate4, ass, _ClearPipe "*)
	
		rule startInit(l == True);
				init <= False;
		endrule
		
		rule accumulate (send < _SEND && depth == 0 && init == True);
					//$display(" depth 0 at send %d ", send);
					let d1 <- vectorize.receiveVector;
					//$display(" depth 0 value %d %d at send %d ", fxptGetInt(d1[0]), fxptGetInt(d1[1]), send);
					for( BramLength i=0 ;i<K*Filters; i = i + 1 ) begin
						_vecSrc.send(d1[i],i);
					end
			send <= send + 1;
		endrule 

		rule accumulate2 (send < _SEND && depth > 0 && depth < _DEPTH && init == True);
                        let d1 <- vectorize.receiveVector;
                        let d2 <- memory.receiveVector;
			//$display(" depth 1 at send = %d ", send);
                        for(BramLength i=0 ;i<K*Filters; i = i + 1) begin
                                DataType d = fxptTruncate(fxptAdd(d1[i], d2[i]));
				//if(i == 0)
				//$display(" adding %d + %d at depth %d @ send  = %d recv = %d", fxptGetInt(d1[i]), fxptGetInt(d2[i])/depth, depth, send, recv);
				_vecSrc.send(d,i);
                        end
			send <= send + 1;
                endrule
	
		rule accumulate4 (send < _SEND - 1  && depth == _DEPTH && init == True);	
			let d1 <- vectorize.receiveVector;
			let d2 <- memory.receiveVector;
			//$display(" depth 3 at send = %d ", send); 
			for(BramLength i=0 ;i<K*Filters; i = i + 1) begin
				DataType d = fxptTruncate(fxptAdd(d1[i], d2[i]));
				_vecSrc1.send(d, i);
                        end
			send <= send + 1;
		endrule

		rule ass(send == _SEND - 1 && depth == _DEPTH && init == True);
				send <= send + 1;
		endrule
		
		//#################################################################################

		rule _LayerInit(params == 8 && init == False);
				_tile.reset(_LayerParameters[0]);
				 let d = unpack(pack(_LayerParameters[2]));
				_SEND <= d;
				 memory.reset (d-1);
				 memory2.reset(d-1);
				_DEPTH <= truncate(_LayerParameters[3]);
				_POOL <=  truncate(_LayerParameters[4]);
				let d2 = _LayerParameters[5];
				memory.reset2(d2);
				memory2.reset2(d2);
				_NUMFILTERS <= truncate(_LayerParameters[6]);
				init <= True;		
		endrule
		
		rule fetchWeights(weights == WEIGHTS && init == True && weightsIn == False);
				Vector#(32,DataType) coeffs = newVector;
				for(int i=0; i<Filters; i = i + 1) begin
					for(int j=0; j<9; j = j + 1)
						coeffs[j] = weightArray[i*Filters + j];
					 if(i == 0)
						_F1.sendVector(coeffs);
					 if(i == 1)
                                                _F2.sendVector(coeffs);
					 if(i == 2)
                                                _F3.sendVector(coeffs);
					 if(i == 3)
                                                _F4.sendVector(coeffs);
					 if(i == 4)
                                                _F5.sendVector(coeffs);
					 if(i == 5)
                                                _F6.sendVector(coeffs);
					 if(i == 6)
                                                _F7.sendVector(coeffs);
					 if(i == 7)
                                                _F8.sendVector(coeffs);

				end
				//	$display(" PARAMETERS = IMG %d RECV %d SEND %d  DEPTH %d  POOL = %d MEM = %d", _LayerParameters[0],_LayerParameters[1],_LayerParameters[2],_LayerParameters[3],_LayerParameters[4],_LayerParameters[5]);
				weightsIn <= True;
		endrule
		
		rule _pad (recv >= _RECV && send < _SEND && init == True);	
				Vector#(K,DataType) pad = replicate(0);
				for(BramLength i=0; i<K; i = i + 1)
					_imgR.send(pad[i],i);
		endrule

		rule flushOut (init == True);
			Vector#(2, DataType) datas = newVector;
			for(UInt#(10) i=0; i<DRAM ; i = i + 1) begin
				let d1 <- memory2.receive(0,i);
				datas[i] = d1;
			end
			if(_POOL == 1)
				pooler.send(datas);
			else
				for(UInt#(10) i=0; i<DRAM ; i = i + 1)
					outQ[i].enq(pack(datas[i]));
		endrule

		rule pooled (_POOL == 1 && init == True);
				let d1 <- pooler.reduced;
				outQ[0].enq(pack(d1));
				outQ[1].enq(0);
		endrule

		rule _ClearPipe(send == _SEND && _c == False && init == True);
                        _tile.clean;
                        _imgR.clean;
                        _c <= True;
			_vecSrc.clean;
                        _F1.clean;
                        _F2.clean;
                        _F3.clean;
                        _F4.clean;
			_F5.clean;
                        _F6.clean;
                        _F7.clean;
                        _F8.clean;
                        memory.clean;
			$display(" depth %d processed for filter %d to %d ", depth, filters, filters + Filters);
                        vectorize.clean;
                endrule

                rule _ResetDone (_c == True && _clear == False && init == True);
                        _tile.cleaned;
                        _clear <= True;
                endrule

                rule _ResetDone3 (_c == True && _clear == True && depth == _DEPTH && init == True);
			_c <= False;
                        _clear <= False;
                        send <= 0;
                        recv <= 0;
			depth <= 0;
			weights <= 0;
			weightsIn <= False;
                        _vecSrc1.clean;
                        memory.extra;
			memory2.cleaned;
			if(filters + Filters == _NUMFILTERS && depth == _DEPTH) begin
                                params <= 0;
                                filters <= 0;
                        end
                        else
                                filters <= filters + Filters;

                        $display("                      FILTERS PROCESSED %d depth %d ", filters + Filters, depth );
                        vectorize.clean;

                endrule
		
		rule _ResetDone2 (_c == True && _clear == True && depth < _DEPTH && init == True);
                        _c <= False;
                        _clear <= False;
                        send <= 0;
                        recv <= 0;
			weights <= 0;
			weightsIn <= False;
                        depth <= depth + 1;
		endrule

        	method ActionValue#(Vector#(DRAM,Bit#(16))) get;
			Vector#(DRAM, Bit#(16)) das = newVector;
				for(int i = 0 ;i< DRAM; i = i + 1) begin
					let d1 = outQ[i].first; outQ[i].deq;
		
					DataType n = unpack(d1);
					if( n < 0)
					das[i] = 0;
					else
					das[i] = d1;
				end
                        return das;
                endmethod

        	method Action put(Vector#(K,Bit#(16)) datas) if(outQ[0].notFull && recv < _RECV);

			
			Vector#(K,DataType) d1 = unpack(pack(datas));
	
			if( params < 8 ) begin	
				if(params == 0) begin
					 l <= True;
					 _RECV <= unpack(datas[1]) ;
				end
				
				for(UInt#(4) i=0;i<K; i = i + 1)
					_LayerParameters[params+i] <= unpack(datas[i]);
				params <= params + K;
			end
		
			if( weights < WEIGHTS && params == 8) begin
				for(UInt#(8) i=0;i<K; i = i + 1)
					weightArray[weights+i] <= d1[i];
				
				weights <= weights + K;
			end
			
			if(weights == WEIGHTS && params == 8) begin
				for(BramLength i=0; i < K; i = i + 1)
					_imgR.send(d1[i],i);
				recv <= recv + 1;	
			end
		
		endmethod

endmodule

endpackage
