package BlurTB;
import BRam::*;
import Blur::*;
import NeuralNet::*;
import TubeHeader::*;
import FixedPoint::*;
import datatypes::*;
import bram::*;
import pulse::*;
import Vector::*;
import Stage::*;
import FIFO::*;

import "BDPI" function Action initialize_image();
import "BDPI" function Bit#(16) streamData(Int#(32) index,Int#(32) la);
import "BDPI" function Action store(Bit#(16) data, Bit#(16) data1, Int#(32) index);
import "BDPI" function Action dumpLayer(Int#(32) index);
import "BDPI" function Action inc(Int#(32) la);

#define WEIGHTS 72 
#define K 2

(*synthesize*)
module mkBlurTB();

		Reg#(int) clk <- mkReg(0);
		Reg#(int) rows  <- mkReg(0);
       		Reg#(int) cols  <- mkReg(0);
		Reg#(Bool) init <- mkReg(True);
		Reg#(int) c0 <- mkReg(0);
		Reg#(int) c1 <- mkReg(0);
		Reg#(int) c3 <- mkReg(0);
		Reg#(int) offset <- mkReg(0);
		Reg#(int) slice <- mkReg(0);
		Reg#(int) z <- mkReg(0);
		Reg#(int) layer <- mkReg(0);
		Std cnnR <- mkNeuralNet;

		Int#(32)  _LayerInputs[3]  =  {301488,1607936,403712};  //{301272, 803392}; // FILTERS/Filters * depth* (Filters * 9 + IMG*IMG) + 6
		Int#(32)  _LayerOutputs[3]  = {394268,197132,96796};     //{197136, 98568}    // Filters * (IMG-2*IMG-2)
		
		Reg#(Bool) stream <- mkReg(True);

		
		rule init_rule (init) ;
                	initialize_image();
                	init <= False;
      		endrule

		rule update_clock;
                   
			clk <= clk + 1;
      		endrule
		
		rule layerIn(clk>=1 && stream == True);

				Vector#(K,Bit#(16)) packet = newVector;
				for(int i = 0; i<K; i = i + 1) begin

				if(z < 8) begin
					Bit#(16) px = streamData(i,layer);
					packet[i] = px;
				end
				if( z >= 8 && z < WEIGHTS ) begin
                                	Bit#(16) px = streamData(i,layer);
                                	CoeffType wei = unpack(px);
                                	CoeffType zero = 0;
                                	if(px[0] == 1)
                                        wei = fxptTruncate(fxptSub(zero,wei));
					packet[i] = pack(wei);
                       		end
				if( z >= WEIGHTS) begin
                                	Bit#(16) px = streamData(i,layer);
					DataType d = unpack(px);
					packet[i] = pack(d);
				end	
				end
				
				cnnR.put(packet);
				inc(layer);
				if( z >= _LayerInputs[layer]+6) begin
                                	stream <= False;
                                	z <= 0;
                        	end
                        	else
                                	z <= z + K;
			
		endrule

		rule layerOut; //(clk%50 == 0);
					if( c0 < _LayerOutputs[layer]-1) begin
					let d <- cnnR.get;
			
							Vector#(K,DataType) dx = unpack(pack(d));		
							store(d[0],d[1], layer+1);
							
					c0 <= c0 + 1;
					end
					else begin
					c0 <= 0;
					dumpLayer(layer + 1);
					$display(" LAYER DONE ");
					stream <= True;
					layer <= layer + 1;
					c1 <= c1 + 1;
					if ( c1 == 2 )
						$finish(0);
					end
		endrule          
endmodule

endpackage
