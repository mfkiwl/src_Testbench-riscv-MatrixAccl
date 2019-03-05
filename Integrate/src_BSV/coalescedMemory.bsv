package coalescedMemory;
import TubeHeader::*;
import BRAM::*;
import Vector::*;
import DefaultValue::*;
import FIFOF::*;
import FixedPoint::*;
import datatypes::*;

//#define VectorLength 256
//#define WORD 16
//#define SZ 256

module mkCoalescedMemory#(Integer size, Bool print)(CoalescedMemory);
	Wire#(Bool) deqStarted <- mkReg(False);
	BRAM_Configure cfg = defaultValue;
	cfg.allowWriteResponseBypass = False;
	cfg.memorySize = size;
	BRAM2Port#(UInt#(20), Bit#(256)) memory <- mkBRAM2Server(cfg);
	Reg#(UInt#(20)) rear <- mkReg(0);
	Reg#(UInt#(20)) front <- mkReg(0);
	Reg#(DataType) cache[16];
	Reg#(UInt#(10)) _CC <- mkReg(0); 
	Reg#(UInt#(10)) _CCR <- mkReg(0); 
	FIFOF#(UInt#(10)) _CCR2 <- mkFIFOF; 
	Reg#(UInt#(10)) _DEGREE <- mkReg(0);
	Reg#(UInt#(20)) _Sz <- mkReg(fromInteger(size-1));
	FIFOF#(Vector#(256,DataType)) send <- mkFIFOF;

	for(int i=0 ;i<16; i = i + 1)
		cache[i] <- mkReg(0);
	

	function BRAMRequest#(UInt#(20), Bit#(256)) makeRequest(Bool write, UInt#(20) addr, Bit#(256) data);
        return BRAMRequest {
                write : write,
                responseOnWrite : False,
                address : addr,
                datain : data
        };
	endfunction

        //rule x (print == True);
	//	$display(" front = %d  rear = %d ", front, rear);
	//endrule

	rule deqRequester (rear != front);

		//if(print == True)
		//	$display(" front = %d  rear = %d ", front, rear);
		
		if(front < 98)
			memory.portB.request.put(makeRequest(False,front,0));
		_CCR2.enq(_CCR);
		if(_CCR + _DEGREE == 16) begin
				if (front == _Sz) begin
                                        front <= 0;
				end
                                else begin
                                        front <= front+1;
				end
			_CCR <= 0;
		end
		else begin	
			_CCR <= _CCR + _DEGREE;
		end
	endrule

	rule fillcache;
		let m = _CCR2.first; _CCR2.deq;
		let d <- memory.portB.response.get;
		Vector#(16,DataType) x = unpack(d);
                Vector#(256,DataType) res = newVector;
		for(UInt#(10) i=0 ;i< 256; i = i + 1) 
			if((m + i) < 16)
				res[i] = x[m + i];
		//$display(" reading from memowry front %d %d CCR2 = %d ", front, fxptGetInt(res[0]), m);
		send.enq(res);
	endrule


	method Action enq(Vector#(256,DataType) datas); //if(send.notFull);
		
		 if(print == True)
                        $display(" front = %d  rear = %d ", front, rear);

		if(_CC == 16) begin
		   for(int i=0; i < 256; i = i + 1)
			if( i < 16)
                        	cache[i] <= datas[i];	
		   _CC <= _DEGREE;
		end
		else begin
		   for(UInt#(10) i=0; i < 256; i = i + 1)
			if((_CC + i) < 16)
                          cache[_CC + i] <= datas[i];       
		end

		if(_CC == 16) begin		
			Vector#(16,DataType) data = newVector;
			for(int i= 0;i <16; i = i + 1)
				data[i] = cache[i];
		
			//$display(" writing to memory rear %d ", rear);	
			memory.portA.request.put(makeRequest(True, rear, pack(data)));
			if (rear == _Sz)
				rear <= 0; 
			else
			rear <= rear +1;
		end
		else 
			_CC <= _CC + _DEGREE;
	
	endmethod

	method ActionValue#(Vector#(256,DataType)) deq;
		let d = send.first; send.deq;
		return d;
	endmethod

	method Action clean (UInt#(10) _D, UInt#(16) sx);
		_Sz <= extend(sx) - 1 ;
		_DEGREE <= _D;
                //send.clear;
		$display(" clearing the rear ");
                rear <= 0;
        endmethod
	
endmodule
endpackage
