package TubeHeader;
import datatypes::*;
import Vector::*;

typedef 256 DspMax;
typedef 256 VectorLength;

typedef struct{
	Integer row1;
	Integer row2; 
	Integer col1; 
	Integer col2;  
	Integer size;
} Relay deriving(Eq, Bits);

typedef struct{
        Integer k;
        Integer stencil;
        Integer img;
        Integer banks;
	Vector#(100,CoeffType) weights;
}Stencil deriving(Eq, Bits);

typedef (function DataType compute(Vector#(VectorLength,DataType) inputs)) Compute;

typedef (function Vector#(VectorLength,DataType) compute(Vector#(VectorLength,DataType) inputs)) Permute;

Relay r = Relay{row1:0,row2:0,col1:0,col2:0};
Relay _samp[1] = {r};
Relay _dRelay[1][1] = {_samp};

interface Reducer;
        method Action send(Vector#(100,DataType) data);
        method ActionValue#(DataType) reduced;
endinterface:Reducer

interface Component;
        method Action send(DataType data, BramLength i);
	method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        method ActionValue#(DataType) forwarded(BramLength i, UInt#(10) _rate);
        
	method ActionValue#(Vector#(VectorLength,DataType)) receiveVector(UInt#(10) _ID);
        method Action sendVector(Vector#(VectorLength, DataType) inx);
	
	method Action  extra;
	method Action clean;
	method Action drain (UInt#(10) _D, UInt#(16) sx);
	method Action cleaned;
	method Action reset(Int#(16) param);
	method Action reset2(Int#(16) _dc);
endinterface: Component

interface Conv;
        method Action sendP(Vector#(VectorLength, DataType) datas);
        method Action sendF(Vector#(VectorLength, CoeffType) filter);
        method ActionValue#(DataType) result;
	method Action clean;
endinterface

interface ConvBlock;
        method Action sendP(Vector#(VectorLength, DataType) datas);
        method Action sendF(Vector#(DspMax, CoeffType) filter);
        method ActionValue#(Vector#(DspMax, DataType)) result;
	method Action reset(Int#(8) _KERNEL);
	method Action cleaned;
endinterface


interface BFIFO;
        method Action enq(DataType val);
        method ActionValue#(DataType) deq;
	method Action clean;
endinterface: BFIFO

interface BFIFO2;
        method Action enq(DataType val);
        method Action enqDDR(Vector#(2,DataType) val);
        method ActionValue#(DataType) deq;
	method ActionValue#(Vector#(2, DataType)) deqDDR;
        method Action clean;
	method Action reset(Int#(16) rx);
endinterface: BFIFO2


interface Distribute;
        method Action put(Vector#(VectorLength, DataType) datas);
        method Action put2(Vector#(DspMax, DataType) datas);
        method ActionValue#(Vector#(DspMax,DataType)) result;
        method Action clean(Int#(8) x);
endinterface

interface TreeSum;
        method Action put(Vector#(DspMax, DataType) datas);
        method ActionValue#(Vector#(DspMax,DataType)) result;
        method Action clean(Int#(8) _KER);
	method Action cleaned;
endinterface

interface CoalescedMemory;
        method Action enq(Vector#(VectorLength,DataType) datas);
        method ActionValue#(Vector#(VectorLength,DataType)) deq;
	method Action clean (UInt#(10) _D, UInt#(16) sx);
endinterface

typedef struct {
   Bit #(1) valid;
   Bit #(128) data;
   Bit #(16) slot;
   Bit #(4) pad;
   Bit #(1) last;
} PCIE_PKT deriving (Bits, Eq, FShow);

 
endpackage: TubeHeader
