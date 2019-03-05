package Dot_product;

import Vector		::*;
import FIFOF		::*;
import GetPut		::*;
import ClientServer	::*;
import StmtFSM		::*;

//import Utils		::*;
//import Req_Rsp		::*;
import Semi_FIFOF	::*;
import Cur_Cycle        ::*;
import Fabric_Defs      ::*;
import AXI4_Lite_Types  ::*;
import CreditCounter :: *;
typedef 64  ASZ;
typedef 64  DSZ;

typedef Bit #(ASZ)  Addr;
typedef Bit #(DSZ)  Data;

typedef 5 N_config_Regs;

interface Dotproduct_IFC;
    method Action reset;
    method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
    interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave;
    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dp_master;
    //interface Client #(Req_I, Rsp_I) mem_bus_ifc;
endinterface

(*synthesize*)
module mkDotproduct(Dotproduct_IFC);

    AXI4_Lite_Slave_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Lite_Slave_Xactor;

    //AXI4_Lite_Master_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Lite_Master_Xactor_2;
    DotEngine_IFC dotEngine <-mkDotEngine;
    
    Reg #(Bit#(2)) alt <-mkReg(0);

    Vector #(N_config_Regs, Reg #(Data)) vrg_configs;
    Integer run	   = 0;     	  	
    Integer addr_A = 1;
    Integer addr_B = 2;
    Integer n      = 3;
    Integer ans    = 4;
 
    vrg_configs [run]       <- mkReg(0);  //0:stop, 1:run
    vrg_configs [addr_A]    <- mkReg(0);
    vrg_configs [addr_B]    <- mkReg(0);
    vrg_configs [n]         <- mkReg(0);
    vrg_configs [ans]	    <- mkReg(0);	

   // Reg #(State)          rg_state      <- mkReg(STATE_START); 
    Reg #(Fabric_Addr)    rg_addr_base  <- mkRegU;
    Reg #(Fabric_Addr)    rg_addr_lim   <- mkRegU;
   
    rule rl_process_rd_req;
	let rda <- pop_o (slave_xactor.o_rd_addr);
	let offset = (rda.araddr - rg_addr_base) >>3;	
	Fabric_Data      rdata = 0;
 	AXI4_Lite_Resp   rresp = AXI4_LITE_OKAY;
	if(offset==0)
		rdata=vrg_configs[0];
	else if(offset==1)
		rdata=vrg_configs[1];
	else if(offset==2)
		rdata=vrg_configs[2];
	else if(offset==3)
	 	rdata=vrg_configs[3];
	else if(offset==4)
		rdata=vrg_configs[4];
	else if(offset >= fromInteger(valueOf(N_config_Regs)))
	  //  rsp=Rsp{command:req.command, data:extend(req.addr), status:DECERR, tid:req.tid};
		rresp = AXI4_LITE_SLVERR;
       let rdr = AXI4_Lite_Rd_Data {rresp: rresp, rdata: rdata , ruser: rda.aruser};
       slave_xactor.i_rd_data.enq(rdr);
      // $display("read");

     endrule

    rule rl_process_wr_req;
	let wra <- pop_o (slave_xactor.o_wr_addr);
	let wrd <- pop_o (slave_xactor.o_wr_data);
        Bit #(64) wdata = zeroExtend(wrd.wdata);	
	let offset = (wra.awaddr - rg_addr_base) >>3;	
 	AXI4_Lite_Resp   bresp = AXI4_LITE_OKAY;
	
	vrg_configs[offset] <= wdata;
//	$display("write");
	$display("vrg_config= %016h_%0d",wdata,offset);
        let wrr = AXI4_Lite_Wr_Resp {bresp: bresp, buser: wra.awuser};
	slave_xactor.i_wr_resp.enq (wrr);
    endrule
  
    rule asgn(vrg_configs[run]==1 && alt==0);
    	dotEngine.start(0,vrg_configs[addr_A],vrg_configs[addr_B],vrg_configs[n],vrg_configs[ans]);
	alt<=1;
//	$display("print");
    endrule

    rule fin(vrg_configs[run]==1 && alt==1);
	//$display("alt=1,done");
	if(dotEngine.done) begin vrg_configs [run] <= 0;
				alt<=0;
//				$display("done");
		           end
    endrule
    
    method Action reset;
	//rg_addr_base <= base_addr;
   	vrg_configs [run] <= 0;
	//$display("reset");
	dotEngine.reset_de;
	slave_xactor.reset;
	//master_xactor.reset;
	//alt<=0;
	
    endmethod
    
    method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
      if (addr_base [1:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_base 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [1:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_lim 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
     // $display("set_addr_map");
      rg_addr_lim  <= addr_lim;
	alt<=0;
    endmethod
 
    interface slave = slave_xactor.axi_side;
    
//    interface config_bus_ifc = toGPServer(f_configReqs, f_configRsps); 
      interface dp_master = dotEngine.dp_master;	
	
endmodule:mkDotproduct

interface DotEngine_IFC;
    method Action reset_de;
    method Action start (UInt #(16) engineId, Addr p1, Addr p2, Addr n, Addr f_ans);
    method Bool done;
    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dp_master;
    //interface Client #(Req_I, Rsp_I) mem_bus_ifc;
endinterface


module mkDotEngine (DotEngine_IFC);
    
    Bit #(Wd_User) dummy_user =?;
   // Bit #(64) rg_st_amo_val =?;
    Bit #(TDiv #(Wd_Data, 8)) fabric_strb='b11111111;
       	
    Reg #(UInt #(16)) rg_engineId <- mkRegU;
    Reg #(Bool) rg_running <- mkReg(False);		    
   
    FIFOF #(Data) f_data0 <-mkFIFOF;
    FIFOF #(Data) f_data1 <-mkFIFOF;
    Reg #(Data) accu <- mkReg(0);
    
    Reg #(Addr) count <- mkReg(0);
    Reg #(Bit #(2)) rg_f <- mkReg(0);
    Reg #(Bit #(2)) rg_fn <- mkReg(0);
     Reg #(Addr) rg_p1 <- mkRegU;
     Reg #(Addr) rg_p2 <- mkRegU;
     Reg #(Addr) rg_ans <- mkRegU;
     Reg #(Addr) rg_n <- mkRegU;
     Reg #(Addr) rg_p1_ad <- mkRegU;
     Reg #(Addr) rg_p2_ad <- mkRegU;
    Reg #(Bit #(2)) rg_f_rsp  <- mkRegU;



/*function Tuple3 #(Bit #(Wd_Addr),               // addr is 32b- or 64b-aligned
		  Bit #(Wd_Data),               // data is lane-aligned
		  Bit #(TDiv #(Wd_Data, 8)))    // strobep
   fn_to_fabric_addr_data_strobe (Bit #(3) f3,
				  Bit #(n) addr,
				  Bit #(64) word64)    // data is in lsbs
   provisos (Add #(_, n, 64));

   // First compute addr, data and strobe for a 64b-wide fabric
   Bit #(8)  strobe64    = 0;
   Bit #(3)  shift_bytes = addr [2:0];
   Bit #(6)  shift_bits  = { shift_bytes, 3'b0 };
   // Bit #(64) addr64      = extend (addr);    TODO: DELETE
   Bit #(64) addr64      = extend (addr & (~ 'b111));    // 64b align

   case (f3 [1:0])
      f3_SIZE_B: begin
		    word64   = (word64 << shift_bits);
		    strobe64 = ('b_1   << shift_bytes);
		 end
      f3_SIZE_H: begin
		    word64   = (word64 << shift_bits);
		    strobe64 = ('b_11  << shift_bytes);
		 end
      f3_SIZE_W: begin
		    word64   = (word64  << shift_bits);
		    strobe64 = ('b_1111 << shift_bytes);
		 end
      f3_SIZE_D: begin
		    strobe64 = 'b_1111_1111;
		 end
   endcase

   // Adjust for 32b fabrics
   if ((valueOf (Wd_Data) == 32) && (addr [2] == 1'b1)) begin
      addr64   = (addr64 | 'b100);
      word64   = { 32'h0, word64 [63:32] };
      strobe64 = { 4'h0, strobe64 [7:4] };
   end

   // Finally, create fabric addr/data/strobe
   Bit #(Wd_Addr)            fabric_addr   = truncate (addr64);
   Bit #(Wd_Data)            fabric_data   = truncate (word64);
   Bit #(TDiv #(Wd_Data, 8)) fabric_strobe = truncate (strobe64);

   return tuple3 (fabric_addr, fabric_data, fabric_strobe);
endfunction: fn_to_fabric_addr_data_strobe*/

    AXI4_Lite_Master_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Lite_Master_Xactor;

    //Rsp_I next_rsp = f_memRsps.first;
    CreditCounter_IFC #(10) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

    rule rl_req0(rg_running && rg_f==0 && (rg_p1 <rg_p1_ad + rg_n*8));
	//Req_I req = Req{command:READ, addr:rg_p1, data:?, b_size:BITS32, tid:0};
	let mem_req = AXI4_Lite_Rd_Addr{araddr: rg_p1,arprot: 0, aruser: dummy_user};	
	rg_p1 <= rg_p1+8;
	//f_memReqs.enq (req);
	master_xactor.i_rd_addr.enq(mem_req);
	rg_f<=1;
//	$display("%0d_req_mem0_%08h",cur_cycle,rg_p1);
    endrule

    //rule rl_rsp0((next_rsp.command==READ) && (next_rsp.tid==0));
    rule rl_rsp0 (rg_running && rg_f_rsp == 0);
	//f_memRsps.deq;
	let mem_rsp <- pop_o (master_xactor.o_rd_data);
	Bit #(64) x64 = zeroExtend (mem_rsp.rdata);
	//f_data0.enq (next_rsp.data);
	f_data0.enq(x64);
        rg_f_rsp <= 1;
//	$display("%0d_rsp_mem0_%0d_%08h",cur_cycle,x64,(rg_p1-8));
    endrule

    rule rl_req1(rg_running && rg_f==1 && (rg_p2<rg_p2_ad + rg_n*8));
	//Req_I req = Req{command:READ, addr:rg_p2, data:?, b_size:BITS32, tid:1};
	let mem_req = AXI4_Lite_Rd_Addr{araddr: rg_p2,arprot: 0, aruser: dummy_user};	
	rg_p2 <= rg_p2+8;
	//f_memReqs.enq (req);
	master_xactor.i_rd_addr.enq(mem_req);
//	$display("%0d_req_mem1_%08h",cur_cycle,rg_p2);
	rg_f<=0;
    endrule

    //rule rl_rsp1((next_rsp.command ==READ) && (next_rsp.tid==1));
    rule rl_rsp1(rg_running && rg_f_rsp == 1);
	//f_memRsps.deq;
	//f_data1.enq (next_rsp.data);
	let mem_rsp <- pop_o (master_xactor.o_rd_data);
	Bit #(64) x64 = zeroExtend (mem_rsp.rdata);
	//$display("%0d_rsp_mem1_%0d_%08h",cur_cycle,x64,(rg_p2-8));
	f_data1.enq(x64);
        rg_f_rsp <= 0;
	count<=count+1;
    endrule
    
    rule dodot(rg_running && (count <=rg_n));
//	$display("datain== %0d*_*%0d ",f_data0.first,f_data1.first);
	accu <= accu + f_data0.first*f_data1.first;
	f_data0.deq;
	f_data1.deq;	
//	$display("dotpro_doing=%0d_%0d",accu+f_data0.first*f_data1.first,count);
	//$display("dotpro");
	if(count==rg_n) begin
    		//Req_I req = Req{command:WRITE, addr:rg_ans, data:accu+f_data0.first*f_data1.first, b_size:BITS32, tid:?};
		//f_memReqs.enq(req);
		 /* match {.fabric_addr,
			 .fabric_data,
			 .fabric_strb } = fn_to_fabric_addr_data_strobe (rg_ans, accu+f_data0.first*f_data1.first, rg_st_amo_val);*/
		let dp_ans_addr = AXI4_Lite_Wr_Addr {awaddr: rg_ans, awprot:0, awuser: dummy_user};
		let dp_ans_data = AXI4_Lite_Wr_Data {wdata: accu+f_data0.first*f_data1.first, wstrb: fabric_strb};
		master_xactor.i_wr_addr.enq (dp_ans_addr);
		master_xactor.i_wr_data.enq (dp_ans_data);
        ctr_wr_rsps_pending.incr;  
		rg_running <= False;
	//	rg_fn<=1;
	//	$display("ans=%0d_%08h",accu+f_data0.first*f_data1.first,rg_ans);
	end
    endrule

    //rule rl_drain_write_rsps(rg_fn==1);
    rule rl_drain_write_rsps;
	//f_memRsps.deq;
	let wr_resp <- pop_o (master_xactor.o_wr_resp);
	//rg_fn<=0;
     if (ctr_wr_rsps_pending.value == 0) begin
     $display ("%0d: ERROR:rl_discard_write_rsp: unexpected W response (ctr_wr_rsps_pending.value == 0)",
           cur_cycle);
     $display ("    ", fshow (wr_resp));
     $finish (1);    // Assertion failure
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != AXI4_LITE_OKAY) begin
     // TODO: need to raise a non-maskable interrupt (NMI) here
     $display ("%0d:rl_discard_write_rsp: fabric response error: exit", cur_cycle);
     $display ("    ", fshow (wr_resp));
     $finish (1);    // TODO: error response on memory write; raise interrupt
      end
      else 
        // if (cfg_verbosity > 1) begin
            $display ("%0d:rl_discard_write_rsp: pending %0d ",
           cur_cycle, ctr_wr_rsps_pending.value, fshow (wr_resp));
     // end
    //rg_fn<=0;
    $display("mem_write_deq------%0d",cur_cycle);

//	$display("drain wr_req");
    endrule
    
    method Action reset_de;
	rg_running <= False;
	f_data0.clear;
	f_data1.clear;
	master_xactor.reset;
    endmethod

    method Action start (UInt #(16)engineId, Addr p1, Addr p2, Addr n, Addr f_ans)
if(! rg_running);
	rg_engineId <=engineId;
	rg_n 	    <= n;
	rg_running <= True;
	rg_p1 <= p1;
	rg_p2 <= p2;
	rg_ans <= f_ans;
        rg_p1_ad <= p1;
        rg_p2_ad <= p2;
	rg_fn<=0;
	accu<=0;
	count<=0;
	rg_f_rsp<=0;
    endmethod

    method Bool done;
	return (! rg_running);
    endmethod	
	
      interface dp_master = master_xactor.axi_side;
			
endmodule:mkDotEngine
endpackage:Dot_product
