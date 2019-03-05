package Dot_product;

import Vector		::*;
import FIFOF		::*;
import GetPut		::*;
import ClientServer	::*;
import StmtFSM		::*;

//import Utils		::*;
//import Req_Rsp		::*;
//import Sys_Configs	::*;
//========================================RISCV_specific============
import Semi_FIFOF   ::*;
import Cur_Cycle        ::*;
import Fabric_Defs      ::*;
import AXI4_Lite_Types  ::*;
import CreditCounter :: *;

typedef 64  ASZ;
typedef 64  DSZ;

typedef Bit #(ASZ)  Addr;
typedef Bit #(DSZ)  Data;
//========================================imports for image processing========
import Hardware     ::*;

typedef 4 N_config_Regs;

interface Dotproduct_IFC;
    method Action reset;
    method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
    interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave;
    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dp_master;
   // interface Server #(Req_T, Rsp_T) config_bus_ifc;
   // interface Client #(Req_I, Rsp_I) mem_bus_ifc;
endinterface

(*synthesize*)
module mkDotproduct(Dotproduct_IFC);
    AXI4_Lite_Slave_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Lite_Slave_Xactor;

    Reg #(Addr) rg_base_addr <-mkRegU;
    Reg #(Bit#(2)) alt <-mkReg(0);

   // FIFOF #(Req_T) f_configReqs <-mkFIFOF;
    //FIFOF #(Rsp_T) f_configRsps <-mkFIFOF;
    Vector #(N_config_Regs, Reg #(Data)) vrg_configs;
    Integer run	   = 0;     	  	
    Integer addr_A = 1;
    //Integer addr_B = 2;
    Integer n      = 2;
    Integer ans    = 3;
 
    vrg_configs [run]       <- mkReg(0);  //0:stop, 1:run
    vrg_configs [addr_A]    <- mkReg(0);
    //vrg_configs [addr_B]    <- mkReg(0);
    vrg_configs [n]         <- mkReg(0);
    vrg_configs [ans]	    <- mkReg(0);	
    Reg #(Fabric_Addr)    rg_addr_base  <- mkRegU;
    Reg #(Fabric_Addr)    rg_addr_lim   <- mkRegU;
   
/*    rule rl_0;
	Req_T req = f_configReqs.first; f_configReqs.deq;
	Rsp_T rsp;
	let offset = (req.addr - rg_base_addr) >>3;
	if(offset >= fromInteger(valueOf(N_config_Regs)))
	    rsp=Rsp{command:req.command, data:extend(req.addr), status:DECERR, tid:req.tid};
	else if(f_configReqs.first.command==READ) 
	    rsp = Rsp{command:req.command, data:vrg_configs[offset], status:OKAY, tid:req.tid};
	else begin
	    vrg_configs[offset] <= req.data;
	    rsp = Rsp{command:req.command, data:extend (req.addr), status:OKAY, tid:req.tid};
	end
	f_configRsps.enq(rsp);
	
     endrule*/
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
   // else if(offset==4)
     //   rdata=vrg_configs[4];
    else if(offset >= fromInteger(valueOf(N_config_Regs)))
      //  rsp=Rsp{command:req.command, data:extend(req.addr), status:DECERR, tid:req.tid};
        rresp = AXI4_LITE_SLVERR;
       let rdr = AXI4_Lite_Rd_Data {rresp: rresp, rdata: rdata , ruser: rda.aruser};
       slave_xactor.i_rd_data.enq(rdr);
      // $display("read");

     endrule

    DotEngine_IFC dotEngine <-mkDotEngine;
    rule rl_process_wr_req;
    let wra <- pop_o (slave_xactor.o_wr_addr);
    let wrd <- pop_o (slave_xactor.o_wr_data);
        Bit #(64) wdata = zeroExtend(wrd.wdata);    
    let offset = (wra.awaddr - rg_addr_base) >>3;   
    AXI4_Lite_Resp   bresp = AXI4_LITE_OKAY;
    
    vrg_configs[offset] <= wdata;
//  $display("write");
    $display("vrg_config= %016h_%0d",wdata,offset);
    let wrr = AXI4_Lite_Wr_Resp {bresp: bresp, buser: wra.awuser};
    slave_xactor.i_wr_resp.enq (wrr);
    endrule

    rule asgn(vrg_configs[run]==1 && alt==0);
    	dotEngine.start(0,vrg_configs[addr_A],vrg_configs[n],vrg_configs[ans]);
	alt<=1;
	$display("dotengine-started_%0d",cur_cycle);
    endrule

    rule fin(vrg_configs[run]==1 && alt==1);
	if(dotEngine.done) begin vrg_configs [run] <= 0;
				alt<=0;
				$display("fin_rule_%0d",cur_cycle);
		           end
    endrule
    
    method Action reset;
	//rg_base_addr <= base_addr;
   	vrg_configs [run] <= 0;
	//f_configReqs.clear;
	//f_configRsps.clear;
	dotEngine.reset_de;
    slave_xactor.reset;
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
    
    interface dp_master = dotEngine.dp_master;    
    
    //interface config_bus_ifc = toGPServer(f_configReqs, f_configRsps); 
    //interface mem_bus_ifc = dotEngine.mem_bus_ifc;	
	
endmodule:mkDotproduct

interface DotEngine_IFC;
    method Action reset_de;
    method Action start (UInt #(16) engineId, Addr p1, Addr n, Addr f_ans);
    method Bool done;
    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dp_master;
   // interface Client #(Req_I, Rsp_I) mem_bus_ifc;
endinterface


module mkDotEngine (DotEngine_IFC);
	  Bit #(Wd_User) dummy_user =?;
   // Bit #(64) rg_st_amo_val =?;
    Bit #(TDiv #(Wd_Data, 8)) fabric_strb='b11111111;

    Reg #(UInt #(16)) rg_engineId <- mkRegU;
    Reg #(Bool) rg_running <- mkReg(False);		    
   // FIFOF #(Req_I) f_memReqs <-mkFIFOF;
   // FIFOF #(Rsp_I) f_memRsps <-mkFIFOF;
    Vector #(196, Reg #(Data)) array<-replicateM(mkReg(0));
   
    FIFOF #(Data) f_data0 <-mkFIFOF;
    FIFOF #(Data) f_data1 <-mkFIFOF;
    Reg #(Data) accu <- mkReg(0);
    Reg #(Data) fin_pix_t <- mkReg(0);
    Reg #(Data) fin_pix <- mkReg(0);
    Reg #(Data) i <- mkReg(0);
    Reg #(Bit#(2)) flg <- mkReg(1);
    Reg #(Bit#(2)) flg2 <- mkReg(0);
    
    Reg #(Addr) count <- mkReg(0);
    Reg #(Addr) count2 <- mkReg(0);
    Reg #(Addr) send <- mkReg(0);
    Reg #(Bit #(2)) rg_f <- mkReg(0);
    Reg #(Bit #(2)) rg_fn <- mkReg(0);
     Reg #(Addr) rg_p1 <- mkRegU;
     Reg #(Addr) req_c <- mkReg(0);
     Reg #(Addr) rg_ans <- mkRegU;
     Reg #(Addr) rg_n <- mkRegU;
     Reg #(Addr) rg_p1_ad <- mkRegU;
 CreditCounter_IFC #(10) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding
//==================================instantiating image processing======= 
     Stdin conv<-mkHardware; 
//=======================================================================  
   AXI4_Lite_Master_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Lite_Master_Xactor;
     //Rsp_I next_rsp = f_memRsps.first;


    rule rl_req0(rg_running && (rg_p1 <rg_p1_ad + rg_n*rg_n*8));
    let mem_req = AXI4_Lite_Rd_Addr{araddr: rg_p1,arprot: 0, aruser: dummy_user};   
//	Req_I req = Req{command:READ, addr:rg_p1, data:?, b_size:BITS32, tid:0};
	rg_p1 <= rg_p1+8;
	//$display("rl_req0_%0d_______%0d",cur_cycle,req_c);
	//memReqs.enq (req);
    master_xactor.i_rd_addr.enq(mem_req);
    req_c<=req_c+1;
    endrule

   // rule rl_rsp0((next_rsp.command==READ) && (next_rsp.tid==0));
    rule rl_rsp0 (rg_running);
	let mem_rsp <- pop_o (master_xactor.o_rd_data);
    Bit #(64) x64 = zeroExtend (mem_rsp.rdata);
   // f_memRsps.deq;
	//$display("rl_rsp0_%0d",cur_cycle);
	f_data0.enq (x64);
    endrule

    rule dodot(rg_running && (count <(rg_n*rg_n)) && flg2==0);
//	$display("doing_put_%0d------accu==%0d---------cycle=%0d",count,f_data0.first,cur_cycle);
    accu<=f_data0.first;
    flg2<=1;
    endrule
    
    rule dodot_put(rg_running && count < (rg_n*rg_n) && flg2==1);
    Bit#(10) accu_t=truncate(accu);
   // $display("accu_t==%0d-------accu==%0d------cycle==%0d",accu_t,accu,cur_cycle);
    conv.put(accu_t);
	f_data0.deq;
    count<=count+1;
    $display("count_of_put================%0d-----%0d",count,cur_cycle);
    flg2<=0;
    endrule

    rule dodt_get(rg_running && (count2 <(rg_n-2)*(rg_n-2)));
    let y <- conv.get();
    array[i]<=extend(y);
    i<=i+1;
   // $display("get_running----------------------======%0d_____y===%0d----fin-pix==%0d----count2===%0d",cur_cycle,y,fin_pix_t,count2);
    count2<=count2+1;
    $display("count_of_get================%0d---------%0d-----%0d",count2,i,cur_cycle);

    endrule
    
    rule writ_mem(rg_running && count2==(rg_n-2)*(rg_n-2) && send<(rg_n-2)*(rg_n-2) && rg_fn==0);
    //Req_I req = Req{command:WRITE, addr:rg_ans, data:array[send], b_size:BITS32, tid:?};1
    //f_memReqs.enq(req);
    let dp_ans_addr = AXI4_Lite_Wr_Addr {awaddr: rg_ans+send*8, awprot:0, awuser: dummy_user};
        let dp_ans_data = AXI4_Lite_Wr_Data {wdata: array[send], wstrb: fabric_strb};
        master_xactor.i_wr_addr.enq (dp_ans_addr);
        master_xactor.i_wr_data.enq (dp_ans_data);
        ctr_wr_rsps_pending.incr;   
	//$display("-------------------------------------------done======%0d----fin_pix_t=%0d-----fin_pix_extend=%0d",cur_cycle,fin_pix_t,fin_pix);
	$display("memory __writing======================send====%0d-----addr==%8h---%0d",send,(rg_ans+send*8),cur_cycle);
    send<=send+1;
    rg_fn<=1;
    endrule
    
    rule writ_mem_fin(rg_running && count2==(rg_n-2)*(rg_n-2) && send==((rg_n-2)*(rg_n-2)) && rg_fn==0);
    //Req_I req = Req{command:WRITE, addr:rg_ans, data:array[send], b_size:BITS32, tid:?};
    //f_memReqs.enq(req);
    let dp_ans_addr = AXI4_Lite_Wr_Addr {awaddr: rg_ans+send*8, awprot:0, awuser: dummy_user};
        let dp_ans_data = AXI4_Lite_Wr_Data {wdata: array[send], wstrb: fabric_strb};
        master_xactor.i_wr_addr.enq (dp_ans_addr);
        master_xactor.i_wr_data.enq (dp_ans_data);
	rg_running <= False;
	ctr_wr_rsps_pending.incr;   
	//$display("-------------------------------------------done======%0d----fin_pix_t=%0d-----fin_pix_extend=%0d",cur_cycle,fin_pix_t,fin_pix);
//	$display("memory __writing==========================");
	$display("memory __writing======================send====%0d--------%0d",send,cur_cycle);
    $display("khatammmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm");
    rg_fn<=1;
    endrule

    rule rl_drain_write_rsps(rg_fn==1);
	//f_memRsps.deq;
    let wr_resp <- pop_o (master_xactor.o_wr_resp);
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
    rg_fn<=0;
    $display("mem_write_deq------%0d",cur_cycle);
    endrule

    
    method Action reset_de;
	rg_running <= False;
	//f_memReqs.clear;
	//f_memRsps.clear;
	f_data0.clear;
	f_data1.clear;
	master_xactor.reset;
    endmethod

    method Action start (UInt #(16)engineId, Addr p1, Addr n, Addr f_ans)
        if(! rg_running);
	        rg_engineId <=engineId;
	        rg_n 	    <= n;
	        rg_running <= True;
	        rg_p1 <= p1;
	        rg_ans <= f_ans;
            rg_p1_ad <= p1;
    endmethod

    method Bool done;
	return (! rg_running);
    endmethod	
	interface dp_master = master_xactor.axi_side;
    //interface mem_bus_ifc = toGPClient (f_memReqs, f_memRsps);
			
endmodule:mkDotEngine
endpackage:Dot_product
