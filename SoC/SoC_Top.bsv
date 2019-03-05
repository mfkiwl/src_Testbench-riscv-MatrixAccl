// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved.

package SoC_Top;

// ================================================================
// This package is the SoC "top-level".

// (Note: there will be further layer(s) above this for
//    simulation top-level, FPGA top-level, etc.)

// ================================================================
// Exports

export SoC_Top_IFC (..), mkSoC_Top;

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import Memory        :: *;

// ----------------
// BSV additional libs

import Cur_Cycle   :: *;
import GetPut_Aux  :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Lite_Types  :: *;
import AXI4_Lite_Fabric :: *;

import Fabric_Defs :: *;
import SoC_Map     :: *;
import SoC_Fabric  :: *;

// SoC components (CPU, mem, and IPs)
import BRVF_Core_IFC  :: *;
import BRVF_Core      :: *;

import Boot_ROM       :: *;
import Mem_Controller :: *;
import Timer          :: *;
import UART_Model     :: *;
import Dot_product    :: *;

`ifdef INCLUDE_CAMERA_MODEL
import Camera_Model   :: *;
`endif

`ifdef INCLUDE_ACCEL0
import Accel_AES      :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import External_Control :: *;    // Control requests/responses from HSFE
import Debug_Module     :: *;
`endif

// ================================================================
// PC reset value

// Original from older RISC-V specs
// Bit #(64)  pc_reset_value    = 'h0200;

// Entry point for Boot ROM used in Spike/Rocket
// (boot code jumps later to 'h_8000_0000)
Bit #(64)  pc_reset_value    = 'h_0000_1000;

// Entry point for code generated for Spike/Rocket
// Bit #(64)  pc_reset_value    = 'h_8000_0000;

// ================================================================
// Local types and constants

typedef enum {SOC_START, SOC_RESETTING, SOC_IDLE} SoC_State
deriving (Bits, Eq, FShow);

// ================================================================
// The outermost interface of the SoC

interface SoC_Top_IFC;
`ifdef INCLUDE_GDB_CONTROL
   // To external controller (E.g., GDB)
   interface Server #(Control_Req, Control_Rsp) server_external_control;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface Get #(Info_CPU_to_Verifier) verify_out;
`endif

   // External real memory
   interface MemoryClient #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word)  to_raw_mem;

   // UART0 to external console
   interface Get #(Bit #(8)) get_to_console;
   interface Put #(Bit #(8)) put_from_console;
endinterface

// ================================================================
// The module

(* synthesize *)
module mkSoC_Top (SoC_Top_IFC);
   Integer verbosity = 1;    // Normally 0; non-zero for debugging

   Reg #(SoC_State) rg_state <- mkReg (SOC_START);

   // SoC address map specifying base and limit for memories, IPs, etc.
   SoC_Map_IFC soc_map <- mkSoC_Map;

   // CPU + Debug module
   BRVF_Core_IFC  brvf_core <- mkBRVF_Core (pc_reset_value);

   // SoC Fabric
   Fabric_IFC  fabric <- mkFabric;

   // SoC Boot ROM
   Boot_ROM_IFC  boot_rom <- mkBoot_ROM;

   // SoC Memory
   Mem_Controller_IFC  mem0_controller <- mkMem_Controller;

   // SoC IPs
   Timer_IFC  timer0 <- mkTimer;
   UART_IFC   uart0  <- mkUART;
   Dotproduct_IFC dotpro0 <- mkDotproduct;

`ifdef INCLUDE_ACCEL0
   // Accel0 master to fabric
   Accel_AES_IFC  accel_aes0 <- mkAccel_AES;
`endif

   // ----------------
   // SoC fabric master connections
   // Note: see 'SoC_Map' for 'master_num' definitions

   // CPU IMem master to fabric
   mkConnection (brvf_core.cpu_imem_master,  fabric.v_from_masters [imem_master_num]);

   // CPU DMem master to fabric
   mkConnection (brvf_core.cpu_dmem_master,  fabric.v_from_masters [dmem_master_num]);

`ifdef INCLUDE_GDB_CONTROL
   // Debug Module system buf interface (mem interface) to fabric
   mkConnection (brvf_core.dm_master,  fabric.v_from_masters [debug_module_master_num]);
`else
   AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dummy_dm_master = dummy_AXI4_Lite_Master_ifc;
   mkConnection (dummy_dm_master,  fabric.v_from_masters [debug_module_master_num]);
`endif

`ifdef INCLUDE_ACCEL0
   // accel_aes0 to fabric
   mkConnection (accel_aes0.master,  fabric.v_from_masters [accel0_master_num]);
`endif
	
   mkConnection (dotpro0.dp_master, fabric.v_from_masters [dotpro0_master_num]);

   // ----------------
   // SoC fabric slave connections
   // Note: see 'SoC_Map' for 'slave_num' definitions

   // Fabric to Boot ROM
   mkConnection (fabric.v_to_slaves [boot_rom_slave_num], boot_rom.slave);

   // Fabric to CPU's Near_Mem back door
   mkConnection (fabric.v_to_slaves [tcm_back_door_slave_num],   brvf_core.cpu_slave);

   // Fabric to Mem Controller
   mkConnection (fabric.v_to_slaves [mem0_controller_slave_num], mem0_controller.slave);

   // Fabric to UART0
   mkConnection (fabric.v_to_slaves [uart0_slave_num],           uart0.slave);

   // Fabric to Timer0
   mkConnection (fabric.v_to_slaves [timer0_slave_num],          timer0.slave);
   
   // Fabric to Dotproduct0
   mkConnection (fabric.v_to_slaves [dotpro0_slave_num],          dotpro0.slave);

`ifdef INCLUDE_ACCEL0
   // Fabric to accel_aes0
   mkConnection (fabric.v_to_slaves [accel0_slave_num],          accel_aes0.slave);
`endif

   // ----------------
   // Connect interrupt sources for CPU interrupt request inputs.

   // External interrupts. TODO: connect to external interrupt controller
   rule rl_connect_external_interrupt_request (False);
      brvf_core.cpu_external_interrupt_req;
   endrule

   // Software interrupt
   rule rl_connect_software_interrupt_request;
      let x <- timer0.get_sw_interrupt_req.get;
      brvf_core.cpu_software_interrupt_req;
   endrule

   // Timer interrupt
   rule rl_connect_timer_interrupt_request;
      let req <- timer0.get_timer_interrupt_req.get;
      brvf_core.cpu_timer_interrupt_req (req);
      if (verbosity > 1)
	 $display ("%0d: SoC_Top.rl_connect_timer_interrupt_request: ", cur_cycle, fshow (req));
   endrule

   // ================================================================
   // BEHAVIOR WITH DEBUG MODULE

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // External debug requests and responses

   FIFOF #(Control_Req) f_external_control_reqs <- mkFIFOF;
   FIFOF #(Control_Rsp) f_external_control_rsps <- mkFIFOF;

   Control_Req req = f_external_control_reqs.first;

   rule rl_handle_external_req_read_request (req.op == external_control_req_op_read_control_fabric);
      f_external_control_reqs.deq;
      brvf_core.dm_dmi.read_addr (truncate (req.arg1));
      if (verbosity != 0) begin
	 $display ("%0d: SoC_Top.rl_handle_external_req_read_request", cur_cycle);
         $display ("    ", fshow (req));
      end
   endrule

   rule rl_handle_external_req_read_response;
      let x <- brvf_core.dm_dmi.read_data;
      let rsp = Control_Rsp {status: external_control_rsp_status_ok, result: signExtend (x)};
      f_external_control_rsps.enq (rsp);
      if (verbosity != 0) begin
	 $display ("%0d: SoC_Top.rl_handle_external_req_read_response", cur_cycle);
         $display ("    ", fshow (rsp));
      end
   endrule

   rule rl_handle_external_req_write (req.op == external_control_req_op_write_control_fabric);
      f_external_control_reqs.deq;
      brvf_core.dm_dmi.write (truncate (req.arg1), truncate (req.arg2));
      // let rsp = Control_Rsp {status: external_control_rsp_status_ok, result: 0};
      // f_external_control_rsps.enq (rsp);
      if (verbosity != 0) begin
         $display ("%0d: SoC_Top.rl_handle_external_req_write", cur_cycle);
         $display ("    ", fshow (req));
      end
   endrule

   rule rl_handle_external_req_err (   (req.op != external_control_req_op_read_control_fabric)
				    && (req.op != external_control_req_op_write_control_fabric));
      f_external_control_reqs.deq;
      let rsp = Control_Rsp {status: external_control_rsp_status_err, result: 0};
      f_external_control_rsps.enq (rsp);

      $display ("%0d: SoC_Top.rl_handle_external_req_err: unknown req.op", cur_cycle);
      $display ("    ", fshow (req));
   endrule

   // ----------------------------------------------------------------
   // NDM reset request (reset all except Debug Module) from debug module

   rule rl_reset_start (rg_state != SOC_RESETTING);
      let req <- brvf_core.dm_ndm_reset_req_get.get;

      brvf_core.cpu_reset_server.request.put (?);
      mem0_controller.server_reset.request.put (?);
      uart0.server_reset.request.put (?);
      timer0.server_reset.request.put (?);
      dotpro0.reset;

      fabric.reset;

      rg_state <= SOC_RESETTING;

      if (verbosity != 0) begin
         $display ("%0d: SoC_Top: ndm resetting (all except debug module) ...", cur_cycle);
      end
   endrule
`endif

   rule rl_reset_complete (rg_state == SOC_RESETTING);
      let cpu_rsp             <- brvf_core.cpu_reset_server.response.get;
      let mem0_controller_rsp <- mem0_controller.server_reset.response.get;
      let uart0_rsp           <- uart0.server_reset.response.get;
      let timer0_rsp          <- timer0.server_reset.response.get;

      // Initialize address maps of slave IPs
      boot_rom.set_addr_map (soc_map.m_boot_rom_addr_base,
			     soc_map.m_boot_rom_addr_lim);

      mem0_controller.set_addr_map (soc_map.m_mem0_controller_addr_base,
				    soc_map.m_mem0_controller_addr_lim);

      uart0.set_addr_map (soc_map.m_uart0_addr_base, soc_map.m_uart0_addr_lim);

      timer0.set_addr_map (soc_map.m_timer0_addr_base, soc_map.m_timer0_addr_lim);
      dotpro0.set_addr_map (soc_map.m_dotpro0_addr_base, soc_map.m_dotpro0_addr_lim);

      rg_state <= SOC_IDLE;

      if (verbosity != 0) begin
         $display ("%0d: SoC_Top: ndm resetting (all except debug module) ... complete", cur_cycle);
	 $display ("  Address map:");
	 $display ("  Boot ROM:        0x%0h .. 0x%0h", soc_map.m_boot_rom_addr_base,        soc_map.m_boot_rom_addr_lim);
	 $display ("  Mem0 Controller: 0x%0h .. 0x%0h", soc_map.m_mem0_controller_addr_base, soc_map.m_mem0_controller_addr_lim);
	 $display ("  Timer0:          0x%0h .. 0x%0h", soc_map.m_timer0_addr_base,          soc_map.m_timer0_addr_lim);
	 $display ("  UART0:           0x%0h .. 0x%0h", soc_map.m_uart0_addr_base,           soc_map.m_uart0_addr_lim);
	 $display ("  DOTPRO0:           0x%0h .. 0x%0h", soc_map.m_dotpro0_addr_base,           soc_map.m_dotpro0_addr_lim);
      end
   endrule

   // ================================================================
   // BEHAVIOR WITHOUT DEBUG MODULE

   rule rl_reset_start_2 (rg_state == SOC_START);
      brvf_core.cpu_reset_server.request.put (?);
      mem0_controller.server_reset.request.put (?);
      uart0.server_reset.request.put (?);
      timer0.server_reset.request.put (?);
      dotpro0.reset;

      fabric.reset;

      rg_state <= SOC_RESETTING;

      if (verbosity != 0) begin
         $display ("%0d: SoC_Top.rl_reset_start: ...", cur_cycle);
      end
   endrule

   // ================================================================
   // INTERFACE

   // To external controller (E.g., GDB)
`ifdef INCLUDE_GDB_CONTROL
   interface server_external_control = toGPServer (f_external_control_reqs, f_external_control_rsps);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface verify_out = brvf_core.tv_verifier_info_get;
`endif

   // External real memory
   interface to_raw_mem = mem0_controller.to_raw_mem;

   // UART to external console
   interface get_to_console   = uart0.get_to_console;
   interface put_from_console = uart0.put_from_console;
endmodule: mkSoC_Top

// ================================================================

endpackage
