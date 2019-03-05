/*
 * Generated by Bluespec Compiler, version 2017.03.beta1 (build 35049, 2017-03-16)
 * 
 * On Thu Nov 29 16:06:08 IST 2018
 * 
 */
#include "bluesim_primitives.h"
#include "mkBuffer.h"


/* String declarations */
static std::string const __str_literal_1("ERROR: %m: mkBRAMSeverAdapter overrun", 37u);


/* Constructor */
MOD_mkBuffer::MOD_mkBuffer(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST__unnamed_(simHdl, "_unnamed_", this, 16u, 0u, (tUInt8)0u),
    INST_memory_memory(simHdl, "memory_memory", this, (tUInt8)0u, 12u, 16u, 1024u, 2u),
    INST_memory_serverAdapterA_cnt(simHdl,
				   "memory_serverAdapterA_cnt",
				   this,
				   3u,
				   (tUInt8)0u,
				   (tUInt8)0u),
    INST_memory_serverAdapterA_cnt_1(simHdl, "memory_serverAdapterA_cnt_1", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterA_cnt_2(simHdl, "memory_serverAdapterA_cnt_2", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterA_cnt_3(simHdl, "memory_serverAdapterA_cnt_3", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterA_outDataCore(simHdl,
					   "memory_serverAdapterA_outDataCore",
					   this,
					   16u,
					   3u,
					   1u,
					   0u),
    INST_memory_serverAdapterA_outData_deqCalled(simHdl,
						 "memory_serverAdapterA_outData_deqCalled",
						 this,
						 0u),
    INST_memory_serverAdapterA_outData_enqData(simHdl,
					       "memory_serverAdapterA_outData_enqData",
					       this,
					       16u,
					       (tUInt8)0u),
    INST_memory_serverAdapterA_outData_outData(simHdl,
					       "memory_serverAdapterA_outData_outData",
					       this,
					       16u,
					       (tUInt8)0u),
    INST_memory_serverAdapterA_s1(simHdl, "memory_serverAdapterA_s1", this, 2u, (tUInt8)0u, (tUInt8)0u),
    INST_memory_serverAdapterA_s1_1(simHdl, "memory_serverAdapterA_s1_1", this, 2u, (tUInt8)0u),
    INST_memory_serverAdapterA_writeWithResp(simHdl,
					     "memory_serverAdapterA_writeWithResp",
					     this,
					     2u,
					     (tUInt8)0u),
    INST_memory_serverAdapterB_cnt(simHdl,
				   "memory_serverAdapterB_cnt",
				   this,
				   3u,
				   (tUInt8)0u,
				   (tUInt8)0u),
    INST_memory_serverAdapterB_cnt_1(simHdl, "memory_serverAdapterB_cnt_1", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterB_cnt_2(simHdl, "memory_serverAdapterB_cnt_2", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterB_cnt_3(simHdl, "memory_serverAdapterB_cnt_3", this, 3u, (tUInt8)0u),
    INST_memory_serverAdapterB_outDataCore(simHdl,
					   "memory_serverAdapterB_outDataCore",
					   this,
					   16u,
					   3u,
					   1u,
					   0u),
    INST_memory_serverAdapterB_outData_deqCalled(simHdl,
						 "memory_serverAdapterB_outData_deqCalled",
						 this,
						 0u),
    INST_memory_serverAdapterB_outData_enqData(simHdl,
					       "memory_serverAdapterB_outData_enqData",
					       this,
					       16u,
					       (tUInt8)0u),
    INST_memory_serverAdapterB_outData_outData(simHdl,
					       "memory_serverAdapterB_outData_outData",
					       this,
					       16u,
					       (tUInt8)0u),
    INST_memory_serverAdapterB_s1(simHdl, "memory_serverAdapterB_s1", this, 2u, (tUInt8)0u, (tUInt8)0u),
    INST_memory_serverAdapterB_s1_1(simHdl, "memory_serverAdapterB_s1_1", this, 2u, (tUInt8)0u),
    INST_memory_serverAdapterB_writeWithResp(simHdl,
					     "memory_serverAdapterB_writeWithResp",
					     this,
					     2u,
					     (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 46u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkBuffer::init_symbols_0()
{
  init_symbol(&symbols[0u], "_unnamed_", SYM_MODULE, &INST__unnamed_);
  init_symbol(&symbols[1u], "b__h2337", SYM_DEF, &DEF_b__h2337, 3u);
  init_symbol(&symbols[2u], "b__h969", SYM_DEF, &DEF_b__h969, 3u);
  init_symbol(&symbols[3u], "memory_memory", SYM_MODULE, &INST_memory_memory);
  init_symbol(&symbols[4u], "memory_serverAdapterA_cnt", SYM_MODULE, &INST_memory_serverAdapterA_cnt);
  init_symbol(&symbols[5u],
	      "memory_serverAdapterA_cnt_1",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_cnt_1);
  init_symbol(&symbols[6u],
	      "memory_serverAdapterA_cnt_2",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_cnt_2);
  init_symbol(&symbols[7u],
	      "memory_serverAdapterA_cnt_3",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_cnt_3);
  init_symbol(&symbols[8u],
	      "memory_serverAdapterA_outData_deqCalled",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_outData_deqCalled);
  init_symbol(&symbols[9u],
	      "memory_serverAdapterA_outData_enqData",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_outData_enqData);
  init_symbol(&symbols[10u],
	      "memory_serverAdapterA_outData_outData",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_outData_outData);
  init_symbol(&symbols[11u],
	      "memory_serverAdapterA_outDataCore",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_outDataCore);
  init_symbol(&symbols[12u], "memory_serverAdapterA_s1", SYM_MODULE, &INST_memory_serverAdapterA_s1);
  init_symbol(&symbols[13u],
	      "memory_serverAdapterA_s1_1",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_s1_1);
  init_symbol(&symbols[14u],
	      "memory_serverAdapterA_writeWithResp",
	      SYM_MODULE,
	      &INST_memory_serverAdapterA_writeWithResp);
  init_symbol(&symbols[15u],
	      "memory_serverAdapterB_cnt",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_cnt);
  init_symbol(&symbols[16u],
	      "memory_serverAdapterB_cnt_1",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_cnt_1);
  init_symbol(&symbols[17u],
	      "memory_serverAdapterB_cnt_2",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_cnt_2);
  init_symbol(&symbols[18u],
	      "memory_serverAdapterB_cnt_3",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_cnt_3);
  init_symbol(&symbols[19u],
	      "memory_serverAdapterB_outData_deqCalled",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_outData_deqCalled);
  init_symbol(&symbols[20u],
	      "memory_serverAdapterB_outData_enqData",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_outData_enqData);
  init_symbol(&symbols[21u],
	      "memory_serverAdapterB_outData_outData",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_outData_outData);
  init_symbol(&symbols[22u],
	      "memory_serverAdapterB_outDataCore",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_outDataCore);
  init_symbol(&symbols[23u], "memory_serverAdapterB_s1", SYM_MODULE, &INST_memory_serverAdapterB_s1);
  init_symbol(&symbols[24u],
	      "memory_serverAdapterB_s1_1",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_s1_1);
  init_symbol(&symbols[25u],
	      "memory_serverAdapterB_writeWithResp",
	      SYM_MODULE,
	      &INST_memory_serverAdapterB_writeWithResp);
  init_symbol(&symbols[26u], "RL_memory_serverAdapterA_cnt_finalAdd", SYM_RULE);
  init_symbol(&symbols[27u], "RL_memory_serverAdapterA_moveToOutFIFO", SYM_RULE);
  init_symbol(&symbols[28u], "RL_memory_serverAdapterA_outData_deqOnly", SYM_RULE);
  init_symbol(&symbols[29u], "RL_memory_serverAdapterA_outData_enqAndDeq", SYM_RULE);
  init_symbol(&symbols[30u], "RL_memory_serverAdapterA_outData_enqOnly", SYM_RULE);
  init_symbol(&symbols[31u], "RL_memory_serverAdapterA_outData_setFirstCore", SYM_RULE);
  init_symbol(&symbols[32u], "RL_memory_serverAdapterA_outData_setFirstEnq", SYM_RULE);
  init_symbol(&symbols[33u], "RL_memory_serverAdapterA_overRun", SYM_RULE);
  init_symbol(&symbols[34u], "RL_memory_serverAdapterA_s1__dreg_update", SYM_RULE);
  init_symbol(&symbols[35u], "RL_memory_serverAdapterA_stageReadResponseAlways", SYM_RULE);
  init_symbol(&symbols[36u], "RL_memory_serverAdapterB_cnt_finalAdd", SYM_RULE);
  init_symbol(&symbols[37u], "RL_memory_serverAdapterB_moveToOutFIFO", SYM_RULE);
  init_symbol(&symbols[38u], "RL_memory_serverAdapterB_outData_deqOnly", SYM_RULE);
  init_symbol(&symbols[39u], "RL_memory_serverAdapterB_outData_enqAndDeq", SYM_RULE);
  init_symbol(&symbols[40u], "RL_memory_serverAdapterB_outData_enqOnly", SYM_RULE);
  init_symbol(&symbols[41u], "RL_memory_serverAdapterB_outData_setFirstCore", SYM_RULE);
  init_symbol(&symbols[42u], "RL_memory_serverAdapterB_outData_setFirstEnq", SYM_RULE);
  init_symbol(&symbols[43u], "RL_memory_serverAdapterB_overRun", SYM_RULE);
  init_symbol(&symbols[44u], "RL_memory_serverAdapterB_s1__dreg_update", SYM_RULE);
  init_symbol(&symbols[45u], "RL_memory_serverAdapterB_stageReadResponseAlways", SYM_RULE);
}


/* Rule actions */

void MOD_mkBuffer::RL_memory_serverAdapterA_outData_setFirstCore()
{
  tUInt32 DEF_memory_serverAdapterA_outDataCore_first____d4;
  DEF_memory_serverAdapterA_outDataCore_first____d4 = INST_memory_serverAdapterA_outDataCore.METH_first();
  INST_memory_serverAdapterA_outData_outData.METH_wset(DEF_memory_serverAdapterA_outDataCore_first____d4);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_outData_setFirstEnq()
{
  DEF_x__h513 = INST_memory_serverAdapterA_outData_enqData.METH_wget();
  INST_memory_serverAdapterA_outData_outData.METH_wset(DEF_x__h513);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_outData_enqOnly()
{
  DEF_x__h513 = INST_memory_serverAdapterA_outData_enqData.METH_wget();
  INST_memory_serverAdapterA_outDataCore.METH_enq(DEF_x__h513);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_outData_deqOnly()
{
  INST_memory_serverAdapterA_outDataCore.METH_deq();
}

void MOD_mkBuffer::RL_memory_serverAdapterA_outData_enqAndDeq()
{
  DEF_x__h513 = INST_memory_serverAdapterA_outData_enqData.METH_wget();
  INST_memory_serverAdapterA_outDataCore.METH_enq(DEF_x__h513);
  INST_memory_serverAdapterA_outDataCore.METH_deq();
}

void MOD_mkBuffer::RL_memory_serverAdapterA_cnt_finalAdd()
{
  tUInt8 DEF_IF_memory_serverAdapterA_cnt_3_whas__3_THEN_me_ETC___d33;
  tUInt8 DEF_b__h949;
  DEF_b__h969 = INST_memory_serverAdapterA_cnt.METH_read();
  DEF_b__h949 = INST_memory_serverAdapterA_cnt_3.METH_wget();
  DEF_memory_serverAdapterA_cnt_3_whas____d23 = INST_memory_serverAdapterA_cnt_3.METH_whas();
  DEF_memory_serverAdapterA_cnt_2_whas____d21 = INST_memory_serverAdapterA_cnt_2.METH_whas();
  DEF_memory_serverAdapterA_cnt_1_whas____d20 = INST_memory_serverAdapterA_cnt_1.METH_whas();
  DEF_IF_memory_serverAdapterA_cnt_3_whas__3_THEN_me_ETC___d33 = DEF_memory_serverAdapterA_cnt_3_whas____d23 ? DEF_b__h949 : (tUInt8)7u & (((tUInt8)7u & (DEF_b__h969 + (DEF_memory_serverAdapterA_cnt_1_whas____d20 ? INST_memory_serverAdapterA_cnt_1.METH_wget() : (tUInt8)0u))) + (DEF_memory_serverAdapterA_cnt_2_whas____d21 ? INST_memory_serverAdapterA_cnt_2.METH_wget() : (tUInt8)0u));
  INST_memory_serverAdapterA_cnt.METH_write(DEF_IF_memory_serverAdapterA_cnt_3_whas__3_THEN_me_ETC___d33);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_s1__dreg_update()
{
  tUInt8 DEF_memory_serverAdapterA_s1_1_whas__4_AND_0_OR_me_ETC___d42;
  tUInt8 DEF_memory_serverAdapterA_s1_1_wget____d35;
  DEF_memory_serverAdapterA_s1_1_wget____d35 = INST_memory_serverAdapterA_s1_1.METH_wget();
  DEF_memory_serverAdapterA_s1_1_whas__4_AND_0_OR_me_ETC___d42 = (tUInt8)3u & (((INST_memory_serverAdapterA_s1_1.METH_whas() && (tUInt8)(DEF_memory_serverAdapterA_s1_1_wget____d35 >> 1u)) << 1u) | (tUInt8)((tUInt8)1u & DEF_memory_serverAdapterA_s1_1_wget____d35));
  INST_memory_serverAdapterA_s1.METH_write(DEF_memory_serverAdapterA_s1_1_whas__4_AND_0_OR_me_ETC___d42);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_stageReadResponseAlways()
{
  tUInt8 DEF_NOT_memory_serverAdapterA_writeWithResp_wget___ETC___d48;
  tUInt8 DEF__1_CONCAT_NOT_memory_serverAdapterA_writeWithRe_ETC___d49;
  tUInt8 DEF_ab_BIT_0___h1428;
  tUInt8 DEF_ab_BIT_1___h1424;
  tUInt8 DEF_ab__h1418;
  DEF_ab__h1418 = INST_memory_serverAdapterA_writeWithResp.METH_wget();
  DEF_ab_BIT_1___h1424 = (tUInt8)(DEF_ab__h1418 >> 1u);
  DEF_ab_BIT_0___h1428 = (tUInt8)((tUInt8)1u & DEF_ab__h1418);
  DEF_NOT_memory_serverAdapterA_writeWithResp_wget___ETC___d48 = !DEF_ab_BIT_1___h1424 || DEF_ab_BIT_0___h1428;
  DEF__1_CONCAT_NOT_memory_serverAdapterA_writeWithRe_ETC___d49 = (tUInt8)3u & (((tUInt8)1u << 1u) | DEF_NOT_memory_serverAdapterA_writeWithResp_wget___ETC___d48);
  INST_memory_serverAdapterA_s1_1.METH_wset(DEF__1_CONCAT_NOT_memory_serverAdapterA_writeWithRe_ETC___d49);
  if (DEF_NOT_memory_serverAdapterA_writeWithResp_wget___ETC___d48)
    INST_memory_serverAdapterA_cnt_1.METH_wset((tUInt8)1u);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_moveToOutFIFO()
{
  tUInt8 DEF_memory_serverAdapterA_s1_1_BIT_0___d54;
  tUInt32 DEF_x__h1536;
  DEF_x__h1536 = INST_memory_memory.METH_a_read();
  DEF_memory_serverAdapterA_s1___d51 = INST_memory_serverAdapterA_s1.METH_read();
  DEF_memory_serverAdapterA_s1_1_BIT_0___d54 = (tUInt8)((tUInt8)1u & DEF_memory_serverAdapterA_s1___d51);
  if (DEF_memory_serverAdapterA_s1_1_BIT_0___d54)
    INST_memory_serverAdapterA_outData_enqData.METH_wset(DEF_x__h1536);
}

void MOD_mkBuffer::RL_memory_serverAdapterA_overRun()
{
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_display(sim_hdl, this, "s", &__str_literal_1);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_outData_setFirstCore()
{
  tUInt32 DEF_memory_serverAdapterB_outDataCore_first____d61;
  DEF_memory_serverAdapterB_outDataCore_first____d61 = INST_memory_serverAdapterB_outDataCore.METH_first();
  INST_memory_serverAdapterB_outData_outData.METH_wset(DEF_memory_serverAdapterB_outDataCore_first____d61);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_outData_setFirstEnq()
{
  DEF_x__h1886 = INST_memory_serverAdapterB_outData_enqData.METH_wget();
  INST_memory_serverAdapterB_outData_outData.METH_wset(DEF_x__h1886);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_outData_enqOnly()
{
  DEF_x__h1886 = INST_memory_serverAdapterB_outData_enqData.METH_wget();
  INST_memory_serverAdapterB_outDataCore.METH_enq(DEF_x__h1886);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_outData_deqOnly()
{
  INST_memory_serverAdapterB_outDataCore.METH_deq();
}

void MOD_mkBuffer::RL_memory_serverAdapterB_outData_enqAndDeq()
{
  DEF_x__h1886 = INST_memory_serverAdapterB_outData_enqData.METH_wget();
  INST_memory_serverAdapterB_outDataCore.METH_enq(DEF_x__h1886);
  INST_memory_serverAdapterB_outDataCore.METH_deq();
}

void MOD_mkBuffer::RL_memory_serverAdapterB_cnt_finalAdd()
{
  tUInt8 DEF_IF_memory_serverAdapterB_cnt_3_whas__0_THEN_me_ETC___d90;
  tUInt8 DEF_b__h2317;
  DEF_b__h2337 = INST_memory_serverAdapterB_cnt.METH_read();
  DEF_b__h2317 = INST_memory_serverAdapterB_cnt_3.METH_wget();
  DEF_memory_serverAdapterB_cnt_3_whas____d80 = INST_memory_serverAdapterB_cnt_3.METH_whas();
  DEF_memory_serverAdapterB_cnt_2_whas____d78 = INST_memory_serverAdapterB_cnt_2.METH_whas();
  DEF_memory_serverAdapterB_cnt_1_whas____d77 = INST_memory_serverAdapterB_cnt_1.METH_whas();
  DEF_IF_memory_serverAdapterB_cnt_3_whas__0_THEN_me_ETC___d90 = DEF_memory_serverAdapterB_cnt_3_whas____d80 ? DEF_b__h2317 : (tUInt8)7u & (((tUInt8)7u & (DEF_b__h2337 + (DEF_memory_serverAdapterB_cnt_1_whas____d77 ? INST_memory_serverAdapterB_cnt_1.METH_wget() : (tUInt8)0u))) + (DEF_memory_serverAdapterB_cnt_2_whas____d78 ? INST_memory_serverAdapterB_cnt_2.METH_wget() : (tUInt8)0u));
  INST_memory_serverAdapterB_cnt.METH_write(DEF_IF_memory_serverAdapterB_cnt_3_whas__0_THEN_me_ETC___d90);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_s1__dreg_update()
{
  tUInt8 DEF_memory_serverAdapterB_s1_1_whas__1_AND_0_OR_me_ETC___d99;
  tUInt8 DEF_memory_serverAdapterB_s1_1_wget____d92;
  DEF_memory_serverAdapterB_s1_1_wget____d92 = INST_memory_serverAdapterB_s1_1.METH_wget();
  DEF_memory_serverAdapterB_s1_1_whas__1_AND_0_OR_me_ETC___d99 = (tUInt8)3u & (((INST_memory_serverAdapterB_s1_1.METH_whas() && (tUInt8)(DEF_memory_serverAdapterB_s1_1_wget____d92 >> 1u)) << 1u) | (tUInt8)((tUInt8)1u & DEF_memory_serverAdapterB_s1_1_wget____d92));
  INST_memory_serverAdapterB_s1.METH_write(DEF_memory_serverAdapterB_s1_1_whas__1_AND_0_OR_me_ETC___d99);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_stageReadResponseAlways()
{
  tUInt8 DEF_NOT_memory_serverAdapterB_writeWithResp_wget___ETC___d105;
  tUInt8 DEF__1_CONCAT_NOT_memory_serverAdapterB_writeWithRe_ETC___d106;
  tUInt8 DEF_ab_BIT_0___h2793;
  tUInt8 DEF_ab_BIT_1___h2789;
  tUInt8 DEF_ab__h2783;
  DEF_ab__h2783 = INST_memory_serverAdapterB_writeWithResp.METH_wget();
  DEF_ab_BIT_1___h2789 = (tUInt8)(DEF_ab__h2783 >> 1u);
  DEF_ab_BIT_0___h2793 = (tUInt8)((tUInt8)1u & DEF_ab__h2783);
  DEF_NOT_memory_serverAdapterB_writeWithResp_wget___ETC___d105 = !DEF_ab_BIT_1___h2789 || DEF_ab_BIT_0___h2793;
  DEF__1_CONCAT_NOT_memory_serverAdapterB_writeWithRe_ETC___d106 = (tUInt8)3u & (((tUInt8)1u << 1u) | DEF_NOT_memory_serverAdapterB_writeWithResp_wget___ETC___d105);
  INST_memory_serverAdapterB_s1_1.METH_wset(DEF__1_CONCAT_NOT_memory_serverAdapterB_writeWithRe_ETC___d106);
  if (DEF_NOT_memory_serverAdapterB_writeWithResp_wget___ETC___d105)
    INST_memory_serverAdapterB_cnt_1.METH_wset((tUInt8)1u);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_moveToOutFIFO()
{
  tUInt8 DEF_memory_serverAdapterB_s1_08_BIT_0___d111;
  tUInt32 DEF_x__h2899;
  DEF_x__h2899 = INST_memory_memory.METH_b_read();
  DEF_memory_serverAdapterB_s1___d108 = INST_memory_serverAdapterB_s1.METH_read();
  DEF_memory_serverAdapterB_s1_08_BIT_0___d111 = (tUInt8)((tUInt8)1u & DEF_memory_serverAdapterB_s1___d108);
  if (DEF_memory_serverAdapterB_s1_08_BIT_0___d111)
    INST_memory_serverAdapterB_outData_enqData.METH_wset(DEF_x__h2899);
}

void MOD_mkBuffer::RL_memory_serverAdapterB_overRun()
{
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_display(sim_hdl, this, "s", &__str_literal_1);
}


/* Methods */

void MOD_mkBuffer::METH_enq(tUInt32 ARG_enq_val, tUInt32 ARG_enq_c)
{
  INST_memory_memory.METH_a_put((tUInt8)1u, ARG_enq_c, ARG_enq_val);
  INST_memory_serverAdapterA_writeWithResp.METH_wset((tUInt8)2u);
}

tUInt8 MOD_mkBuffer::METH_RDY_enq()
{
  tUInt8 DEF_CAN_FIRE_enq;
  tUInt8 PORT_RDY_enq;
  DEF_b__h969 = INST_memory_serverAdapterA_cnt.METH_read();
  DEF_CAN_FIRE_enq = primSLT8(1u, 3u, (tUInt8)(DEF_b__h969), 3u, (tUInt8)3u);
  PORT_RDY_enq = DEF_CAN_FIRE_enq;
  return PORT_RDY_enq;
}

void MOD_mkBuffer::METH_latchData()
{
  DEF_memory_serverAdapterB_outData_outData_wget____d115 = INST_memory_serverAdapterB_outData_outData.METH_wget();
  INST_memory_serverAdapterB_outData_deqCalled.METH_wset();
  INST_memory_serverAdapterB_cnt_2.METH_wset((tUInt8)7u);
  INST__unnamed_.METH_write(DEF_memory_serverAdapterB_outData_outData_wget____d115);
}

tUInt8 MOD_mkBuffer::METH_RDY_latchData()
{
  tUInt8 DEF_CAN_FIRE_latchData;
  tUInt8 PORT_RDY_latchData;
  DEF_memory_serverAdapterB_outData_enqData_whas____d63 = INST_memory_serverAdapterB_outData_enqData.METH_whas();
  DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 = INST_memory_serverAdapterB_outDataCore.METH_notEmpty();
  DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118 = (DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 || DEF_memory_serverAdapterB_outData_enqData_whas____d63) && INST_memory_serverAdapterB_outData_outData.METH_whas();
  DEF_CAN_FIRE_latchData = DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118;
  PORT_RDY_latchData = DEF_CAN_FIRE_latchData;
  return PORT_RDY_latchData;
}

void MOD_mkBuffer::METH_deq(tUInt32 ARG_deq_c)
{
  INST_memory_memory.METH_b_put((tUInt8)0u, ARG_deq_c, 0u);
  INST_memory_serverAdapterB_writeWithResp.METH_wset((tUInt8)0u);
}

tUInt8 MOD_mkBuffer::METH_RDY_deq()
{
  tUInt8 DEF_CAN_FIRE_deq;
  tUInt8 PORT_RDY_deq;
  DEF_b__h2337 = INST_memory_serverAdapterB_cnt.METH_read();
  DEF_CAN_FIRE_deq = primSLT8(1u, 3u, (tUInt8)(DEF_b__h2337), 3u, (tUInt8)3u);
  PORT_RDY_deq = DEF_CAN_FIRE_deq;
  return PORT_RDY_deq;
}

void MOD_mkBuffer::METH_clean()
{
  DEF_memory_serverAdapterB_outData_outData_wget____d115 = INST_memory_serverAdapterB_outData_outData.METH_wget();
  INST_memory_serverAdapterB_outData_deqCalled.METH_wset();
  INST_memory_serverAdapterB_cnt_2.METH_wset((tUInt8)7u);
  INST__unnamed_.METH_write(DEF_memory_serverAdapterB_outData_outData_wget____d115);
}

tUInt8 MOD_mkBuffer::METH_RDY_clean()
{
  tUInt8 DEF_CAN_FIRE_clean;
  tUInt8 PORT_RDY_clean;
  DEF_memory_serverAdapterB_outData_enqData_whas____d63 = INST_memory_serverAdapterB_outData_enqData.METH_whas();
  DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 = INST_memory_serverAdapterB_outDataCore.METH_notEmpty();
  DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118 = (DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 || DEF_memory_serverAdapterB_outData_enqData_whas____d63) && INST_memory_serverAdapterB_outData_outData.METH_whas();
  DEF_CAN_FIRE_clean = DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118;
  PORT_RDY_clean = DEF_CAN_FIRE_clean;
  return PORT_RDY_clean;
}

tUInt32 MOD_mkBuffer::METH_get()
{
  tUInt32 PORT_get;
  PORT_get = INST__unnamed_.METH_read();
  return PORT_get;
}

tUInt8 MOD_mkBuffer::METH_RDY_get()
{
  tUInt8 DEF_CAN_FIRE_get;
  tUInt8 PORT_RDY_get;
  DEF_CAN_FIRE_get = (tUInt8)1u;
  PORT_RDY_get = DEF_CAN_FIRE_get;
  return PORT_RDY_get;
}


/* Reset routines */

void MOD_mkBuffer::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_memory_serverAdapterB_s1.reset_RST(ARG_rst_in);
  INST_memory_serverAdapterB_outDataCore.reset_RST(ARG_rst_in);
  INST_memory_serverAdapterB_cnt.reset_RST(ARG_rst_in);
  INST_memory_serverAdapterA_s1.reset_RST(ARG_rst_in);
  INST_memory_serverAdapterA_outDataCore.reset_RST(ARG_rst_in);
  INST_memory_serverAdapterA_cnt.reset_RST(ARG_rst_in);
  INST__unnamed_.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkBuffer::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkBuffer::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST__unnamed_.dump_state(indent + 2u);
  INST_memory_memory.dump_state(indent + 2u);
  INST_memory_serverAdapterA_cnt.dump_state(indent + 2u);
  INST_memory_serverAdapterA_cnt_1.dump_state(indent + 2u);
  INST_memory_serverAdapterA_cnt_2.dump_state(indent + 2u);
  INST_memory_serverAdapterA_cnt_3.dump_state(indent + 2u);
  INST_memory_serverAdapterA_outDataCore.dump_state(indent + 2u);
  INST_memory_serverAdapterA_outData_deqCalled.dump_state(indent + 2u);
  INST_memory_serverAdapterA_outData_enqData.dump_state(indent + 2u);
  INST_memory_serverAdapterA_outData_outData.dump_state(indent + 2u);
  INST_memory_serverAdapterA_s1.dump_state(indent + 2u);
  INST_memory_serverAdapterA_s1_1.dump_state(indent + 2u);
  INST_memory_serverAdapterA_writeWithResp.dump_state(indent + 2u);
  INST_memory_serverAdapterB_cnt.dump_state(indent + 2u);
  INST_memory_serverAdapterB_cnt_1.dump_state(indent + 2u);
  INST_memory_serverAdapterB_cnt_2.dump_state(indent + 2u);
  INST_memory_serverAdapterB_cnt_3.dump_state(indent + 2u);
  INST_memory_serverAdapterB_outDataCore.dump_state(indent + 2u);
  INST_memory_serverAdapterB_outData_deqCalled.dump_state(indent + 2u);
  INST_memory_serverAdapterB_outData_enqData.dump_state(indent + 2u);
  INST_memory_serverAdapterB_outData_outData.dump_state(indent + 2u);
  INST_memory_serverAdapterB_s1.dump_state(indent + 2u);
  INST_memory_serverAdapterB_s1_1.dump_state(indent + 2u);
  INST_memory_serverAdapterB_writeWithResp.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkBuffer::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 41u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "b__h2337", 3u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "b__h969", 3u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterA_cnt_1_whas____d20", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterA_cnt_2_whas____d21", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterA_cnt_3_whas____d23", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterA_s1___d51", 2u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_cnt_1_whas____d77", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_cnt_2_whas____d78", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_cnt_3_whas____d80", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_outDataCore_notEmpty____d59", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_outData_enqData_whas____d63", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_outData_outData_wget____d115", 16u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memory_serverAdapterB_s1___d108", 2u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "x__h1886", 16u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "x__h513", 16u);
  num = INST__unnamed_.dump_VCD_defs(num);
  num = INST_memory_memory.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_cnt.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_cnt_1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_cnt_2.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_cnt_3.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_outDataCore.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_outData_deqCalled.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_outData_enqData.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_outData_outData.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_s1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_s1_1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterA_writeWithResp.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_cnt.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_cnt_1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_cnt_2.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_cnt_3.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_outDataCore.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_outData_deqCalled.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_outData_enqData.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_outData_outData.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_s1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_s1_1.dump_VCD_defs(num);
  num = INST_memory_serverAdapterB_writeWithResp.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkBuffer::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkBuffer &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkBuffer::vcd_defs(tVCDDumpType dt, MOD_mkBuffer &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 3u);
    vcd_write_x(sim_hdl, num++, 3u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 2u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 16u);
    vcd_write_x(sim_hdl, num++, 2u);
    vcd_write_x(sim_hdl, num++, 16u);
    vcd_write_x(sim_hdl, num++, 16u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
      if ((backing.DEF_b__h2337) != DEF_b__h2337)
      {
	vcd_write_val(sim_hdl, num, DEF_b__h2337, 3u);
	backing.DEF_b__h2337 = DEF_b__h2337;
      }
      ++num;
      if ((backing.DEF_b__h969) != DEF_b__h969)
      {
	vcd_write_val(sim_hdl, num, DEF_b__h969, 3u);
	backing.DEF_b__h969 = DEF_b__h969;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterA_cnt_1_whas____d20) != DEF_memory_serverAdapterA_cnt_1_whas____d20)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterA_cnt_1_whas____d20, 1u);
	backing.DEF_memory_serverAdapterA_cnt_1_whas____d20 = DEF_memory_serverAdapterA_cnt_1_whas____d20;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterA_cnt_2_whas____d21) != DEF_memory_serverAdapterA_cnt_2_whas____d21)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterA_cnt_2_whas____d21, 1u);
	backing.DEF_memory_serverAdapterA_cnt_2_whas____d21 = DEF_memory_serverAdapterA_cnt_2_whas____d21;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterA_cnt_3_whas____d23) != DEF_memory_serverAdapterA_cnt_3_whas____d23)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterA_cnt_3_whas____d23, 1u);
	backing.DEF_memory_serverAdapterA_cnt_3_whas____d23 = DEF_memory_serverAdapterA_cnt_3_whas____d23;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterA_s1___d51) != DEF_memory_serverAdapterA_s1___d51)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterA_s1___d51, 2u);
	backing.DEF_memory_serverAdapterA_s1___d51 = DEF_memory_serverAdapterA_s1___d51;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_cnt_1_whas____d77) != DEF_memory_serverAdapterB_cnt_1_whas____d77)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_cnt_1_whas____d77, 1u);
	backing.DEF_memory_serverAdapterB_cnt_1_whas____d77 = DEF_memory_serverAdapterB_cnt_1_whas____d77;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_cnt_2_whas____d78) != DEF_memory_serverAdapterB_cnt_2_whas____d78)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_cnt_2_whas____d78, 1u);
	backing.DEF_memory_serverAdapterB_cnt_2_whas____d78 = DEF_memory_serverAdapterB_cnt_2_whas____d78;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_cnt_3_whas____d80) != DEF_memory_serverAdapterB_cnt_3_whas____d80)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_cnt_3_whas____d80, 1u);
	backing.DEF_memory_serverAdapterB_cnt_3_whas____d80 = DEF_memory_serverAdapterB_cnt_3_whas____d80;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118) != DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118, 1u);
	backing.DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118 = DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_outDataCore_notEmpty____d59) != DEF_memory_serverAdapterB_outDataCore_notEmpty____d59)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_outDataCore_notEmpty____d59, 1u);
	backing.DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 = DEF_memory_serverAdapterB_outDataCore_notEmpty____d59;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_outData_enqData_whas____d63) != DEF_memory_serverAdapterB_outData_enqData_whas____d63)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_outData_enqData_whas____d63, 1u);
	backing.DEF_memory_serverAdapterB_outData_enqData_whas____d63 = DEF_memory_serverAdapterB_outData_enqData_whas____d63;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_outData_outData_wget____d115) != DEF_memory_serverAdapterB_outData_outData_wget____d115)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_outData_outData_wget____d115, 16u);
	backing.DEF_memory_serverAdapterB_outData_outData_wget____d115 = DEF_memory_serverAdapterB_outData_outData_wget____d115;
      }
      ++num;
      if ((backing.DEF_memory_serverAdapterB_s1___d108) != DEF_memory_serverAdapterB_s1___d108)
      {
	vcd_write_val(sim_hdl, num, DEF_memory_serverAdapterB_s1___d108, 2u);
	backing.DEF_memory_serverAdapterB_s1___d108 = DEF_memory_serverAdapterB_s1___d108;
      }
      ++num;
      if ((backing.DEF_x__h1886) != DEF_x__h1886)
      {
	vcd_write_val(sim_hdl, num, DEF_x__h1886, 16u);
	backing.DEF_x__h1886 = DEF_x__h1886;
      }
      ++num;
      if ((backing.DEF_x__h513) != DEF_x__h513)
      {
	vcd_write_val(sim_hdl, num, DEF_x__h513, 16u);
	backing.DEF_x__h513 = DEF_x__h513;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF_b__h2337, 3u);
      backing.DEF_b__h2337 = DEF_b__h2337;
      vcd_write_val(sim_hdl, num++, DEF_b__h969, 3u);
      backing.DEF_b__h969 = DEF_b__h969;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterA_cnt_1_whas____d20, 1u);
      backing.DEF_memory_serverAdapterA_cnt_1_whas____d20 = DEF_memory_serverAdapterA_cnt_1_whas____d20;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterA_cnt_2_whas____d21, 1u);
      backing.DEF_memory_serverAdapterA_cnt_2_whas____d21 = DEF_memory_serverAdapterA_cnt_2_whas____d21;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterA_cnt_3_whas____d23, 1u);
      backing.DEF_memory_serverAdapterA_cnt_3_whas____d23 = DEF_memory_serverAdapterA_cnt_3_whas____d23;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterA_s1___d51, 2u);
      backing.DEF_memory_serverAdapterA_s1___d51 = DEF_memory_serverAdapterA_s1___d51;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_cnt_1_whas____d77, 1u);
      backing.DEF_memory_serverAdapterB_cnt_1_whas____d77 = DEF_memory_serverAdapterB_cnt_1_whas____d77;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_cnt_2_whas____d78, 1u);
      backing.DEF_memory_serverAdapterB_cnt_2_whas____d78 = DEF_memory_serverAdapterB_cnt_2_whas____d78;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_cnt_3_whas____d80, 1u);
      backing.DEF_memory_serverAdapterB_cnt_3_whas____d80 = DEF_memory_serverAdapterB_cnt_3_whas____d80;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118, 1u);
      backing.DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118 = DEF_memory_serverAdapterB_outDataCore_notEmpty__9__ETC___d118;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_outDataCore_notEmpty____d59, 1u);
      backing.DEF_memory_serverAdapterB_outDataCore_notEmpty____d59 = DEF_memory_serverAdapterB_outDataCore_notEmpty____d59;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_outData_enqData_whas____d63, 1u);
      backing.DEF_memory_serverAdapterB_outData_enqData_whas____d63 = DEF_memory_serverAdapterB_outData_enqData_whas____d63;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_outData_outData_wget____d115, 16u);
      backing.DEF_memory_serverAdapterB_outData_outData_wget____d115 = DEF_memory_serverAdapterB_outData_outData_wget____d115;
      vcd_write_val(sim_hdl, num++, DEF_memory_serverAdapterB_s1___d108, 2u);
      backing.DEF_memory_serverAdapterB_s1___d108 = DEF_memory_serverAdapterB_s1___d108;
      vcd_write_val(sim_hdl, num++, DEF_x__h1886, 16u);
      backing.DEF_x__h1886 = DEF_x__h1886;
      vcd_write_val(sim_hdl, num++, DEF_x__h513, 16u);
      backing.DEF_x__h513 = DEF_x__h513;
    }
}

void MOD_mkBuffer::vcd_prims(tVCDDumpType dt, MOD_mkBuffer &backing)
{
  INST__unnamed_.dump_VCD(dt, backing.INST__unnamed_);
  INST_memory_memory.dump_VCD(dt, backing.INST_memory_memory);
  INST_memory_serverAdapterA_cnt.dump_VCD(dt, backing.INST_memory_serverAdapterA_cnt);
  INST_memory_serverAdapterA_cnt_1.dump_VCD(dt, backing.INST_memory_serverAdapterA_cnt_1);
  INST_memory_serverAdapterA_cnt_2.dump_VCD(dt, backing.INST_memory_serverAdapterA_cnt_2);
  INST_memory_serverAdapterA_cnt_3.dump_VCD(dt, backing.INST_memory_serverAdapterA_cnt_3);
  INST_memory_serverAdapterA_outDataCore.dump_VCD(dt, backing.INST_memory_serverAdapterA_outDataCore);
  INST_memory_serverAdapterA_outData_deqCalled.dump_VCD(dt,
							backing.INST_memory_serverAdapterA_outData_deqCalled);
  INST_memory_serverAdapterA_outData_enqData.dump_VCD(dt,
						      backing.INST_memory_serverAdapterA_outData_enqData);
  INST_memory_serverAdapterA_outData_outData.dump_VCD(dt,
						      backing.INST_memory_serverAdapterA_outData_outData);
  INST_memory_serverAdapterA_s1.dump_VCD(dt, backing.INST_memory_serverAdapterA_s1);
  INST_memory_serverAdapterA_s1_1.dump_VCD(dt, backing.INST_memory_serverAdapterA_s1_1);
  INST_memory_serverAdapterA_writeWithResp.dump_VCD(dt,
						    backing.INST_memory_serverAdapterA_writeWithResp);
  INST_memory_serverAdapterB_cnt.dump_VCD(dt, backing.INST_memory_serverAdapterB_cnt);
  INST_memory_serverAdapterB_cnt_1.dump_VCD(dt, backing.INST_memory_serverAdapterB_cnt_1);
  INST_memory_serverAdapterB_cnt_2.dump_VCD(dt, backing.INST_memory_serverAdapterB_cnt_2);
  INST_memory_serverAdapterB_cnt_3.dump_VCD(dt, backing.INST_memory_serverAdapterB_cnt_3);
  INST_memory_serverAdapterB_outDataCore.dump_VCD(dt, backing.INST_memory_serverAdapterB_outDataCore);
  INST_memory_serverAdapterB_outData_deqCalled.dump_VCD(dt,
							backing.INST_memory_serverAdapterB_outData_deqCalled);
  INST_memory_serverAdapterB_outData_enqData.dump_VCD(dt,
						      backing.INST_memory_serverAdapterB_outData_enqData);
  INST_memory_serverAdapterB_outData_outData.dump_VCD(dt,
						      backing.INST_memory_serverAdapterB_outData_outData);
  INST_memory_serverAdapterB_s1.dump_VCD(dt, backing.INST_memory_serverAdapterB_s1);
  INST_memory_serverAdapterB_s1_1.dump_VCD(dt, backing.INST_memory_serverAdapterB_s1_1);
  INST_memory_serverAdapterB_writeWithResp.dump_VCD(dt,
						    backing.INST_memory_serverAdapterB_writeWithResp);
}